#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

// ---- Common types ----------------------------------------------------------

enum class MutFlag {
  Imm,
  Mut
}; // was 'imm'/'mut' in Haskell; must be capitalized in C++

struct Pos {
  int line{1};
  int col{1};
};

struct Diag : std::runtime_error {
  Pos pos;
  explicit Diag(Pos p, const std::string &msg)
      : std::runtime_error(msg), pos(p) {}
};

// ---- Lexer -----------------------------------------------------------------

enum class TokKind {
  Eof,
  Ident,
  Int,
  Sym, // single char symbols: (){}[],;=+-*/<>, etc.
  Kw_let,
  Kw_mut,
  Kw_fn,
  Kw_return
  // add more keywords as needed
};

struct Tok {
  TokKind kind;
  std::string lex;
  Pos pos{};
};

struct Lexer {
  std::string src;
  size_t i{0};
  Pos p{1, 1};

  explicit Lexer(std::string s) : src(std::move(s)) {}

  bool done() const { return i >= src.size(); }
  char ch() const { return done() ? '\0' : src[i]; }

  void bump() {
    if (done())
      return;
    if (src[i] == '\n') {
      p.line++;
      p.col = 1;
    } else {
      p.col++;
    }
    ++i;
  }

  static bool isIdentStart(char c) {
    return std::isalpha(static_cast<unsigned char>(c)) || c == '_' || c == '$';
  }
  static bool isIdentCont(char c) {
    return std::isalnum(static_cast<unsigned char>(c)) || c == '_' || c == '$';
  }

  void skipWSandComments() {
    for (;;) {
      // whitespace
      while (std::isspace(static_cast<unsigned char>(ch())))
        bump();

      // line comments: // -- #
      if (ch() == '/' && i + 1 < src.size() && src[i + 1] == '/') {
        while (!done() && ch() != '\n')
          bump();
        continue;
      }
      if (ch() == '-' && i + 1 < src.size() && src[i + 1] == '-') {
        while (!done() && ch() != '\n')
          bump();
        continue;
      }
      if (ch() == '#') {
        while (!done() && ch() != '\n')
          bump();
        continue;
      }
      // "rem ..." whole line
      if (matchKeywordAhead("rem")) {
        // only if at start of token boundary
        size_t j = i + 3;
        while (j < src.size() && (src[j] == ' ' || src[j] == '\t'))
          ++j;
        // consume until EOL
        i = j;
        while (!done() && ch() != '\n')
          bump();
        continue;
      }
      break;
    }
  }

  bool matchKeywordAhead(std::string_view kw) const {
    // case-insensitive
    size_t j = i;
    for (char c : kw) {
      if (j >= src.size())
        return false;
      char d = src[j];
      if (std::tolower(static_cast<unsigned char>(d)) !=
          std::tolower(static_cast<unsigned char>(c)))
        return false;
      ++j;
    }
    // ensure not part of a longer identifier
    if (j < src.size() && isIdentCont(src[j]))
      return false;
    return true;
  }

  Tok next() {
    skipWSandComments();
    Pos start = p;
    if (done())
      return {TokKind::Eof, "", start};

    char c = ch();

    // identifiers / keywords
    if (isIdentStart(c)) {
      std::string s;
      s.push_back(c);
      bump();
      while (isIdentCont(ch())) {
        s.push_back(ch());
        bump();
      }

      // lowercase copy for keyword matching
      std::string low = s;
      std::transform(low.begin(), low.end(), low.begin(),
                     [](unsigned char x) { return std::tolower(x); });
      if (low == "let")
        return {TokKind::Kw_let, s, start};
      if (low == "mut")
        return {TokKind::Kw_mut, s, start};
      if (low == "fn")
        return {TokKind::Kw_fn, s, start};
      if (low == "return")
        return {TokKind::Kw_return, s, start};
      return {TokKind::Ident, s, start};
    }

    // integer literals (decimal)
    if (std::isdigit(static_cast<unsigned char>(c))) {
      std::string s;
      while (std::isdigit(static_cast<unsigned char>(ch()))) {
        s.push_back(ch());
        bump();
      }
      return {TokKind::Int, s, start};
    }

    // single-symbol tokens
    std::string s(1, c);
    bump();
    return {TokKind::Sym, s, start};
  }
};

// ---- AST -------------------------------------------------------------------

struct Expr {
  // You’ll expand this as needed
  enum class Tag { Int, Var, Add, Sub, Mul, Div } tag;
  Pos pos{};
  int64_t val{};
  std::string name;
  std::vector<Expr> args;

  static Expr Int(Pos p, int64_t v) { return Expr{Tag::Int, p, v, {}, {}}; }
  static Expr Var(Pos p, std::string n) {
    Expr e;
    e.tag = Tag::Var;
    e.pos = p;
    e.name = std::move(n);
    return e;
  }
  static Expr Bin(Pos p, Tag t, Expr a, Expr b) {
    Expr e;
    e.tag = t;
    e.pos = p;
    e.args.reserve(2);
    e.args.push_back(std::move(a));
    e.args.push_back(std::move(b));
    return e;
  }
};

struct Stmt {
  enum class Tag { Let, Return, ExprStmt } tag;
  Pos pos{};
  // let name (= expr); optional mut
  std::string name;
  MutFlag mut{MutFlag::Imm};
  Expr expr{};
  // return expr
  static Stmt Let(Pos p, std::string n, MutFlag m, Expr rhs) {
    Stmt s;
    s.tag = Tag::Let;
    s.pos = p;
    s.name = std::move(n);
    s.mut = m;
    s.expr = std::move(rhs);
    return s;
  }
  static Stmt Ret(Pos p, Expr e) {
    Stmt s;
    s.tag = Tag::Return;
    s.pos = p;
    s.expr = std::move(e);
    return s;
  }
  static Stmt ExprOnly(Pos p, Expr e) {
    Stmt s;
    s.tag = Tag::ExprStmt;
    s.pos = p;
    s.expr = std::move(e);
    return s;
  }
};

struct Program {
  std::vector<Stmt> stmts;
};

// ---- Parser (toy grammar) --------------------------------------------------
// Grammar (example):
//   program  := (stmt)* EOF
//   stmt     := 'let' ['mut'] ident '=' expr ';'
//             | 'return' expr ';'
//             | expr ';'
//   expr     := term (('+'|'-') term)*
//   term     := factor (('*'|'/') factor)*
//   factor   := INT | IDENT | '(' expr ')'

struct Parser {
  Lexer lx;
  Tok cur;

  explicit Parser(std::string s) : lx(std::move(s)) { cur = lx.next(); }

  [[noreturn]] void errHere(const std::string &m) { throw Diag(cur.pos, m); }

  bool is(TokKind k) const { return cur.kind == k; }
  bool sym(std::string_view s) const {
    return cur.kind == TokKind::Sym && cur.lex == s;
  }

  Tok take() {
    Tok t = cur;
    cur = lx.next();
    return t;
  }
  Tok expect(TokKind k, const char *m) {
    if (!is(k))
      errHere(m);
    return take();
  }
  void expectSym(const char *s, const char *m) {
    if (!sym(s))
      errHere(m);
    take();
  }

  Program parseProgram() {
    Program p;
    while (!is(TokKind::Eof)) {
      p.stmts.push_back(parseStmt());
    }
    return p;
  }

  Stmt parseStmt() {
    if (is(TokKind::Kw_let)) {
      Pos p = cur.pos;
      take();
      MutFlag mf = MutFlag::Imm;
      if (is(TokKind::Kw_mut)) {
        mf = MutFlag::Mut;
        take();
      }
      Tok id = expect(TokKind::Ident, "expected identifier after let");
      expectSym("=", "expected '='");
      Expr e = parseExpr();
      expectSym(";", "expected ';'");
      return Stmt::Let(p, id.lex, mf, std::move(e));
    }
    if (is(TokKind::Kw_return)) {
      Pos p = cur.pos;
      take();
      Expr e = parseExpr();
      expectSym(";", "expected ';'");
      return Stmt::Ret(p, std::move(e));
    }
    // fallback expr stmt
    Pos p = cur.pos;
    Expr e = parseExpr();
    expectSym(";", "expected ';'");
    return Stmt::ExprOnly(p, std::move(e));
  }

  Expr parseExpr() {
    Expr lhs = parseTerm();
    while (sym("+") || sym("-")) {
      Tok op = take();
      Expr rhs = parseTerm();
      if (op.lex == "+")
        lhs = Expr::Bin(op.pos, Expr::Tag::Add, std::move(lhs), std::move(rhs));
      else
        lhs = Expr::Bin(op.pos, Expr::Tag::Sub, std::move(lhs), std::move(rhs));
    }
    return lhs;
  }

  Expr parseTerm() {
    Expr lhs = parseFactor();
    while (sym("*") || sym("/")) {
      Tok op = take();
      Expr rhs = parseFactor();
      if (op.lex == "*")
        lhs = Expr::Bin(op.pos, Expr::Tag::Mul, std::move(lhs), std::move(rhs));
      else
        lhs = Expr::Bin(op.pos, Expr::Tag::Div, std::move(lhs), std::move(rhs));
    }
    return lhs;
  }

  Expr parseFactor() {
    if (is(TokKind::Int)) {
      Tok t = take();
      return Expr::Int(t.pos, std::stoll(t.lex));
    }
    if (is(TokKind::Ident)) {
      Tok t = take();
      return Expr::Var(t.pos, t.lex);
    }
    if (sym("(")) {
      take();
      Expr e = parseExpr();
      expectSym(")", "expected ')'");
      return e;
    }
    errHere("expected expression");
  }
};

// ---- Semantic check / IR (placeholder) ------------------------------------

struct EnvEntry {
  MutFlag mut;
  int64_t value{0};
  bool hasValue{false};
};

struct Env {
  std::vector<std::pair<std::string, EnvEntry>> table;

  EnvEntry *find(std::string_view name) {
    for (auto &kv : table)
      if (kv.first == name)
        return &kv.second;
    return nullptr;
  }
  EnvEntry &bind(const std::string &name, MutFlag mf) {
    if (find(name))
      throw std::runtime_error("duplicate variable: " + name);
    table.push_back({name, EnvEntry{mf, 0, false}});
    return table.back().second;
  }
};

// very tiny evaluator to demonstrate the pipeline
int64_t evalExpr(const Expr &e, Env &env) {
  switch (e.tag) {
  case Expr::Tag::Int:
    return e.val;
  case Expr::Tag::Var: {
    auto *ent = env.find(e.name);
    if (!ent || !ent->hasValue)
      throw std::runtime_error("unbound variable: " + e.name);
    return ent->value;
  }
  case Expr::Tag::Add:
    return evalExpr(e.args[0], env) + evalExpr(e.args[1], env);
  case Expr::Tag::Sub:
    return evalExpr(e.args[0], env) - evalExpr(e.args[1], env);
  case Expr::Tag::Mul:
    return evalExpr(e.args[0], env) * evalExpr(e.args[1], env);
  case Expr::Tag::Div: {
    auto b = evalExpr(e.args[1], env);
    if (b == 0)
      throw std::runtime_error("division by zero");
    return evalExpr(e.args[0], env) / b;
  }
  }
  return 0;
}

// ---- “Codegen” placeholder --------------------------------------------------
// Instead of generating machine code, we’ll just “interpret” to prove the
// pipeline. Replace this with actual code emission.

int compileAndRun(const Program &prog) {
  Env env;
  int64_t last = 0;
  for (auto &s : prog.stmts) {
    switch (s.tag) {
    case Stmt::Tag::Let: {
      auto &e = env.bind(s.name, s.mut);
      e.value = evalExpr(s.expr, env);
      e.hasValue = true;
      break;
    }
    case Stmt::Tag::Return: {
      last = evalExpr(s.expr, env);
      return static_cast<int>(last);
    }
    case Stmt::Tag::ExprStmt: {
      last = evalExpr(s.expr, env);
      break;
    }
    }
  }
  return static_cast<int>(last);
}

// ---- CLI -------------------------------------------------------------------

static std::string readFileOrDie(const std::string &path) {
  std::ifstream ifs(path, std::ios::binary);
  if (!ifs) {
    std::ostringstream oss;
    oss << "cannot open file: " << path << " (errno " << errno << ")";
    throw std::runtime_error(oss.str());
  }
  std::ostringstream oss;
  oss << ifs.rdbuf();
  return oss.str();
}

int main(int argc, char **argv) {
  try {
    if (argc < 2) {
      std::cerr << "Usage: blinkc <input> [-o out]\n";
      return 2;
    }
    std::string in = argv[1];
    std::string out; // (not used in this placeholder)
    for (int i = 2; i < argc; ++i) {
      std::string arg = argv[i];
      if (arg == "-o" && i + 1 < argc) {
        out = argv[++i];
      } else {
        std::cerr << "Unknown arg: " << arg << "\n";
        return 2;
      }
    }

    std::string src = readFileOrDie(in);
    Parser ps(src);
    Program prog = ps.parseProgram();

    // For now, just evaluate and print a result.
    int rc = compileAndRun(prog);
    std::cout << rc << std::endl;

    // TODO: replace the evaluator with real codegen and write to 'out' if
    // given.
    return 0;
  } catch (const Diag &d) {
    std::cerr << "parse error at " << d.pos.line << ":" << d.pos.col << ": "
              << d.what() << "\n";
    return 1;
  } catch (const std::exception &e) {
    std::cerr << "error: " << e.what() << "\n";
    return 1;
  }
}
