// blinkc_fixed.c — C rewrite (standard C11 only, no typeof)
//   * macOS ARM64 (Apple Silicon)  -> --target=arm64-macos
//   * Linux x86_64 (System V ABI)  -> --target=x86_64-linux
//
// Build:
//   clang -std=c11 -O2 -Wall -Wextra -o blinkc blinkc_fixed.c
//
// Sample program:
//   let x := 10
//   mut y := 3
//   y = (x * 2) + 5
//   println(y |> inc |> neg)

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// -----------------------------------------------------------------------------
// Small vector helper
// -----------------------------------------------------------------------------
#define VEC(T)                                                                 \
  struct {                                                                     \
    T *data;                                                                   \
    int len;                                                                   \
    int cap;                                                                   \
  }
static void *xrealloc(void *p, size_t n) {
  void *r = realloc(p, n);
  if (!r) {
    fprintf(stderr, "oom\n");
    exit(1);
  }
  return r;
}
#define vec_push(v, val)                                                       \
  do {                                                                         \
    if ((v)->len == (v)->cap) {                                                \
      (v)->cap = (v)->cap ? (v)->cap * 2 : 8;                                  \
      (v)->data = xrealloc((v)->data, (v)->cap * sizeof(*(v)->data));          \
    }                                                                          \
    (v)->data[(v)->len++] = (val);                                             \
  } while (0)

static char *strndup0(const char *s, size_t n) {
  char *r = malloc(n + 1);
  if (!r) {
    fprintf(stderr, "oom\n");
    exit(1);
  }
  memcpy(r, s, n);
  r[n] = '\0';
  return r;
}
static void die(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

// -----------------------------------------------------------------------------
// Lexer
// -----------------------------------------------------------------------------

typedef enum {
  T_LET,
  T_MUT,
  T_PRINTLN,
  T_IDENT,
  T_INT,
  T_ASSIGNCOLON, // :=
  T_EQ,          // =
  T_LPAREN,
  T_RPAREN,
  T_PLUS,
  T_MINUS,
  T_STAR,
  T_SLASH,
  T_PERCENT,
  T_PIPEGT, // |>
  T_COMMA,
  T_EOF
} TokType;

typedef struct {
  TokType t;
  char *lex;
  long long ival;
} Tok;

typedef struct {
  const char *s;
  size_t i;
} Lex;

static void skip_ws(Lex *L) {
  for (;;) {
    char c = L->s[L->i];
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
      L->i++;
      continue;
    }
    break;
  }
}

static Tok next_tok(Lex *L) {
  skip_ws(L);
  char c = L->s[L->i];
  if (!c)
    return (Tok){.t = T_EOF};
  if (isalpha((unsigned char)c) || c == '_') {
    size_t j = L->i + 1;
    while (isalnum((unsigned char)L->s[j]) || L->s[j] == '_')
      j++;
    char *w = strndup0(L->s + L->i, j - L->i);
    L->i = j;
    if (!strcmp(w, "let"))
      return (Tok){.t = T_LET};
    if (!strcmp(w, "mut"))
      return (Tok){.t = T_MUT};
    if (!strcmp(w, "println"))
      return (Tok){.t = T_PRINTLN};
    return (Tok){.t = T_IDENT, .lex = w};
  }
  if (isdigit((unsigned char)c)) {
    long long v = 0;
    size_t j = L->i;
    while (isdigit((unsigned char)L->s[j])) {
      v = v * 10 + (L->s[j] - '0');
      j++;
    }
    L->i = j;
    return (Tok){.t = T_INT, .ival = v};
  }
  L->i++;
  switch (c) {
  case ':':
    if (L->s[L->i] == '=') {
      L->i++;
      return (Tok){.t = T_ASSIGNCOLON};
    }
    break;
  case '=':
    return (Tok){.t = T_EQ};
  case '(':
    return (Tok){.t = T_LPAREN};
  case ')':
    return (Tok){.t = T_RPAREN};
  case '+':
    return (Tok){.t = T_PLUS};
  case '-':
    return (Tok){.t = T_MINUS};
  case '*':
    return (Tok){.t = T_STAR};
  case '/':
    return (Tok){.t = T_SLASH};
  case '%':
    return (Tok){.t = T_PERCENT};
  case ',':
    return (Tok){.t = T_COMMA};
  case '|':
    if (L->s[L->i] == '>') {
      L->i++;
      return (Tok){.t = T_PIPEGT};
    }
    break;
  }
  die("lex error");
  return (Tok){.t = T_EOF};
}

// -----------------------------------------------------------------------------
// AST
// -----------------------------------------------------------------------------

typedef enum { OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD } Op;

typedef enum { E_INT, E_VAR, E_BIN, E_CALL } ETag;

typedef struct Expr Expr;

struct Expr {
  ETag tag;
  union {
    long long ival; // E_INT
    char *var;      // E_VAR
    struct {
      Op op;
      Expr *a, *b;
    } bin; // E_BIN
    struct {
      char *fn;
      VEC(Expr *) args;
    } call; // E_CALL
  };
};

typedef enum { S_LET, S_MUT, S_ASSIGN, S_PRINT } STag;

typedef struct Stmt {
  STag tag;
  char *name;
  Expr *e;
} Stmt;

typedef struct Program {
  VEC(Stmt *) stmts;
} Program;

static Expr *mk_int(long long v) {
  Expr *e = calloc(1, sizeof *e);
  if (!e)
    die("oom");
  e->tag = E_INT;
  e->ival = v;
  return e;
}
static Expr *mk_var(char *s) {
  Expr *e = calloc(1, sizeof *e);
  if (!e)
    die("oom");
  e->tag = E_VAR;
  e->var = s;
  return e;
}
static Expr *mk_bin(Op op, Expr *a, Expr *b) {
  Expr *e = calloc(1, sizeof *e);
  if (!e)
    die("oom");
  e->tag = E_BIN;
  e->bin.op = op;
  e->bin.a = a;
  e->bin.b = b;
  return e;
}
static Expr *mk_call(char *fn) {
  Expr *e = calloc(1, sizeof *e);
  if (!e)
    die("oom");
  e->tag = E_CALL;
  e->call.fn = fn;
  e->call.args.data = NULL;
  e->call.args.len = 0;
  e->call.args.cap = 0;
  return e;
}
static Stmt *mk_stmt(STag t, char *name, Expr *e) {
  Stmt *s = calloc(1, sizeof *s);
  if (!s)
    die("oom");
  s->tag = t;
  s->name = name;
  s->e = e;
  return s;
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

typedef struct {
  Lex L;
  Tok cur;
} Parser;
static void p_next(Parser *P) { P->cur = next_tok(&P->L); }
static bool p_accept(Parser *P, TokType t) {
  if (P->cur.t == t) {
    p_next(P);
    return true;
  }
  return false;
}
static void p_expect(Parser *P, TokType t, const char *msg) {
  if (!p_accept(P, t))
    die(msg);
}
static char *p_ident(Parser *P) {
  if (P->cur.t != T_IDENT)
    die("expected identifier");
  char *s = P->cur.lex;
  p_next(P);
  return s;
}

static Expr *parse_expr(Parser *P);

static Expr *parse_factor(Parser *P) {
  if (P->cur.t == T_INT) {
    long long v = P->cur.ival;
    p_next(P);
    return mk_int(v);
  }
  if (P->cur.t == T_IDENT) {
    char *s = p_ident(P);
    return mk_var(s);
  }
  if (P->cur.t == T_LPAREN) {
    p_next(P);
    Expr *e = parse_expr(P);
    p_expect(P, T_RPAREN, ") expected");
    return e;
  }
  if (P->cur.t == T_MINUS) {
    p_next(P);
    Expr *inner = parse_factor(P);
    Expr *call = mk_call("neg");
    vec_push(&call->call.args, inner);
    return call;
  }
  die("bad factor");
  return NULL;
}

static Expr *parse_muldiv(Parser *P) {
  Expr *a = parse_factor(P);
  for (;;) {
    TokType t = P->cur.t;
    Op op;
    bool isop = true;
    if (t == T_STAR)
      op = OP_MUL;
    else if (t == T_SLASH)
      op = OP_DIV;
    else if (t == T_PERCENT)
      op = OP_MOD;
    else {
      isop = false;
    }
    if (!isop)
      break;
    p_next(P);
    Expr *b = parse_factor(P);
    a = mk_bin(op, a, b);
  }
  return a;
}

static Expr *parse_addsub(Parser *P) {
  Expr *a = parse_muldiv(P);
  for (;;) {
    TokType t = P->cur.t;
    Op op;
    bool isop = true;
    if (t == T_PLUS)
      op = OP_ADD;
    else if (t == T_MINUS)
      op = OP_SUB;
    else {
      isop = false;
    }
    if (!isop)
      break;
    p_next(P);
    Expr *b = parse_muldiv(P);
    a = mk_bin(op, a, b);
  }
  return a;
}

static Expr *parse_pipe(Parser *P) {
  Expr *e = parse_addsub(P);
  for (;;) {
    if (!p_accept(P, T_PIPEGT))
      break;
    if (P->cur.t != T_IDENT)
      die("expected function name after |>");
    char *fn = p_ident(P);
    if (p_accept(P, T_LPAREN)) {
      p_expect(P, T_RPAREN, ") expected");
    }
    Expr *call = mk_call(fn);
    vec_push(&call->call.args, e);
    e = call;
  }
  return e;
}

static Expr *parse_expr(Parser *P) { return parse_pipe(P); }

static Stmt *parse_stmt(Parser *P) {
  if (p_accept(P, T_LET)) {
    char *n = p_ident(P);
    p_expect(P, T_ASSIGNCOLON, ":= expected");
    Expr *e = parse_expr(P);
    return mk_stmt(S_LET, n, e);
  }
  if (p_accept(P, T_MUT)) {
    char *n = p_ident(P);
    p_expect(P, T_ASSIGNCOLON, ":= expected");
    Expr *e = parse_expr(P);
    return mk_stmt(S_MUT, n, e);
  }
  if (p_accept(P, T_PRINTLN)) {
    p_expect(P, T_LPAREN, "( expected");
    Expr *e = parse_expr(P);
    p_expect(P, T_RPAREN, ") expected");
    return mk_stmt(S_PRINT, NULL, e);
  }
  if (P->cur.t == T_IDENT) {
    char *n = p_ident(P);
    p_expect(P, T_EQ, "= expected");
    Expr *e = parse_expr(P);
    return mk_stmt(S_ASSIGN, n, e);
  }
  die("bad statement");
  return NULL;
}

static Program parse_program(const char *src) {
  Parser P = {.L = {src, 0}};
  p_next(&P);
  Program prog;
  prog.stmts.data = NULL;
  prog.stmts.len = 0;
  prog.stmts.cap = 0;
  while (P.cur.t != T_EOF) {
    Stmt *s = parse_stmt(&P);
    vec_push(&prog.stmts, s);
  }
  return prog;
}

// -----------------------------------------------------------------------------
// Semantics & layout
// -----------------------------------------------------------------------------

typedef enum { MF_Imm, MF_Mut } MutFlag;

typedef struct {
  char *name;
  MutFlag mf;
  int offset;
} Binding;

typedef struct {
  VEC(Binding) items;
} Env;

static void env_init(Env *E) {
  E->items.data = NULL;
  E->items.len = 0;
  E->items.cap = 0;
}

static int env_find(Env *E, const char *name) {
  for (int i = 0; i < E->items.len; i++)
    if (strcmp(E->items.data[i].name, name) == 0)
      return i;
  return -1;
}

static void check_and_layout(Program *p, Env *E, int *localsBytes) {
  env_init(E);
  for (int i = 0; i < p->stmts.len; i++) {
    Stmt *s = p->stmts.data[i];
    if (s->tag == S_LET || s->tag == S_MUT) {
      if (env_find(E, s->name) >= 0)
        die("duplicate binding");
      Binding b = {s->name, s->tag == S_LET ? MF_Imm : MF_Mut, 0};
      vec_push(&E->items, b);
    }
  }
  for (int i = 0; i < E->items.len; i++)
    E->items.data[i].offset = (i + 1) * 8;
  *localsBytes = 8 * E->items.len;
  for (int i = 0; i < p->stmts.len; i++) {
    Stmt *s = p->stmts.data[i];
    if (s->tag == S_ASSIGN) {
      int idx = env_find(E, s->name);
      if (idx < 0)
        die("assign to unknown var");
      if (E->items.data[idx].mf != MF_Mut)
        die("assign to immutable var");
    }
  }
}

// -----------------------------------------------------------------------------
// Codegen
// -----------------------------------------------------------------------------

typedef enum { TGT_AARCH64_MAC, TGT_X64_LINUX } Target;

static int roundUp16(int n) { return ((n + 15) / 16) * 16; }

static int env_offset(Env *E, const char *name) {
  int idx = env_find(E, name);
  if (idx < 0)
    die("unknown var");
  return E->items.data[idx].offset;
}
static void gen_expr(FILE *out, Target tgt, Env *E, Expr *e);

static void load_var(FILE *out, Target tgt, int off) {
  if (tgt == TGT_AARCH64_MAC)
    fprintf(out, "  ldr x0, [x29, -%d]\n", off);
  else
    fprintf(out, "  mov -%d(%%rbp), %%rax\n", off);
}
static void imm_load(FILE *out, Target tgt, long long v) {
  if (tgt == TGT_AARCH64_MAC)
    fprintf(out, "  mov x0, #%lld\n", v);
  else
    fprintf(out, "  mov $%lld, %%rax\n", v);
}
static void push(FILE *out, Target tgt) {
  if (tgt == TGT_AARCH64_MAC)
    fprintf(out, "  sub sp, sp, #16\n  str x0, [sp]\n");
  else
    fprintf(out, "  push %%rax\n");
}

static void pop_bin_op(FILE *out, Target tgt, Op op) {
  if (tgt == TGT_AARCH64_MAC) {
    switch (op) {
    case OP_ADD:
      fprintf(out, "  ldr x1, [sp]\n  add x0, x1, x0\n  add sp, sp, #16\n");
      break;
    case OP_SUB:
      fprintf(out, "  ldr x1, [sp]\n  sub x0, x1, x0\n  add sp, sp, #16\n");
      break;
    case OP_MUL:
      fprintf(out, "  ldr x1, [sp]\n  mul x0, x1, x0\n  add sp, sp, #16\n");
      break;
    case OP_DIV:
      fprintf(out, "  ldr x1, [sp]\n  sdiv x0, x1, x0\n  add sp, sp, #16\n");
      break;
    case OP_MOD:
      fprintf(out, "  ldr x1, [sp]\n  sdiv x2, x1, x0\n  msub x0, x2, x0, x1\n "
                   " add sp, sp, #16\n");
      break;
    }
  } else {
    switch (op) {
    case OP_ADD:
      fprintf(out, "  pop %%rcx\n  add %%rcx, %%rax\n");
      break;
    case OP_SUB:
      fprintf(out, "  pop %%rcx\n  sub %%rax, %%rcx\n  mov %%rcx, %%rax\n");
      break;
    case OP_MUL:
      fprintf(out, "  pop %%rcx\n  imul %%rcx, %%rax\n");
      break;
    case OP_DIV:
      fprintf(out, "  pop %%rcx\n  mov %%rax, %%r8\n  mov %%rcx, %%rax\n  "
                   "cqo\n  idiv %%r8\n");
      break;
    case OP_MOD:
      fprintf(out, "  pop %%rcx\n  mov %%rax, %%r8\n  mov %%rcx, %%rax\n  "
                   "cqo\n  idiv %%r8\n  mov %%rdx, %%rax\n");
      break;
    }
  }
}

static void gen_call1(FILE *out, Target tgt, const char *fn) {
  if (!strcmp(fn, "inc")) {
    if (tgt == TGT_AARCH64_MAC)
      fprintf(out, "  add x0, x0, #1\n");
    else
      fprintf(out, "  add $1, %%rax\n");
    return;
  }
  if (!strcmp(fn, "dec")) {
    if (tgt == TGT_AARCH64_MAC)
      fprintf(out, "  sub x0, x0, #1\n");
    else
      fprintf(out, "  sub $1, %%rax\n");
    return;
  }
  if (!strcmp(fn, "neg")) {
    if (tgt == TGT_AARCH64_MAC)
      fprintf(out, "  neg x0, x0\n");
    else
      fprintf(out, "  neg %%rax\n");
    return;
  }
  die("unknown function (only inc/dec/neg supported)");
}

static void gen_expr(FILE *out, Target tgt, Env *E, Expr *e) {
  switch (e->tag) {
  case E_INT:
    imm_load(out, tgt, e->ival);
    break;
  case E_VAR:
    load_var(out, tgt, env_offset(E, e->var));
    break;
  case E_CALL:
    if (e->call.args.len != 1)
      die("only unary built-ins supported");
    gen_expr(out, tgt, E, e->call.args.data[0]);
    gen_call1(out, tgt, e->call.fn);
    break;
  case E_BIN:
    gen_expr(out, tgt, E, e->bin.a);
    push(out, tgt);
    gen_expr(out, tgt, E, e->bin.b);
    pop_bin_op(out, tgt, e->bin.op);
    break;
  }
}

static void gen_store(FILE *out, Target tgt, int off) {
  if (tgt == TGT_AARCH64_MAC)
    fprintf(out, "  str x0, [x29, -%d]\n", off);
  else
    fprintf(out, "  mov %%rax, -%d(%%rbp)\n", off);
}

static void gen_print(FILE *out, Target tgt) {
  if (tgt == TGT_AARCH64_MAC) {
    fprintf(out, "  mov x1, x0\n"
                 "  adrp x0, L_fmt@PAGE\n"
                 "  add  x0, x0, L_fmt@PAGEOFF\n"
                 "  bl _printf\n");
  } else {
    fprintf(out, "  mov %%rax, %%rsi\n"
                 "  leaq .LC0(%%rip), %%rdi\n"
                 "  xor %%eax, %%eax\n"
                 "  call printf@PLT\n");
  }
}

static void gen_program(FILE *out, Target tgt, Program *p) {
  Env E;
  int localsBytes = 0;
  check_and_layout(p, &E, &localsBytes);

  if (tgt == TGT_AARCH64_MAC) {
    fprintf(out, ".text\n"
                 ".globl _main\n"
                 ".p2align 2\n"
                 "_main:\n"
                 "  stp x29, x30, [sp, #-16]!\n"
                 "  mov x29, sp\n");
    int pad = roundUp16(localsBytes);
    if (pad)
      fprintf(out, "  sub sp, sp, #%d\n", pad);
    fprintf(out, ".section __TEXT,__cstring\n"
                 "L_fmt: .asciz \"%ld\\n\"\n"
                 ".text\n");
  } else {
    fprintf(out, ".text\n"
                 ".globl main\n"
                 "main:\n"
                 "  push %%rbp\n"
                 "  mov %%rsp, %%rbp\n");
    int stackSpace =
        roundUp16(localsBytes) + 8; // keep rsp 16B-aligned before calls
    fprintf(out, "  sub $%d, %%rsp\n", stackSpace);
    fprintf(out, ".section .rodata\n"
                 ".LC0: .string \"%ld\\n\"\n"
                 ".text\n");
  }

  for (int i = 0; i < p->stmts.len; i++) {
    Stmt *s = p->stmts.data[i];
    switch (s->tag) {
    case S_LET:
    case S_MUT: {
      int off = env_offset(&E, s->name);
      gen_expr(out, tgt, &E, s->e);
      gen_store(out, tgt, off);
      break;
    }
    case S_ASSIGN: {
      int off = env_offset(&E, s->name);
      gen_expr(out, tgt, &E, s->e);
      gen_store(out, tgt, off);
      break;
    }
    case S_PRINT:
      gen_expr(out, tgt, &E, s->e);
      gen_print(out, tgt);
      break;
    }
  }

  if (tgt == TGT_AARCH64_MAC) {
    fprintf(out, "  mov w0, #0\n  ldp x29, x30, [sp], #16\n  ret\n");
  } else {
    fprintf(out, "  mov $0, %%eax\n  leave\n  ret\n");
  }
}

// -----------------------------------------------------------------------------
// Main
// -----------------------------------------------------------------------------

int main(int argc, char **argv) {
  if (argc != 5 || strcmp(argv[3], "-o") != 0) {
    fprintf(stderr,
            "Usage:\n  %s --target=arm64-macos  <input.blink> -o out.s\n  %s "
            "--target=x86_64-linux <input.blink> -o out.s\n",
            argv[0], argv[0]);
    return 1;
  }
  Target tgt;
  if (!strcmp(argv[1], "--target=arm64-macos"))
    tgt = TGT_AARCH64_MAC;
  else if (!strcmp(argv[1], "--target=x86_64-linux"))
    tgt = TGT_X64_LINUX;
  else
    die("unknown --target");

  const char *inpath = argv[2];
  const char *outpath = argv[4];

  FILE *f = fopen(inpath, "rb");
  if (!f)
    die("could not open input file");
  fseek(f, 0, SEEK_END);
  long n = ftell(f);
  fseek(f, 0, SEEK_SET);
  char *src = malloc(n + 1);
  if (!src)
    die("oom");
  if (fread(src, 1, n, f) != (size_t)n)
    die("read fail");
  src[n] = '\0';
  fclose(f);

  Parser P = {.L = {src, 0}};
  p_next(&P);
  Program prog;
  prog.stmts.data = NULL;
  prog.stmts.len = 0;
  prog.stmts.cap = 0;
  while (P.cur.t != T_EOF) {
    Stmt *s = parse_stmt(&P);
    vec_push(&prog.stmts, s);
  }

  FILE *out = fopen(outpath, "wb");
  if (!out)
    die("cannot open output file");
  gen_program(out, tgt, &prog);
  fclose(out);

  printf("Wrote %s\n", outpath);
  return 0;
}
