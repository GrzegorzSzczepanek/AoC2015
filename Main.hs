{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- Mini compiler in Haskell that emits assembly for:
--   * macOS ARM64 (Apple Silicon)  -> target: arm64-macos
--   * Linux x86_64 (System V ABI)  -> target: x86_64-linux
--
-- MVP language (subset aligned to your spec):
--  - Immutable by default: `let x := 10`
--  - Mutable: `mut y := 3`; assignment: `y = y + 1`
--  - Integers (64-bit signed), + - * / % and parentheses
--  - println(expr)
--  - Pipe operator (limited): `expr |> f` or `expr |> f()` desugars to f(expr)
--  - Built-in unary funcs: inc, dec, neg (used with or without pipe)
--
-- This is intentionally small but end-to-end: parses, checks (integers-only),
-- lowers, and emits assembly that you can assemble+link with clang/gcc.
--
-- Build (with GHC only):
--   ghc -O2 -Wall -o blinkc Main.hs
--
-- Usage examples:
--   ./blinkc --target=arm64-macos example.blink -o out.s
--   clang -arch arm64 out.s -o a.out && ./a.out
--
--   ./blinkc --target=x86_64-linux example.blink -o out.s
--   gcc out.s -o a.out && ./a.out
--
-- example.blink
--   let x := 10
--   mut y := 3
--   y = (x * 2) + 5
--   println(y |> inc |> neg)
--
module Main where

import qualified Data.Map.Strict as M
import           Data.Char (isAlphaNum, isAlpha, isDigit)
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.IO (readFile, writeFile)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

type Name = String

data Program = Program [Stmt]
  deriving (Show)

data Stmt
  = SLet  Name Expr            -- immutable by default
  | SMut  Name Expr            -- mutable
  | SAssign Name Expr          -- assignment to mutable
  | SPrint Expr                -- println(expr)
  deriving (Show)

data Expr
  = EInt Integer
  | EVar Name
  | EBin Op Expr Expr
  | ECall Name [Expr]          -- builtin unary funcs only in MVP
  deriving (Show)

data Op = Add | Sub | Mul | Div | Mod
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Lexer / Parser (hand-rolled, tiny; whitespace- and newline-sensitive)
--------------------------------------------------------------------------------

data Tok
  = TLet | TMut | TIdent String | TInt Integer
  | TAssignColon               -- :=
  | TEq                        -- =
  | TLParen | TRParen
  | TPlus | TMinus | TStar | TSlash | TPercent
  | TPipeGT                    -- |>
  | TComma
  | TPrintln                   -- println
  | TEOF
  deriving (Show, Eq)

lexTokens :: String -> Either String [Tok]
lexTokens = go []
  where
    go acc [] = Right (reverse (TEOF:acc))
    go acc (c:cs)
      | c == ' ' || c == '\t' || c == '\r' = go acc cs
      | c == '\n' = go acc cs -- newline just separates statements implicitly
      | c == '(' = go (TLParen:acc) cs
      | c == ')' = go (TRParen:acc) cs
      | c == '+' = go (TPlus:acc) cs
      | c == '-' = go (TMinus:acc) cs
      | c == '*' = go (TStar:acc) cs
      | c == '/' = go (TSlash:acc) cs
      | c == '%' = go (TPercent:acc) cs
      | c == ',' = go (TComma:acc) cs
      | c == ':' = case cs of
          ('=':cs') -> go (TAssignColon:acc) cs'
          _         -> Left "Unexpected ':' (did you mean ':=')"
      | c == '=' = go (TEq:acc) cs
      | c == '|' = case cs of
          ('>':cs') -> go (TPipeGT:acc) cs'
          _         -> Left "Unexpected '|' (did you mean '|>')"
      | isDigit c =
          let (ds, rest) = span isDigit (c:cs)
          in go (TInt (read ds):acc) rest
      | isAlpha c || c == '_' =
          let (as, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
          in go (kw as:acc) rest
      | otherwise = Left ("Unexpected character: " ++ [c])

    kw "let"     = TLet
    kw "mut"     = TMut
    kw "println" = TPrintln
    kw s          = TIdent s

newtype P a = P { runP :: [Tok] -> Either String (a, [Tok]) }

instance Functor P where
  fmap f p = P $ \ts -> do
    (a, ts') <- runP p ts
    pure (f a, ts')

instance Applicative P where
  pure a = P $ \ts -> Right (a, ts)
  pf <*> pa = P $ \ts -> do
    (f, ts')  <- runP pf ts
    (a, ts'') <- runP pa ts'
    pure (f a, ts'')

instance Monad P where
  p >>= f = P $ \ts -> do
    (a, ts') <- runP p ts
    runP (f a) ts'

satisfy :: (Tok -> Bool) -> String -> P Tok
satisfy pred err = P $ \case
  (t:ts) | pred t -> Right (t, ts)
  _               -> Left err

peek :: P Tok
peek = P $ \case
  []     -> Left "Unexpected end of tokens"
  (t:ts) -> Right (t, t:ts)

consume :: Tok -> String -> P ()
consume t err = do
  _ <- satisfy (== t) err
  pure ()

parseProgram :: P Program
parseProgram = do
  ss <- manyStmts
  _  <- satisfy (== TEOF) "Expected end of input"
  pure (Program ss)

manyStmts :: P [Stmt]
manyStmts = go []
  where
    go acc = do
      t <- peek
      case t of
        TEOF -> pure (reverse acc)
        _    -> do s <- parseStmt
                    go (s:acc)

parseStmt :: P Stmt
parseStmt = do
  t <- peek
  case t of
    TLet -> do
      _ <- satisfy (== TLet) "expected 'let'"
      name <- parseIdent
      consume TAssignColon "expected ':=' after let name"
      e <- parseExpr
      pure (SLet name e)
    TMut -> do
      _ <- satisfy (== TMut) "expected 'mut'"
      name <- parseIdent
      consume TAssignColon "expected ':=' after mut name"
      e <- parseExpr
      pure (SMut name e)
    TPrintln -> do
      _ <- satisfy (== TPrintln) "expected 'println'"
      consume TLParen "expected '(' after println"
      e <- parseExpr
      consume TRParen "expected ')' after println(expr)"
      pure (SPrint e)
    TIdent _ -> do
      name <- parseIdent
      consume TEq "expected '=' in assignment"
      e <- parseExpr
      pure (SAssign name e)
    _ -> P $ \_ -> Left ("Unexpected token in statement: " ++ show t)

parseIdent :: P Name
parseIdent = do
  t <- satisfy isIdent "expected identifier"
  case t of
    TIdent s -> pure s
    _        -> P $ \_ -> Left "internal: not an ident"
  where isIdent (TIdent _) = True
        isIdent _          = False

parseExpr :: P Expr
parseExpr = parsePipe

-- expr |> f  ==> f(expr)
parsePipe :: P Expr
parsePipe = do
  e <- parseAddSub
  go e
  where
    go e = do
      t <- peek
      case t of
        TPipeGT -> do
          _ <- satisfy (== TPipeGT) "expected '|>'"
          f <- parseFuncCallOrName
          let call = case f of
                (nm, Just argList) -> ECall nm (e:argList)
                (nm, Nothing)      -> ECall nm [e]
          go call
        _ -> pure e

parseFuncCallOrName :: P (Name, Maybe [Expr])
parseFuncCallOrName = do
  nm <- parseIdent
  t <- peek
  case t of
    TLParen -> do
      _ <- satisfy (== TLParen) "("
      args <- parseArgs
      consume TRParen ")"
      pure (nm, Just args)
    _ -> pure (nm, Nothing)

parseArgs :: P [Expr]
parseArgs = do
  t <- peek
  case t of
    TRParen -> pure []
    _ -> do e <- parseExpr
            more e
  where
    more e = do
      t <- peek
      case t of
        TComma -> do _ <- satisfy (== TComma) ","
                     e2 <- parseExpr
                     rest <- more e2
                     pure (e:rest)
        _      -> pure [e]

parseAddSub :: P Expr
parseAddSub = chainl1 parseMulDiv (op "+" TPlus Add <|> op "-" TMinus Sub)
  where
    op _ tok o = do _ <- satisfy (== tok) "op"
                    pure (\a b -> EBin o a b)

parseMulDiv :: P Expr
parseMulDiv = chainl1 parseFactor (op "*" TStar Mul <|> op "/" TSlash Div <|> op "%" TPercent Mod)
  where
    op _ tok o = do _ <- satisfy (== tok) "op"
                    pure (\a b -> EBin o a b)

parseFactor :: P Expr
parseFactor = do
  t <- peek
  case t of
    TInt _ -> do
      TInt n <- satisfy isInt "int"
      pure (EInt n)
    TIdent _ -> EVar <$> parseIdent
    TLParen -> do
      _ <- satisfy (== TLParen) "("
      e <- parseExpr
      consume TRParen ")"
      pure e
    TMinus -> do -- unary neg
      _ <- satisfy (== TMinus) "-"
      e <- parseFactor
      pure (ECall "neg" [e])
    _ -> P $ \_ -> Left ("Unexpected token in expression: " ++ show t)
  where
    isInt (TInt _) = True
    isInt _        = False

chainl1 :: P a -> P (a -> a -> a) -> P a
chainl1 pa pop = do
  a <- pa
  rest a
  where
    rest a = do
      t <- peek
      case runP pop [t] of
        Left _  -> pure a
        Right _ -> do f <- pop
                      b <- pa
                      rest (f a b)

(<|>) :: P a -> P a -> P a
pa <|> pb = P $ \ts -> case runP pa ts of
  Right r -> Right r
  Left _  -> runP pb ts

--------------------------------------------------------------------------------
-- Type check (trivial: integers only, ensure assignments to mut only)
--------------------------------------------------------------------------------

data MutFlag = Imm | Mut deriving (Eq, Show)

type Env = M.Map Name (MutFlag, Int)  -- (mutability, stack offset)

checkProgram :: Program -> Either String ()
checkProgram (Program ss) = go M.empty ss >> pure ()
  where
    go :: M.Map Name MutFlag -> [Stmt] -> Either String (M.Map Name MutFlag)
    go env [] = Right env
    go env (s:ss') = case s of
      SLet n _ -> if M.member n env then Left ("duplicate binding: "++n)
                   else go (M.insert n Imm env) ss'
      SMut n _ -> if M.member n env then Left ("duplicate binding: "++n)
                   else go (M.insert n Mut env) ss'
      SAssign n _ -> case M.lookup n env of
                       Just mf -> if mf == Imm then Left ("cannot assign to immutable '"++n++"'") else go env ss'
                       Nothing  -> Left ("assign to unknown variable '"++n++"'")
      SPrint _ -> go env ss'

--------------------------------------------------------------------------------
-- Codegen
--------------------------------------------------------------------------------

data Target = AArch64_Mac | X64_Linux deriving (Eq, Show)

compile :: Target -> Program -> Either String String
compile tgt (Program ss) = do
  -- assign stack slots (8 bytes each) in order of first declaration
  let decls = [ n | s <- ss, n <- case s of
                                  SLet n _ -> [n]
                                  SMut n _ -> [n]
                                  _        -> []]
  let offsets = M.fromList (zip decls (map (*8) [1..]))
  -- produce code with env mapping name -> (mutFlag, offset)
  let mutMap = foldl (\m s -> case s of
                          SLet n _ -> M.insert n Imm m
                          SMut n _ -> M.insert n Mut m
                          _        -> m) M.empty ss
  let env = M.intersectionWith (\m o -> (m,o)) mutMap offsets
  let localsBytes = 8 * length decls
  body <- concat <$> mapM (codegenStmt tgt env) ss
  pure (prologue tgt localsBytes ++ body ++ epilogue tgt)

prologue :: Target -> Int -> String
prologue AArch64_Mac locals = unlines
  [ ".text"
  , ".globl _main"
  , ".p2align 2"
  , "_main:"
  , "  stp x29, x30, [sp, #-16]!"      -- frame
  , "  mov x29, sp"
  , if localsAlign == 0 then "" else "  sub sp, sp, #" ++ show localsAlign
  , rodataAArch64 -- emit fmt in same file for simplicity
  ]
  where
    localsAlign = roundUp16 locals

prologue X64_Linux locals = unlines
  [ ".text"
  , ".globl main"
  , "main:"
  , "  push %rbp"
  , "  mov %rsp, %rbp"
  , "  sub $" ++ show stackSpace ++ ", %rsp" -- keep 16B alignment for calls
  , rodataX64
  ]
  where
    -- After push %rbp, %rsp is 8 mod 16. To be 0 mod 16 before calls, subtract 8 mod 16.
    stackSpace = roundUp16 locals + 8

rodataAArch64 :: String
rodataAArch64 = unlines
  [ ".section __TEXT,__cstring"
  , "L_fmt: .asciz \"%ld\\n\""
  , ".text"
  ]

rodataX64 :: String
rodataX64 = unlines
  [ ".section .rodata"
  , ".LC0: .string \"%ld\\n\""
  , ".text"
  ]

epilogue :: Target -> String
epilogue AArch64_Mac = unlines
  [ "  mov w0, #0"  -- return 0
  , "  ldp x29, x30, [sp], #16"
  , "  ret"
  ]
epilogue X64_Linux = unlines
  [ "  mov $0, %eax"  -- return 0
  , "  leave"
  , "  ret"
  ]

roundUp16 :: Int -> Int
roundUp16 n = ((n + 15) `div` 16) * 16

-- Codegen of statements
codegenStmt :: Target -> Env -> Stmt -> Either String String
codegenStmt tgt env = \case
  SLet n e   -> storeToSlot tgt env n e
  SMut n e   -> storeToSlot tgt env n e
  SAssign n e -> case M.lookup n env of
    Nothing -> Left ("unknown variable '"++n++"'")
    Just (mf, off) -> if mf == Imm then Left ("cannot assign to immutable '"++n++"'")
                       else do (pre, acc) <- codeExpr tgt env e
                               pure (pre ++ storeAtOffset tgt off ++ acc)
  SPrint e   -> do
    (pre, acc) <- codeExpr tgt env e
    pure (pre ++ printInstr tgt ++ acc)

storeToSlot :: Target -> Env -> Name -> Expr -> Either String String
storeToSlot tgt env n e = case M.lookup n env of
  Nothing -> Left ("internal: slot not allocated for '"++n++"'")
  Just (_, off) -> do
    (pre, acc) <- codeExpr tgt env e
    pure (pre ++ storeAtOffset tgt off ++ acc)

storeAtOffset :: Target -> Int -> String
storeAtOffset AArch64_Mac off = unlines ["  str x0, [x29, -" ++ show off ++ "]"]
storeAtOffset X64_Linux  off = unlines ["  mov %rax, -" ++ show off ++ "(%rbp)"]

printInstr :: Target -> String
printInstr AArch64_Mac = unlines
  [ "  mov x1, x0"                           -- arg value
  , "  adrp x0, L_fmt@PAGE"                 -- fmt ptr in x0
  , "  add  x0, x0, L_fmt@PAGEOFF"
  , "  bl _printf"
  ]
printInstr X64_Linux = unlines
  [ "  mov %rax, %rsi"           -- rsi = value
  , "  leaq .LC0(%rip), %rdi"    -- rdi = fmt
  , "  xor %eax, %eax"           -- rax = 0 for varargs
  , "  call printf@PLT"          -- call libc printf
  ]

-- Expression codegen: returns (prefixCode, postfixCode) where
--   * prefixCode: evaluates expression into x0 (arm64) or rax (x64)
--   * postfixCode: empty (for possible future cleanup); keep API for symmetry
codeExpr :: Target -> Env -> Expr -> Either String (String, String)
codeExpr tgt env = \case
  EInt n -> pure (immLoad tgt n, "")
  EVar v -> case M.lookup v env of
    Nothing -> Left ("unknown variable '"++v++"'")
    Just (_, off) -> pure (loadFromOffset tgt off, "")
  ECall f [e] -> do
    (pre, post) <- codeExpr tgt env e
    case (tgt, f) of
      (_, "inc") -> pure (pre ++ alu1 tgt "add" 1, post)
      (_, "dec") -> pure (pre ++ alu1 tgt "sub" 1, post)
      (_, "neg") -> pure (pre ++ negInstr tgt, post)
      _ -> Left ("unknown function '"++f++"' (only inc/dec/neg supported in MVP)")
  ECall f _ -> Left ("function '"++f++"' expects 1 argument in MVP")
  EBin op a b -> do
    (pa, _) <- codeExpr tgt env a
    (pb, _) <- codeExpr tgt env b
    pure (pa ++ pushVal tgt ++ pb ++ popAndOp tgt op, "")

immLoad :: Target -> Integer -> String
immLoad AArch64_Mac n = "  mov x0, #" ++ show n ++ "\n"
immLoad X64_Linux  n = "  mov $" ++ show n ++ ", %rax\n"

loadFromOffset :: Target -> Int -> String
loadFromOffset AArch64_Mac off = "  ldr x0, [x29, -" ++ show off ++ "]\n"
loadFromOffset X64_Linux  off = "  mov -" ++ show off ++ "(%rbp), %rax\n"

alu1 :: Target -> String -> Integer -> String
alu1 AArch64_Mac op k = "  " ++ op ++ " x0, x0, #" ++ show k ++ "\n"
alu1 X64_Linux  op k = case op of
  "add" -> "  add $" ++ show k ++ ", %rax\n"
  "sub" -> "  sub $" ++ show k ++ ", %rax\n"
  _      -> error "unsupported alu1 op"

negInstr :: Target -> String
negInstr AArch64_Mac = unlines ["  neg x0, x0"]
negInstr X64_Linux  = unlines ["  neg %rax"]

pushVal :: Target -> String
pushVal AArch64_Mac = unlines
  [ "  sub sp, sp, #16"
  , "  str x0, [sp]"
  ]
pushVal X64_Linux = unlines ["  push %rax"]

popAndOp :: Target -> Op -> String
popAndOp AArch64_Mac op = case op of
  Add -> unlines ["  ldr x1, [sp]", "  add x0, x1, x0", "  add sp, sp, #16"]
  Sub -> unlines ["  ldr x1, [sp]", "  sub x0, x1, x0", "  add sp, sp, #16"]
  Mul -> unlines ["  ldr x1, [sp]", "  mul x0, x1, x0", "  add sp, sp, #16"]
  Div -> unlines ["  ldr x1, [sp]", "  sdiv x0, x1, x0", "  add sp, sp, #16"]
  Mod -> unlines [ "  ldr x1, [sp]"
                 , "  sdiv x2, x1, x0"      -- q = x1 / x0
                 , "  msub x0, x2, x0, x1"  -- r = x1 - q*x0
                 , "  add sp, sp, #16"]

popAndOp X64_Linux op = case op of
  Add -> unlines ["  pop %rbx", "  add %rbx, %rax"]
  Sub -> unlines ["  pop %rbx", "  sub %rax, %rbx", "  mov %rbx, %rax"]
  Mul -> unlines ["  pop %rbx", "  imul %rbx, %rax"]
  Div -> unlines ["  pop %rbx"                    -- rbx = left, rax = right
                 , "  mov %rax, %rcx"            -- rcx = right
                 , "  mov %rbx, %rax"            -- rax = left
                 , "  cqo"
                 , "  idiv %rcx" ]
  Mod -> unlines ["  pop %rbx"
                 , "  mov %rax, %rcx"
                 , "  mov %rbx, %rax"
                 , "  cqo"
                 , "  idiv %rcx"
                 , "  mov %rdx, %rax"           -- remainder in rdx -> rax
                 ]

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--target=arm64-macos", infile, "-o", outfile] -> run AArch64_Mac infile outfile
    ["--target=x86_64-linux", infile, "-o", outfile] -> run X64_Linux infile outfile
    _ -> die $ unlines
      [ "Usage:"
      , "  blinkc --target=arm64-macos <input.blink> -o out.s"
      , "  blinkc --target=x86_64-linux <input.blink> -o out.s"
      ]

run :: Target -> FilePath -> FilePath -> IO ()
run tgt infile outfile = do
  src <- readFile infile
  case lexTokens src >>= runP parseProgram of
    Left e -> die ("Parse error: " ++ e)
    Right (prog, _) -> case checkProgram prog >> compile tgt prog of
      Left e     -> die ("Compile error: " ++ e)
      Right asm  -> writeFile outfile asm >> putStrLn ("Wrote " ++ outfile)

