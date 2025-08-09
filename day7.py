import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional, Union


class Operation(Enum):
    AND = 1
    OR = 2
    LSHIFT = 3
    RSHIFT = 4
    NOT = 5
    PUT = 6  # '->' operator
    IDENT = 7
    INT_LIT = 8


class ArenaAllocator:
    def __init__(self, max_num_bytes=None):
        self.objects = []

    def alloc(self, obj_type):
        obj = obj_type.__new__(obj_type)
        self.objects.append(obj)
        return obj

    def emplace(self, obj_type, *args, **kwargs):
        obj = obj_type(*args, **kwargs)
        self.objects.append(obj)
        return obj


@dataclass
class Instruction:
    wire: str
    privided_signal: int | None


@dataclass()
class Token:
    TokenType: Operation
    line: int
    value: str | None = None


class Tokenizer:
    def __init__(self) -> None:
        super().__init__()
        self._line_count: int = 0
        self._m_index: int = 0

    def _consume(self) -> str:
        current_char = self._m_src[self._m_index]
        self._m_index += 1
        return current_char

    def _peek(self, offset: int = 0) -> str | None:
        if self._m_index + offset >= len(self._m_src):
            return None
        return self._m_src[self._m_index + offset]

    def tokenize(self, src: str) -> List[Token]:
        self._m_src = src
        self._m_index = 0
        self._line_count = 1
        tokens: List[Token] = []

        while self._m_index < len(self._m_src):
            current_char = self._peek()

            if current_char.isspace() and current_char != "\n":
                self._consume()
                continue

            # 1) Handle integer literals first
            elif current_char.isdigit():
                buf = ""
                buf += self._consume()
                while self._peek() is not None and self._peek().isdigit():
                    buf += self._consume()
                tokens.append(Token(Operation.INT_LIT, self._line_count, buf))

            # 2) Then handle identifiers/keywords (start with a letter)
            elif current_char.isalpha():
                buf = ""
                buf += self._consume()
                while self._peek() is not None and self._peek().isalnum():
                    buf += self._consume()

                if buf == "AND":
                    tokens.append(Token(Operation.AND, self._line_count, buf))
                elif buf == "OR":
                    tokens.append(Token(Operation.OR, self._line_count, buf))
                elif buf == "LSHIFT":
                    tokens.append(Token(Operation.LSHIFT, self._line_count, buf))
                elif buf == "RSHIFT":
                    tokens.append(Token(Operation.RSHIFT, self._line_count, buf))
                elif buf == "NOT":
                    tokens.append(Token(Operation.NOT, self._line_count, buf))
                else:
                    tokens.append(Token(Operation.IDENT, self._line_count, buf))

            elif current_char == "-" and self._peek(1) == ">":
                self._consume()
                self._consume()
                tokens.append(Token(Operation.PUT, self._line_count, "->"))

            elif current_char == "\n":
                self._consume()
                self._line_count += 1
            else:
                print(
                    f"Warning: Skipping unexpected character '{current_char}' at line {self._line_count}"
                )
                self._consume()

        return tokens


@dataclass
class NodeTermIntLit:
    int_lit: Token


@dataclass
class NodeTermIdent:
    ident: Token


@dataclass
class NodeTerm:
    var: Union[NodeTermIntLit, NodeTermIdent]


@dataclass
class NodeExpr:
    var: Union[NodeTerm, "NodeBinExpr"]


# e.g., 123 -> xd lub x and y -> d
@dataclass
class NodeAssignment:
    left_expr: NodeExpr
    right_wire: Token


@dataclass
class NodeBinExprAnd:
    left_operand: NodeExpr
    right_operand: NodeExpr


@dataclass
class NodeBinExprOr:
    left_operand: NodeExpr
    right_operand: NodeExpr


@dataclass
class NodeBinExprLShift:
    left_operand: NodeExpr
    right_operand: NodeExpr


@dataclass
class NodeBinExprRShift:
    left_operand: NodeExpr
    right_operand: NodeExpr


@dataclass
class NodeBinExprNot:
    operand: NodeExpr


@dataclass
class NodeBinExpr:
    var: Union[
        NodeBinExprAnd,
        NodeBinExprOr,
        NodeBinExprLShift,
        NodeBinExprRShift,
        NodeBinExprNot,
    ]


@dataclass
class NodeStmt:
    var: NodeAssignment


@dataclass
class NodeProg:
    stmts: List[NodeStmt] = field(default_factory=list)


@dataclass
class Wire:
    name: str
    value: Optional[int] = None
    dependencies: List[str] = field(default_factory=list)
    expression: Optional[NodeExpr] = None


class Parser:
    def __init__(self, tokens: List[Token]) -> None:
        self._m_tokens = tokens or []
        self._m_allocator = ArenaAllocator()
        self._m_index = 0

    def error_expected(self, msg: str) -> None:
        print(
            f"[Parse Error] Expected {msg} on line {self._peek(-1).line if self._peek(-1) else '?'}"
        )
        exit(1)

    def parse_term(self) -> Optional[NodeTerm]:
        if int_lit := self._try_consume(Operation.INT_LIT):
            term_int_lit = self._m_allocator.emplace(NodeTermIntLit, int_lit)
            term = self._m_allocator.emplace(NodeTerm, term_int_lit)
            return term

        # Try to parse an identifier
        if ident := self._try_consume(Operation.IDENT):
            expr_ident = self._m_allocator.emplace(NodeTermIdent, ident)
            term = self._m_allocator.emplace(NodeTerm, expr_ident)
            return term

        return None

    def parse_expr(self) -> Optional[NodeExpr]:
        if self._peek() is not None and self._peek().TokenType == Operation.NOT:
            self._consume()  # consume NOT
            term = self.parse_term()
            if not term:
                self.error_expected("term after NOT")

            expr = self._m_allocator.emplace(NodeExpr, var=term)
            not_expr = self._m_allocator.emplace(NodeBinExprNot, operand=expr)
            bin_expr = self._m_allocator.emplace(NodeBinExpr, var=not_expr)
            return self._m_allocator.emplace(NodeExpr, var=bin_expr)

        term_lhs = self.parse_term()
        if not term_lhs:
            return None

        expr_lhs = self._m_allocator.emplace(NodeExpr, var=term_lhs)

        curr_tok = self._peek()
        if curr_tok is None:
            return expr_lhs

        if curr_tok.TokenType in [
            Operation.AND,
            Operation.OR,
            Operation.LSHIFT,
            Operation.RSHIFT,
        ]:
            op_type = self._consume().TokenType

            term_rhs = self.parse_term()
            if not term_rhs:
                self.error_expected("right-hand term")

            expr_rhs = self._m_allocator.emplace(NodeExpr, var=term_rhs)

            if op_type == Operation.AND:
                bin_op = self._m_allocator.emplace(
                    NodeBinExprAnd, left_operand=expr_lhs, right_operand=expr_rhs
                )
            elif op_type == Operation.OR:
                bin_op = self._m_allocator.emplace(
                    NodeBinExprOr, left_operand=expr_lhs, right_operand=expr_rhs
                )
            elif op_type == Operation.LSHIFT:
                bin_op = self._m_allocator.emplace(
                    NodeBinExprLShift, left_operand=expr_lhs, right_operand=expr_rhs
                )
            elif op_type == Operation.RSHIFT:
                bin_op = self._m_allocator.emplace(
                    NodeBinExprRShift, left_operand=expr_lhs, right_operand=expr_rhs
                )

            bin_expr = self._m_allocator.emplace(NodeBinExpr, var=bin_op)
            return self._m_allocator.emplace(NodeExpr, var=bin_expr)

        return expr_lhs

    def parse_instruction(self):
        left_expr = self.parse_expr()
        if not left_expr:
            return None
        if not self._try_consume(Operation.PUT):
            return None
        right_wire = self._try_consume(Operation.IDENT)
        if not right_wire:
            self.error_expected("Wire identifier after ->")
        assignment = self._m_allocator.emplace(
            NodeAssignment, left_expr=left_expr, right_wire=right_wire
        )
        return self._m_allocator.emplace(NodeStmt, var=assignment)

    def parse_program(self):
        prog = self._m_allocator.emplace(NodeProg)
        while self._peek() is not None:
            if stmt := self.parse_instruction():
                prog.stmts.append(stmt)
            else:
                self.error_expected("valid instruction")

        return prog

    def _bin_prec(self, token: Token) -> Optional[int]:
        if token.TokenType == Operation.AND or token.TokenType == Operation.OR:
            return 1
        elif token.TokenType == Operation.LSHIFT or token.TokenType == Operation.RSHIFT:
            return 2
        return None

    def _peek(self, offset: int = 0) -> Optional[Token]:
        if self._m_index + offset >= len(self._m_tokens) or self._m_index + offset < 0:
            return None
        return self._m_tokens[self._m_index + offset]

    def _consume(self) -> Token:
        token = self._m_tokens[self._m_index]
        self._m_index += 1
        return token

    def _try_consume(self, type: Operation) -> Optional[Token]:
        if self._peek() is not None and self._peek().TokenType == type:
            return self._consume()
        return None

    def _try_consume_err(self, type: Operation) -> Token:
        if token := self._try_consume(type):
            return token
        self.error_expected(str(type))
        return Token(TokenType=type, line=0)


class Day7:
    def __init__(self, filepath: str):
        self.program_text = open(filepath, "r").read()
        self.wires = {}  # values
        self.wire_expressions = {}  # AST

    def _evaluate_term(self, term: NodeTerm) -> int:
        if isinstance(term.var, NodeTermIntLit):
            return int(term.var.int_lit.value)
        elif isinstance(term.var, NodeTermIdent):
            wire_name = term.var.ident.value
            return self._get_wire_value(wire_name)

        raise ValueError(f"Unknown term type: {term}")

    def _evaluate_expr(self, expr: NodeExpr) -> int:
        if isinstance(expr.var, NodeTerm):
            return self._evaluate_term(expr.var)
        bin_expr = expr.var

        if isinstance(bin_expr.var, NodeBinExprAnd):
            left = self._evaluate_expr(bin_expr.var.left_operand)
            right = self._evaluate_expr(bin_expr.var.right_operand)
            return left & right

        elif isinstance(bin_expr.var, NodeBinExprOr):
            left = self._evaluate_expr(bin_expr.var.left_operand)
            right = self._evaluate_expr(bin_expr.var.right_operand)
            return left | right

        elif isinstance(bin_expr.var, NodeBinExprLShift):
            left = self._evaluate_expr(bin_expr.var.left_operand)
            right = self._evaluate_expr(bin_expr.var.right_operand)
            return (left << right) & 0xFFFF

        elif isinstance(bin_expr.var, NodeBinExprRShift):
            left = self._evaluate_expr(bin_expr.var.left_operand)
            right = self._evaluate_expr(bin_expr.var.right_operand)
            return left >> right

        elif isinstance(bin_expr.var, NodeBinExprNot):
            operand = self._evaluate_expr(bin_expr.var.operand)
            return (~operand) & 0xFFFF

        raise ValueError(f"Unknown binary expression type: {bin_expr}")

    def _get_wire_value(self, wire_name: str) -> int:
        if wire_name in self.wires:
            return self.wires[wire_name]
        if wire_name in self.wire_expressions:
            value = self._evaluate_expr(self.wire_expressions[wire_name])
            self.wires[wire_name] = value
            return value
        raise ValueError(f"Wire {wire_name} not found")

    def part1(self) -> int:

        self.wires.clear()
        self.wire_expressions.clear()
        res = 0
        tokenizer = Tokenizer()
        tokens = tokenizer.tokenize(self.program_text)

        parser = Parser(tokens)
        program = parser.parse_program()

        for stmt in program.stmts:
            assignment = stmt.var
            wire_name = assignment.right_wire.value
            self.wire_expressions[wire_name] = assignment.left_expr

        return self._get_wire_value("a")

    def part2(self) -> int:
        a = self.part1()

        self.wires.clear()
        self.wire_expressions.clear()

        base = self.program_text
        if not base.endswith("\n"):
            base += "\n"
        new_text = base + f"{a} -> b\n"

        tokenizer = Tokenizer()
        tokens = tokenizer.tokenize(new_text)
        parser = Parser(tokens)
        program = parser.parse_program()

        for stmt in program.stmts:
            assignment = stmt.var
            wire_name = assignment.right_wire.value
            self.wire_expressions[wire_name] = assignment.left_expr

        return self._get_wire_value("a")


if __name__ == "__main__":
    filepath = "./input7.txt"
    day = Day7(filepath)

    # tokenizer = Tokenizer()
    # tokens = tokenizer.tokenize(day.program_text)
    # parser = Parser(tokens)
    # program = parser.parse_program()
    #
    # for stmt in program.stmts:
    #     assignment = stmt.var
    #     wire_name = assignment.right_wire.value
    #     day.wire_expressions[wire_name] = assignment.left_expr
    #
    # for wire in ["d", "e", "f", "g", "h", "i", "x", "y"]:
    #     try:
    #         value = day._get_wire_value(wire)
    #         print(f"{wire}: {value}")
    #     except ValueError as e:
    #         print(f"Error: {e}")
    #
    # print(f"Part 1: {day._get_wire_value('a')}")

    print(f"Part 1: {day.part1()}")
    print(f"Part 2: {day.part2()}")
