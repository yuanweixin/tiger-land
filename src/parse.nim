import nimyacc

import patty
import nimyacc
import strutils
import common

# Based on the grammar given here:
# https://www.cs.princeton.edu/~appel/modern/ml/chap3/tiger.grm

type LexerState* = object
    startPos*: int   # needed for lexim
    endPosExcl*: int # needed for lexim
    strBody*: string
    commentDepth*: int

variantp Token:
    Type
    Var
    Function
    Break
    Of
    End
    In
    Nil
    Let
    Do
    To
    For
    While
    Else
    Then
    If
    Array
    Assign
    Or
    And
    Ge
    Gt
    Le
    Lt
    Neq
    Eq
    Divide
    Times
    Minus
    Plus
    Dot
    Rbrace
    LBrace
    Rbrack
    Lbrack
    Rparen
    Lparen
    Semicolon
    Colon
    Comma
    String(s: string)
    Int(i: int)
    Id(id: string)
    Eof


genStringMatcher tigerLexer[LexerState, Token]:
    r"\n":
        discard
    r"\s":
        discard
    r"type":
        yield Type()
    r"var":
        yield Var()
    r"function":
        yield Function()
    r"break":
        yield Break()
    r"of":
        yield Of()
    r"end":
        yield End()
    r"in":
        yield In()
    r"nil":
        yield Nil()
    r"let":
        yield Let()
    r"do":
        yield Do()
    r"to":
        yield To()
    r"for":
        yield For()
    r"while":
        yield While()
    r"else":
        yield Else()
    r"then":
        yield Then()
    r"if":
        yield If()
    r"array":
        yield Array()
    r"\:=":
        yield Assign()
    r"\|":
        yield Or()
    r"&":
        yield And()
    r">=":
        yield Ge()
    r">":
        yield Gt()
    r"<=":
        yield Le()
    r"<":
        yield Lt()
    r"=":
        yield Eq()
    r"<>":
        yield Neq()
    r"/":
        yield Divide()
    r"\*":
        yield Times()
    r"-":
        yield Minus()
    r"\+":
        yield Plus()
    r"\.":
        yield Dot()
    r"\}":
        yield Rbrace()
    r"\{":
        yield Lbrace()
    r"\]":
        yield Rbrack()
    r"\[":
        yield Lbrack()
    r"\)":
        yield Rparen()
    r"\(":
        yield Lparen()
    r"\:":
        yield Colon()
    r";":
        yield Semicolon()
    r",":
        yield Comma()
    """["]""":
        beginState(string)
    string:
        """["]""":
            beginState(initial)
            yield String(lexState.strBody)
            lexState.strBody = ""
        r"\\t":
            lexState.strBody.add "\t"
        r"\\n":
            lexState.strBody.add "\n"
        """\\\"""":
            lexState.strBody.add "\""
        r"\\\\":
            lexState.strBody.add "\\"
        r"\\b":
            lexState.strBody.add "\b"
        r"\\r":
            lexState.strBody.add "\r"
        r"\\f":
            lexState.strBody.add "\f"
        r"\\[0-9]{3,3}":
            let i = parseInt(input.substr(oldPos+1, pos-1))
            lexState.strBody.add $chr(i)
        """\\(\t|\f|\n| )+\\""":
            discard
        r".":
            lexState.strBody.add input.substr(oldPos, pos-1)
    comment:
        r"/\*":
            inc lexState.commentDepth
        r"\*/":
            dec lexState.commentDepth
            if lexState.commentDepth == 0:
                beginState(initial)
        r".":
            discard
    r"/\*":
        inc lexState.commentDepth
        beginState(comment)
    r"[0-9]+":
        yield Int(parseInt(input.substr(oldPos, pos-1)))
    r"[a-zA-Z][a-zA-Z_0-9]*":
        yield Id(input.substr(oldPos, pos-1))
    "\0":
        yield Eof()
    r".":
        raise newException(Exception, "Unexpected character###" & input.substr(
                oldPos, pos-1) & "### at [" & $oldPos & "," & $(pos-1) & "]")

nimy tigerParser[Token]:
    %nonassoc Eq Neq Lt Le Gt Ge
    %left Plus Minus
    %left Times Divide
    %left Uminus
    prog[string]:
        exp:
            return "ok"
    exp[string]:
        Nil:
            return "ok"
        Int:
            return "ok"
        String:
            return "ok"
        Id Lbrack exp Rbrack Of exp:
            return "ok"
        Id Lbrace field_value_list Rbrace:
            return "ok"
        lvalue:
            return "ok"
        lvalue Assign exp:
            return "ok"
        exp Lparen args Rparen:
            return "ok"
        exp Or exp:
            return "ok"
        exp And exp:
            return "ok"
        exp Eq exp:
            return "ok"
        exp Neq exp:
            return "ok"
        exp Lt exp:
            return "ok"
        exp Le exp:
            return "ok"
        exp Gt exp:
            return "ok"
        exp Ge exp:
            return "ok"
        exp Plus exp:
            return "ok"
        exp Minus exp:
            return "ok"
        exp Times exp:
            return "ok"
        exp Divide exp:
            return "ok"
        Minus exp %prec Uminus:
            return "ok"
        If exp Then exp Else exp:
            return "ok"
        If exp Then exp:
            return "ok"
        While exp Do exp:
            return "ok"
        For Id Assign exp To exp Do exp:
            return "ok"
        Break:
            return "ok"
        Let decs In sequence End:
            return "ok"
        Lparen sequence Rparen:
            return "ok"
    field_value_list[string]:
        Id Eq exp:
            return "ok"
        Id Eq exp Comma field_value_list:
            return "ok"
    lvalue[string]:
        Id:
            return "ok"
        Id lvaluetail:
            return "ok"
    lvaluetail[string]:
        []:
            return "ok"
        Dot Id lvaluetail:
            return "ok"
        Lbrack exp Rbrack lvaluetail:
            return "ok"
    fields[string]:
        []:
            return "ok"
        Id Colon Id tyfieldstail:
            return "ok"
    tyfieldstail[string]:
        []:
            return "ok"
        Comma Id Colon Id tyfieldstail:
            return "ok"
    sequence[string]:
        []:
            return "ok"
        exp:
            return "ok"
        exp Semicolon sequence:
            return "ok"
    args[string]:
        []:
            return "ok"
        exp:
            return "ok"
        exp Comma args_rest:
            return "ok"
    args_rest[string]:
        exp:
            return "ok"
        exp Comma args_rest:
            return "ok"
    type_opt[string]:
        []:
            return "ok"
        Colon Id:
            return "ok"
    vardec[string]:
        Var Id type_opt Assign exp:
            return "ok"
    fundec[string]:
        Function Id Lparen fields Rparen type_opt Eq exp:
            return "ok"
        Function Id Lparen fields Rparen type_opt Eq exp fundec:
            return "ok"
    tydec[string]:
        Type Id Eq ty:
            return "ok"
        Type Id Eq ty tydec:
            return "ok"
    ty[string]:
        Id:
            return "ok"
        Lbrace fields Rbrace:
            return "ok"
        Array Of Id:
            return "ok"
    decs[string]:
        dec decs:
            return "ok"
        []:
            return "ok"
    dec[string]:
        vardec:
            return "ok"
        fundec:
            return "ok"
        tydec:
            return "ok"
