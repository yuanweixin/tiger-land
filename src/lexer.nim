import lexim
import tokens 
import strutils

type TigerlexState* = object
    strBody : string
    commentDepth : int 

proc newLexState*() : TigerlexState = 
    result = TigerlexState(strBody: "", commentDepth: 0)

match false,TigerlexState,Token,tigerTokenIter:
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
        yield tokens.End()
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
    r":=":
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
            yield tokens.String(lexState.strBody)
            lexState.strBody = ""
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
        raise newException(Exception, "Unexpected character###" & input.substr(oldPos, pos-1) & "### at [" & $oldPos & "," & $(pos-1) & "]")