import nimyacc except Symbol
import patty
import absyn
import strutils
import symbol
import options

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
    Id(val: string)
    Eof


genStringMatcher tigerLexer[LexerState, Token]:
    r"\n":
        # TODO we can enhance later to track line and col numbers.
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
    %right Of
    %nonassoc Do Then
    %nonassoc Else
    %nonassoc Assign
    %left Or
    %left And
    %nonassoc Eq Neq Lt Le Gt Ge
    %left Plus Minus
    %left Times Divide
    %left Uminus

    prog[Exp]:
        exp:
            return $1
    exp[Exp]:
        Nil:
            return Exp(kind: NilExp)
        Int:
            return Exp(kind: IntExp, i: ($1).i)
        String:
            return Exp(kind: StringExp, str: ($1).s,
                    strp: nimyacctree.getStartPos(1))
        Id Lparen args Rparen:
            return Exp(kind: CallExp, fun: symbol ($1).val, args: $3,
                    cp: nimyacctree.getStartPos(1))
        Id Lbrack exp Rbrack Of exp:
            return Exp(kind: ArrayExp,
                        arrtyp: symbol ($1).val,
                        size: $3,
                        init: $6,
                        arrpos: nimyacctree.getStartPos(1))
        Id Lbrace field_value_list Rbrace:
            return Exp(kind: RecordExp,
                        fields: $3,
                        rectyp: symbol ($1).val,
                        rpos: nimyacctree.getStartPos(1))
        lvalue:
            return Exp(kind: VarExp, v: $1)
        lvalue Assign exp:
            return Exp(kind: AssignExp, avar: $1, aexp: $3,
                    apos: nimyacctree.getStartPos(2))
        exp Or exp:
            let trueExpr = Exp(kind: IntExp, i: 1)
            return Exp(kind: IfExp,
                iftest: $1,
                then: trueExpr,
                els: some[Exp]($3),
                ifpos: nimyacctree.getStartPos(2))
        exp And exp:
            let falseExpr = Exp(kind: IntExp, i: 0)
            return Exp(kind: IfExp,
                iftest: $1,
                then: $3,
                els: some[Exp](falseExpr),
                ifpos: nimyacctree.getStartPos(2))
        exp Eq exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: EqOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Neq exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: NeqOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Lt exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: LtOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Le exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: LeOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Gt exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: GtOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Ge exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: GeOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Plus exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: PlusOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Minus exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: MinusOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Times exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: TimesOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        exp Divide exp:
            return Exp(kind: OpExp,
                        left: $1,
                        oper: DivideOp,
                        right: $3,
                        opos: nimyacctree.getStartPos(2))
        Minus exp %prec Uminus:
            # TODO appel book lets slide the edge case of -INT_MIN being invalid.
            # TODO if add floating point support, 0-x != -x if x=0.
            let zeroExp = Exp(kind: IntExp, i: 0)
            return Exp(kind: OpExp,
                left: zeroExp,
                oper: MinusOp,
                right: $2,
                opos: nimyacctree.getStartPos(1))
        If exp Then exp Else exp:
            return Exp(kind: IfExp,
                        iftest: $2,
                        then: $4,
                        els: some[Exp]($6),
                        ifpos: nimyacctree.getStartPos(1))
        If exp Then exp:
            return Exp(kind: IfExp,
                        iftest: $2,
                        then: $4,
                        els: none[Exp](),
                        ifpos: nimyacctree.getStartPos(1))
        While exp Do exp:
            return Exp(kind: WhileExp,
                        wtest: $2,
                        wbody: $4,
                        wpos: nimyacctree.getStartPos(1))
        For Id Assign exp To exp Do exp:
            return Exp(kind: ForExp,
                        fvar: symbol ($2).val,
                        escape: true,
                        lo: $4,
                        hi: $6,
                        fbody: $8,
                        fpos: nimyacctree.getStartPos(1))
        Break:
            return Exp(kind: BreakExp, bpos: nimyacctree.getStartPos(1))
        Let decs In sequence End:
            return Exp(kind: LetExp,
                        decs: $2,
                        letbody: $4,
                        letpos: nimyacctree.getStartPos(1))
        Lparen sequence Rparen:
            return $2
    field_value_list[seq[(Symbol, Exp, pos)]]:
        # a = <exp>, totally different from fields which is a : <some type id>
        []:
            return @[]
        Id Eq exp:
            return @[(symbol ($1).val, $3,
                    nimyacctree.getStartPos(2))]
        field_value_list Comma Id Eq exp:
            var fvl = $1
            fvl.add (symbol ($3).val, $5, nimyacctree.getStartPos(4))
            return fvl
    lvalue[VarR]:
        Id:
            return VarR(kind: SimpleVar, svs: symbol ($1).val,
                    svp: nimyacctree.getStartPos(1))
        lvalue_complex:
            return $1
    lvalue_complex[VarR]:
        Id Dot Id:
            let fvar = VarR(kind: SimpleVar, svs: symbol ($1).val,
                    svp: nimyacctree.getStartPos(1))
            return VarR(kind: FieldVar, fvar: fvar, fvs: symbol ($3).val,
                    fvp: nimyacctree.getStartPos(2))
        Id Lbrack exp Rbrack:
            let subvar = VarR(kind: SimpleVar, svs: symbol ($1).val,
                    svp: nimyacctree.getStartPos(1))
            return VarR(kind: SubscriptVar, subvar: subvar, exp: $3,
                    pos: nimyacctree.getStartPos(2))
        lvalue_complex Dot Id:
            # a.b.c.d
            # (((a.b).c).d)
            let fvar = $1
            return VarR(kind: FieldVar, fvar: fvar, fvs: symbol ($3).val,
                    fvp: nimyacctree.getStartPos(2))
        lvalue_complex Lbrack exp Rbrack:
            # a[b][c][d]
            # ((a[b])[c])[d]
            let subvar = $1
            return VarR(kind: SubscriptVar, subvar: subvar, exp: $3,
                    pos: nimyacctree.getStartPos(2))
    fields[seq[Field]]:
        []:
            return @[]
        field_complex:
            return $1
    field_complex[seq[Field]]:
        Id Colon Id:
            return @[Field(name: symbol ($1).val, escape: true, typ: symbol (
                    $3).val, pos: nimyacctree.getStartPos(2))]
        field_complex Comma Id Colon Id:
            var fl = $1
            fl.add Field(name: symbol ($3).val, escape: true, typ: symbol (
                    $5).val, pos: nimyacctree.getStartPos(4))
            return fl
    sequence[Exp]:
        []:
            return Exp(kind: SeqExp, eplist: @[])
        sequence_helper:
            return $1
    sequence_helper[Exp]:
        exp:
            let eplist = @[($1, nimyacctree.getStartPos(1))]
            return Exp(kind: SeqExp, eplist: eplist)
        sequence_helper Semicolon exp:
            var seqexp = $1
            seqexp.eplist.add ($3, nimyacctree.getStartPos(3))
            return seqexp
    args[seq[Exp]]:
        []:
            return @[]
        args_helper:
            return $1
    args_helper[seq[Exp]]:
        exp:
            return @[$1]
        args_helper Comma exp:
            var prefix = $1
            prefix.add $3
            return prefix
    type_opt[Option[(Symbol, pos)]]:
        []:
            return none[(Symbol, pos)]()
        Colon Id:
            let sym = symbol ($2).val
            return some[(Symbol, pos)] (sym, nimyacctree.getStartPos(2))
    vardec[Dec]:
        Var Id type_opt Assign exp:
            return Dec(kind: VarDec,
                vdname: symbol ($2).val,
                escape: true,
                vdtyp: $3,
                init: $5,
                vdpos: nimyacctree.getStartPos(1))
    fundec[Dec]:
        Function Id Lparen fields Rparen type_opt Eq exp:
            let fd = FunDec(name: symbol ($2).val,
                    params: $4,
                    result: $6,
                    body: $8,
                    pos: nimyacctree.getStartPos(2))
            return Dec(kind: FunctionDec, fundecs: @[fd])
        fundec Function Id Lparen fields Rparen type_opt Eq exp:
            let functiondec = $1
            let fundec = FunDec(name: symbol ($3).val,
                    params: $5,
                    result: $7,
                    body: $9,
                    pos: nimyacctree.getStartPos(3))
            functiondec.fundecs.add fundec
            return functiondec
    tydec[Dec]:
        Type Id Eq ty:
            let tydec = TyDec(tdname: symbol ($2).val,
                            tdty: $4,
                            tdpos: nimyacctree.getStartPos(2))

            return Dec(kind: TypeDec, decs: @[tydec])
        tydec Type Id Eq ty:
            var typedec = $1
            let tydec = TyDec(tdname: symbol ($3).val,
                            tdty: $5,
                            tdpos: nimyacctree.getStartPos(3))
            typedec.decs.add tydec
            return typedec
    ty[Ty]:
        Id:
            return Ty(kind: NameTy, nts: symbol ($1).val,
                    ntpos: nimyacctree.getStartPos(1))
        Lbrace Rbrace:
            return Ty(kind: RecordTy, rtyfields: @[])
        Lbrace fields Rbrace:
            return Ty(kind: RecordTy, rtyfields: $2)
        Array Of Id:
            return Ty(kind: ArrayTy, arrs: symbol ($3).val,
                    arrpos: nimyacctree.getStartPos(1))
    decs[seq[Dec]]:
        []:
            return @[]
        decs_helper:
            return $1
    decs_helper[seq[Dec]]:
        tydec:
            return @[$1]
        fundec:
            return @[$1]
        vardec:
            return @[$1]
        decs_helper tydec:
            var prefix = $1
            prefix.add $2
            return prefix
        decs_helper fundec:
            var prefix = $1
            prefix.add $2
            return prefix
        decs_helper vardec:
            var prefix = $1
            prefix.add $2
            return prefix

proc parseString*(input: string): Option[Exp] =
    var s: LexerState
    var testLexer = tigerLexer.newWithString(s, input)
    var parser = tigerParser.newParser()
    let ast = parser.parse_tigerParser(testLexer)
    if parser.hasError:
        echo "Syntax errors detected."
    return ast


proc parse*(filename: string): Option[Exp] =
    var input = readFile(filename)
    return parseString(input)
