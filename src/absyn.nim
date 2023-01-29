import symbol
import options

type
    pos* = int

    VarKind* = enum
        SimpleVar
        FieldVar
        SubscriptVar
    VarR* = ref VarObj
    VarObj* = object
        case kind*: VarKind
        of SimpleVar:
            svs*: Symbol
            svp*: pos
        of FieldVar:
            fvar*: VarR
            fvs*: Symbol
            fvp*: pos
        of SubscriptVar:
            subvar*: VarR
            exp*: Exp
            pos*: pos

    ExpKind* = enum
        VarExp
        NilExp
        IntExp
        StringExp
        CallExp
        OpExp
        RecordExp
        SeqExp
        AssignExp
        IfExp
        WhileExp
        ForExp
        BreakExp
        LetExp
        ArrayExp
    Exp* = ref ExpObj
    ExpObj* = object
        case kind*: ExpKind
        of VarExp:
            v*: VarR
        of NilExp:
            discard
        of IntExp:
            i*: int
        of StringExp:
            str*: string
            strp*: pos
        of CallExp:
            fun*: Symbol
            args*: seq[Exp]
            cp*: pos
        of OpExp:
            left*: Exp
            oper*: Oper
            right*: Exp
            opos*: pos
        of RecordExp:
            fields*: seq[(Symbol, Exp, pos)]
            rectyp*: Symbol
            rpos*: pos
        of SeqExp:
            eplist*: seq[(Exp, pos)]
        of AssignExp:
            avar*: VarR
            aexp*: Exp
            apos*: pos
        of IfExp:
            iftest*: Exp
            then*: Exp
            els*: Option[Exp]
            ifpos*: pos
        of WhileExp:
            wtest*: Exp
            wbody*: Exp
            wpos*: pos
        of ForExp:
            fvar*: Symbol
            escape*: bool
            lo*: Exp
            hi*: Exp
            fbody*: Exp
            fpos*: pos
        of BreakExp:
            bpos*: pos
        of LetExp:
            decs*: seq[Dec]
            letbody*: Exp
            letpos*: pos
        of ArrayExp:
            arrtyp*: Symbol
            size*: Exp
            init*: Exp
            arrpos*: pos
    DecKind* = enum
        FunctionDec
        VarDec
        TypeDec
    Dec* = ref DecObj
    DecObj* = object
        case kind*: DecKind
        of FunctionDec:
            fundecs*: seq[FunDec]
        of VarDec:
            vdname*: Symbol
            escape*: bool
            vdtyp*: Option[(Symbol, pos)]
            init*: Exp
            vdpos*: pos
        of TypeDec:
            decs*: seq[TyDec]
    TyDec* = object
        tdname*: Symbol
        tdty*: Ty
        tdpos*: pos
    TyKind* = enum
        NameTy
        RecordTy
        ArrayTy
    Ty* = ref TyObj
    TyObj* = object
        case kind*: TyKind
        of NameTy:
            nts*: Symbol
            ntpos*: pos
        of RecordTy:
            rtyfields*: seq[Field]
        of ArrayTy:
            arrs*: Symbol
            arrpos*: pos
    Oper* = enum
        PlusOp
        MinusOp
        TimesOp
        DivideOp
        EqOp
        NeqOp
        LtOp
        LeOp
        GtOp
        GeOp

    Field* = ref FieldObj
    FieldObj* = object
        name*: Symbol
        escape*: bool
        typ*: Symbol
        pos*: pos

    FunDec* = ref FunDecObj
    FunDecObj* = object
        name*: Symbol
        params*: seq[Field]
        result*: Option[(Symbol, pos)]
        body*: Exp
        pos*: pos



