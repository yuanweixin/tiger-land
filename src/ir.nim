## appel's tree ir language. 
import temp

type 
    IrExpKind* {.pure.} = enum
        Const
        Name
        Temp
        Binop
        Mem
        Call
        Eseq
    IrStmKind* {.pure.} = enum
        Move
        Exp
        Jump
        Cjump
        Seq
        Label
    IrBinopKind* {.pure.} = enum
        Plus
        Minus
        Mul
        Div
        And
        Or
        Lshift
        Rshift
        ArShift
        Xor
    IrRelopKind* {.pure.} = enum
        Eq
        Ne
        Lt
        Gt
        Le
        Ge
        Ult
        Ule
        Ugt
        Uge
    IrExp* = ref IrExpObj
    IrExpObj = object
        case kind*: IrExpKind
        of Const:
            i*: int
        of Name: 
            n*: temp.Label
        of Temp:
            t*: temp.Temp
        of Binop:
            o*: IrBinopKind
            e1*: IrExp 
            e2*: IrExp 
        of Mem: 
            e*: IrExp
        of Call: 
            f*: IrExp
            l*: seq[IrExp]
        of Eseq: 
            s*: IrStm
            ee*: IrExp
    IrStm* = ref IrStmObj
    IrStmObj = object 
        case kind*: IrStmKind 
        of Move: 
            dst*: IrExp
            src*: IrExp
        of Exp: ## evaluate and discard result. 
            e*: IrExp
        of Jump: 
            to*: IrExp
            labs*: seq[temp.Label]
        of Cjump:
            o*: IrRelopKind
            e1*: IrExp
            e2*: IrExp
            t*: temp.Label
            f*: temp.Label
        of Seq: 
            s1*: IrStm
            s2*: IrStm
        of Label:
            n*: temp.Label
        
# constructors. 
proc Const*(i: int) : IrExp = 
    return IrExp(kind: Const, i: i)

proc Name*(n: Label) : IrExp = 
    return IrExp(kind: Name, n: n)

proc Temp*(t: temp.Temp) : IrExp = 
    return IrExp(kind: Temp, t: t)

proc Binop*(o: IrBinopKind, e1: IrExp, e2: IrExp) : IrExp = 
    return IrExp(kind: Binop, o: o, e1: e1, e2: e2)

proc Mem*(e: IrExp) : IrExp = 
    return IrExp(kind: Mem, e: e)

proc Call*(f: IrExp, l: seq[IrExp]) : IrExp = 
    return IrExp(kind: Call, f: f, l: l)

proc Eseq*(s: IrStm, ee: IrExp) : IrExp = 
    return IrExp(kind: Eseq, s: s, ee: ee)

proc Move*(dst: IrExp, src: IrExp) : IrStm = 
    return IrStm(kind: Move, dst: dst, src: src)

proc Exp*(e: IrExp) : IrStm = 
    return IrStm(kind: Exp, e: e)

proc Jump*(to: IrExp, labs: seq[temp.Label]) : IrStm = 
    return IrStm(kind: Jump, to: to, labs: labs)

proc Cjump*(o: IrRelopKind, e1: IrExp, e2: IrExp, t: temp.Label, f: temp.Label) : IrStm = 
    return IrStm(kind: Cjump, o:o, e1: e1, e2: e2, t:t, f:f)

proc Seq*(s1: IrStm, s2: IrStm) : IrStm = 
    return IrStm(kind: Seq, s1: s1, s2: s2)

proc Label*(n: temp.Label): IrStm = 
    return IrStm(kind: Label, n: n)