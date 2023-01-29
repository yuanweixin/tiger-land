import absyn
import options
import symbol
import translate

{.experimental: "implicitDeref".}

var typeTagCounter = 0
type TypeTag = int

proc newTypeTag(): int =
    result = typeTagCounter
    inc typeTagCounter

type TypeKind* = enum
    RecordT
    NilT
    IntT
    StringT
    ArrayT
    UnitT
    NameT

type Type* = object
    case kind*: TypeKind
    of RecordT:
        symTy*: seq[(Symbol, Ty)]
        rtt*: TypeTag
    of ArrayT:
        ty*: Ty
        att*: TypeTag
    of NameT:
        s*: Symbol
        tyopt*: Option[Ty]
    of NilT, IntT, StringT, UnitT:
        discard

type
    EnvEntryKind* = enum
        VarEntry
        FunEntry
    EnvEntry* = object
        case kind*: EnvEntryKind
        of VarEntry:
            ty*: Type
        of FunEntry:
            formals*: seq[Type]
            result*: Type

type TEnv = SymTab[Type]
type VEnv = SymTab[EnvEntry]

type ExpTy = (TranslatedExp, Type)

# TODO fill us
var baseTEnv: var TEnv
var baseVEnv: var VEnv

proc error (hasErr: var bool, pos: int, msg: string) =
    hasErr = true
    echo pos, " ", msg

proc transVar*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        v: absyn.VarR): ExpTy =
    discard

proc transExp*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        e: absyn.Exp): ExpTy =
    case e.kind
    of OpExp:
        let (_, tyleft) = transExp(hasErr, venv, tenv, e.left)
        let (_, tyright) = transExp(hasErr, venv, tenv, e.right)
        case e.oper
        of PlusOp, MinusOp, TimesOp, DivideOp:
            if tyleft.kind != IntT:
                error hasErr, e.opos, "integer expected on lhs for arithmetic operator"
            if tyright.kind != IntT:
                error hasErr, e.opos, "integer expected on rhs for airthmetic operator"
            return (42, Type(kind: IntT))
        of LtOp, LeOp, GtOp, GeOp:
            if tyleft.kind != IntT:
                error hasErr, e.opos, "integer expected on lhs for number comparison"
            if tyright.kind != IntT:
                error hasErr, e.opos, "integer expected on rhs for number comparison"
            return (42, Type(kind: IntT))
        of EqOp, NeqOp:
            if tyleft.kind != tyright.kind:
                error hasErr, e.opos, "both sides must be of same type for comparison"
            else:
                let (lk, rk) = (tyleft.kind, tyright.kind)
                if (lk, rk) notin [(IntT, IntT), (ArrayT, ArrayT), (RecordT, RecordT)]:
                    error hasErr, e.opos, "comparison only supported for integer, array or record types"
            return (42, Type(kind: IntT))
    of NilExp:
        return (42, Type(kind: NilT))
    of IntExp:
        return (42, Type(kind: IntT))
    of VarExp:
        return "TODO"
    of StringExp:
        return (42, Type(kind: StringT))
    of CallExp:
        return "TODO"
    of RecordExp:
        return "TODO"
    of SeqExp:
        return "TODO"
    of AssignExp:
        return "TODO"
    of IfExp:
        let (_, tytest) = transExp(hasErr, venv, tenv, e.iftest)
        if tytest.kind != IntT:
            error hasErr, e.ifpos, "if condition must be of type integer"
        let (_, tythen) = transExp(hasErr, venv, tenv, e.then)
        if e.els.isNone:
            if tythen.kind != UnitT:
                error hasErr, e.ifpos, "if-then branch must not produce a value"
        else:
            let (_, tyelse) = transExp(hasErr, venv, tenv, e.els.get)
            if tythen.kind != tyelse.kind:
                error hasErr, e.ifpos, "if-then-else branches must have same type"
        # if type error happens with branches, will just return the then.
        return (42, tythen)
    of WhileExp:
        let (_, tytest) = transExp(hasErr, venv, tenv, e.wtest)
        if tytest.kind != IntT:
            error hasErr, e.wpos, "while condition must be of type integer"
        let (_, tybody) = transExp(hasErr, venv, tenv, e.wbody)
        if tybody.kind != UnitT:
            error hasErr, e.wpos, "while body must not produce a value"
        return (42, Type(kind: UnitT))
    of ForExp:
        let (_, tylo) = transExp(hasErr, venv, tenv, e.lo)
        let (_, tyhi) = transExp(hasErr, venv, tenv, e.hi)
        if tylo.kind != IntT or tyhi.kind != IntT:
            error hasErr, e.fpos, "values of for range must be integer"
        else:
            venv.beginScope()
            venv.enter(e.fvar, EnvEntry(kind: VarEntry, ty: Type(kind: IntT)))
            let (_, tyfbody) = transExp(hasErr, venv, tenv, e.fbody)
            if tyfbody.kind != UnitT:
                error hasErr, e.fpos, "for body must not produce a value"
            venv.endScope()
        return (42, Type(kind: UnitT))
    of BreakExp:
        # breaks need to be in context of a while or for, not crossing a procedure
        # call boundary, i.e. if p calls q and break is in q, does not affect p.
        # TODO fix this in part b.
        return (42, Type(kind: UnitT))
    of LetExp:
        return "TODO"
    of ArrayExp: # Id Lbrack exp Rbrack Of exp:
        let aty = tenv.look e.arrtyp
        if aty.isNone:
            error hasErr, e.arrpos, e.arrtyp.name & " is undefined."
        elif ty.get.kind != ArrayT:
            error hasErr, e.arrpos, e.arrtyp.name & " is not array type!"

        return "TODO"

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec) =
    discard

proc transTy*(tenv: var TEnv, ty: absyn.Ty): Type =
    ## translates the type expressions as found in ast to digsted type
    ## description that will be placed into the type environment. it maps
    ## absyn.Ty into semant.Type.
    discard

proc transProg*(ast: Exp) =
    discard
