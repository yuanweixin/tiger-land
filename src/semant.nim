import absyn
import options
import symbol
import translate

var typeTagCounter = 0

# A new tag is assigned to each array/record declaration.
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

# This is the type representation used by the type checker.
type Type* = object
    case kind*: TypeKind
    of RecordT:
        symTy*: seq[(Symbol, Type)]
        rtt*: TypeTag
    of ArrayT:
        ty*: Type
        att*: TypeTag
    of NameT:
        s*: Symbol
        tyopt*: Option[Type]
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
            assignable*: bool = true
        of FunEntry:
            formals*: seq[Type]
            result*: Type

type TEnv = SymTab[Type]
type VEnv = SymTab[EnvEntry]

type ExpTy = (TranslatedExp, Type)

# TODO fill us
var baseTEnv: var TEnv
var baseVEnv: var VEnv

proc error (hasErr: var bool, pos: int, msg: varargs[string, `$`]) =
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

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec) =
    discard

proc transTy*(tenv: var TEnv, ty: absyn.Ty): Type =
    ## translates the type expressions as found in ast to digsted type
    ## description that will be placed into the type environment. it maps
    ## absyn.Ty into semant.Type.
    discard

proc transProg*(ast: Exp) =
    discard
