import absyn
import options
import symbol

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

proc transProg*(ast: Exp) =
    discard
