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
    of NilExp:
        return (42, Type(kind: NilT))
    of IntExp:
        return (42, Type(kind: IntT))
    of VarExp:
        let (_, tyv) = transVar(hasErr, venv, tenv, e.v)
        return (42, tyv)
    of StringExp:
        return (42, Type(kind: StringT))
    # of CallExp:
        let fentryOpt = tenv.look e.fun
        if fentryOpt.isNone:
            error hasErr, e.cp, "Trying to call an undeclared function ", e.fun.name
        elif fentryOpt.get.kind != FunEntry:
            error hasErr, e.cp, e.fun.name, " is not a function!"
            if e.args.len != fentry.get.formals.len:
                error hasErr, e.cp, e.fun.name, " has ", fentry.get.formals.len,
                        " arguments but is called with ", e.args.len
            for i in 0..e.args.high:
                let (_, tyargi) = transExp(hasErr, venv, tenv, e.args[i])
                if tyargi != fentry.get.formals[i]:
                    error hasErr, e.cp, e.fun.name, " call expects type ",
                            fentry.get.formals[i], " at argument ", i+1,
                            " but got ", tyargi
            # TODO move to funcdec checking.
            # venv.beginScope()
            # for formal in fentry.get.formals:
            #     when defined(tigerdevel):
            #         doAssert formal.kind==NameT, "type checker bug, function args should always have type NameT"
            #         doAssert formal.tyopt.isSome, "type checker bug, function param does not have a type."
                # venv.enter formal.s, EnvEntry(kind: VarEntry, ty: formal.tyopt.get)
            # venv.endScope()
            return (42, fentryOpt.get.result)
    of FunEntry:
        # TODO
        return (42, Type(kind: StringT))
    of RecordExp:
        # TODO
        return (42, Type(kind: StringT))
    of SeqExp:
        # TODO
        return (42, Type(kind: StringT))
    of AssignExp:
        # nil can be assigned to record type vars
        let (_, lhs) = transVar(hasErr, venv, tenv, e.avar)
        let (_, rhs) = transExp(hasErr, venv, tenv, e.aexp)
        if rhs.kind == NilT and lhs.kind != RecordT:
            error hasErr, e.apos, "nil can only be assigned to a record type"
        if lhs.kind != rhs.kind:
            error hasErr, e.apos, "attempting to assign type ", rhs.kind,
                    " to ", lhs.kind
        # assignment produces no value
        return (42, Type(kind: UnitT))
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
            # the spec says the index of for loop cannot be assigned to.
            venv.enter(e.fvar, EnvEntry(kind: VarEntry, ty: Type(kind: IntT,
                    assignable: false)))
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
        # TODO
        return (42, Type(kind: StringT))
    of ArrayExp: # Id Lbrack exp Rbrack Of exp:
        # is this array type declared?
        let aty = tenv.look e.arrtyp
        if aty.isNone:
            error hasErr, e.arrpos, "trying to use an undeclared array type ", e.arrtyp.name
        elif ty.get.kind != ArrayT:
            error hasErr, e.arrpos, e.arrtyp.name, " is not array type!"
        # check the size and initial values.
        let (_, tysize) = transExp(hasErr, venv, tenv, e.size)
        if tysize.kind != IntT:
            error hasErr, e.arrpos, "must pass integer for array size"
        let (_, tyinit) = transExp(hasErr, venv, tenv, e.init)
        # the initializer type must match the array's element type.
        if tyinit.kind != aty.ty:
            error hasErr, e.arrpos, "array initializer type is ", tyinit.kind,
                    " but array is declared with type ", aty.ty
        return (42, aty.ty)

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec) =
    discard

proc transTy*(tenv: var TEnv, ty: absyn.Ty): Type =
    ## translates the type expressions as found in ast to digsted type
    ## description that will be placed into the type environment. it maps
    ## absyn.Ty into semant.Type.
    discard

proc transProg*(ast: Exp) =
    discard
