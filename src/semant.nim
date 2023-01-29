import absyn
import options
import symbol
import translate
import sets

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
    ErrorT # returned when type checking fails

           # This is the type representation used by the type checker.
type Type* = ref object
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
    of NilT, IntT, StringT, UnitT, ErrorT:
        discard

proc `$`(x: Type): string =
    return $x[]

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

# separate namespaces for type declaration vs var/func decls
# to handle cases like this:
# let type a = int
#      var a:a := 5
#      var b:b := 5
#   in b+a
# end
type TEnv = SymTab[Type]
type VEnv = SymTab[EnvEntry]

type ExpTy = (TranslatedExp, Type)

# TODO fill us
var baseTEnv: TEnv
var baseVEnv: VEnv


proc error (hasErr: var bool, pos: int, msg: varargs[string, `$`]) =
    hasErr = true
    echo pos, " ", msg

proc transExp*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        e: absyn.Exp): ExpTy

proc transVar*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        v: absyn.VarR): ExpTy =
    case v.kind
    of SimpleVar:
        let tyOpt = venv.look v.svs
        if tyOpt.isNone:
            error hasErr, v.svp, "use of undeclared variable."
            raise newException(Exception, "todo return some error type")
        return (42, tyOpt.get.ty)
    of FieldVar: # id.id
        let (_, tyLhs) = transVar(hasErr, venv, tenv, v.fvar)
        if tyLhs.kind != RecordT:
            error hasErr, v.fvp, "tried to access field of a non-record type"
        else:
            for (sym, symty) in tyLhs.symTy:
                if sym == v.fvs:
                    return (42, symty)
            error hasErr, v.fvp, "Field ", v.fvs.name,
                    " is not part of record ", tyLhs
        raise newException(Exception, "todo return error type")
    of SubscriptVar:
        let (_, tyLhs) = transVar(hasErr, venv, tenv, v.subvar)
        if tyLhs.kind != ArrayT:
            error hasErr, v.pos, "tried to access a non-array"
        else:
            let (_, tyExp) = transExp(hasErr, venv, tenv, v.exp)
            if tyExp.kind != IntT:
                error hasErr, v.pos, "array index is not an integer!"
            else:
                return (42, tyLhs.ty)
        raise newException(Exception, "todo return err type")

## TODO gonna write it with global error var, and pretend this is first site of type
## error, and see what happens.

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec)

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
    of CallExp:
        let fentryOpt = venv.look e.fun
        if fentryOpt.isNone:
            error hasErr, e.cp, "Trying to call an undeclared function ", e.fun.name
        elif fentryOpt.get.kind != FunEntry:
            error hasErr, e.cp, e.fun.name, " is not a function!"
        else:
            if e.args.len != fentryOpt.get.formals.len:
                error hasErr, e.cp, e.fun.name, " has ",
                        fentryOpt.get.formals.len,
                                " arguments but is called with ", e.args.len
            for i in 0..e.args.high:
                let (_, tyargi) = transExp(hasErr, venv, tenv, e.args[i])
                if tyargi != fentryOpt.get.formals[i]:
                    error hasErr, e.cp, e.fun.name, " call expects type ",
                            fentryOpt.get.formals[i], " at argument ", i+1,
                            " but got ", tyargi
            return (42, fentryOpt.get.result)
    of RecordExp:
        let recEntryOpt = venv.look e.rectyp
        if recEntryOpt.isNone:
            error hasErr, e.rpos, e.rectyp.name, " has not been declared"
        elif recEntryOpt.get.kind != VarEntry or recEntryOpt.get.ty.kind != RecordT:
            error hasErr, e.rpos, e.rectyp.name, " is not a record type"
        else:
            let expRecTy = recEntryOpt.get.ty
            if expRecTy.symTy.len != e.fields.len:
                error hasErr, e.rpos, e.rectyp.name, " does not have same fields as the ones given "
            else:
                for (sym, exp, pos) in e.fields:
                    for (symInTy, typExpected) in expRecTy.symTy:
                        if sym == symInTy:
                            let (_, tyActual) = transExp(hasErr, venv, tenv, exp)
                            if typExpected != tyActual:
                                error hasErr, pos, sym.name,
                                        " is expected to have type ",
                                        typExpected, " but got ", tyActual
            return (42, expRecTy)
        raise newException(Exception, "todo handle when can't return a type")
    of SeqExp:
        var i = 0
        while i < e.eplist.len:
            let (_, tyExp) = transExp(hasErr, venv, tenv, e.eplist[i][0])
            inc i
            if i == e.eplist.len:
                return (42, tyExp)
        return (42, Type(kind: UnitT))
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
            venv.enter(e.fvar, EnvEntry(kind: VarEntry, ty: Type(kind: IntT),
                    assignable: false))
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
        tenv.beginScope
        venv.beginScope
        for dec in e.decs:
            # TODO this probably should indicate error, so
            # that type checking can be stopped if decl doesn't check out.
            transDec(hasErr, venv, tenv, dec)
        let (_, tyExp) = transExp(hasErr, venv, tenv, e.letbody)
        tenv.endScope
        venv.endScope
        return (42, tyExp)
    of ArrayExp: # Id Lbrack exp Rbrack Of exp:
        # is this array type declared?
        let atyOpt = tenv.look e.arrtyp
        if atyOpt.isNone:
            error hasErr, e.arrpos, "trying to use an undeclared array type ", e.arrtyp.name
        elif atyOpt.get.kind != ArrayT:
            error hasErr, e.arrpos, e.arrtyp.name, " is not array type!"
        # check the size and initial values.
        let (_, tysize) = transExp(hasErr, venv, tenv, e.size)
        if tysize.kind != IntT:
            error hasErr, e.arrpos, "must pass integer for array size"
        let (_, tyinit) = transExp(hasErr, venv, tenv, e.init)
        # the initializer type must match the array's element type.
        if tyinit != atyOpt.get:
            error hasErr, e.arrpos, "array initializer type is ", tyinit.kind,
                    " but array is declared with type ", atyOpt.get
        return (42, atyOpt.get)

proc transTy*(hasErr: var bool, tenv: var TEnv, ty: absyn.Ty): Type =
    ## translates the type expressions as found in ast to digsted type
    ## description that will be placed into the type environment. it maps
    ## absyn.Ty into semant.Type.
    case ty.kind
    of NameTy: # Id
        let tyOpt = tenv.look ty.nts
        if tyOpt.isNone:
            error hasErr, ty.ntpos, ty.nts.name, " is not declared"
            raise newException(Exception, "todo return error type")
        else:
            return Type(kind: NameT, s: ty.nts, tyopt: tyOpt)
    of ArrayTy: # Array of Id
        let tyOpt = tenv.look ty.arrs
        if tyOpt.isNone:
            error hasErr, ty.arrpos, ty.arrs.name, " is not declared and hence is invalid for use as array element"
            raise newException(Exception, "todo return error type")
        return Type(kind: ArrayT, ty: tyOpt.get, att: newTypeTag())
    of RecordTy: # Lbrace fields Rbrace
        # no duplicate names.
        var seen: HashSet[Symbol]
        var symTyp: seq[(Symbol, Type)]
        for field in ty.rtyfields:
            if field.name in seen:
                error hasErr, field.pos,
                        "duplicate declaration of record field ", field.name
                raise newException(Exception, "todo return error type")
            else:
                seen.incl field.name
                let tyOpt = tenv.look field.typ
                if tyOpt.isNone:
                    error hasErr, field.pos, "undeclared type ", field.typ.name, " used in record, declare it first"
                    raise newException(Exception, "todo return error type")
                else:
                    symTyp.add (field.name, tyOpt.get)
        return Type(kind: RecordT, symTy: symTyp, rtt: newTypeTag())

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec) =
    case d.kind
    of TypeDec: # type id eq ty
        for dec in d.decs:
            # TODO error handling?
            let ty = transTy(hasErr, tenv, dec.tdty)
            tenv.enter dec.tdname, ty
    of FunctionDec: # Function Id Lparen fields Rparen type_opt Eq exp
        # TODO add in the recursive defs by doing another pass.
        for fundec in d.fundecs:
            let retTy =
                if fundec.result.isNone:
                    Type(kind: UnitT)
                else:
                    let (ressym, respos) = fundec.result.get
                    let restyOpt = tenv.look ressym
                    if restyOpt.isNone:
                        error hasErr, respos, ressym.name, " is not a valid type"
                        raise newException(Exception, "todo return error type")
                    restyOpt.get

            # extract the NameT types from the params.
            # the ty info is used to construct the FunEntry.
            # the (name, ty) is injected into the venv prior to body type check.
            var params: seq[Type]
            for field in fundec.params:
                let fieldTyOpt = tenv.look field.typ
                if fieldTyOpt.isNone:
                    error hasErr, field.pos, field.typ.name, " used in function declaration is not a valid type"
                    raise newException(Exception, "todo")
                else:
                    params.add Type(kind: NameT, s: field.name,
                            tyopt: fieldTyOpt)

            var formals: seq[Type]
            for p in params:
                formals.add p.tyopt.get
            let funentry = EnvEntry(kind: FunEntry, formals: formals, result: retTy)

            venv.enter fundec.name, funentry
            # inject the function's params as VarEntry prior to type checking body.
            venv.beginScope()
            for p in params:
                let varEntry = EnvEntry(kind: VarEntry, ty: p.tyopt.get)
                venv.enter p.s, varEntry
            let (_, tyRes) = transExp(hasErr, venv, tenv, fundec.body)
            if retTy != tyRes:
                error hasErr, fundec.pos, "expected return type of ", retTy,
                        " for function but got ", tyRes
                raise newException(Exception, "todo return error type")
            venv.endScope()
    of VarDec: # Var Id type_opt Assign exp:
        let (_, tyExp) = transExp(hasErr, venv, tenv, d.init)
        if d.vdtyp.isSome:
            let (retTypeSym, pos) = d.vdtyp.get
            let retTyOpt = tenv.look retTypeSym
            if retTyOpt.isNone:
                error hasErr, pos, "Return type ", retTyOpt.get, " is unknown"
            elif tyExp != retTyOpt.get:
                error hasErr, pos, "Return type of ", retTyOpt.get,
                        " does not match actual type of initializer, ", tyExp
        let varEntry = EnvEntry(kind: VarEntry, ty: tyExp)
        venv.enter d.vdname, varEntry


proc transProg*(ast: Exp) =
    discard
