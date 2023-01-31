import absyn
import options
import symbol
import translate
import sets
import sequtils

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

proc `==`*(a, b: Type): bool =
    if a.kind != b.kind:
        return false
    case a.kind
    of RecordT:
        return a.symTy == b.symTy and a.rtt == b.rtt
    of ArrayT:
        return a.att == b.att and a.ty == b.ty
    of NameT:
        return a.s == b.s and a.tyopt == b.tyopt
    else:
        return true

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
            readonly*: bool
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

proc newBaseTEnv(): TEnv =
    ## the built-in types
    result.enter symbol "int", Type(kind: IntT)
    result.enter symbol "string", Type(kind: StringT)

proc newBaseVEnv(): Venv =
    ## the built-in vars and functions
    result.enter symbol "print", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT)], result: Type(kind: UnitT))
    result.enter symbol "flush", EnvEntry(kind: FunEntry, formals: @[],
            result: Type(kind: UnitT))
    result.enter symbol "getchar", EnvEntry(kind: FunEntry, formals: @[],
            result: Type(kind: StringT))
    result.enter symbol "ord", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT)], result: Type(kind: IntT))
    result.enter symbol "chr", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: IntT)], result: Type(kind: StringT))
    result.enter symbol "size", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT)], result: Type(kind: IntT))
    result.enter symbol "substring", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT), Type(kind: IntT), Type(kind: IntT)], result: Type(kind: StringT))
    result.enter symbol "concat", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT), Type(kind: StringT)], result: Type(kind: StringT))
    result.enter symbol "not", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: IntT)], result: Type(kind: IntT))
    result.enter symbol "exit", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: IntT)], result: Type(kind: UnitT))



proc error (hasErr: var bool, pos: int, msg: varargs[string, `$`]) =
    hasErr = true
    echo pos, " ", msg

proc transExp*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        e: absyn.Exp): ExpTy

proc transVar*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        v: absyn.VarR): ExpTy =
    result =
        case v.kind
        of SimpleVar:
            let tyOpt = venv.look v.svs
            if tyOpt.isNone:
                error hasErr, v.svp, "use of undeclared variable."
                (42, Type(kind: ErrorT))
            else:
                (42, tyOpt.get.ty)
        of FieldVar: # id.id
            let (_, tyLhs) = transVar(hasErr, venv, tenv, v.fvar)
            if tyLhs.kind != RecordT:
                error hasErr, v.fvp, "tried to access field of a non-record type ", tyLhs.kind
                (42, Type(kind: ErrorT))
            else:
                var ret: Type = nil
                for (sym, symty) in tyLhs.symTy:
                    if sym == v.fvs:
                        ret = symty
                        break
                if isNil(ret):
                    error hasErr, v.fvp, "Field ", v.fvs.name,
                            " is not part of record ", tyLhs
                    (42, Type(kind: ErrorT))
                else:
                    (42, ret)
        of SubscriptVar:
            let (_, tyLhs) = transVar(hasErr, venv, tenv, v.subvar)
            if tyLhs.kind != ArrayT:
                error hasErr, v.pos, "tried to access a non-array"
                (42, Type(kind: ErrorT))
            else:
                let (_, tyExp) = transExp(hasErr, venv, tenv, v.exp)
                if tyExp.kind != IntT:
                    error hasErr, v.pos, "array index is not an integer!"
                    (42, Type(kind: ErrorT))
                else:
                    (42, tyLhs.ty)


proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec)

proc transExp*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        e: absyn.Exp): ExpTy =
    result =
        case e.kind
        of OpExp:
            let (_, tyleft) = transExp(hasErr, venv, tenv, e.left)
            let (_, tyright) = transExp(hasErr, venv, tenv, e.right)
            case e.oper
            of PlusOp, MinusOp, TimesOp, DivideOp:
                if tyleft.kind != IntT and tyleft.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on lhs for arithmetic operator"
                    (42, Type(kind: ErrorT))
                elif tyright.kind != IntT and tyright.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on rhs for airthmetic operator"
                    (42, Type(kind: ErrorT))
                elif tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (42, Type(kind: ErrorT))
                else:
                    (42, Type(kind: IntT))
            of LtOp, LeOp, GtOp, GeOp:
                if tyleft.kind != IntT and tyleft.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on lhs for number comparison"
                    (42, Type(kind: ErrorT))
                elif tyright.kind != IntT and tyright.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on rhs for number comparison"
                    (42, Type(kind: ErrorT))
                elif tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (42, Type(kind: ErrorT))
                else:
                    (42, Type(kind: IntT))
            of EqOp, NeqOp:
                if tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (42, Type(kind: ErrorT))
                elif tyleft.kind != tyright.kind:
                    error hasErr, e.opos, "both sides must be of same type for comparison"
                    (42, Type(kind: ErrorT))
                elif tyleft.kind notin [IntT, ArrayT, RecordT, StringT]:
                    error hasErr, e.opos,
                            "comparison only supported for integer, array or record types, but is used on ",
                            tyleft.kind, tyright.kind
                    (42, Type(kind: ErrorT))
                else:
                    (42, Type(kind: IntT))
        of NilExp:
            (42, Type(kind: NilT))
        of IntExp:
            (42, Type(kind: IntT))
        of VarExp:
            let (_, tyv) = transVar(hasErr, venv, tenv, e.v)
            (42, tyv)
        of StringExp:
            (42, Type(kind: StringT))
        of CallExp:
            let fentryOpt = venv.look e.fun
            if fentryOpt.isNone:
                error hasErr, e.cp, "Trying to call an undeclared function ", e.fun.name
                (42, Type(kind: ErrorT))
            elif fentryOpt.get.kind != FunEntry:
                error hasErr, e.cp, e.fun.name, " is not a function!"
                (42, Type(kind: ErrorT))
            elif e.args.len != fentryOpt.get.formals.len:
                error hasErr, e.cp, e.fun.name, " has ",
                        fentryOpt.get.formals.len,
                                " arguments but is called with ", e.args.len
                (42, Type(kind: ErrorT))
            else:
                var err = false
                for i in 0..e.args.high:
                    let (_, tyargi) = transExp(hasErr, venv, tenv, e.args[i])
                    if tyargi.kind == ErrorT or fentryOpt.get.formals[i].kind == ErrorT:
                        err = true
                        break
                    elif tyargi != fentryOpt.get.formals[i]:
                        err = true
                        error hasErr, e.cp, e.fun.name, " call expects type ",
                                fentryOpt.get.formals[i], " at argument ", i+1,
                                " but got ", tyargi
                if err:
                    (42, Type(kind: ErrorT))
                else:
                    (42, fentryOpt.get.result)
        of RecordExp:
            let recEntryOpt = venv.look e.rectyp
            if recEntryOpt.isNone:
                error hasErr, e.rpos, e.rectyp.name, " has not been declared"
                (42, Type(kind: ErrorT))
            elif recEntryOpt.get.kind != VarEntry or recEntryOpt.get.ty.kind != RecordT:
                if recEntryOpt.get.ty.kind != ErrorT:
                    error hasErr, e.rpos, e.rectyp.name, " is not a record type"
                (42, Type(kind: ErrorT))
            else:
                let expRecTy = recEntryOpt.get.ty
                if expRecTy.symTy.len != e.fields.len:
                    error hasErr, e.rpos, e.rectyp.name, " does not have same fields as the ones given "
                    (42, Type(kind: ErrorT))
                else:
                    var err = false
                    for (sym, exp, pos) in e.fields:
                        for (symInTy, typExpected) in expRecTy.symTy:
                            if sym == symInTy:
                                let (_, tyActual) = transExp(hasErr, venv, tenv, exp)
                                if typExpected != tyActual:
                                    err = true
                                    error hasErr, pos, sym.name,
                                            " is expected to have type ",
                                            typExpected, " but got ", tyActual
                    if err:
                        (42, Type(kind: ErrorT))
                    else:
                        (42, expRecTy)
        of SeqExp:
            var i = 0
            var retVal: Type = nil
            while i < e.eplist.len:
                let (_, tyExp) = transExp(hasErr, venv, tenv, e.eplist[i][0])
                inc i
                if i == e.eplist.len:
                    retVal = tyExp
            if isNil(retVal):
                (42, Type(kind: UnitT))
            else:
                (42, retVal)
        of AssignExp:
            # nil can be assigned to record type vars
            let (_, lhs) = transVar(hasErr, venv, tenv, e.avar)
            let (_, rhs) = transExp(hasErr, venv, tenv, e.aexp)
            if lhs.kind == ErrorT or rhs.kind == ErrorT:
                (42, Type(kind: ErrorT))
            elif rhs.kind == NilT and lhs.kind != RecordT:
                error hasErr, e.apos, "nil can only be assigned to a record type"
                (42, Type(kind: ErrorT))
            elif lhs.kind != rhs.kind:
                error hasErr, e.apos, "attempting to assign type ", rhs.kind,
                        " to ", lhs.kind
                (42, Type(kind: ErrorT))
            else:
                if e.avar.kind == SimpleVar:
                    let ventry = venv.look e.avar.svs
                    doAssert ventry.isSome, "bug in impl"
                    # this can occur in for-loop counter.
                    if ventry.get.readonly:
                        error hasErr, e.apos, "cannot assign to readonly location."
                        (42, Type(kind: ErrorT))
                    else:
                        (42, Type(kind: UnitT))
                else:
                    (42, Type(kind: UnitT))
        of IfExp:
            let (_, tytest) = transExp(hasErr, venv, tenv, e.iftest)
            if tytest.kind != IntT:
                if tytest.kind != ErrorT:
                    error hasErr, e.ifpos,
                            "if condition must be of type integer but got ", tytest.kind
                (42, Type(kind: ErrorT))
            else:
                let (_, tythen) = transExp(hasErr, venv, tenv, e.then)
                if e.els.isNone:
                    if tythen.kind != UnitT:
                        error hasErr, e.ifpos, "if-then branch must not produce a value"
                        (42, Type(kind: ErrorT))
                    else:
                        (42, tythen)
                else:
                    let (_, tyelse) = transExp(hasErr, venv, tenv, e.els.get)
                    if tythen.kind != tyelse.kind:
                        error hasErr, e.ifpos, "if-then-else branches must have same type"
                        (42, Type(kind: ErrorT))
                    else:
                        (42, tythen)
        of WhileExp:
            let (_, tytest) = transExp(hasErr, venv, tenv, e.wtest)
            if tytest.kind != IntT:
                error hasErr, e.wpos, "while condition must be of type integer"
                (42, Type(kind: ErrorT))
            else:
                let (_, tybody) = transExp(hasErr, venv, tenv, e.wbody)
                if tybody.kind != UnitT:
                    error hasErr, e.wpos, "while body must not produce a value"
                    (42, Type(kind: ErrorT))
                else:
                    (42, Type(kind: UnitT))
        of ForExp:
            let (_, tylo) = transExp(hasErr, venv, tenv, e.lo)
            let (_, tyhi) = transExp(hasErr, venv, tenv, e.hi)
            if tylo.kind != IntT or tyhi.kind != IntT:
                error hasErr, e.fpos, "values of for range must be integer"
                (42, Type(kind: ErrorT))
            else:
                venv.beginScope()
                # TODO the spec says the index of for loop cannot be assigned to.
                venv.enter(e.fvar, EnvEntry(kind: VarEntry, ty: Type(
                        kind: IntT), readonly: true))
                let (_, tyfbody) = transExp(hasErr, venv, tenv, e.fbody)
                venv.endScope()
                if tyfbody.kind != UnitT:
                    error hasErr, e.fpos, "for body must not produce a value"
                    (42, Type(kind: ErrorT))
                else:
                    (42, Type(kind: UnitT))
        of BreakExp:
            # breaks need to be in context of a while or for, not crossing a procedure
            # call boundary, i.e. if p calls q and break is in q, does not affect p.
            # TODO fix this in part b.
            (42, Type(kind: UnitT))
        of LetExp:
            tenv.beginScope
            venv.beginScope
            for dec in e.decs:
                transDec(hasErr, venv, tenv, dec)
            let (_, tyExp) = transExp(hasErr, venv, tenv, e.letbody)
            tenv.endScope
            venv.endScope
            (42, tyExp)
        of ArrayExp: # Id Lbrack exp Rbrack Of exp:
            # is this array type declared?
            let atyOpt = tenv.look e.arrtyp
            if atyOpt.isNone:
                error hasErr, e.arrpos, "trying to use an undeclared array type ", e.arrtyp.name
                (42, Type(kind: ErrorT))
            elif atyOpt.get.kind != ArrayT:
                error hasErr, e.arrpos, e.arrtyp.name, " is not array type!"
                (42, Type(kind: ErrorT))
            else:
                # check the size and initial values.
                let (_, tysize) = transExp(hasErr, venv, tenv, e.size)
                if tysize.kind != IntT:
                    error hasErr, e.arrpos, "must pass integer for array size"
                    (42, Type(kind: ErrorT))
                else:
                    let (_, tyinit) = transExp(hasErr, venv, tenv, e.init)
                    # the initializer type must match the array's element type.
                    if tyinit != atyOpt.get:
                        error hasErr, e.arrpos, "array initializer type is ",
                                tyinit.kind,
                                        " but array is declared with type ", atyOpt.get
                        (42, Type(kind: ErrorT))
                    else:
                        (42, atyOpt.get)

proc transTy*(hasErr: var bool, tenv: var TEnv, ty: absyn.Ty): Type =
    ## translates the type expressions as found in ast to digsted type
    ## description that will be placed into the type environment. it maps
    ## absyn.Ty into semant.Type.
    result =
        case ty.kind
        of NameTy: # Id
            let tyOpt = tenv.look ty.nts
            if tyOpt.isNone:
                error hasErr, ty.ntpos, ty.nts.name, " is not declared"
                Type(kind: ErrorT)
            else:
                Type(kind: NameT, s: ty.nts, tyopt: tyOpt)
        of ArrayTy: # Array of Id
            let tyOpt = tenv.look ty.arrs
            if tyOpt.isNone:
                error hasErr, ty.arrpos, ty.arrs.name, " is not declared and hence is invalid for use as array element"
                Type(kind: ErrorT)
            else:
                Type(kind: ArrayT, ty: tyOpt.get, att: newTypeTag())
        of RecordTy: # Lbrace fields Rbrace
            var seen: HashSet[Symbol]
            var symTyp: seq[(Symbol, Type)]
            var err = false # track the local error, diff than global hasErr var.
            for field in ty.rtyfields:
                if field.name in seen:
                    err = true
                    error hasErr, field.pos,
                            "duplicate declaration of record field ", field.name
                else:
                    seen.incl field.name
                    let tyOpt = tenv.look field.typ
                    if tyOpt.isNone:
                        err = true
                        error hasErr, field.pos, "undeclared type ",
                                field.typ.name, " used in record, declare it first"
                    else:
                        symTyp.add (field.name, tyOpt.get)
            if err:
                Type(kind: ErrorT)
            else:
                Type(kind: RecordT, symTy: symTyp, rtt: newTypeTag())

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec) =
    case d.kind
    of TypeDec: # type id eq ty
        for dec in d.decs:
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
                        Type(kind: ErrorT)
                    else:
                        restyOpt.get

            # extract the NameT types from the params.
            # the ty info is used to construct the FunEntry.
            # the (name, ty) is injected into the venv prior to body type check.
            var params: seq[Type]
            for field in fundec.params:
                let fieldTyOpt = tenv.look field.typ
                if fieldTyOpt.isNone:
                    error hasErr, field.pos, field.typ.name, " used in function declaration is not a valid type"
                    params.add Type(kind: ErrorT)
                else:
                    params.add Type(kind: NameT, s: field.name,
                            tyopt: fieldTyOpt)

            var formals: seq[Type]
            for p in params:
                formals.add p.tyopt.get
            var funentry = EnvEntry(kind: FunEntry, formals: formals, result: retTy)

            venv.enter fundec.name, funentry
            venv.beginScope()
            # inject the function's params as VarEntry prior to type checking body.
            for p in params:
                let varEntry = EnvEntry(kind: VarEntry, ty: p.tyopt.get)
                venv.enter p.s, varEntry
            let (_, tyRes) = transExp(hasErr, venv, tenv, fundec.body)
            if retTy != tyRes and retTy.kind != ErrorT and tyRes.kind != ErrorT:
                error hasErr, fundec.pos, "expected return type of ", retTy,
                        " for function but got ", tyRes
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

proc transProg*(ast: Exp): bool =
    var baseTEnv = newBaseTEnv()
    var baseVEnv = newBaseVEnv()
    ## return whether there was type errors.
    baseVEnv.beginScope()
    baseTEnv.beginScope()
    var hasErr = false
    discard transExp(hasErr, baseVEnv, baseTEnv, ast)
    return hasErr
