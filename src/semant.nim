
import absyn
import options
import symbol
import translate
import sets
import strutils
import hashes

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

type Type* = ref object
    # This is the type representation used by the type checker.
    case kind*: TypeKind
    of RecordT:
        symTy*: seq[(Symbol, Type)]
        rtt*: TypeTag
    of ArrayT:
        ty*: Type
        att*: TypeTag
    of NameT:
        s*: Symbol
    of NilT, IntT, StringT, UnitT, ErrorT:
        discard

let ErrorTy = Type(kind: ErrorT)
let UnitTy = Type(kind: UnitT)
let IntTy = Type(kind: IntT)
let StringTy = Type(kind: StringT)
let NilTy = Type(kind: NilT)

proc hash(t: Type): Hash =
    # just copy the hash*[T](x: ref[T) in hashes.nim
    # because that needs -d:nimPreviewHashRef, ridiculous.
    return hash(cast[pointer](t))

proc helper(t: Type, s: var string, seen: var HashSet[Type]) =
    # to string helper.
    # handles recursion by tracking Type, which is a ref obj.
    case t.kind
    of NilT:
        s.add "NilT"
    of IntT:
        s.add "IntT"
    of StringT:
        s.add "StringT"
    of UnitT:
        s.add "UnitT"
    of ErrorT:
        s.add "ErrorT"
    of NameT:
        s.add "NameT(s:"
        s.add t.s.name
        s.add ")"
    of RecordT:
        seen.incl t
        s.add "RecordT(symTy:["
        for (sym, ty) in t.symTy:
            s.add "("
            s.add sym.name
            s.add ","
            if ty notin seen:
                seen.incl ty
                helper(ty, s, seen)
            else:
                s.add "..."
            s.add ") "
        s.add "rtt: "
        s.add $t.rtt
        s.add ")"
    of ArrayT:
        seen.incl t
        s.add "ArrayT(ty: "
        if t.ty notin seen:
            seen.incl t.ty
            helper(t.ty, s, seen)
        else:
            s.add "..."
        s.add ", att"
        s.add $t.att
        s.add ")"

proc `$`*(t: Type): string =
    var seen: HashSet[Type] # for recursion...
    helper(t, result, seen)

func `==`*(a, b: Type): bool =
    if a.kind != b.kind:
        return false
    case a.kind
    of RecordT:
        return a.symTy == b.symTy and a.rtt == b.rtt
    of ArrayT:
        return a.att == b.att and a.ty == b.ty
    of NameT:
        return a.s == b.s
    else:
        return true

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
    result.beginScope()
    ## the built-in types
    result.enter symbol "int", IntTy
    result.enter symbol "string", StringTy

proc newBaseVEnv(): Venv =
    result.beginScope()
    ## the built-in vars and functions
    result.enter symbol "print", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT)], result: UnitTy)
    result.enter symbol "flush", EnvEntry(kind: FunEntry, formals: @[],
            result: UnitTy)
    result.enter symbol "getchar", EnvEntry(kind: FunEntry, formals: @[],
            result: StringTy)
    result.enter symbol "ord", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT)], result: IntTy)
    result.enter symbol "chr", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: IntT)], result: StringTy)
    result.enter symbol "size", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT)], result: IntTy)
    result.enter symbol "substring", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT), IntTy, IntTy], result: StringTy)
    result.enter symbol "concat", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: StringT), StringTy], result: StringTy)
    result.enter symbol "not", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: IntT)], result: IntTy)
    result.enter symbol "exit", EnvEntry(kind: FunEntry, formals: @[Type(
            kind: IntT)], result: UnitTy)

proc errorDedup(ty: Type, hasErr: var bool, pos: int, msg: varargs[string, `$`]) =
    hasErr = true
    # stfu about errors we already warned about.
    if ty.kind != ErrorT:
        echo pos, " ", msg.join("")

proc error(hasErr: var bool, pos: int, msg: varargs[string, `$`]) =
    hasErr = true
    echo pos, " ", msg.join("")


proc transExp*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        e: absyn.Exp, loopContext: bool): ExpTy

proc transVar*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        v: absyn.VarR, loopContext: bool): ExpTy =
    result =
        case v.kind
        of SimpleVar:
            let tyOpt = venv.look v.svs
            if tyOpt.isNone:
                error hasErr, v.svp, "use of undeclared variable ", v.svs.name
                (42, ErrorTy)
            else:
                (42, tyOpt.get.ty)
        of FieldVar: # id.id
            let (_, tyLhs) = transVar(hasErr, venv, tenv, v.fvar, loopContext)
            if tyLhs.kind != RecordT:
                errorDedup tyLhs, hasErr, v.fvp,
                        "tried to access field of a non-record type ", tyLhs.kind
                (42, ErrorTy)
            else:
                var ret: Type = nil
                for (sym, symty) in tyLhs.symTy:
                    if sym == v.fvs:
                        ret = symty
                        break
                if isNil(ret):
                    error hasErr, v.fvp, "Field ", v.fvs.name,
                            " is not part of record ", tyLhs
                    (42, ErrorTy)
                else:
                    (42, ret)
        of SubscriptVar:
            let (_, tyLhs) = transVar(hasErr, venv, tenv, v.subvar, loopContext)
            if tyLhs.kind != ArrayT:
                errorDedup tyLhs, hasErr, v.pos, "tried to access a non-array ", tyLhs.kind
                (42, ErrorTy)
            else:
                let (_, tyExp) = transExp(hasErr, venv, tenv, v.exp, loopContext)
                if tyExp.kind != IntT:
                    errorDedup tyExp, hasErr, v.pos, "array index is not an integer!"
                    (42, ErrorTy)
                else:
                    (42, tyLhs.ty)

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec,
        loopContext: bool)

func typesCompatible(a, b: Type): bool =
    return a == b or (a.kind, b.kind) in [(NilT, RecordT), (RecordT, NilT)]

proc transExp*(hasErr: var bool, venv: var VEnv, tenv: var TEnv,
        e: absyn.Exp, loopContext: bool): ExpTy =
    # note: the case statement uses implicit return
    # so that we force the compiler to check there
    # is a return value on all paths.
    result =
        case e.kind
        of OpExp:
            let (_, tyleft) = transExp(hasErr, venv, tenv, e.left, loopContext)
            let (_, tyright) = transExp(hasErr, venv, tenv, e.right, loopContext)
            case e.oper
            of PlusOp, MinusOp, TimesOp, DivideOp:
                if tyleft.kind != IntT and tyleft.kind != ErrorT:
                    error hasErr, e.opos,
                            "integer expected on lhs for arithmetic operator got ", tyleft.kind
                    (42, ErrorTy)
                elif tyright.kind != IntT and tyright.kind != ErrorT:
                    error hasErr, e.opos,
                            "integer expected on rhs for arithmetic operator but got ", tyright.kind
                    (42, ErrorTy)
                elif tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (42, ErrorTy)
                else:
                    (42, IntTy)
            of LtOp, LeOp, GtOp, GeOp:
                if tyleft.kind != IntT and tyleft.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on lhs for number comparison"
                    (42, ErrorTy)
                elif tyright.kind != IntT and tyright.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on rhs for number comparison"
                    (42, ErrorTy)
                elif tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (42, ErrorTy)
                else:
                    (42, IntTy)
            of EqOp, NeqOp:
                if tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (42, ErrorTy)
                elif not typesCompatible(tyleft, tyright):
                    error hasErr, e.opos,
                            "both sides must be of same type for comparison, got ",
                            tyleft, " on lhs and got ", tyright, " on the rhs."
                    (42, ErrorTy)
                elif tyleft.kind notin [IntT, ArrayT, RecordT, StringT] and
                        tyright.kind notin [IntT, ArrayT, RecordT, StringT]:
                    # note: this condition is after the compatibility check
                    # so we can just make sure at least one side has a valid
                    # type.
                    error hasErr, e.opos,
                            "comparison only supported for integer, array or record types, but is used on ",
                            tyleft, " and ", tyright
                    (42, ErrorTy)
                else:
                    (42, IntTy)
        of NilExp:
            (42, NilTy)
        of IntExp:
            (42, IntTy)
        of VarExp:
            let (_, tyv) = transVar(hasErr, venv, tenv, e.v, loopContext)
            (42, tyv)
        of StringExp:
            (42, StringTy)
        of CallExp:
            let fentryOpt = venv.look e.fun
            if fentryOpt.isNone:
                error hasErr, e.cp, "Trying to call an undeclared function ", e.fun.name
                (42, ErrorTy)
            elif fentryOpt.get.kind != FunEntry:
                error hasErr, e.cp, e.fun.name, " is not a function!"
                (42, ErrorTy)
            elif e.args.len != fentryOpt.get.formals.len:
                error hasErr, e.cp, e.fun.name, " has ",
                        fentryOpt.get.formals.len,
                                " arguments but is called with ", e.args.len
                (42, ErrorTy)
            else:
                var err = false
                for i in 0..e.args.high:
                    let (_, tyargi) = transExp(hasErr, venv, tenv, e.args[i], false)
                    if tyargi.kind == ErrorT or fentryOpt.get.formals[i].kind == ErrorT:
                        err = true
                        break
                    elif not typesCompatible(tyargi, fentryOpt.get.formals[i]):
                        err = true
                        error hasErr, e.cp, e.fun.name, " call expects type ",
                                fentryOpt.get.formals[i], " at argument ", i+1,
                                " but got ", tyargi
                if err:
                    (42, ErrorTy)
                else:
                    (42, fentryOpt.get.result)
        of RecordExp:
            let recEntryOpt = tenv.look e.rectyp
            if recEntryOpt.isNone:
                error hasErr, e.rpos, e.rectyp.name, " has not been declared"
                (42, ErrorTy)
            elif recEntryOpt.get.kind != RecordT:
                errorDedup recEntryOpt.get.ty, hasErr, e.rpos, e.rectyp.name, " is not a record type"
                (42, ErrorTy)
            else:
                let expRecTy = recEntryOpt.get
                if expRecTy.symTy.len != e.fields.len:
                    error hasErr, e.rpos, e.rectyp.name, " does not have same fields as the ones given "
                    (42, ErrorTy)
                else:
                    var err = false
                    for (sym, exp, pos) in e.fields:
                        for (symInTy, typExpected) in expRecTy.symTy:
                            if sym == symInTy:
                                let (_, tyActual) = transExp(hasErr, venv, tenv,
                                        exp, loopContext)
                                if not typesCompatible(typExpected, tyActual):
                                    err = true
                                    error hasErr, pos, sym.name,
                                            " is expected to have type ",
                                            typExpected, " but got ", tyActual
                    if err:
                        (42, ErrorTy)
                    else:
                        (42, expRecTy)
        of SeqExp:
            var i = 0
            var retVal: Type = nil
            while i < e.eplist.len:
                let (_, tyExp) = transExp(hasErr, venv, tenv, e.eplist[i][0], loopContext)
                inc i
                if i == e.eplist.len:
                    retVal = tyExp
            if isNil(retVal):
                (42, UnitTy)
            else:
                (42, retVal)
        of AssignExp:
            # nil can be assigned to record type vars
            let (_, lhs) = transVar(hasErr, venv, tenv, e.avar, loopContext)
            let (_, rhs) = transExp(hasErr, venv, tenv, e.aexp, loopContext)
            if lhs.kind == ErrorT or rhs.kind == ErrorT:
                (42, ErrorTy)
            elif rhs.kind == NilT and lhs.kind != RecordT:
                error hasErr, e.apos, "nil can only be assigned to a record type but is being assigned to ", lhs
                (42, ErrorTy)
            elif not typesCompatible(lhs, rhs):
                error hasErr, e.apos, "attempting to assign type ", rhs.kind,
                        " to ", lhs.kind
                (42, ErrorTy)
            else:
                if e.avar.kind == SimpleVar:
                    let ventry = venv.look e.avar.svs
                    doAssert ventry.isSome, "bug in impl"
                    # this can occur in for-loop counter.
                    if ventry.get.readonly:
                        error hasErr, e.apos, "cannot assign to readonly location."
                        (42, ErrorTy)
                    else:
                        (42, UnitTy)
                else:
                    (42, UnitTy)
        of IfExp:
            let (_, tytest) = transExp(hasErr, venv, tenv, e.iftest, loopContext)
            if tytest.kind != IntT:
                errorDedup tytest, hasErr, e.ifpos,
                            "if condition must be of type integer but got ", tytest.kind
                (42, ErrorTy)
            else:
                let (_, tythen) = transExp(hasErr, venv, tenv, e.then, loopContext)
                if e.els.isNone:
                    if tythen.kind != UnitT:
                        errorDedup tythen, hasErr, e.ifpos,
                                "if-then branch must not produce a value, got ", tythen
                        (42, ErrorTy)
                    else:
                        (42, tythen)
                else:
                    let (_, tyelse) = transExp(hasErr, venv, tenv, e.els.get, loopContext)
                    if not typesCompatible(tythen, tyelse):
                        if tythen.kind != ErrorT and tyelse.kind != ErrorT:
                            error hasErr, e.ifpos,
                                "if-then-else branches must have same type, but got then with type ",
                                tythen, " else with type ", tyelse
                        (42, ErrorTy)
                    else:
                        (42, tythen)
        of WhileExp:
            let (_, tytest) = transExp(hasErr, venv, tenv, e.wtest, true)
            if tytest.kind != IntT:
                errorDedup tytest, hasErr, e.wpos, "while condition must be of type integer"
                (42, ErrorTy)
            else:
                let (_, tybody) = transExp(hasErr, venv, tenv, e.wbody, true)
                if tybody.kind != UnitT:
                    errorDedup tybody, hasErr, e.wpos, "while body must not produce a value"
                    (42, ErrorTy)
                else:
                    (42, UnitTy)
        of ForExp:
            # technically, the
            let (_, tylo) = transExp(hasErr, venv, tenv, e.lo, true)
            let (_, tyhi) = transExp(hasErr, venv, tenv, e.hi, true)
            var err = false
            if tylo.kind != IntT:
                err = true
                errorDedup tylo, hasErr, e.fpos, "lo of for range must be integer"
            if tyhi.kind != IntT:
                err = true
                errorDedup tyhi, hasErr, e.fpos, "hi of for range must be integer"
            venv.beginScope()
            venv.enter(e.fvar, EnvEntry(kind: VarEntry, ty: Type(
                    kind: IntT), readonly: true))
            let (_, tyfbody) = transExp(hasErr, venv, tenv, e.fbody, true)
            venv.endScope()

            if tyfbody.kind != UnitT:
                errorDedup tyfbody, hasErr, e.fpos, "for body must not produce a value"
                (42, ErrorTy)
            else:
                if err: (42, ErrorTy) else: (42, UnitTy)
        of BreakExp:
            # breaks need to be in context of a while or for, not crossing a procedure
            # call boundary, i.e. if p calls q and break is in q, does not affect p.
            if not loopContext:
                error hasErr, e.bpos, "break is valid only in a while/for loop"
                (42, ErrorTy)
            else:
                (42, UnitTy)
        of LetExp:
            tenv.beginScope
            venv.beginScope
            for dec in e.decs:
                transDec(hasErr, venv, tenv, dec, loopContext)
            let (_, tyExp) = transExp(hasErr, venv, tenv, e.letbody, loopContext)
            tenv.endScope
            venv.endScope
            (42, tyExp)
        of ArrayExp: # Id Lbrack exp Rbrack Of exp:
            # is this array type declared?
            let atyOpt = tenv.look e.arrtyp
            if atyOpt.isNone:
                error hasErr, e.arrpos, "trying to use an undeclared array type ", e.arrtyp.name
                (42, ErrorTy)
            elif atyOpt.get.kind != ArrayT:
                errorDedup atyOpt.get, hasErr, e.arrpos, e.arrtyp.name, " is not array type!"
                (42, ErrorTy)
            else:
                # check the size and initial values.
                let (_, tysize) = transExp(hasErr, venv, tenv, e.size, loopContext)
                if tysize.kind != IntT:
                    errorDedup tysize, hasErr, e.arrpos, "must pass integer for array size"
                    (42, ErrorTy)
                else:
                    let (_, tyinit) = transExp(hasErr, venv, tenv, e.init, loopContext)
                    # the initializer type must match the array's element type.
                    if tyinit != atyOpt.get.ty:
                        errorDedup tyinit, hasErr, e.arrpos,
                                "array initializer type is ", tyinit.kind,
                                        " but array is declared with type ", atyOpt.get.ty
                        (42, ErrorTy)
                    else:
                        (42, atyOpt.get)

proc transTy*(hasErr: var bool, tenv: var TEnv, ty: absyn.Ty): Type =
    ## translates the type expressions as found in ast to digsted type
    ## description that will be placed into the type environment. it maps
    ## absyn.Ty into semant.Type.
    # note: the case statement uses implicit return
    # so that we force the compiler to check there
    # is a return value on all paths.
    result =
        case ty.kind
        of NameTy: # Id
            let tyOpt = tenv.look ty.nts
            if tyOpt.isNone:
                # only use NameT as a placeholder.
                Type(kind: NameT, s: ty.nts)
            else:
                tyOpt.get
        of ArrayTy: # Array of Id
            let tyOpt = tenv.look ty.arrs
            if tyOpt.isNone:
                Type(kind: ArrayT, ty: Type(kind: NameT, s: ty.arrs),
                        att: newTypeTag())
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
                        symTyp.add (field.name, Type(kind: NameT, s: field.typ))
                    else:
                        symTyp.add (field.name, tyOpt.get)
            if err:
                ErrorTy
            else:
                Type(kind: RecordT, symTy: symTyp, rtt: newTypeTag())

proc fixup(hasErr: var bool, tenv: var TEnv, tofix: var Type, pos: pos) =
    # we can mutate `tofix` because it's a ref type.
    case tofix.kind
    of NameT:
        let tyopt = tenv.look tofix.s
        if tyopt.isNone:
            error hasErr, pos, tofix.s.name, " is undeclared type."
        else:
            tofix = tyopt.get
    of ArrayT:
        if tofix.ty.kind == NameT:
            let tyopt = tenv.look tofix.ty.s
            if tyopt.isNone:
                error hasErr, pos, tofix.ty.s.name, " is undeclared type."
            else:
                tofix.ty = tyopt.get
    of RecordT:
        for (sym, typ) in tofix.symTy.mitems():
            if typ.kind == NameT:
                let tyopt = tenv.look typ.s
                if tyopt.isNone:
                    error hasErr, pos, typ.s.name, " is undeclared type."
                else:
                    typ = tyopt.get
    else:
        when defined(tigerdevel):
            doAssert tofix.kind == ErrorT, "bug in impl, expecting only ErrorT to show in the else branch of case statement for fixing up missing types."
        discard # ErrorT could show up here.

proc transDec*(hasErr: var bool, venv: var VEnv, tenv: var TEnv, d: absyn.Dec,
        loopContext: bool) =
    case d.kind
    of TypeDec: # seq[type id eq ty] 
        # spec from appendix:
        # mutually recursive types are declared by a conseq seq of type dec
        # without intervening value or func dec. Each recursion cycle must 
        # pass through a record or array type. 
        # 
        # ...no two types in a seq of mutually recursive types may have the
        # same name.
        var seen: HashSet[Symbol]
        var tofixup: seq[(Type, pos)]
        for dec in d.decs:
            if dec.tdname in seen:
                error hasErr, dec.tdpos, "the type name ", dec.tdname.name, " is declared more than once in a sequence of mutually recursive types, which is illegal."
                tenv.enter dec.tdname, ErrorTy
            else:
                seen.incl dec.tdname
                let ty = transTy(hasErr, tenv, dec.tdty)
                tenv.enter dec.tdname, ty
                tofixup.add (ty, dec.tdpos)
        when defined(tigerdevel):
            echo "tofixup: ", $tofixup
        # cycle detection: if it's all NameT back to the dec it's bad.
        for dec in d.decs:
            seen.clear # reuse.
            var ty = (tenv.look dec.tdname).get
            seen.incl dec.tdname
            while ty.kind == NameT:
                if ty.s in seen:
                    error hasErr, dec.tdpos,
                            "circular type definition detected for type ",
                            dec.tdname.name
                    break
                seen.incl ty.s
                ty = (tenv.look ty.s).get
        # now fix up the stuff we just added.
        for (tofix, pos) in tofixup.mitems():
            fixup(hasErr, tenv, tofix, pos)
        when defined(tigerdevel):
            echo "after fixup ", $tofixup
    of FunctionDec: # Function Id Lparen fields Rparen type_opt Eq exp
        # no two functions in a sequence of mutually recursive functions 
        # may have the same name. 
        # 
        # functions may be recursive. mutually recursive functions and procs
        # are declared by a seq of conseq func declarations with no intervening
        # type or var decls.
        var seen: HashSet[Symbol]
        for fundec in d.fundecs:
            if fundec.name in seen:
                error hasErr, fundec.pos, fundec.name.name, " is declared more than once in a sequence of mutually recursive types, which is illegal."
            else:
                seen.incl fundec.name

            let retTy =
                if fundec.result.isNone:
                    UnitTy
                else:
                    let (ressym, _) = fundec.result.get
                    let restyOpt = tenv.look ressym
                    if restyOpt.isNone:
                        ErrorTy
                    else:
                        restyOpt.get

            var formals: seq[Type]
            for field in fundec.params:
                let fieldTyOpt = tenv.look field.typ
                if fieldTyOpt.isNone:
                    formals.add ErrorTy
                else:
                    formals.add fieldTyOpt.get
            var funentry = EnvEntry(kind: FunEntry, formals: formals, result: retTy)

            venv.enter fundec.name, funentry

        # type check the Exp of the functions.
        for fundec in d.fundecs:
            let funentry = (venv.look fundec.name).get
            var i = 0
            venv.beginScope()
            while i < fundec.params.len:
                let varEntry = EnvEntry(kind: VarEntry, ty: funentry.formals[i])
                venv.enter fundec.params[i].name, varEntry
                inc i
            let (_, tyRes) = transExp(hasErr, venv, tenv, fundec.body, false)
            if not typesCompatible(funentry.result, tyRes) and
                    funentry.result.kind != ErrorT and tyRes.kind != ErrorT:
                error hasErr, fundec.pos, "expected return type of ",
                        funentry.result, " for function but got ", tyRes
            venv.endScope()
    of VarDec: # Var Id type_opt Assign exp:
        let (_, tyExp) = transExp(hasErr, venv, tenv, d.init, loopContext)
        if d.vdtyp.isSome:
            let (retTypeSym, pos) = d.vdtyp.get
            let retTyOpt = tenv.look retTypeSym
            if retTyOpt.isNone:
                error hasErr, pos, "Return type ", retTyOpt.get, " is unknown"
                venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: ErrorTy)
            elif not typesCompatible(tyExp, retTyOpt.get):
                error hasErr, pos, "Return type of ", retTyOpt.get,
                        " does not match actual type of initializer, ", tyExp
                venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: ErrorTy)
            else:
                venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: retTyOpt.get)
        elif tyExp.kind == NilT:
            errorDedup tyExp, hasErr, d.vdpos, "Variable ", d.vdname.name,
                    " is declared with unknown type and initialized with nil. Fix by using the long form, e.g. var ",
                    d.vdname.name, " : <my_type> = ..."
            venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: ErrorTy)
        else:
            # no return type specified. infer it from the initializer.
            venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: tyExp)

proc transProg*(ast: Exp): Option[TranslatedExp] =
    var baseTEnv = newBaseTEnv()
    var baseVEnv = newBaseVEnv()
    ## return whether there was type errors.
    var hasErr = false
    let (texp, _) = transExp(hasErr, baseVEnv, baseTEnv, ast,
            loopContext = false)
    if hasErr:
        return none[TranslatedExp]()
    return some[TranslatedExp](texp)
