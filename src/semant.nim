
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
    result.enter symbol "int", Type(kind: IntT)
    result.enter symbol "string", Type(kind: StringT)

proc newBaseVEnv(): Venv =
    result.beginScope()
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
                error hasErr, v.svp, "use of undeclared variable."
                (42, Type(kind: ErrorT))
            else:
                (42, tyOpt.get.ty)
        of FieldVar: # id.id
            let (_, tyLhs) = transVar(hasErr, venv, tenv, v.fvar, loopContext)
            if tyLhs.kind == ErrorT:
                (42, Type(kind: ErrorT))
            elif tyLhs.kind != RecordT:
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
            let (_, tyLhs) = transVar(hasErr, venv, tenv, v.subvar, loopContext)
            if tyLhs.kind != ArrayT:
                error hasErr, v.pos, "tried to access a non-array"
                (42, Type(kind: ErrorT))
            else:
                let (_, tyExp) = transExp(hasErr, venv, tenv, v.exp, loopContext)
                if tyExp.kind != IntT:
                    error hasErr, v.pos, "array index is not an integer!"
                    (42, Type(kind: ErrorT))
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
                elif not typesCompatible(tyleft, tyright):
                    error hasErr, e.opos,
                            "both sides must be of same type for comparison, got ",
                            tyleft, " on lhs and got ", tyright, " on the rhs."
                    (42, Type(kind: ErrorT))
                elif tyleft.kind notin [IntT, ArrayT, RecordT, StringT] and
                        tyright.kind notin [IntT, ArrayT, RecordT, StringT]:
                    # note: this condition is after the compatibility check
                    # so we can just make sure at least one side has a valid
                    # type.
                    error hasErr, e.opos,
                            "comparison only supported for integer, array or record types, but is used on ",
                            tyleft, " and ", tyright
                    (42, Type(kind: ErrorT))
                else:
                    (42, Type(kind: IntT))
        of NilExp:
            (42, Type(kind: NilT))
        of IntExp:
            (42, Type(kind: IntT))
        of VarExp:
            let (_, tyv) = transVar(hasErr, venv, tenv, e.v, loopContext)
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
                    let (_, tyargi) = transExp(hasErr, venv, tenv, e.args[i], loopContext)
                    if tyargi.kind == ErrorT or fentryOpt.get.formals[i].kind == ErrorT:
                        err = true
                        break
                    elif not typesCompatible(tyargi, fentryOpt.get.formals[i]):
                        err = true
                        error hasErr, e.cp, e.fun.name, " call expects type ",
                                fentryOpt.get.formals[i], " at argument ", i+1,
                                " but got ", tyargi
                if err:
                    (42, Type(kind: ErrorT))
                else:
                    (42, fentryOpt.get.result)
        of RecordExp:
            let recEntryOpt = tenv.look e.rectyp
            if recEntryOpt.isNone:
                error hasErr, e.rpos, e.rectyp.name, " has not been declared"
                (42, Type(kind: ErrorT))
            elif recEntryOpt.get.kind != RecordT:
                if recEntryOpt.get.ty.kind != ErrorT:
                    error hasErr, e.rpos, e.rectyp.name, " is not a record type"
                (42, Type(kind: ErrorT))
            else:
                let expRecTy = recEntryOpt.get
                if expRecTy.symTy.len != e.fields.len:
                    error hasErr, e.rpos, e.rectyp.name, " does not have same fields as the ones given "
                    (42, Type(kind: ErrorT))
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
                        (42, Type(kind: ErrorT))
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
                (42, Type(kind: UnitT))
            else:
                (42, retVal)
        of AssignExp:
            # nil can be assigned to record type vars
            let (_, lhs) = transVar(hasErr, venv, tenv, e.avar, loopContext)
            let (_, rhs) = transExp(hasErr, venv, tenv, e.aexp, loopContext)
            if lhs.kind == ErrorT or rhs.kind == ErrorT:
                (42, Type(kind: ErrorT))
            elif rhs.kind == NilT and lhs.kind != RecordT:
                error hasErr, e.apos, "nil can only be assigned to a record type but is being assigned to ", lhs
                (42, Type(kind: ErrorT))
            elif not typesCompatible(lhs, rhs):
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
            let (_, tytest) = transExp(hasErr, venv, tenv, e.iftest, loopContext)
            if tytest.kind != IntT:
                if tytest.kind != ErrorT:
                    error hasErr, e.ifpos,
                            "if condition must be of type integer but got ", tytest.kind
                (42, Type(kind: ErrorT))
            else:
                let (_, tythen) = transExp(hasErr, venv, tenv, e.then, loopContext)
                if e.els.isNone:
                    if tythen.kind == ErrorT:
                        (42, Type(kind: ErrorT))
                    elif tythen.kind != UnitT:
                        error hasErr, e.ifpos,
                                "if-then branch must not produce a value, got ", tythen
                        (42, Type(kind: ErrorT))
                    else:
                        (42, tythen)
                else:
                    let (_, tyelse) = transExp(hasErr, venv, tenv, e.els.get, loopContext)
                    if not typesCompatible(tythen, tyelse):
                        error hasErr, e.ifpos,
                                "if-then-else branches must have same type, but got then with type ",
                                tythen, " else with type ", tyelse
                        (42, Type(kind: ErrorT))
                    else:
                        (42, tythen)
        of WhileExp:
            # TODO
            let (_, tytest) = transExp(hasErr, venv, tenv, e.wtest, loopContext)
            if tytest.kind != IntT:
                error hasErr, e.wpos, "while condition must be of type integer"
                (42, Type(kind: ErrorT))
            else:
                # TODO
                let (_, tybody) = transExp(hasErr, venv, tenv, e.wbody, loopContext)
                if tybody.kind != UnitT:
                    if tybody.kind != ErrorT:
                        error hasErr, e.wpos, "while body must not produce a value"
                    (42, Type(kind: ErrorT))
                else:
                    (42, Type(kind: UnitT))
        of ForExp:
            # technically, the
            let (_, tylo) = transExp(hasErr, venv, tenv, e.lo, loopContext)
            let (_, tyhi) = transExp(hasErr, venv, tenv, e.hi, loopContext)
            if tylo.kind != IntT or tyhi.kind != IntT:
                error hasErr, e.fpos, "values of for range must be integer"
                (42, Type(kind: ErrorT))
            else:
                venv.beginScope()
                venv.enter(e.fvar, EnvEntry(kind: VarEntry, ty: Type(
                        kind: IntT), readonly: true))
                let (_, tyfbody) = transExp(hasErr, venv, tenv, e.fbody, loopContext)
                venv.endScope()

                if tyfbody.kind != UnitT:
                    if tyfbody.kind != ErrorT:
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
                (42, Type(kind: ErrorT))
            elif atyOpt.get.kind != ArrayT:
                error hasErr, e.arrpos, e.arrtyp.name, " is not array type!"
                (42, Type(kind: ErrorT))
            else:
                # check the size and initial values.
                let (_, tysize) = transExp(hasErr, venv, tenv, e.size, loopContext)
                if tysize.kind != IntT:
                    error hasErr, e.arrpos, "must pass integer for array size"
                    (42, Type(kind: ErrorT))
                else:
                    let (_, tyinit) = transExp(hasErr, venv, tenv, e.init, loopContext)
                    # the initializer type must match the array's element type.
                    if tyinit != atyOpt.get.ty:
                        error hasErr, e.arrpos, "array initializer type is ",
                                tyinit.kind,
                                        " but array is declared with type ", atyOpt.get.ty
                        (42, Type(kind: ErrorT))
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
                Type(kind: ErrorT)
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
                    error hasErr, pos, typ.s, " is undeclared type."
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
                error hasErr, dec.tdpos, "the type name ", dec.tdname, " is declared more than once in a sequence of mutually recursive types, which is illegal."
                tenv.enter dec.tdname, Type(kind: ErrorT)
            else:
                seen.incl dec.tdname
                let ty = transTy(hasErr, tenv, dec.tdty)
                tenv.enter dec.tdname, ty
                tofixup.add (ty, dec.tdpos)
        when defined(tigerdevel):
            echo "tofixup: ", $tofixup
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
        var tofixup: seq[(Type, pos)]
        for fundec in d.fundecs:
            if fundec.name in seen:
                error hasErr, fundec.pos, fundec.name, " is declared more than once in a sequence of mutually recursive types, which is illegal."
                # probably need to insert an error entry here so that later
                # typechecks of function body fails or knows to error?
            else:
                seen.incl fundec.name

            let retTy =
                if fundec.result.isNone:
                    Type(kind: UnitT)
                else:
                    let (ressym, pos) = fundec.result.get
                    let restyOpt = tenv.look ressym
                    if restyOpt.isNone:
                        let t = Type(kind: NameT, s: ressym)
                        tofixup.add (t, pos)
                        t
                    else:
                        restyOpt.get

            var formals: seq[Type]
            for field in fundec.params:
                let fieldTyOpt = tenv.look field.typ
                if fieldTyOpt.isNone:
                    let t = Type(kind: NameT, s: field.typ)
                    tofixup.add (t, field.pos)
                    formals.add t
                else:
                    formals.add fieldTyOpt.get
            var funentry = EnvEntry(kind: FunEntry, formals: formals, result: retTy)

            venv.enter fundec.name, funentry

        # do the fix up.
        for fundec in d.fundecs:
            var funentry = (venv.look fundec.name).get
            var changed = false
            when defined(tigerdevel):
                doAssert funentry.formals.len == fundec.params.len,
                        "bug, FunEntry.formals different len than in AST, funentry.formals.len=" &
                        $funentry.formals.len & " fundec.params.len=" &
                        $fundec.params.len
            var i = 0
            while i < fundec.params.len:
                if funentry.formals[i].kind == NameT:
                    changed = true
                    let lookup = tenv.look funentry.formals[i].s
                    if lookup.isNone:
                        error hasErr, fundec.params[i].pos, funentry.formals[
                                i].s.name, " is undeclared type."
                        funentry.formals[i] = Type(kind: ErrorT)
                    else:
                        funentry.formals[i] = lookup.get
                inc i
            if funentry.result.kind == NameT:
                let lookup = tenv.look funentry.result.s
                if lookup.isNone:
                    changed = true
                    error hasErr, fundec.result.get[1], funentry.result.s.name, " is undeclared type."
                    funentry.result = Type(kind: ErrorT)
                else:
                    funentry.result = lookup.get
            if changed:
                # FunEntry is a value type.
                # we could have written a proc to return a lvalue from
                # the env's internal table entry, but
                # mutation is generally evil and hard to get right, so
                # here we will just override the existing entry with
                # the updated one. it will get popped once we exit the
                # venv scope anyway. call it, the quasi-functional approach.
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
            let (_, tyRes) = transExp(hasErr, venv, tenv, fundec.body, loopContext)
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
                venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: Type(kind: ErrorT))
            elif not typesCompatible(tyExp, retTyOpt.get):
                error hasErr, pos, "Return type of ", retTyOpt.get,
                        " does not match actual type of initializer, ", tyExp
                venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: Type(kind: ErrorT))
            else:
                venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: retTyOpt.get)
        elif tyExp.kind == NilT:
            error hasErr, d.vdpos, "Variable ", d.vdname.name, " is declared with unknown type and initialized with nil. Fix by annotating it with the type you want."
            venv.enter d.vdname, EnvEntry(kind: VarEntry, ty: Type(kind: ErrorT))
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
