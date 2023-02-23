
import absyn
import dev
import options
import symbol
import translate
import sets
import strutils
import hashes
import escape

var typeTagCounter = 0
# A new tag is assigned to each array/record declaration.
type TypeTag = int

proc newTypeTag(): int =
    result = typeTagCounter
    inc typeTagCounter

type TypeKind* {.pure.} = enum
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
        symTy*: seq[(Symbol, Type)] ## note: this ordering is used in IR. 
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

proc toStringHelper(t: Type, s: var string, seen: var HashSet[Type]) =
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
                toStringHelper(ty, s, seen)
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
            toStringHelper(t.ty, s, seen)
        else:
            s.add "..."
        s.add ", att"
        s.add $t.att
        s.add ")"

proc `$`*(t: Type): string =
    var seen: HashSet[Type] # for recursion...
    toStringHelper(t, result, seen)

func `==`*(a, b: Type): bool =
    if isNil(a):
        return isNil(b)
    if isNil(b):
        return false
        
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
    EnvEntryKind* {.pure.} = enum
        VarEntry
        FunEntry
    EnvEntry*[T] = object
        case kind*: EnvEntryKind
        of VarEntry:
            access*: Access[T]
            ty*: Type
            readonly*: bool
        of FunEntry:
            level*: Level[T]
            label*: Label
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
type VEnv[T] = SymTab[EnvEntry[T]]

type ExpTy = (TrExp, Type)

proc newBaseTEnv(): TEnv =
    result.beginScope()
    
    ## the built-in types
    result.enter symbol "int", IntTy
    result.enter symbol "string", StringTy


proc newBaseVEnv[T](): Venv[T] =
    result.beginScope()
    
    ## the built-in vars and functions
    result.enter symbol "print", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: StringT)], result: UnitTy)
    
    result.enter symbol "flush", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[], result: UnitTy)
    
    result.enter symbol "getchar", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[], result: StringTy)
    
    result.enter symbol "ord", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: StringT)], result: IntTy)
    
    result.enter symbol "chr", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: IntT)], result: StringTy)
    
    result.enter symbol "size", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: StringT)], result: IntTy)
    
    result.enter symbol "substring", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: StringT), IntTy, IntTy], result: StringTy)

    result.enter symbol "concat", EnvEntry[T](kind: FunEntry,level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: StringT), StringTy], result: StringTy)

    result.enter symbol "not", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: IntT)], result: IntTy)
    
    result.enter symbol "exit", EnvEntry[T](kind: FunEntry, level: outerMostLevel[T](), label: newLabel(), formals: @[Type(kind: IntT)], result: UnitTy)

proc errorDedup(ty: Type, hasErr: var bool, pos: int, msg: varargs[string, `$`]) =
    hasErr = true
    # stfu about errors we already warned about.
    if ty.kind != ErrorT:
        echo pos, " ", msg.join("")

proc error(hasErr: var bool, pos: int, msg: varargs[string, `$`]) =
    hasErr = true
    echo pos, " ", msg.join("")


proc transExp*[T](ctx: var TranslateCtx[T], level: Level[T], hasErr: var bool, venv: var VEnv[T], tenv: var TEnv, e: absyn.Exp, doneLabel: Option[Label]): ExpTy

proc transVar*[T](ctx: var TranslateCtx[T], level: Level[T], hasErr: var bool, venv: var VEnv[T], tenv: var TEnv, v: absyn.VarR, doneLabel: Option[Label]): ExpTy =
    result =
        case v.kind
        of SimpleVar:
            let entryOpt = venv.look v.svs
            if entryOpt.isNone:
                error hasErr, v.svp, "use of undeclared variable ", v.svs.name
                (translate.ErrorTyExp, ErrorTy)
            elif entryOpt.get.kind == FunEntry:
                # in the current version of tiger 
                # SimpleVar can occur in
                # 1. id
                # 2. id.id
                # 3. subscripts
                # and in none of these contexts is function supported. 
                # this scenario happens when function shadows
                # the var definition, then it is used in a 
                # context that expects a var. 
                error hasErr, v.svp, "variable expected but got a function"
                (translate.ErrorTyExp, ErrorTy)
            elif entryOpt.get.ty.kind == ErrorT:
                (translate.ErrorTyExp, ErrorTy)
            else:
                (translate.simpleVar(entryOpt.get.access, level), entryOpt.get.ty)
        of FieldVar: # id.id
            let (lhsIr, tyLhs) = transVar(ctx, level, hasErr, venv, tenv, v.fvar, doneLabel)
            if tyLhs.kind != RecordT:
                errorDedup tyLhs, hasErr, v.fvp,
                        "tried to access field of a non-record type ", tyLhs.kind
                (translate.ErrorTyExp, ErrorTy)
            else:
                var ret: Type = nil
                var symPos = 0 
                for (sym, symty) in tyLhs.symTy:
                    if sym == v.fvs:
                        ret = symty
                        break
                    else:
                        inc symPos 
                if isNil(ret):
                    error hasErr, v.fvp, "Field ", v.fvs.name,
                            " is not part of record ", tyLhs
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    # cute toy language every record field 
                    # is scalar or pointer so they have same
                    # size, so we don't even have to do any 
                    # extra work calculating the record size. 
                    (translate.recordField[T](lhsIr, symPos), ret)
        of SubscriptVar:
            let (lhsIr, tyLhs) = transVar(ctx, level, hasErr, venv, tenv, v.subvar, doneLabel)
            if tyLhs.kind != ArrayT:
                errorDedup tyLhs, hasErr, v.pos, "tried to access a non-array ", tyLhs.kind
                (translate.ErrorTyExp, ErrorTy)
            else:
                let (expIr, tyExp) = transExp(ctx, level, hasErr, venv, tenv, v.exp, doneLabel)
                if tyExp.kind != IntT:
                    errorDedup tyExp, hasErr, v.pos, "array index is not an integer!"
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    let exitEntry = venv.look (symbol "exit")
                    devAssert exitEntry.isSome, "impl bug, missing 'exit' built-in function declaration."
                    devAssert exitEntry.get.kind == FunEntry, "impl bug, 'exit' should be a built-in function but it's not"
                    let exitCallLabel = exitEntry.get.label
                    (translate.subscriptVar[T](lhsIr, expIr, exitCallLabel), tyLhs.ty)

proc transDec*[T](ctx: var TranslateCtx[T], level: Level[T], hasErr: var bool, venv: var VEnv[T], tenv: var TEnv, d: absyn.Dec, doneLabel: Option[Label], varExpList: var seq[TrExp])

func typesCompatible(a, b: Type): bool =
    return a == b or (a.kind, b.kind) in [(NilT, RecordT), (RecordT, NilT)]

proc transExp*[T](ctx: var TranslateCtx[T], level: Level[T], hasErr: var bool, venv: var VEnv[T], tenv: var TEnv, e: absyn.Exp, doneLabel: Option[Label]): ExpTy =
    # note: the case statement uses implicit return
    # so that we force the compiler to check there
    # is a return value on all paths.
    result =
        case e.kind
        of OpExp:
            let (lhsIr, tyleft) = transExp(ctx, level, hasErr, venv, tenv, e.left, doneLabel)
            let (rhsIr, tyright) = transExp(ctx, level, hasErr, venv, tenv, e.right, doneLabel)
            case e.oper
            of PlusOp, MinusOp, TimesOp, DivideOp:
                if tyleft.kind != IntT and tyleft.kind != ErrorT:
                    error hasErr, e.opos,
                            "integer expected on lhs for arithmetic operator got ", tyleft.kind
                    (translate.ErrorTyExp, ErrorTy)
                elif tyright.kind != IntT and tyright.kind != ErrorT:
                    error hasErr, e.opos,
                            "integer expected on rhs for arithmetic operator but got ", tyright.kind
                    (translate.ErrorTyExp, ErrorTy)
                elif tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    (translate.binop(e.oper, lhsIr, rhsIr), IntTy)
            of LtOp, LeOp, GtOp, GeOp:
                if tyleft.kind != IntT and tyleft.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on lhs for number comparison"
                    (translate.ErrorTyExp, ErrorTy)
                elif tyright.kind != IntT and tyright.kind != ErrorT:
                    error hasErr, e.opos, "integer expected on rhs for number comparison"
                    (translate.ErrorTyExp, ErrorTy)
                elif tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    (translate.binop(e.oper, lhsIr, rhsIr), IntTy)
            of EqOp, NeqOp:
                if tyleft.kind == ErrorT or tyright.kind == ErrorT:
                    (translate.ErrorTyExp, ErrorTy)
                elif not typesCompatible(tyleft, tyright):
                    error hasErr, e.opos,
                            "both sides must be of same type for comparison, got ",
                            tyleft, " on lhs and got ", tyright, " on the rhs."
                    (translate.ErrorTyExp, ErrorTy)
                elif tyleft.kind notin [IntT, ArrayT, RecordT, StringT] and
                        tyright.kind notin [IntT, ArrayT, RecordT, StringT]:
                    # note: this condition is after the compatibility check
                    # so we can just make sure at least one side has a valid
                    # type.
                    error hasErr, e.opos,
                            "comparison only supported for integer, array or record types, but is used on ",
                            tyleft, " and ", tyright
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    if tyleft.kind == StringT and tyright.kind == StringT:
                        (translate.stringCmp[T](e.oper == EqOp, lhsIr, rhsIr), IntTy)
                    else:
                        (translate.binop(e.oper, lhsIr, rhsIr), IntTy)
        of NilExp:
            (translate.nilExp(), NilTy)
        of IntExp:
            (translate.intExp(e.i), IntTy)
        of VarExp:
            transVar(ctx, level, hasErr, venv, tenv, e.v, doneLabel)
        of StringExp:
            (translate.stringExp[T](ctx, e.str), StringTy)
        of CallExp:
            let fentryOpt = venv.look e.fun
            if fentryOpt.isNone:
                error hasErr, e.cp, "Trying to call an undeclared function ", e.fun.name
                (translate.ErrorTyExp, ErrorTy)
            elif fentryOpt.get.kind != FunEntry:
                error hasErr, e.cp, e.fun.name, " is not a function!"
                (translate.ErrorTyExp, ErrorTy)
            elif e.args.len != fentryOpt.get.formals.len:
                error hasErr, e.cp, e.fun.name, " has ",
                        fentryOpt.get.formals.len,
                                " arguments but is called with ", e.args.len
                (translate.ErrorTyExp, ErrorTy)
            else:
                var err = false
                var argIrs : seq[TrExp]
                for i in 0..e.args.high:
                    let (argiIr, tyargi) = transExp(ctx, level, hasErr, venv, tenv, e.args[i], none[Label]())
                    if tyargi.kind == ErrorT or fentryOpt.get.formals[i].kind == ErrorT:
                        err = true
                        break
                    elif not typesCompatible(tyargi, fentryOpt.get.formals[i]):
                        err = true
                        error hasErr, e.cp, e.fun.name, " call expects type ",
                                fentryOpt.get.formals[i], " at argument ", i+1,
                                " but got ", tyargi
                    argIrs.add argiIr
                if err:
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    (translate.callExp(fentryOpt.get.label, callerLevel=level, argIrs, calleeLevel=fentryOpt.get.level), fentryOpt.get.result)
        of RecordExp:
            let recEntryOpt = tenv.look e.rectyp
            if recEntryOpt.isNone:
                error hasErr, e.rpos, e.rectyp.name, " has not been declared"
                (translate.ErrorTyExp, ErrorTy)
            elif recEntryOpt.get.kind != RecordT:
                errorDedup recEntryOpt.get.ty, hasErr, e.rpos, e.rectyp.name, " is not a record type"
                (translate.ErrorTyExp, ErrorTy)
            else:
                let expRecTy = recEntryOpt.get
                if expRecTy.symTy.len != e.fields.len:
                    error hasErr, e.rpos, e.rectyp.name, " does not have same fields as the ones given "
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    var err = false
                    var fieldIrs : seq[TrExp]
                    for (sym, exp, pos) in e.fields:
                        for (symInTy, typExpected) in expRecTy.symTy:
                            if sym == symInTy:
                                let (fieldIr, tyActual) = transExp(ctx, level, hasErr,
                                        venv, tenv, exp, doneLabel)
                                if not typesCompatible(typExpected, tyActual):
                                    err = true
                                    error hasErr, pos, sym.name,
                                            " is expected to have type ",
                                            typExpected, " but got ", tyActual
                                fieldIrs.add fieldIr
                    if err:
                        (translate.ErrorTyExp, ErrorTy)
                    else:
                        (translate.recordExp[T](fieldIrs), expRecTy)
        of SeqExp:
            var i = 0
            var retVal: Type = nil
            var seqIrs : seq[TrExp]
            while i < e.eplist.len:
                let (seqIr, tyExp) = transExp(ctx, level, hasErr, venv, tenv, e.eplist[i][0], doneLabel)
                inc i
                if i == e.eplist.len:
                    retVal = tyExp
                seqIrs.add seqIr

            if isNil(retVal):
                (translate.seqExp(seqIrs, false), UnitTy)
            else:
                (translate.seqExp(seqIrs, true), retVal)
        of AssignExp:
            # nil can be assigned to record type vars
            let (dstIr, lhs) = transVar(ctx, level, hasErr, venv, tenv, e.avar, doneLabel)
            let (srcIr, rhs) = transExp(ctx, level, hasErr, venv, tenv, e.aexp, doneLabel)
            if lhs.kind == ErrorT or rhs.kind == ErrorT:
                (translate.ErrorTyExp, ErrorTy)
            elif rhs.kind == NilT and lhs.kind != RecordT:
                error hasErr, e.apos, "nil can only be assigned to a record type but is being assigned to ", lhs
                (translate.ErrorTyExp, ErrorTy)
            elif not typesCompatible(lhs, rhs):
                error hasErr, e.apos, "attempting to assign type ", rhs.kind,
                        " to ", lhs.kind
                (translate.ErrorTyExp, ErrorTy)
            else:
                if e.avar.kind == SimpleVar:
                    let ventry = venv.look e.avar.svs
                    devAssert ventry.isSome, "bug in impl"
                    # this can occur in for-loop counter.
                    if ventry.get.readonly:
                        error hasErr, e.apos, "cannot assign to readonly location."
                        (translate.ErrorTyExp, ErrorTy)
                    elif ventry.get.ty.kind == ErrorT:
                        (translate.ErrorTyExp, ErrorTy)
                    else:
                        (translate.assignment(dstIr, srcIr), UnitTy)
                else:
                    (translate.assignment(dstIr, srcIr), UnitTy)
        of IfExp:
            let (condIr, tytest) = transExp(ctx, level, hasErr, venv, tenv, e.iftest, doneLabel)
            if tytest.kind != IntT:
                errorDedup tytest, hasErr, e.ifpos,
                            "if condition must be of type integer but got ", tytest.kind
                (translate.ErrorTyExp, ErrorTy)
            else:
                let (thenIr, tythen) = transExp(ctx, level, hasErr, venv, tenv, e.then, doneLabel)
                if e.els.isNone:
                    if tythen.kind != UnitT:
                        errorDedup tythen, hasErr, e.ifpos,
                                "if-then branch must not produce a value, got ", tythen
                        (translate.ErrorTyExp, ErrorTy)
                    else:
                        (translate.conditional(condIr, thenIr), tythen)
                else:
                    let (elseIr, tyelse) = transExp(ctx, level, hasErr, venv, tenv, e.els.get, doneLabel)
                    if not typesCompatible(tythen, tyelse):
                        if tythen.kind != ErrorT and tyelse.kind != ErrorT:
                            error hasErr, e.ifpos,
                                "if-then-else branches must have same type, but got then with type ",
                                tythen, " else with type ", tyelse
                        (translate.ErrorTyExp, ErrorTy)
                    else:
                        (translate.conditional(condIr, thenIr, elseIr), tythen)
        of WhileExp:
            let doneLabel = some[Label](newLabel())
            # if they put a break in the condition, it will just break this loop. 
            let (condIr, tytest) = transExp(ctx, level, hasErr, venv, tenv, e.wtest, doneLabel)
            if tytest.kind != IntT:
                errorDedup tytest, hasErr, e.wpos, "while condition must be of type integer"
                (translate.ErrorTyExp, ErrorTy)
            else:
                let (bodyIr, tybody) = transExp(ctx, level, hasErr, venv, tenv, e.wbody, doneLabel)
                if tybody.kind != UnitT:
                    errorDedup tybody, hasErr, e.wpos, "while body must not produce a value"
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    (translate.whileLoop(condIr, bodyIr, doneLabel.get), UnitTy)
        of ForExp:
            let doneLabel = some[Label](newLabel())
            # if they break in the lo, hi params we will just break this for loop.
            let (loIr, tylo) = transExp(ctx, level, hasErr, venv, tenv, e.lo, doneLabel)
            let (hiIr, tyhi) = transExp(ctx, level, hasErr, venv, tenv, e.hi, doneLabel)
            var err = false
            if tylo.kind != IntT:
                err = true
                errorDedup tylo, hasErr, e.fpos, "lo of for range must be integer"
            if tyhi.kind != IntT:
                err = true
                errorDedup tyhi, hasErr, e.fpos, "hi of for range must be integer"

            venv.beginScope()

            let acc = level.allocLocal(e.escape)
            venv.enter(e.fvar, EnvEntry[T](kind: VarEntry, access: acc, ty: Type(kind: IntT), readonly: true))
            let (bodyIr, tyfbody) = transExp(ctx, level, hasErr, venv, tenv, e.fbody, doneLabel)

            venv.endScope()

            if tyfbody.kind != UnitT:
                errorDedup tyfbody, hasErr, e.fpos, "for body must not produce a value"
                (translate.ErrorTyExp, ErrorTy)
            else:
                if err: 
                    (translate.ErrorTyExp, ErrorTy) 
                else: 
                    (translate.forLoop(acc=acc, loIr=loIr, hiIr=hiIr, body=bodyIr, doneLabel=doneLabel.get), UnitTy)
        of BreakExp:
            # breaks need to be in context of a while or for, not crossing a procedure
            # call boundary, i.e. if p calls q and break is in q, does not affect p.
            if doneLabel.isNone:
                error hasErr, e.bpos, "break is valid only in a while/for loop"
                (translate.ErrorTyExp, ErrorTy)
            else:
                (translate.breakStmt(doneLabel.get), UnitTy)
        of LetExp:
            tenv.beginScope
            venv.beginScope
            
            var varInitIrs : seq[TrExp]
            for dec in e.decs:
                transDec(ctx, level, hasErr, venv, tenv, dec, doneLabel, varInitIrs)
            let (letBodyIr, tyExp) = transExp(ctx, level, hasErr, venv, tenv, e.letbody, doneLabel)
            
            tenv.endScope
            venv.endScope
            
            (translate.letExp(varInitIrs, letBodyIr), tyExp)
        of ArrayExp: # Id Lbrack exp Rbrack Of exp:
            # is this array type declared?
            let atyOpt = tenv.look e.arrtyp
            if atyOpt.isNone:
                error hasErr, e.arrpos, "trying to use an undeclared array type ", e.arrtyp.name
                (translate.ErrorTyExp, ErrorTy)
            elif atyOpt.get.kind != ArrayT:
                errorDedup atyOpt.get, hasErr, e.arrpos, e.arrtyp.name, " is not array type!"
                (translate.ErrorTyExp, ErrorTy)
            else:
                # check the size and initial values.
                let (sizeIr, tysize) = transExp(ctx, level, hasErr, venv, tenv, e.size, doneLabel)
                if tysize.kind != IntT:
                    errorDedup tysize, hasErr, e.arrpos, "must pass integer for array size"
                    (translate.ErrorTyExp, ErrorTy)
                else:
                    let (initValIr, tyinit) = transExp(ctx, level, hasErr, venv, tenv,
                            e.init, doneLabel)
                    # the initializer type must match the array's element type.
                    if tyinit != atyOpt.get.ty:
                        errorDedup tyinit, hasErr, e.arrpos,
                                "array initializer type is ", tyinit.kind,
                                        " but array is declared with type ", atyOpt.get.ty
                        (translate.ErrorTyExp, ErrorTy)
                    else:
                        (translate.arrayExp[T](sizeIr, initValIr), atyOpt.get)
    
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
            # note: this ordering is the one we use to layout the record. 
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
        devAssert tofix.kind == ErrorT, "bug in impl, expecting only ErrorT to show in the else branch of case statement for fixing up missing types."

proc transDec*[T](ctx: var TranslateCtx[T], level: Level[T], hasErr: var bool, venv: var VEnv[T], tenv: var TEnv, d: absyn.Dec, doneLabel: Option[Label], varExpList: var seq[TrExp]) =
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
        devEcho "tofixup: ", $tofixup
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
        devEcho "after fixup ", $tofixup
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
            var escapes : seq[bool]
            
            for field in fundec.params:
                let fieldTyOpt = tenv.look field.typ
                if fieldTyOpt.isNone:
                    formals.add ErrorTy
                else:
                    formals.add fieldTyOpt.get
                escapes.add field.escape 

            let newlevel = newLevel(parent=level, formals=escapes)
            var funentry = EnvEntry[T](kind: FunEntry, level: newlevel, label: level.getLabel, formals: formals, result: retTy)

            venv.enter fundec.name, funentry
        # type check the Exp of the functions.
        for fundec in d.fundecs:
            let funentry = (venv.look fundec.name).get

            venv.beginScope()

            let formalsAccess = funentry.level.formals
            var i = 0
            while i < fundec.params.len:
                let varEntry = EnvEntry[T](kind: VarEntry, access: formalsAccess[i], ty: funentry.formals[i])
                venv.enter fundec.params[i].name, varEntry
                inc i
            
            # the break label does not get inherited in a function call. 
            let (funBodyIr, tyRes) = transExp(ctx, funentry.level, hasErr, venv, tenv, fundec.body, none[Label]())

            if not typesCompatible(funentry.result, tyRes) and
                    funentry.result.kind != ErrorT and tyRes.kind != ErrorT:
                error hasErr, fundec.pos, "expected return type of ",
                        funentry.result, " for function but got ", tyRes
            
            translate.procEntryExit(ctx, level, funBodyIr)
            
            venv.endScope()
    of VarDec: # Var Id type_opt Assign exp:
        let (initIr, tyExp) = transExp(ctx, level, hasErr, venv, tenv, d.init, doneLabel)
        varExpList.add initIr 
        if d.vdtyp.isSome:
            let (retTypeSym, pos) = d.vdtyp.get
            let retTyOpt = tenv.look retTypeSym
            if retTyOpt.isNone:
                error hasErr, pos, "Return type ", retTyOpt.get, " is unknown"
                venv.enter d.vdname, EnvEntry[T](kind: VarEntry,  ty: ErrorTy)
            elif not typesCompatible(tyExp, retTyOpt.get):
                error hasErr, pos, "Return type of ", retTyOpt.get,
                        " does not match actual type of initializer, ", tyExp
                venv.enter d.vdname, EnvEntry[T](kind: VarEntry, ty: ErrorTy)
            else:
                let acc = level.allocLocal(d.escape)
                venv.enter d.vdname, EnvEntry[T](kind: VarEntry, access: acc, ty: retTyOpt.get)
        elif tyExp.kind == NilT:
            errorDedup tyExp, hasErr, d.vdpos, "Variable ", d.vdname.name,
                    " is declared with unknown type and initialized with nil. Fix by using the long form, e.g. var ",
                    d.vdname.name, " : <my_type> = ..."
            venv.enter d.vdname, EnvEntry[T](kind: VarEntry, ty: ErrorTy)
        else:
            # no return type specified. infer it from the initializer.
            let acc = level.allocLocal(d.escape)
            venv.enter d.vdname, EnvEntry[T](kind: VarEntry, access: acc, ty: tyExp)

proc transProg*[T](ast: Exp): Option[TrExp] =
    ## the T should be a Frame concept implementation. 
    var baseTEnv = newBaseTEnv()
    var baseVEnv = newBaseVEnv[T]()
    ## return whether there was type errors.
    var hasErr = false

    var ctx : TranslateCtx[T]
    # do escape analysis before everything else. 
    # it mutates the `escape` field in relevant Exp nodes. 
    ast.findEscape

    # allocate the main program frame! 
    # it shouldn't need a static link (i think translate.callExp should already handle all cases)
    let (texp, _) = transExp[T](ctx, outerMostLevel[T]().newLevel(formals = @[]), hasErr, baseVEnv, baseTEnv, ast, none[Label]())
    if hasErr:
        return none[TrExp]()
    return some[TrExp](texp)
