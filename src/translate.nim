import temp
import frame
export temp
import ir 
import absyn 
import return_codes
import dev

type 
    LevelKind* {.pure.} = enum
        Top 
        Nested
    Level*[T: Frame] = ref object
        case kind* : LevelKind 
        of Top: discard
        of Nested:
            parent*: Level[T]
            frame* : T 

    ## basically the current frame something is declared at along with
    ## whether that something should be in memory or in registers. 
    Access*[T] = (Level[T], frame.Access)

    TrExpKind = enum
        Ex
        Nx
        Cx
    
    Conditional = proc (l1: temp.Label, l2: temp.Label): ir.IrStm

    # TODO prob makes sense to be ref as well? 
    TrExp* = object
        case kind: TrExpKind
        of Ex:
            exp: ir.IrExp
        of Nx:
            stm: ir.IrStm
        of Cx:
            f: Conditional
    TranslateCtx*[T] = object
        fragments : seq[Frag[T]]
        

proc getLabel*[T](l : Level[T]) : Label = 
    return l.frame.name

proc `==`[T](a,b: Level[T]) : bool = 
    if a.kind != b.kind:
        return false 
    case a.kind
    of Top:
        return true 
    of Nested:
        # this assumes the expected usage in our codebase
        # where frames are always assigned a new label. 
        return a.getLabel == b.getLabel

proc toEx(exp: ir.IrExp) : TrExp = 
    result = TrExp(kind: Ex, exp : exp)

proc toNx(stm: ir.IrStm) : TrExp = 
    result = TrExp(kind: Nx, stm: stm)

proc toCx(f: proc (l1: temp.Label, l2: temp.Label): ir.IrStm) : TrExp = 
    result = TrExp(kind: Cx, f : f)

let ErrorTyExp* = toEx Const(42)

proc getLevel[T](access: Access[T]) : Level[T] = 
    return access[0]

proc getFrameAccess[T](access: Access[T]) : frame.Access = 
    return access[1]

proc makeSeq(seqs: varargs[IrStm]) : IrStm = 
    devAssert seqs.len > 0, "bug, should give me at least 1 IrStm"
    if seqs.len == 1:
        return seqs[0]
    var i = seqs.high - 2 
    var soFar = Seq(seqs[seqs.high-1], seqs[seqs.high])
    while i >= 0:
        soFar = Seq(seqs[i], soFar)
        dec i 
    return soFar

proc unEx(tr: TrExp) : ir.IrExp = 
    case tr.kind
    of Ex:
        return tr.exp
    of Cx:
        let 
            r = temp.newTemp()
            t = temp.newLabel()
            f = temp.newLabel()
        return Eseq(makeSeq(
                Move(ir.Temp(r), Const(1)),
                tr.f(t,f),
                ir.Label(f),
                Move(ir.Temp(r), Const(0)),
                ir.Label(t)), 
             ir.Temp(r))
    of Nx:
        return Eseq(tr.stm, Const(0))
    
proc unCx(tr: TrExp) : Conditional = 
    case tr.kind
    of Nx:
        devAssert false, "unCx(Nx_) should never occur in a well typed tiger program."
    of Cx:
        return tr.f
    of Ex:
        if tr.exp == Const(0):
            return proc(t: Label, f: Label) : ir.IrStm = 
                Jump(Name(t), @[t])
        elif tr.exp == Const(1):
            return proc(t: Label, f: Label) : ir.IrStm = 
                Jump(Name(t), @[t])
        return proc(t: Label, f: Label) : ir.IrStm = 
            Cjump(Ne, Const(0), tr.exp, t, f)

proc unNx(tr: TrExp) : ir.IrStm = 
    case tr.kind
    of Nx:
        return tr.stm
    of Ex:
        return Exp(tr.exp)
    of Cx:
        let 
            whatever = temp.newLabel()
        return makeSeq(tr.f(whatever,whatever), ir.Label(whatever))

proc outerMostLevel*[T](): Level[T] = 
    return Level[T](kind: Top)

proc newLevel*[T](parent: Level[T], formals: seq[Escape]): Level[T] =
    # we always generate a fresh label for each new level. said label can 
    # be accessed using level.getLabel. 
    # 
    # formals: we prepend true for the static link. 
    var augmented = @[true]
    for formal in formals:
        augmented.add formal
    return Level[T](kind: Nested, parent: parent, frame: T.newFrame(newLabel(), augmented))

func staticLink[T](level: Level[T]) : frame.Access = 
    return level.frame.formals[0]

proc formals*[T](level: Level[T]): seq[Access[T]] =
    ## called by semant to see the frame.Access of the function's formal params. 
    devAssert level.kind == Nested, "invalid usage, formals only available in nested level"
    devAssert level.frame.formals.len > 0, "expect at least 1 argument in frame (static link) but got 0, impl bug."
    var i = 1
    var hi = level.frame.formals.len
    while i < hi:
        result.add (level, level.frame.formals[i])
        inc i 

proc allocLocal*[T](level: Level[T], escape: bool): Access[T] =
    devAssert level.kind != Top, "cannot allocate locals in the top context"
    let frameAcc = level.frame.allocLocal(escape)
    return (level, frameAcc)

proc simpleVar*[T](access: Access, curLevel: Level[T]) : TrExp = 
    let finalLvl = access.getLevel
    var res = ir.Temp(T.FP)
    var curLevel = curLevel 
    # move up 
    while finalLvl != curLevel:
        res = T.exp(curLevel.staticLink, res)
        curLevel = curLevel.parent
    # finally access in the frame pointer. 
    toEx T.exp(access.getFrameAccess, res)

proc subscriptVar*[T](lhsIr: TrExp, rhsIr: TrExp, exitCallLabel: Label) : TrExp = 
    # this is the array access expression.
    # also, bounds checking is baked in here. 
    # array layout: 
    # [size, e0, e1, ..., ek] where each element is wordSize sized
    # 
    # outputs something like this: 
    # if i >= 0 and i < arr.size: 
    #   Mem(arr, W + i*W)
    # else:
    #   exit(INVALID_ARRAY_INDEX) 
    #
    # lhsIr can be result of simple var (Mem), subscript (Mem), or dot (Mem)
    let 
        idx = Binop(Plus, Const(T.wordSize), Binop(Mul, unEx rhsIr, Const(T.wordSize)))
        bad = newLabel()
        upperCheck = newLabel()
        access = newLabel()
        
    toEx Eseq(makeSeq(
            Cjump(Ge, idx, Const(0), upperCheck, bad),
            ir.Label(upperCheck),
            Cjump(Lt, idx, unEx lhsIr, access, bad),
            ir.Label(bad),
            # this never returns 
            Exp(T.externalCall("exit", @[Const(INVALID_ARRAY_INDEX)])),
            ir.Label(access)
        ), 
        Mem(Binop(Plus, unEx lhsIr, idx)))

proc recordField*[T](lhsIr: TrExp, symPos: int) : TrExp = 
    # this is basically like an array access, but sans the bounds check 
    # or size field so much simpler! lack of non-scalar l-values significantly
    # simplifies the effort. 
    toEx Mem(Binop(Plus, unEx lhsIr, Const(symPos * T.wordSize)))

proc stringCmp*[T:Frame](isEquality: bool, lhs: TrExp, rhs: TrExp) : TrExp = 
    result = 
        if isEquality:
            toEx T.externalCall("stringEqual", @[unEx lhs, unEx rhs])
        else:
            let r = newTemp()
            toEx Eseq(
                    Move(ir.Temp(r), 
                        T.externalCall("stringEqual", @[unEx lhs, unEx rhs])),
                    Binop(Xor, ir.Temp(r), Const(1)))

proc binop*(o: absyn.Oper, lhs: TrExp, rhs: TrExp) : TrExp = 
    result = 
        case o 
        of Oper.PlusOp:
            toEx Binop(IrBinopKind.Plus, unEx(lhs), unEx(rhs))
        of Oper.MinusOp: 
            toEx Binop(IrBinopKind.Minus, unEx(lhs), unEx(rhs))
        of Oper.TimesOp: 
            toEx Binop(IrBinopKind.Mul, unEx(lhs), unEx(rhs))
        of Oper.DivideOp: 
            toEx Binop(IrBinopKind.Div, unEx(lhs), unEx(rhs))
        of Oper.EqOp:
            toCx proc (t: Label, f: Label) : IrStm = 
                Cjump(IrRelopKind.Eq, unEx(lhs), unEx(rhs), t, f)
        of Oper.NeqOp:
            toCx proc (t: Label, f: Label) : IrStm = 
                Cjump(IrRelopKind.Ne, unEx(lhs), unEx(rhs), t, f)
        of Oper.LtOp: 
            toCx proc (t: Label, f: Label) : IrStm = 
                Cjump(IrRelopKind.Lt, unEx(lhs), unEx(rhs), t, f)
        of Oper.LeOp: 
            toCx proc (t: Label, f: Label) : IrStm = 
                Cjump(IrRelopKind.Le, unEx(lhs), unEx(rhs), t, f)
        of Oper.GtOp: 
            toCx proc (t: Label, f: Label) : IrStm = 
                Cjump(IrRelopKind.Gt, unEx(lhs), unEx(rhs), t, f)
        of Oper.GeOp: 
            toCx proc (t: Label, f: Label) : IrStm = 
                Cjump(IrRelopKind.Ge, unEx(lhs), unEx(rhs), t, f)

proc nilExp*() : TrExp = 
    toEx(Const(0))

proc intExp*(i: int) : TrExp = 
    toEx(Const(i))

proc stringExp*[T](ctx: var TranslateCtx, s: string) : TrExp = 
    # this is a string constant. 
    for frag in ctx.fragments:
        if frag.kind == String and frag.val == s:
            return toEx Name(frag.label)
    let l = newLabel()
    let newFrag = Frag[T](kind: String, label: l, val: s)
    ctx.fragments.add newFrag
    toEx Name(l)

proc callExp*[T](function: Label, callerLevel: Level[T], args: seq[TrExp], calleeLevel: Level[T]) : TrExp = 
    # there are a few cases. 
    ## self calls: 
    ##      need to pass the static link to callee. 
    ## direct child call
    ##      pass FP to callee
    ## call a sibling (share parent):
    ##      pass parent fp (i.e. static link)
    ## call (distant) parent: 
    ##      
    var augmentedArgs : seq[IrExp]
    if calleeLevel.kind == Top:
        # calling a top level function, i.e. predefined
        # these are in the runtime, so have to be external calls. 
        for arg in args:
            augmentedArgs.add unEx arg
        return toEx T.externalCall(function.name, augmentedArgs)
    
    if callerLevel == calleeLevel: # self call
        # Recursive calls, pass its caller's FP instead. 
        augmentedArgs.add T.exp(callerLevel.staticLink, ir.Temp(T.FP))
    elif calleeLevel.parent == callerLevel: # child call
        # second param is ignored for InReg
        augmentedArgs.add T.exp(frame.Access(kind: InReg, reg: T.FP), ir.Temp(T.FP))
    elif callerLevel.parent == calleeLevel.parent: # sibling
        augmentedArgs.add T.exp(callerLevel.staticLink, ir.Temp(T.FP))
    else: # calling some parent
        var cur = callerLevel 
        var exp = ir.Temp(T.FP)
        while cur != calleeLevel:
            exp = T.exp(cur.staticLink, exp)
            cur = cur.parent
        exp = T.exp(cur.staticLink, exp)
        augmentedArgs.add exp 
    for arg in args:
        augmentedArgs.add unEx arg
    toEx Call(Name(function), augmentedArgs)

proc recordExp*[T](fieldIrs: seq[TrExp]) : TrExp = 
    # a {f1 = e1, ..., fn = en}
    # 
    # call external alloc routine (malloc)
    # generate the sequence of moves
    # return the address of the record
    let 
        r = newTemp()
        i = newTemp()
        done = newLabel()
        body = newLabel()
        test = newLabel()
    var instrs : seq[IrStm]
    instrs.add Move(ir.Temp(r), T.externalCall("malloc", @[Const(T.wordSize * fieldIrs.len)]))
    var idx = 0 
    for fieldIr in fieldIrs:
        let offset = idx * T.wordSize
        instrs.add Move(Mem(Binop(Plus, ir.Temp(r), Const(offset))), unEx fieldIr)
        inc idx
    toEx Eseq(makeSeq(instrs), ir.Temp(r))

proc seqExp*(seqIrs: seq[TrExp], hasReturnVal: bool) : TrExp = 
    result = 
        if seqIrs.len == 0:
            # you can have an empty sequence. for instance empty let body. 
            toNx ir.Exp(Const 0)
        elif seqIrs.len == 1:
            if hasReturnVal:
                toEx unEx(seqIrs[0])
            else:
                toNx unNx(seqIrs[0])
        else:
            if hasReturnVal:
                var irStms : seq[IrStm]
                for i in 0..seqIrs.high-1:
                    irStms.add unNx seqIrs[i]
                toEx Eseq(makeSeq(irStms), unEx seqIrs[^1])
            else:
                var irStms : seq[IrStm]
                for ir in seqIrs:
                    irStms.add unNx ir
                toNx makeSeq(irStms)

proc assignment*(lhsIr: TrExp, rhsIr: TrExp) : TrExp = 
    toNx Move(unEx lhsIr, unEx rhsIr)

proc conditional*(condIr: TrExp, trueIr: TrExp, falseIr: TrExp) : TrExp = 
    let 
        branchTrue = newLabel()
        branchFalse = newLabel()
        trueBranch = newLabel()
        falseBranch = newLabel()
        done = newLabel()
        r = newTemp()
    proc makeBranchStmt(branchIr : TrExp) : IrStm = 
        result = 
            if branchIr.kind == Cx:
                unCx(branchIr)(branchTrue,branchFalse)
            elif branchIr.kind == Ex:
                let unwrapped = unEx branchIr
                if unwrapped.kind == IrExpKind.Const:
                    if unwrapped.i != 0:
                        makeSeq(
                            Move(ir.Temp(r), Const(1)),
                            Jump(Name(done), @[done])
                        )
                    else:
                        makeSeq(
                            Move(ir.Temp(r), Const(0)),
                            Jump(Name(done), @[done])
                        )
                else:
                    Cjump(Ne, unwrapped, Const(0), branchTrue, branchFalse)
            else:
                makeSeq(
                    unNx branchIr, 
                    Jump(Name(done), @[done])
                )

    let trueBranchStm = makeBranchStmt(trueIr)
    let falseBranchStm = makeBranchStmt(falseIr)
    result = 
        if trueIr.kind == Nx:
            devAssert falseIr.kind == Nx, "unexpected, true branch is Nx but false branch is ", falseIr.kind
            toNx makeSeq(
                unCx(condIr)(trueBranch, falseBranch),
                ir.Label(trueBranch),
                trueBranchStm,
                ir.Label(falseBranch),
                falseBranchStm,
                ir.Label(done)
            )
        else:
            toEx Eseq(makeSeq(
                        unCx(condIr)(trueBranch, falseBranch),
                        ir.Label(trueBranch),
                        trueBranchStm,
                        ir.Label(falseBranch),
                        falseBranchStm,
                        ir.Label(branchFalse),
                        Move(ir.Temp(r), Const(0)),
                        Jump(Name(done), @[done]),
                        ir.Label(branchTrue),
                        Move(ir.Temp(r), Const(1)),
                        Jump(Name(done), @[done]),
                        ir.Label(done)
                    ),
                    ir.Temp(r))

proc conditional*(condIr: TrExp, trueIr: TrExp) : TrExp = 
    ## if-then produces no value. 
    let 
        t = newLabel()
        f = newLabel()
    toNx makeSeq(
            unCx(condIr)(t,f),
            ir.Label(t),
            unNx trueIr,
            ir.Label(f)
        )

proc whileLoop*(condIr: TrExp, bodyIr: TrExp, doneLabel: Label) : TrExp = 
    let 
        test = newLabel()
        done = newLabel()
        body = newLabel()
    toNx makeSeq(
        ir.Label(test),
        unCx(condIr)(body, done),
        ir.Label(body),
        unNx(bodyIr),
        Jump(Name(test), @[test]),
        ir.Label(done)
    )

proc forLoop*[T](acc: Access[T], loIr: TrExp, hiIr: TrExp, body: TrExp, doneLabel: Label): TrExp = 
    ## Roughly this: 
    ## let 
    ##  var i := lo
    ##  var limit := hi 
    ## in 
    ##  if i <= limit: 
    ##      loop:
    ##          body; 
    ##          if i < limit:
    ##              i := i + 1 
    ##              goto loop
    ##      done:
    ## 
    ## note the i < limit check is done BEFORE incrementing i to avoid the edge
    ## case where limit == intmax, where if we increment i first we either get 
    ## overflow error or an infinite loop, depending on the platform. 
    let 
        loop = newLabel()
        update = newLabel()
        r = newTemp()
    toNx makeSeq(
        Move(ir.Temp(r), unEx loIr),
        Cjump(IrRelopKind.Le, ir.Temp(r), unEx hiIr, loop, doneLabel), 
        unNx body,
        Cjump(IrRelopKind.Lt, ir.Temp(r), unEx loIr, update, doneLabel),
        ir.Label(update),
        Move(ir.Temp(r), Binop(Plus, ir.Temp(r), Const(1))),
        Jump(Name(loop), @[loop]),
        ir.Label(doneLabel)
    )

proc breakStmt*(doneLabel: Label) : TrExp = 
    toNx Jump(Name(doneLabel), @[doneLabel])

proc arrayExp*[T](sizeIr: TrExp, initValIr: TrExp) : TrExp = 
    toEx T.externalCall("initArray", @[unEx sizeIr, unEx initValIr])
    
proc letExp*(varInitIrs: seq[TrExp], letBodyIr: TrExp) : TrExp = 
    if varInitIrs.len == 0:
        return letBodyIr 
    var seqs : seq[IrStm]
    for ir in varInitIrs:
        seqs.add unNx ir 
    if letBodyIr.kind == Nx:
        seqs.add unNx letBodyIr
        toNx makeSeq(seqs)
    else:
        # in case of lets with no var declaration, we just return the body.
        # func decs are saved into the frag list elsewhere. 
        toEx Eseq(makeSeq(seqs), unEx letBodyIr)

proc procEntryExit*[T](ctx: var TranslateCtx[T], level: Level[T], body: TrExp) = 
    ## called to translate a function declaration. 
    # `body` is the result of translating function body; 
    devAssert level.kind == Nested, "impl bug"
    let augmented = level.frame.procEntryExit1(unNx body)
    ctx.fragments.add Frag[T](kind: Proc, body: augmented, frame: level.frame)