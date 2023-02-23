import ir 
import temp
import sugar
import sets 
import tables 
import dev

# validation routines 
proc validateLiftedTreeExp(e: IrExp) = 
    case e.kind
    of IrExpKind.Binop:
        doAssert e.e1.kind notin [IrExpKind.Eseq, IrExpKind.Call]
        doAssert e.e2.kind notin [IrExpKind.Eseq, IrExpKind.Call]
        validateLiftedTreeExp(e.e1)
        validateLiftedTreeExp(e.e2)
    of IrExpKind.Mem:
        doAssert e.e.kind notin [IrExpKind.Eseq, IrExpKind.Call]
        validateLiftedTreeExp(e.e)
    of IrExpKind.Call:
        doAssert e.f.kind notin [IrExpKind.Eseq, IrExpKind.Call]
        validateLiftedTreeExp(e.f)
        for e in e.l:
            doAssert e.kind notin [IrExpKind.Eseq, IrExpKind.Call]
            validateLiftedTreeExp(e)
    else:
        discard

proc validateLiftedTree(s: IrStm) = 
    # No Eseqs
    # Exp(Call) or Move(Temp, Call)
    case s.kind
    of IrStmKind.Seq:
        validateLiftedTree(s.s1)
        validateLiftedTree(s.s2)
    of IrStmKind.Exp:
        doAssert s.e.kind != IrExpKind.Eseq
    of IrStmKind.Move:
        doAssert s.dst.kind notin [IrExpKind.Eseq, IrExpKind.Call]
        doAssert s.src.kind != IrExpKind.Eseq
    of IrStmKind.Jump:
        doAssert s.to.kind != IrExpKind.Eseq
    of IrStmKind.Cjump:
        doAssert s.e1.kind notin [IrExpKind.Eseq, IrExpKind.Call]
        doAssert s.e2.kind notin [IrExpKind.Eseq, IrExpKind.Call]
    of IrStmKind.Label:
        discard

let nop = Exp(Const 0)

proc liftExp(e: IrExp) : (IrStm, IrExp)

proc commutes(s: IrStm, e: IrExp) : bool = 
    ## the naive commute function presented in Appel book. 
    ## the main use is for places where the statement lifted from a 
    ## later expression can commute with an earlier expression. 
    ## 
    ## TODO enhance with these strategies:
    ## 1. assuming runtime mapping of virtual addr to phys addr is 1:1, then we can commute Mem(Temp(t) + Const(k1)), Mem(Temp(t) + Const(k2)) if k1 != k2
    ## 2. Keep type info for each Mem node. Then, Mem(t1, e1) and Mem(t2,e2) cannot conflict if t1 is not compatible with t2. 
    ## 3. pointer analysis so that we know when Mem operations commute. 
    ## https://www.cs.cornell.edu/courses/cs4120/2020sp/lectures/15lowering/lec15-sp18.pdf?1583460315
    case s.kind 
    of IrStmKind.Exp:
        if s.e.kind == IrExpKind.Const:
            return true 
    else:
        case e.kind
        of IrExpKind.Name:
            return true 
        of IrExpKind.Const:
            return true 
        else:
            return false 

proc `%`(a, b : IrStm) : IrStm = 
    if a.kind == IrStmKind.Exp and a.e.kind == IrExpKind.Const:
        return b 
    elif b.kind == IrStmKind.Exp and b.e.kind == IrExpKind.Const:
        return a 
    return Seq(a,b)

proc reorder(el: varargs[IrExp]) : (IrStm, seq[IrExp]) = 
    ## helper routine to orchestrate the recursive calls and handle the 
    ## commuting of the IrStm (generated from processing IrExp later in the 
    ## input) with the IrExp earlier. this is more or less shared logic 
    ## between the processes of lifting side effects from IrStm and IrExp. 
    if el.len == 0:
        return (nop, @[])
    if el[0].kind == IrExpKind.Call:
        var newArgs : seq[IrExp] 
        newArgs.add el 
        let t = Temp(newTemp())
        # TODO copying bad
        newArgs[0] = Eseq(Move(t, el[0]), t)
        return reorder(newArgs)
    let 
        (stm, expHead) = liftExp(el[0])
        (stmRest, expList) = reorder(el[1..^1])
    if commutes(stmRest, expHead): 
        return (stm % stmRest, expHead & expList)
    else:
        let t = Temp(newTemp())
        return (stm % Move(t, expHead) % stmRest, t & expList)

proc reorderStm(el: varargs[IrExp], build: varargs[IrExp] -> IrStm) : IrStm = 
    ## convenience proc that process subexpressions then puts it back together
    ## into a IrStm, which becomes the tail of the IrStm sequence (implicitly as
    ## Seq objects) that is then output. 
    let (stm, exps) = reorder(el)
    return stm % build(exps)

proc reorderExp(el: varargs[IrExp], build: varargs[IrExp] -> IrExp) : (IrStm, IrExp) = 
    ## convenience proc that process subexpressions then puts it back together 
    ## into an IrExp, then output it along with the associated IrStm. 
    let (stm, exps) = reorder(el)
    return (stm, build(exps))

proc liftStm(s : IrStm) : IrStm = 
    ## only handle the "positive" cases where transformation is needed. 
    ## the other cases are simply "pass through". 
    result = 
        case s.kind
        of Move:
            # Move(Temp, Call) is a base pattern that must be matched to avoid 
            # infinitely expanding Call
            if s.dst.kind == IrExpKind.Temp and s.src.kind == IrExpKind.Call:
                var exps: seq[IrExp]
                exps.add s.src.f
                exps.add s.src.l
                reorderStm(exps, (el) => Move(s.to, Call(el[0], el[1..^1])))
            else:
                reorderStm(s.dst, s.src, (el) => Move(el[0], el[1]))
        of Exp:
            # Exp(Call) is a base pattern that must be matched to avoid infinite
            # expansion of Call
            if s.e.kind == IrExpKind.Call:
                var exps: seq[IrExp]
                exps.add s.e.f
                exps.add s.e.l
                reorderStm(exps, (el) => Exp(Call(el[0], el[1..^1])))
            else:
                reorderStm(s.e, (el) => Exp(el[0]))
        of Jump:
            reorderStm(s.to, (el) => Jump(el[0], s.labs))
        of Cjump:
            reorderStm(s.e1, s.e2, (el) => Cjump(s.o, el[0], el[1], s.t, s.f))
        of Seq:
            liftStm(s.s1) % liftStm(s.s2)
        else:
            s
    

proc liftExp(e: IrExp) : (IrStm, IrExp) = 
    result = 
        case e.kind 
        of IrExpKind.Eseq:
            let (stm, exp) = reorderExp(e.ee, (el) => el[1])
            (Seq(e.s, stm), exp)
        of IrExpKind.Binop:
            reorderExp(e.e1, e.e2, (el) => Binop(e.o, el[0], el[1]))
        of IrExpKind.Mem:
            reorderExp(e.e, (el) => Mem(el[0]))
        of IrExpKind.Call:
            var exps : seq[IrExp]
            exps.add e.f
            exps.add e.l
            reorderExp(exps, (el) => Call(el[0], el[1..^1]))
        else:
            (nop, e)

proc linearize*(stm : IrStm) : seq[IrStm] =
    ## From an arbitrary Tree statement, produce a list of cleaned trees
    ## satisfying the following properties:
    ## 1.  No SEQ's or ESEQ's
    ## 2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
    proc helper(s: IrStm, r: var seq[IrStm]) = 
        case s.kind
        of Seq: 
            helper(s.s1, r)
            helper(s.s2, r)
        else:
            r.add s

    ## From an arbitrary Tree statement, produce a list of cleaned tree 
    ## satisfying the following properties:
    ## 1.  No SEQ's or ESEQ's
    ## 2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
    
    # Implementation of the rules described here
    # https://www.cs.cornell.edu/courses/cs4120/2020sp//lectures/15lowering/lec15-sp18.pdf?1583460315
    # 
    # but follow the mutually recursive code structure given in 
    # https://www.cs.princeton.edu/~appel/modern/ml/chap8/canon.sml
    let lifted = liftStm(stm)
    when defined(tigerdevel):
        validateLiftedTree(lifted)
    helper(lifted, result)    


type BasicBlock = seq[IrStm] 

proc basicBlocks*(stms: seq[IrStm]) : (Table[temp.Label, BasicBlock], temp.Label) = 
    ## Input: a list of cleaned trees satisfying 1,2
    ## Output: list of blocks and an end label satisfying 
    ## 1,2 as above
    ## 3.  Every block begins with a LABEL;
    ## 4.  A LABEL appears only at the beginning of a block;
    ## 5.  Any JUMP or CJUMP is the last stm in a block;
    ## 6.  Every block ends with a JUMP or CJUMP;
    ## Also produce the "label" to which control will be passed upon exit.
    var i = 0 
    var res : Table[temp.Label, BasicBlock]
    var curblock : BasicBlock
    while i < stms.len:
        if curblock.len == 0 and stms[i].kind != IrStmKind.Label:
            curblock.add Label(newLabel())
        curblock.add stms[i]
        if stms[i].kind in [IrStmKind.Jump, IrStmKind.Cjump]:
            res[curblock[0].n] = curblock
            curblock = @[]
            inc i 
            continue 
        inc i 
        if i < stms.len and stms[i].kind == IrStmKind.Label:
            curblock.add Jump(Name(stms[i].n), @[stms[i].n])
            res[curblock[0].n] = curblock
            curblock = @[]
    if curblock.len > 0:
        let epilogueLabel = newLabel()
        curblock.add Jump(Name(epilogueLabel), @[epilogueLabel])
        res[curblock[0].n] = curblock
        return (res, epilogueLabel)
    # is this possible? it seems that all (c)jumps that can be generated in 
    # translate have some non-jump statements after them. 
    doAssert false, "last basic block should not end in a jump or cjump"

func invertCjump(o: IrRelopKind, e1: IrExp, e2: IrExp, t: temp.Label, f: temp.Label) : IrStm = 
    let newOp = 
        case o 
        of Eq:
            Ne
        of Ne:
            Eq
        of Lt:
            Ge
        of Gt:
            Le
        of Le:
            Gt
        of Ge:
            Lt
        of Ult:
            Uge
        of Ule:
            Ugt
        of Ugt:
            Ule
        of Uge:
            Ult
    return Cjump(newOp, e1, e2, f, t)


proc traceSchedule*(basicBlockMap : Table[temp.Label, BasicBlock], label: temp.Label) : seq[IrStm] = 
    ## From a list of basic blocks satisfying properties 1-6,
    ## along with an "exit" label, produce a list of stms such that:
    ## 1,2 as bove
    ## 7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
    ## The blocks are reordered to satisfy property 7; also
    ## in this reordering as many JUMP(T.NAME(lab)) statements
    ## as possible are eliminated by falling through into T.LABEL(lab).
    #
    ## Algo in Appel:
    ## put all blocks into list Q 
    ## while Q not empty:
    ##  start new empty trace T
    ##  remove head b from Q
    ## while b is not marked
    ##  mark b, append b to end of current trace T
    ##  examine successor of b (to which it branches)
    ##  if exist unmarked successor c
    ##      b <- c 
    ##  end current trace T 
    var seen: HashSet[temp.Label]

    for b in basicBlockMap.values():
        devAssert b.len > 0, "basic block shouldn't be empty"
        devAssert b[0].kind == IrStmKind.Label, "basic block has to start with label!"
        devAssert b[^1].kind in [IrStmKind.Jump, IrStmKind.Cjump], "basic block should end with Jump/Cjum but got " & $b[^1].kind 
        
        if b[0].n in seen:
            continue 

        var cur = b 
        while cur[0].n notin seen:
            seen.incl cur[0].n # mark 
            let blockEndStmt = cur[^1]
            case blockEndStmt.kind 
            of IrStmKind.Jump:
                let l = blockEndStmt.labs[0]
                if l in basicBlockMap:
                    # do not add cur to result here to eliminate the jump 
                    # and directly fall into the target label. 
                    cur = basicBlockMap[l]
                else:
                    result.add cur 
                    break
            of IrStmKind.Cjump:
                # try to find and use false label
                # if can't try find and use true label, invert Cjump
                # if can't, rewrite as
                #   Cjump(c,a,b,t,f')
                #   Label(f')
                #   Jump(Name f)
                #
                #  where f is the original false label. 
                devAssert blockEndStmt.f in basicBlockMap, "Cjump f label missing in basicblock"
                devAssert blockEndStmt.t in basicBlockMap, "Cjump t label missing in basicblock"
                let fb = basicBlockMap[blockEndStmt.f]
                if fb[0].n notin seen:
                    result.add cur 
                    cur = fb 
                else:
                    let tb = basicBlockMap[blockEndStmt.t]
                    if tb[0].n notin seen:
                        result.add invertCjump(blockEndStmt.o, blockEndStmt.e1, blockEndStmt.e2, blockEndStmt.t, blockEndStmt.f)
                        cur = tb 
                    else:
                        let lf = newLabel()
                        result.add Cjump(blockEndStmt.o, blockEndStmt.e1, blockEndStmt.e2, blockEndStmt.t, lf)
                        result.add Label(lf)
                        result.add Jump(Name(blockEndStmt.f))
                        break # no successor 
            else:
                doAssert false, "basic block should end in jump or cjump but got " & $b[^1].kind



