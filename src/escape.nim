import absyn
import symbol
import options

## definition of escape in appel:
## passed by reference 
## OR 
## address is taken 
## OR 
## accessed from a nested function 

# no address operator in tiger. 
# arrays, records and strings start out as pointers. 
# so, only way is to access a var from inside a nested function. 

# this is complicated by the redeclaration rules: 
# func/var namespace conflict is allowed
# recursive func decl block means func name conflict effective immediately
# var redeclaration allowed 

## example
## let 
##  var i := 1
##  var i := 2 /* i above irrelevant for escape analysis */
##  var k := 3
##  /* function i's scope effectively at start of recursive blk */
##  function j () = 
##      i() 
##      k + 1 /* k escapes */
##  function i () =  /* shadows var i */
##      2 
##  
## in 
## end
type 
    Depth = int 
    DepthEscapeRef = (Depth, ptr bool)
    EscEnv = Symtab[DepthEscapeRef]

func depth (entry: DepthEscapeRef) : Depth = 
    return entry[0]

func escape (entry: DepthEscapeRef) : ptr bool = 
    return entry[1]

proc traverseExp*(env: var EscEnv, d: Depth, e: absyn.Exp) 

proc traverseVar*(env: var EscEnv, d: Depth, v: absyn.VarR) = 
    case v.kind
    of SimpleVar: # id 
        let ventry = env.look v.svs
        if ventry.isSome: # none could happen in bad code. 
            if ventry.get.depth < d:
                ventry.get.escape[] = true 
    of FieldVar: # X.id
        env.traverseVar(d, v.fvar)
    of SubscriptVar: # X[y]
        env.traverseExp(d, v.exp)
        env.traverseVar(d, v.subvar)

proc traverseDecs*(env: var EscEnv, d: Depth, dec: absyn.Dec) 

proc traverseExp*(env: var EscEnv, d: Depth, e: absyn.Exp) = 
    case e.kind
    of OpExp:
        env.traverseExp(d, e.left)
        env.traverseExp(d, e.right)
    of NilExp: discard
    of IntExp: discard
    of VarExp:
        env.traverseVar(d, e.v)
    of StringExp: discard
    of CallExp: 
        for arg in e.args:
            env.traverseExp(d, arg)
    of RecordExp: 
        for (_, exp, _) in e.fields:
            env.traverseExp(d, exp)
    of SeqExp:
        for (exp, _) in e.eplist:
            env.traverseExp(d, exp)
    of AssignExp:
        env.traverseVar(d, e.avar)
        env.traverseExp(d, e.aexp)
    of IfExp:
        env.traverseExp(d, e.iftest)
        env.traverseExp(d, e.then)
        if e.els.isSome:
            env.traverseExp(d, e.els.get)
    of WhileExp:
        env.traverseExp(d, e.wtest)
        env.traverseExp(d, e.wbody)
    of ForExp:
        # because the loop counter only valid in the loop.
        env.beginScope()
        # check lo, hi before adding the loop counter. 
        # because their expr can refer to var with same name. 
        env.traverseExp(d, e.lo)
        env.traverseExp(d, e.hi)
        env.enter e.fvar, (d, addr e.escape)
        env.traverseExp(d, e.fbody)
        env.endScope()
    of BreakExp: discard
    of LetExp:
        # because these decls are only valid in the let scope.
        env.beginScope()
        for dec in e.decs:
            env.traverseDecs(d, dec)
        env.traverseExp(d, e.letbody)
        env.endScope()
    of ArrayExp: 
        env.traverseExp(d, e.size)
        env.traverseExp(d, e.init)

proc traverseDecs*(env: var EscEnv, d: Depth, dec: absyn.Dec) = 
    case dec.kind 
    of TypeDec: discard
    of FunctionDec:
        let newDepth = d + 1 
        for fundec in dec.fundecs:
            for field in fundec.params:
                env.enter field.name, (newDepth, addr field.escape)
            env.traverseExp(newDepth, fundec.body)
    of VarDec:
        # in case of shadowing, this has an analogous 
        # structure, so it works. 
        env.enter dec.vdname, (d, addr dec.escape)

proc findEscape*(prog: absyn.Exp) =  
    var env : EscEnv 
    env.beginScope()
    traverseExp(env, 0, prog)

