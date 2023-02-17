import ../frame
import ../temp
import ../ir

const ws = 4 

type X86IA32Frame* = object
    name: Label
    formals: seq[Access]
    locals: seq[Access]
    nextOffset : int # this goes up 
    nextLocal : int  # this goes down 

proc newFrame*(x: typedesc[X86IA32Frame], name: Label, formals: seq[Escape]): X86IA32Frame =
    result.name = name
    # arg 1 
    # return addr
    # oldFp <--- FP 
    result.nextOffset = 8
    result.nextLocal = -4
    for escape in formals:
        if escape:
            result.formals.add Access(kind: InFrame, offset: result.nextOffset)
            result.nextOffset += ws
        else:
            result.formals.add Access(kind: InReg, reg: newTemp())            

proc name*(f: X86IA32Frame): Label =
    return f.name

proc formals*(f: X86IA32Frame): seq[Access] =
    return f.formals

proc allocLocalInFrame*(vf: var X86IA32Frame, escapes: bool): Access =
    if escapes:
        vf.locals.add Access(kind: InFrame, offset : vf.nextLocal)
        vf.nextLocal -= ws
    else:
        vf.locals.add Access(kind: InReg, reg: newTemp())

proc externalCall*(x: typedesc[X86IA32Frame], name: string, args: seq[IrExp]) : IrExp = 
    return Call(Name(temp.namedLabel name), args)


let fp = temp.newTemp()
proc FP*(x: typedesc[X86IA32Frame]) : temp.Temp = 
    return fp 

proc wordSize*(x: typedesc[X86IA32Frame]) : int = 
    return ws

proc exp*(x: typedesc[X86IA32Frame], acc: Access, tr: ir.IrExp) : ir.IrExp = 
    case acc.kind
    of InReg:
        return ir.Temp(acc.reg)
    of InFrame:
        return Mem(Binop(Plus, tr, Const(acc.offset)))

proc procEntryExit1*(f: X86IA32Frame, stm: IrStm) : IrStm = 
    discard