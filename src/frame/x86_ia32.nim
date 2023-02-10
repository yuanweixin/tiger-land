import ../frame
import ../temp

const wordsize = 4 

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
            result.nextOffset += wordsize
        else:
            result.formals.add Access(kind: InReg, reg: newTemp())            

proc name*(f: X86IA32Frame): Label =
    return f.name

proc formals*(f: X86IA32Frame): seq[Access] =
    return f.formals

proc allocLocalInFrame*(vf: var X86IA32Frame, escapes: bool): Access =
    if escapes:
        vf.locals.add Access(kind: InFrame, offset : vf.nextLocal)
        vf.nextLocal -= wordsize
    else:
        vf.locals.add Access(kind: InReg, reg: newTemp())