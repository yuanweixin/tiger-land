import ../frame
import ../temp

const wordsize = 8
type X86IA64Frame* = object
    name: Label
    formals: seq[Access]
    locals: seq[Access]
    nextOffset : int # this goes up 
    nextLocal : int  # this goes down 

proc newFrame*(x: typedesc[X86IA64Frame], name: Label, formals: seq[Escape]): X86IA64Frame =
    result.name = name
    # arg 8, etc
    # arg 7
    # saved rip 
    # saved rbp <--- rbp
    result.nextOffset = 16
    result.nextLocal = -8
    
    for escape in formals:
        if escape:
            result.formals.add Access(kind: InFrame, offset: result.nextOffset)
            result.nextOffset += wordsize
        else:
            result.formals.add Access(kind: InReg, reg: newTemp())            

proc name*(f: X86IA64Frame): Label =
    return f.name

proc formals*(f: X86IA64Frame): seq[Access] =
    return f.formals

proc allocLocalInFrame*(vf: var X86IA64Frame, escapes: Escape): Access =
    if escapes:
        vf.locals.add Access(kind: InFrame, offset : vf.nextLocal)
        vf.nextLocal -= wordsize
    else:
        vf.locals.add Access(kind: InReg, reg: newTemp())