import frame
import temp

# type
#         AccessKind* = enum
#             InFrame
#             InReg

#         Access* = object
#             case kind* : AccessKind
#             of InFrame:
#                 frame: int
#             of InReg:
#                 temp: Temp

#         Frame* = concept x, var vf, ptr px, type F
#             newFrame(Label, seq[bool]) is F
#             name(px) is Label
#             formals(px): seq[Access]
#             allocLocal(vf, bool) : Access

type X86Frame* = object
    name: Label
    formals: seq[Access]
    locals: seq[Access]

proc newFrame*(name: Label, formals: seq[bool]): X86Frame =
    result.name = name
    # TODO
    for escape in formals:
        if escape:
            let a = Access(kind: InFrame, )
    result.formals = formals

proc name*(f: ptr X86Frame): Label =
    return f.name

proc formals*(f: ptr X86Frame): seq[Access] =
    return f.formals


proc allocLocal*(vf: var X86Frame, escapes: bool): Access =
    let a = Access(kind: InReg, reg: 42.Temp)
    vf.locals.add a
    result = a
