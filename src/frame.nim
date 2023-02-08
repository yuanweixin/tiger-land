import temp

type
        AccessKind* = enum
                InFrame
                InReg

        Access* = object
                case kind*: AccessKind
                of InFrame:
                        frame*: int
                of InReg:
                        reg*: Temp

        Frame* = concept x, var vf, ptr px, type F
                newFrame(Label, seq[bool]) is F
                name(px) is Label
                formals(px): seq[Access]
                ## second parameter means whether parameter escapes.
                allocLocal(vf, bool): Access

