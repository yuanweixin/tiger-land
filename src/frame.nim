import temp

type
        AccessKind* {.pure.} = enum
                InFrame 
                InReg

        Access* = object
                case kind*: AccessKind
                of InFrame:
                        ## indicates mem loc at offset x from frame pointer
                        offset*: int
                of InReg:
                        ## Indicates an abstract register 
                        reg*: Temp

        ## whether a local var or func param "escapes", 
        ## meaning 
        ## 1. passed by reference
        ## 2. address is taken (C & operator)
        ## 3. accessed from nested function
        Escape* = bool 

        ## interface that architecture-dependent frame impl must support. 
        Frame* = concept x, var vf, type F
                F.newFrame(Label, seq[Escape]) is F
                name(x) is Label
                formals(x) is seq[Access]
                vf.allocLocalInFrame(Escape) is Access
