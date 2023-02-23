import temp
import ir

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

        FragKind* {.pure.} = enum
                Proc 
                String 
        Frag*[T: Frame] = object
                case kind*: FragKind
                of Proc:
                        body*: ir.IrStm
                        frame*: T
                of String:
                        label*: temp.Label
                        val*: string

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
                vf.allocLocal(Escape) is Access
                F.externalCall(string, seq[IrExp]) is IrExp
                F.FP is temp.Temp ## frame pointer 
                F.wordSize is int ## machine word size
                
                # ## given access and IrExp. if IrExp is a register, 
                # ## returns Temp(irExp). Otherwise, it is in frame, 
                # ## where IrExp represents the base, and this returns 
                # ## the Mem expression that represents reading from 
                # ## location IrExp+Access.offset.
                F.exp(Access, IrExp) is IrExp
                
                # # procEntryExit1 is called to tag on the "view shift" 
                # # which does: 
                # # 1. save escaping arguments including static link, and move nonescaping 
                # # args into fresh temp registers. 
                # # 2. function body
                # # 3. restore callee-save registers. 
                x.procEntryExit1(IrStm) is IrStm

# a default fallback, duck typing, necessary when the caller uses generics
# and does not import the actual implementations. so this isn't really as 
# modular as i like because of the duck typing but...better than shit not working. 
proc name*(x: Frame) : Label = 
        return x.name()

proc formals*(x: Frame) : seq[Access] = 
        return x.formals()

proc allocLocal*(x: var Frame, e: Escape) : Access = 
        return x.allocLocal(e)