import translate
import frame
import ../src/frame/x86_ia64
import ../src/frame/x86_ia32

# this would crap out if the thing does not fit the concept. 
type T1* = Translate[X86IA64Frame]
type T2* = Translate[X86IA32Frame]
