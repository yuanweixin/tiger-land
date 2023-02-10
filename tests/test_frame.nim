import unittest
import translate
import ../src/frame/x86_ia64
import ../src/frame/x86_ia32

test "Frame concept check":
    doAssert X86IA64Frame is Frame
    doAssert X86IA32Frame is Frame

