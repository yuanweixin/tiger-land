import parse
import parseopt
import print
import semant
import options
import os
import print
import frame

const version = "0.3.0"

type Arch = enum
    x86ia32
    x86ia64

proc help() = 
    echo "TODO help me"

type TransX86IA64 = Translate[X86IA64Frame]

proc main() =
    var filename = ""
    var dumpAst = false
    var arch = x86ia64
    for kind, key, val in getopt():
        case kind
        of cmdArgument:
            filename = key
        of cmdShortOption:
            discard
        of cmdLongOption:
            if key == "dumpAst":
                dumpAst = true
            elif key == "help":
                help()
                system.quit(0)
            elif key == "version":
                echo "tiger compiler version ", version
                system.quit(0)
            elif key == "arch":
                arch = parseEnum[Arch](val.toLower())
        of cmdEnd:
            # doc says "There is no need to check for cmdEnd while iterating."
            discard
    if not fileExists(filename):
        echo filename, " does not exist, stopping."
        system.quit(-1)
    let astOpt = parse(filename)
    if astOpt.isNone:
        echo "syntax errors detected, stopping."
        system.quit(-1)
    else:
        echo "parsing finished. starting type check."

    if dumpAst:
        print astOpt.get
    
    let texpOpt = 
        case arch 
        of x86ia32:
            transProg[X86IA32Frame](astOpt.get)
        of x86ia64:
            transProg[X86IA64Frame](astOpt.get)
            
    if texpOpt.isNone:
        echo "type errors detected, stopping."
        system.quit(-1)
    else:
        echo "type checking finished."
    system.quit(0)

main()
