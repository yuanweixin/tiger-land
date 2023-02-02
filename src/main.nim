import parse
import parseopt
import print
import semant
import options
import os
import print

proc main() =
    var filename = ""
    var dumpAst = false
    for kind, key, val in getopt():
        case kind
        of cmdArgument:
            filename = key
        of cmdShortOption:
            discard
        of cmdLongOption:
            if key == "dumpAst":
                dumpAst = true
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

    let texpOpt = transProg(astOpt.get)
    if texpOpt.isNone:
        echo "type errors detected, stopping."
        system.quit(-1)
    else:
        echo "type checking finished."
    system.quit(0)

main()
