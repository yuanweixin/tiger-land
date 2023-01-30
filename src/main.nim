import parse
import parseopt
import print
import semant
import options

proc main() =
    var filename = ""
    for kind, key, val in getopt():
        case kind
        of cmdArgument:
            filename = key
        of cmdShortOption, cmdLongOption:
            discard
        of cmdEnd:
            # doc says "There is no need to check for cmdEnd while iterating."
            discard
    let astOpt = parse(filename)
    if astOpt.isNone:
        echo "syntax errors detected, stopping."
        system.quit(-1)
    let hasTypeErr = transProg(astOpt.get)
    if hasTypeErr:
        echo "type errors detected, stopping."
        system.quit(-1)
    system.quit(0)

main()
