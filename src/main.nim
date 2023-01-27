import parse
import parseopt
import print

proc main() =
    var filename = ""
    for kind, key, val in getopt():
        case kind
        of cmdArgument:
            filename = key
        of cmdShortOption, cmdLongOption:
            discard
        of cmdEnd:
            # doc says
            # "There is no need to check for cmdEnd while iterating."
            discard
    let ast = parse(filename)
    print ast

main()
