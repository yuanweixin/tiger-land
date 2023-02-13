import temp
import frame
export temp
export frame.Frame

type TranslatedExp* = int

type 
    LevelKind* {.pure.} = enum
        Top 
        Nested
    Level*[T: Frame] = ref object
        case kind* : LevelKind 
        of Top: discard
        of Nested:
            parent*: Level[T]
            frame* : T 
    Access*[T: Frame] = (Level[T], frame.Access)

proc outerMostLevel*[T: Frame](): Level[T] = 
    return Level[T](kind: Top)

proc newLevel*[T: Frame](parent: Level[T], name: Label, formals: seq[Escape]): Level[T] =
    return Level[T](kind: Nested, parent: parent, frame: T.newFrame(name, formals))

proc formals*[T: Frame](level: Level[T]): seq[Access[T]] =
    doAssert level.kind == Nested, "invalid usage, formals only available in nested level"
    for formal in level.frame.formals:
        result.add (level, formal)

proc allocLocal*[T: Frame](level: Level[T], escape: bool): Access[T] =
    doAssert level.kind != Top, "cannot allocate locals in the top context"
    let frameAcc = level.frame.allocLocalInFrame(escape)
    return (level, frameAcc)
