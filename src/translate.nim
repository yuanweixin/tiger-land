import temp
import frame

type TranslatedExp* = int

type Level* = int

type Access* = (Level, frame.Access)

const outerMostLevel*: Level = -1

proc newLevel*(parent: Level, name: Label, formals: seq[bool]): Level =

    return parent + 1

proc formals*(level: Level): seq[Access] =
    discard

proc allocLocal*(level: Level, escape: bool): Access =
    discard
