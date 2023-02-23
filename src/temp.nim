import tables
import symbol
export symbol.name

type
    Temp* = int
    TempTable*[T] = Table[Temp, T]
    Label* = Symbol

var tempCounter = 0
var labelCounter = 0

proc newTemp*(): Temp =
    result = tempCounter.Temp
    inc tempCounter

proc makeString*(t: Temp): string =
    result.add "__tmp__"
    result.add $t

proc newLabel*(): Label =
    let id = labelCounter
    inc labelCounter
    result = symbol("__label__" & $id)

proc namedLabel*(name: string): Label =
    result = symbol(name)
