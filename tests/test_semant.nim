import unittest
import semant

test "Type ==":
    var x, y: Type
    x = Type(kind: ErrorT)
    y = Type(kind: ErrorT)
    check x == y
