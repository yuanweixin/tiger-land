import unittest
import symbol
import options

test "symtab scopes":
    var isymtab = newSymtab[int]()
    check (isymtab.look (symbol "x")) == none[int]()
    isymtab.enter symbol "x", 100
    isymtab.beginScope()
    isymtab.enter symbol "x", 101
    check (isymtab.look (symbol "x")) == some[int](101)
    isymtab.enter symbol "x", 102
    check (isymtab.look (symbol "x")) == some[int](102)
    isymtab.endScope()
    check (isymtab.look (symbol "x")) == some[int](100)
