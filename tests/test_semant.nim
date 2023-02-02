import unittest
import symbol
import semant

test "Type ==":
    var x, y: Type
    x = Type(kind: ErrorT)
    y = Type(kind: ErrorT)
    check x == y

test "$ Symtab[Type]":
    var st = newSymtab[Type]()
    st.beginScope()
    for i in 0..100:
        st.enter symbol $i, Type(kind: ErrorT)
    discard $st

test "$ Symtab[EnvEntry]":
    var st = newSymtab[EnvEntry]()
    st.beginScope()
    for i in 0..100:
        st.enter symbol $i, EnvEntry(kind: VarEntry, ty: Type(kind: ErrorT))
    discard $st

test "var a := nil fails type check":
    discard

test "var a: my_record := nil type checks":
    discard

test "nil valid cases in appendix":
    discard

test "for loop counter cannot be assigned to":
    discard
