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
