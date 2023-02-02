import unittest
import symbol
import semant
import parse
import options

test "Type ==":
    var x, y: semant.Type
    x = Type(kind: ErrorT)
    y = Type(kind: ErrorT)
    check x == y

test "$ Symtab[Type]":
    var st = newSymtab[semant.Type]()
    st.beginScope()
    for i in 0..100:
        st.enter symbol $i, semant.Type(kind: ErrorT)
    discard $st

test "$ Symtab[EnvEntry]":
    var st = newSymtab[EnvEntry]()
    st.beginScope()
    for i in 0..100:
        st.enter symbol $i, EnvEntry(kind: VarEntry, ty: Type(kind: ErrorT))
    discard $st

test "var a := nil fails type check":
    let source = """let 
    var a := nil
    in 1 
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    if texpOpt.isSome:
        echo "unexpected, textOpt.get is ", texpOpt.get
    check texpOpt.isNone

test "var a: my_record := nil type checks":
    discard

test "nil valid cases in appendix":
    discard

test "a == nil where type(a)==RecordT":
    discard

test "a <> nil where type(a)==RecordT":
    discard

test "f(nil) where f expects a RecordT":
    discard

test "record a{f1=nil,...} works":
    discard

test "if...then... can both return nil":
    discard

test "if...then..., if returns RecordT, then returns nil":
    discard

test "if...then..., if returns nil, then returns RecordT":
    discard

test "function a() = ... where body returns nil":
    discard

test "function a() = ... where body returns unit":
    discard

test "function a(): RecordT = ... where body returns nil":
    discard

test "var a : RecordT := ... where body returns nil":
    discard

test "for loop counter cannot be assigned to":
    discard

test "records same but separate decls are distinct types":
    discard

test "arrays same but separate decls are distinct types":
    discard

test "nil value in initializing expression of var decl require long form (specify :type_id)":
    discard

test "local redeclarations, appendix example":
    discard

test "break not in while fails":
    discard

test "break not in for fails":
    discard

test "standard library print call":
    discard

test "standard library flush call":
    discard

test "standard library getchar call":
    discard

test "standard library ord call":
    discard

test "standard library chr call":
    discard

test "standard library size call":
    discard

test "standard library substring call":
    discard

test "standard library concat call":
    discard

test "standard library not call":
    discard

test "standard library exit call":
    discard

