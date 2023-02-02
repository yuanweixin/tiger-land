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

test "nil valid cases in appendix":
    let source = """let 
    type any = {any: int}
    var a : any := nil
    function f(a: any) : int = 100
    in 
    a := nil;
    if a <> nil then 1 else 1;
    if nil <> a then 2 else 2;
    if a = nil then 3 else 3;
    if nil = a then 4 else 4;
    f(nil)
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "record a{f1=nil,...} works":
    let source = """let 
    type any = {any: int, x: any}
    var a : any := any{any=1, x=nil}
    in 
    42
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "if...then... can both return nil":
    let source = """let 
    type any = {any: int}
    in 
    if 1 = 1 then nil else nil
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "if...then..., if returns RecordT, then returns nil":
    let source = """let 
    type any = {any: int}
    var a: any := any{any=1}
    in 
    if 1 = 1 then a else nil
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "if...then..., if returns nil, then returns RecordT":
    let source = """let 
    type any = {any: int}
    var a: any := any{any=1}
    in 
    if 1 = 1 then nil else a
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "function a() : some_record = ... where body returns nil":
    let source = """let 
    type any = {any: int}
    function f(a: any) : any = nil
    in 
    f(nil)
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "function a() = () should be accepted":
    let source = """let 
    function f() = ()
    in 
    f()
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "for loop counter cannot be assigned to":
    let source = """let 
    in 
    for r := 0 to 10 do 
        if 1 = 1 then r := 1; ()
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isNone

test "records same but separate decls are distinct types":
    let source = """let 
    type any1 = {any: int}
    type any2 = {any: int}
    var a : any1 := nil
    var b : any2 := nil
    in 
    a = b 
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isNone

test "arrays same but separate decls are distinct types":
    let source = """let 
    type arr1 = array of int
    type arr2 = array of int
    var a1 := arr1[8] of 0 
    var a2 := arr2[8] of 0
    in 
    a1 = a2
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isNone

test "local redeclarations, appendix example":
    let source = """
    let 
        function print(v:int) = ()
        function f(v:int) = 
        let var v := 6
            in print(v);
            let var v := 7 in print (v) end;
            print(v);
            let var v := 8 in print (v) end;
            print (v)
        end
    in 
    ()
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "break not in while/for fails":
    let source = """
    let 
    in 
        break
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isNone

test "break in a while loop is legal":
    let source = """
    let 
    in 
        while 1 = 1 do
            break
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "break in a for loop is legal":
    let source = """
    let 
    in 
        for i :=0 to 100 do
            break
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

test "standard library calls":
    let source = """
    let 
    in 
        print("die");
        flush();
        getchar();
        ord("");
        chr(0);
        size("");
        substring("hello", 0, 1);
        concat("h","i");
        not(1);
        exit(1)
    end"""
    let astOpt = parseString(source)
    check astOpt.isSome
    let texpOpt = transProg(astOpt.get)
    check texpOpt.isSome

