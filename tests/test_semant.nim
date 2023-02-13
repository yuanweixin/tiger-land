import unittest
import symbol
import semant
import parse
import options
import os
import frame
import translate
import escape
import print
import absyn

type TestFrame* = object
    name: Label
    formals: seq[frame.Access]

proc newFrame*(x: typedesc[TestFrame], name: Label, formals: seq[Escape]): TestFrame =
    result.name = name 
    for escape in formals:
        if escape:
            result.formals.add frame.Access(kind: InFrame, offset: 42)
        else:
            result.formals.add frame.Access(kind: InReg, reg: newTemp())            

proc name*(f: TestFrame): Label =
    return f.name

proc formals*(f: TestFrame): seq[frame.Access] =
    return f.formals

proc allocLocalInFrame*(vf: var TestFrame, escapes: Escape): frame.Access =
    discard

test "sanity check TestFrame is Frame":
    doAssert TestFrame is Frame

proc testInput(input: string, expectGood: bool) =
    let astOpt = parseString(input)
    doAssert astOpt.isSome
    let texpOpt = transProg[TestFrame](astOpt.get)
    if expectGood:
        doAssert texpOpt.isSome
    else:
        doAssert texpOpt.isNone

proc testInputIsGood(input: string) =
    testInput input, true

proc testInputIsBad(input: string) =
    testInput input, false

proc testBad(input: string) =
    let astOpt = parseString(input)
    doAssert astOpt.isSome
    let texpOpt = transProg[TestFrame](astOpt.get)
    doAssert texpOpt.isNone

proc testFile(f: string, expectGood: bool) =
    echo "\ntesting with input: ", f
    var input = readFile(f)
    let astOpt = parseString(input)
    doAssert astOpt.isSome
    let texpOpt = transProg[TestFrame](astOpt.get)
    if expectGood:
        doAssert texpOpt.isSome, f
    else:
        doAssert texpOpt.isNone, f

test "appel tiger test programs good":
    for f in walkFiles("tests/tiger_test_programs/semant/good/*"):
        testFile f, true

test "appel tiger test programs bad":
    for f in walkFiles("tests/tiger_test_programs/semant/bad/*"):
        testFile f, false

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
    var st = newSymtab[EnvEntry[TestFrame]]()
    st.beginScope()
    for i in 0..100:
        st.enter symbol $i, EnvEntry[TestFrame](kind: VarEntry, ty: Type(kind: ErrorT))
    discard $st

test "var a := nil fails type check":
    let source = """let 
    var a := nil
    in 1 
    end"""
    testInputIsBad source

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
    testInputIsGood source

test "record a{f1=nil,...} works":
    let source = """let 
    type any = {any: int, x: any}
    var a : any := any{any=1, x=nil}
    in 
    42
    end"""
    testInputIsGood source


test "if...then... can both return nil":
    let source = """let 
    type any = {any: int}
    in 
    if 1 = 1 then nil else nil
    end"""
    testInputIsGood source


test "if...then..., if returns RecordT, then returns nil":
    let source = """let 
    type any = {any: int}
    var a: any := any{any=1}
    in 
    if 1 = 1 then a else nil
    end"""
    testInputIsGood source


test "if...then..., if returns nil, then returns RecordT":
    let source = """let 
    type any = {any: int}
    var a: any := any{any=1}
    in 
    if 1 = 1 then nil else a
    end"""
    testInputIsGood source


test "function a() : some_record = ... where body returns nil":
    let source = """let 
    type any = {any: int}
    function f(a: any) : any = nil
    in 
    f(nil)
    end"""
    testInputIsGood source


test "function a() = () should be accepted":
    let source = """let 
    function f() = ()
    in 
    f()
    end"""
    testInputIsGood source


test "for loop counter cannot be assigned to":
    let source = """let 
    in 
    for r := 0 to 10 do 
        if 1 = 1 then r := 1; ()
    end"""
    testInputIsBad source


test "records same but separate decls are distinct types":
    let source = """let 
    type any1 = {any: int}
    type any2 = {any: int}
    var a : any1 := nil
    var b : any2 := nil
    in 
    a = b 
    end"""
    testInputIsBad source


test "arrays same but separate decls are distinct types":
    let source = """let 
    type arr1 = array of int
    type arr2 = array of int
    var a1 := arr1[8] of 0 
    var a2 := arr2[8] of 0
    in 
    a1 = a2
    end"""
    testInputIsBad source


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
    testInputIsGood source


test "break not in while/for fails":
    let source = """
    let 
    in 
        break
    end"""
    testInputIsBad source


test "break in a while loop is legal":
    let source = """
    let 
    in 
        while 1 = 1 do
            break
    end"""
    testInputIsGood source


test "break in a for loop is legal":
    let source = """
    let 
    in 
        for i :=0 to 100 do
            break
    end"""
    testInputIsGood source


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
    testInputIsGood source


test "circular to self bad":
    let source = """
    let 
        type a = a 
    in 
    end"""
    testInputIsBad source


test "circular through seq of type decls bad":
    let source = """
    let 
        type a = b
        type b = a 
    in 
    end"""
    testInputIsBad source


test "circular self through array okay":
    let source = """
    let 
        type a = array of a 
    in 
    end"""
    testInputIsGood source


test "circular self through record okay":
    let source = """
    let 
        type a = {x: a}
    in 
    end"""
    testInputIsGood source


test "circular nonself through array ok":
    let source = """
    let 
        type a = b 
        type b = array of a 
    in 
    end"""
    testInputIsGood source


test "circular nonself through record ok":
    let source = """
    let 
        type a = b 
        type b = {x: a}
    in 
    end"""
    testInputIsGood source

test "redeclaration":
    let source = """
    let 
        var i := 1
        var i := 2 /* i above irrelevant for escape analysis */
        var k := 3
        /* function i's scope effectively at start of recursive blk */
        function j () = 
            (i();
            k + 1;
            ())
        function i () = 
            (2;
            ())
        var i := 4 
        function j () : int = 
            i + 1  /* i escapes here */
    in 
    end
    """
    testInputIsGood source

test "fundec block shadow var name":
    let source = """
    let
        var i := 1 
        function j () = 
            i := 2 /* this is illegal because i is a func */
        function i () =  /* i defined as func at start of j */
            ()
    in 
    end
    """
    testInputIsBad source

test "escape simple":
    let source = """
    let
        var i := 1 
        function j () = 
            i := 2 
    in 
        let 
            function k() = 
                i := 3
        in 
        end
    end
    """
    let astOpt = parseString(source)
    doAssert astOpt.isSome
    let ast = astOpt.get
    check ast.decs[0].escape == false
    ast.findEscape
    check ast.decs[0].escape == true 
    testInputIsGood source

test "escape var get redeclared as function":
    let source = """
    let
        var i := 1 
        function j () = 
            i()
        function i () = 
            ()
    in 
    end
    """
    let astOpt = parseString(source)
    doAssert astOpt.isSome
    let ast = astOpt.get
    check ast.decs[0].escape == false
    ast.findEscape
    check ast.decs[0].escape == false
    testInputIsGood source

test "escape var gets redeclared":
    let source = """
    let
        var i := 1 /* get shadowed so shouldn't move */
        var i := 2 /* i escapes */
        function j () = 
            i := 3 
        var i := 4 /* this is fresh and shouldn't escape */
        var j := 5  /* gets used later */
    in 
        let 
            function x() = 
                j := 42 
        in
        end
    end
    """
    let astOpt = parseString(source)
    doAssert astOpt.isSome
    let ast = astOpt.get
    check ast.decs[0].escape == false
    check ast.decs[1].escape == false
    check ast.decs[3].escape == false
    check ast.decs[4].escape == false
    ast.findEscape
    check ast.decs[0].escape == false
    check ast.decs[1].escape == true
    check ast.decs[3].escape == false
    check ast.decs[4].escape == true
    testInputIsGood source

test "for variable escapes":
    let source = """
    let
        function x() = 
            for i := 1 to 100 
            do
                let 
                    function j () : int = 
                        i + 1 
                    var i := 2 /* redeclare */ 
                    var k := 0 
                    function j () : int = 
                        i + 1  /* this now affects the inner i */
                in 
                end
    in
    end
    """
    let astOpt = parseString(source)
    doAssert astOpt.isSome
    let ast = astOpt.get
    check ast.decs[0].fundecs[0].body.escape == false
    check ast.decs[0].fundecs[0].body.fbody.decs[1].escape == false # inner i 
    check ast.decs[0].fundecs[0].body.fbody.decs[2].escape == false # k 
    ast.findEscape
    check ast.decs[0].fundecs[0].body.escape == true
    check ast.decs[0].fundecs[0].body.fbody.decs[1].escape == true # inner i 
    check ast.decs[0].fundecs[0].body.fbody.decs[2].escape == false # k 
    testInputIsGood source

test "for lo, hi does not affect escape of for loop var":
    let source = """
    let
        var i:= 1 
        function x() = 
            for i := let function j() = i:= 1 in 1 end to let function j() = i:= 1 in 100 end 
            do
                ()
    in
    end
    """
    let astOpt = parseString(source)
    doAssert astOpt.isSome
    let ast = astOpt.get
    check ast.decs[0].escape == false # outer i 
    check ast.decs[1].fundecs[0].body.escape == false # for loop i 
    ast.findEscape
    check ast.decs[0].escape == true # outer i 
    check ast.decs[1].fundecs[0].body.escape == false # for loop i 
    testInputIsGood source

test "for loop bad input to access for loop var in lo or hi":
    let source = """
    let
        function x() = 
            for i :=  let function j() = i:= 1 in 1 end to let function j() = i:= 100 in 100 end 
            do
                ()
    in 
    end
    """
    testInputIsBad source