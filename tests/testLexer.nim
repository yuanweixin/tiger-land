import lexer 
import std/sugar 
import std/unittest 
import tokens_util
import experimental/diff
import utils 

proc printDiff(exp: seq[Token], act: seq[Token]) =
    var ie, ia = 0 
    var ok = true 
    while ia < act.len and ie < exp.len:
        if exp[ie] != act[ia]:
            echo "diff at idx " & $ia
            echo $exp[ie] & " != " & $act[ia]
            ok = false 
        inc ia 
        inc ie 
    if ie < exp.len:
        echo "remaining items in expected"
        echo $exp[ie..^1]
        ok = false
    if ia < act.len:
        echo "remaining items in actual"
        echo $act[ia..^1]
        ok = false 
    doAssert ok, "expected != actual, see above"

proc runTest(input: string, expected: seq[Token]) = 
    var lexState = newLexState()
    let actual = collect(newSeq):
            for token in tigerTokenIter(input, lexState): token
    doAssert expected, actual, printDiff

suite "string":
    test "\n":
        let input = """"ho\nho\nho""""
        let expected = @[String("ho\nho\nho")]
        runTest(input, expected)

    test "\t":
        let input = """"ho\tho\tho""""
        let expected = @[String("ho\tho\tho")]
        runTest(input, expected)

    test r"\^c":
        # just gonna use python's set of escape characters 
        # \r carriage return 
        # \b backspace
        # \f form feed
        let input = """"h\re\br\f""""
        let expected = @[String("h\re\br\f")]
        runTest(input, expected)

    test r"\ddd":
        discard

    test r"\":
        let input = """"hell\\o""""
        let expected = @[String("hell\\o")]
        runTest(input, expected)

    test "\"":
        let input = """"quote\"o""""
        let expected = @[String("quote\"o")]
        runTest(input, expected)

    test r"\f___f\":
        discard 

suite "merge.tig lex":
    let data : seq[(string, seq[Token])] = 
        @[
            ("""let 

                type any = {any : int}
                var buffer := getchar()
                """, 
                @[Let, Type, Id("any"), Eq, LBrace, Id("any"), Colon, Id("int"), Rbrace, Var, Id("buffer"), Assign, Id("getchar"), Lparen, Rparen]
            ),
            ("""
            function readint(any: any) : int =
            let var i := 0
                function isdigit(s : string) : int = 
                            ord(buffer)>=ord("0") & ord(buffer)<=ord("9")
                function skipto() =
                while buffer=" " | buffer="\n"
                    do buffer := getchar()
            in skipto();
                any.any := isdigit(buffer);
                while isdigit(buffer)
                do (i := i*10+ord(buffer)-ord("0"); buffer := getchar());
                i
            end

            """, @[Function, Id("readint"), Lparen, Id("any"), Colon, Id("any"), Rparen, Colon, Id("int"), Eq, Let, Var, Id("i"), Assign, Int(0), Function, Id("isdigit"), Lparen, Id("s"), Colon, Id("string"), Rparen, Colon, Id("int"), Eq, Id("ord"), LParen, Id("buffer"), Rparen, Ge, Id("ord"), Lparen, String("0"), Rparen, And, Id("ord"), Lparen, Id("buffer"), Rparen, Le, Id("ord"), Lparen, String("9"), Rparen, Function, Id("skipto"), Lparen, Rparen, Eq, While, Id("buffer"), Eq, String(" "), Or, Id("buffer"), Eq, String("\n"), Do, Id("buffer"), Assign, Id("getchar"), Lparen, Rparen, In, Id("skipto"), Lparen, Rparen, Semicolon, Id("any"), Dot, Id("any"), Assign, Id("isdigit"), Lparen, Id("buffer"), Rparen, Semicolon, While, Id("isdigit"), Lparen, Id("buffer"), Rparen, Do, Lparen, Id("i"), Assign, Id("i"), Times, Int(10), Plus, Id("ord"), Lparen, Id("buffer"), Rparen, Minus, Id("ord"), Lparen, String("0"), Rparen, Semicolon, Id("buffer"), Assign, Id("getchar"), Lparen, Rparen, Rparen, Semicolon, Id("i"), End]),
            (
            """
            type list = {first: int, rest: list}

            function readlist() : list =
                let var any := any{any=0}
                    var i := readint(any)
                in if any.any
                    then list{first=i,rest=readlist()}
                    else nil
                end

            """, @[Type, Id("list"), Eq, Lbrace, Id("first"), Colon, Id("int"), Comma, Id("rest"), Colon, Id("list"), Rbrace, Function, Id("readlist"), Lparen, Rparen, Colon, Id("list"), Eq, Let, Var, Id("any"), Assign, Id("any"), Lbrace, Id("any"), Eq, Int(0), Rbrace, Var, Id("i"), Assign, Id("readint"), Lparen, Id("any"), Rparen, In, If, Id("any"), Dot, Id("any"), Then, Id("list"), LBrace, Id("first"), Eq, Id("i"), Comma, Id("rest"), Eq, Id("readlist"), Lparen, Rparen, Rbrace, Else, Nil, End]),
            (
            """
            function merge(a: list, b: list) : list =
            if a=nil then b
            else if b=nil then a
            else if a.first < b.first 
                then list{first=a.first,rest=merge(a.rest,b)}
                else list{first=b.first,rest=merge(a,b.rest)}

            """, @[Function, Id("merge"), Lparen, Id("a"), Colon, Id("list"), Comma, Id("b"), Colon, Id("list"), Rparen, Colon, Id("list"), Eq, If, Id("a"), Eq, Nil, Then, Id("b"), Else, If, Id("b"), Eq, Nil, Then, Id("a"), Else, If, Id("a"), Dot, Id("first"), Lt, Id("b"), Dot, Id("first"), Then, Id("list"), Lbrace, Id("first"), Eq, Id("a"), Dot, Id("first"), Comma, Id("rest"), Eq, Id("merge"), Lparen, Id("a"), Dot, Id("rest"), Comma, Id("b"), Rparen, Rbrace, Else, Id("list"), Lbrace, Id("first"), Eq, Id("b"), Dot, Id("first"), Comma, Id("rest"), Eq, Id("merge"), Lparen, Id("a"), Comma, Id("b"), Dot, Id("rest"), Rparen, Rbrace]),
            (
            """
            function printint(i: int) =
            let function f(i:int) = if i>0 
                        then (f(i/10); print(chr(i-i/10*10+ord("0"))))
            in if i<0 then (print("-"); f(-i))
                else if i>0 then f(i)
                else print("0")
            end

            """, @[Function, Id("printint"), Lparen, Id("i"), Colon, Id("int"), Rparen, Eq, Let, Function, Id("f"), Lparen, Id("i"), Colon, Id("int"), Rparen, Eq, If, Id("i"), Gt, Int(0), Then, Lparen, Id("f"), Lparen, Id("i"), Divide, Int(10), Rparen, Semicolon, Id("print"), Lparen, Id("chr"), Lparen, Id("i"), Minus, Id("i"), Divide, Int(10), Times, Int(10), Plus, Id("ord"), Lparen, String("0"), Rparen, Rparen, Rparen, Rparen, In, If, Id("i"), Lt, Int(0), Then, Lparen, Id("print"), Lparen, String("-"), Rparen, Semicolon, Id("f"), Lparen, Minus, Id("i"), Rparen, Rparen, Else, If, Id("i"), Gt, Int(0), Then, Id("f"), Lparen, Id("i"), Rparen, Else, Id("print"), Lparen, String("0"), Rparen, End]),
            (
            """
            function printlist(l: list) =
            if l=nil then print("\n")
            else (printint(l.first); print(" "); printlist(l.rest))

            var list1 := readlist()
            var list2 := (buffer:=getchar(); readlist())


            /* BODY OF MAIN PROGRAM */
            in printlist(merge(list1,list2))
            end
            """, @[Function, Id("printlist"), Lparen, Id("l"), Colon, Id("list"), Rparen, Eq, If, Id("l"), Eq, Nil, Then, Id("print"), Lparen, String("\n"), Rparen, Else, Lparen, Id("printint"), Lparen, Id("l"), Dot, Id("first"), Rparen, Semicolon, Id("print"), Lparen, String(" "), Rparen, Semicolon, Id("printlist"), Lparen, Id("l"), Dot, Id("rest"), Rparen, Rparen, Var, Id("list1"), Assign, Id("readlist"), Lparen, Rparen, Var, Id("list2"), Assign, Lparen, Id("buffer"), Assign, Id("getchar"), Lparen, Rparen, Semicolon, Id("readlist"), Lparen, Rparen, Rparen, In, Id("printlist"), Lparen, Id("merge"), Lparen, Id("list1"), Comma, Id("list2"), Rparen, Rparen, End]
            )
        ]

    test "merge.tig":
        for (input, expected) in data:
            runTest(input, expected)

suite "queens.tig":
    let data : seq[(string, seq[Token])] = @[
        (
            """
            /* A program to solve the 8-queens problem */

            let
                var N := 8

                type intArray = array of int

                var row := intArray [ N ] of 0
                var col := intArray [ N ] of 0
                var diag1 := intArray [N+N-1] of 0
                var diag2 := intArray [N+N-1] of 0
            """, 
            @[Let, Var, Id("N"), Assign, Int(8), Type, Id("intArray"), Eq, Array, Of, Id("int"), Var, Id("row"), Assign, Id("intArray"), Lbrack, Id("N"), Rbrack, Of, Int(0), Var, Id("col"), Assign, Id("intArray"), Lbrack, Id("N"), Rbrack, Of, Int(0), Var, Id("diag1"), Assign, Id("intArray"), Lbrack, Id("N"), Plus, Id("N"), Minus, Int(1), Rbrack, Of, Int(0), Var, Id("diag2"), Assign, Id("intArray"), Lbrack, Id("N"), Plus, Id("N"), Minus, Int(1), Rbrack, Of, Int(0)]
        ),
        (
            """
            function printboard() =
            (for i := 0 to N-1
            do (for j := 0 to N-1 
                do print(if col[i]=j then " O" else " .");
                print("\n"));
                print("\n"))
            """, 
            @[Function, Id("printboard"), Lparen, Rparen, Eq, Lparen, For, Id("i"), Assign, Int(0), To, Id("N"), Minus, Int(1), Do, Lparen, For, Id("j"), Assign, Int(0), To, Id("N"), Minus, Int(1), Do, Id("print"), Lparen, If, Id("col"), Lbrack, Id("i"), Rbrack, Eq, Id("j"), Then, String(" O"), Else, String(" ."), Rparen, Semicolon, Id("print"), Lparen, String("\n"), Rparen, Rparen, Semicolon, Id("print"), Lparen, String("\n"), Rparen, Rparen]
        ),
        (
            """
                function try(c:int) = 
            ( /*  for i:= 0 to c do print("."); print("\n"); flush();*/
                if c=N
                then printboard()
                else for r := 0 to N-1
                do if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
                        then (row[r]:=1; diag1[r+c]:=1; diag2[r+7-c]:=1;
                            col[c]:=r;
                                try(c+1);
                        row[r]:=0; diag1[r+c]:=0; diag2[r+7-c]:=0)

            )
            in try(0)
            end
            """, 
            @[Function, Id("try"), Lparen, Id("c"), Colon, Id("int"), Rparen, Eq, Lparen, If, Id("c"), Eq, Id("N"), Then, Id("printboard"), Lparen, Rparen, Else, For, Id("r"), Assign, Int(0), To, Id("N"), Minus, Int(1), Do, If, Id("row"), Lbrack, Id("r"), Rbrack, Eq, Int(0), And, Id("diag1"), Lbrack, Id("r"), Plus, Id("c"), Rbrack, Eq, Int(0), And, Id("diag2"), Lbrack, Id("r"), Plus, Int(7), Minus, Id("c"), Rbrack, Eq, Int(0), Then, Lparen, Id("row"), Lbrack, Id("r"), Rbrack, Assign, Int(1), Semicolon, Id("diag1"), Lbrack, Id("r"), Plus, Id("c"), Rbrack, Assign, Int(1), Semicolon, Id("diag2"), Lbrack, Id("r"), Plus, Int(7), Minus, Id("c"), Rbrack, Assign, Int(1), Semicolon, Id("col"), Lbrack, Id("c"), Rbrack, Assign, Id("r"), Semicolon, Id("try"), Lparen, Id("c"), Plus, Int(1), Rparen, Semicolon, Id("row"), Lbrack, Id("r"), Rbrack, Assign, Int(0), Semicolon, Id("diag1"), Lbrack, Id("r"), Plus, Id("c"), Rbrack, Assign, Int(0), Semicolon, Id("diag2"), Lbrack, Id("r"), Plus, Int(7), Minus, Id("c"), Rbrack, Assign, Int(0), Rparen, Rparen, In, Id("try"), Lparen, Int(0), Rparen, End]
        )
    ]
    test "queens.tig":
        for (input, expected) in data:
            runTest(input, expected)
