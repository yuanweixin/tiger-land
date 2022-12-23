import lexer 
import std/sugar 

var input = readFile("tests/programs/merge.tig")
var res = collect(newSeq):
    for token in lexer(input): token
echo res