import lexer 
import std/sugar 
import tokens

var lexState = newLexState()
var input = readFile("tests/programs/merge.tig")
var res = collect(newSeq):
    for token in tigerTokenIter(input, lexState): token
echo res 

lexState = newLexState()
input = readFile("tests/programs/queens.tig")
res = collect(newSeq):
    for token in tigerTokenIter(input, lexState): token
echo res