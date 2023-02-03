import parse
import unittest
import nimyacc
import print
import options
import os

test "good programs":
  for f in walkFiles("tests/tiger_test_programs/parsing/good/*"):
    var input = readFile(f)
    var s: LexerState
    var testLexer = tigerLexer.newWithString(s, input)

    var parser = tigerParser.newParser()
    let ast = parser.parse_tigerParser(testLexer)
    doAssert parser.hasError == false, f
    doAssert ast.isSome, f

test "bad programs":
  for f in walkFiles("tests/tiger_test_programs/parsing/bad/*"):
    var input = readFile(f)
    var s: LexerState
    var testLexer = tigerLexer.newWithString(s, input)

    var parser = tigerParser.newParser()
    let ast = parser.parse_tigerParser(testLexer)
    doAssert parser.hasError, f
    doAssert ast.isNone, f
