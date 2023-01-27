import parse
import unittest
import nimyacc
import print
import options

test "no error in merge.tig":
  var input = readFile("tests/merge.tig")
  var s: LexerState
  var testLexer = tigerLexer.newWithString(s, input)

  var parser = tigerParser.newParser()
  let ast = parser.parse_tigerParser(testLexer)
  check parser.hasError == false
  check ast.isSome

test "no error in queens.tig":
  var input = readFile("tests/queens.tig")
  var s: LexerState
  var testLexer = tigerLexer.newWithString(s, input)

  var parser = tigerParser.newParser()
  let ast = parser.parse_tigerParser(testLexer)
  check parser.hasError == false
  check ast.isSome

