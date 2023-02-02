# About
Implementation of the tiger language specified in Appel's "Modern Compiler Implementation in ML", in the Nim language. 

# Naming
tiger-land was a typo when I made the repo, but it sounds way cooler than tiger-lang, so I kept it. 

# Progress
- [x] 2 Lexical analysis
- [x] 3 Parsing
- [x] 4 Abstract syntax
- [x] 5 Semantic analysis
- [ ] 6 Activation records
- [ ] 7 Translation to IR 
- [ ] 8 Basic blocks and traces 
- [ ] 9 Instruction selection
- [ ] 10 Liveness analysis
- [ ] 11 Register allocation
- [ ] 12 Checkpoint 
- [ ] Dataflow analysis
- [ ] Loop optimization
- [ ] SSA
- [ ] Pipelining, scheduling 
- [ ] Memory hierarchies
- [ ] Garbage collection
- [ ] Functional features
- [ ] Polymorphic types 
- [ ] Garbage collection
- [ ] Oop features 

I rolled my own lexer and parser generators. They took more effort than expected, but are quite fun to implement and get right. 

# Build

nim c -d:release src/main.nim

# Run

(Assuming your exe is in src)

./src/main

# Usage (of the tiger compiler)

## Flags

TBD

# Mini-review of the Nim language
Nim was chosen because 
* I needed a hobby language that have good performance. 
* The community seems small enough for me to have some impact in. I apparently like to use languages that aren't popular (e.g. F#, Ocaml, Haskell). 
* I like whitespace significant syntax. 
* I don't want to look at or write C++, hurts my eyes and I am not a masochist. 
* Rust seems like a big hype train (riding on Mozilla name/resources and now corporate support) but I find the syntax ugly and the language feels almost as masochist as C++. I don't want to manually move variables. I think Nim is going in the right direction with (mostly) compiler generated moves and compile time ref counting. 
* I am impressed at the caliber of core Nim devs. 
* The community is fairly active. 

Overall, the Nim language has been a solid choice for this project. It has case objects which is more or less algebraic data type (with rough edge, like you can't use the same field name for different cases). For the most part the language gets out of the way and lets me get stuff done. There are rough edges but none serious enough that make me want to stop using it. 

Some rough edges include: 
* experimental features don't always work, or have weird limitations. for example, code reordering doesn't support mutually recursive procs. The language creator said they are working on getting rid of that hacky solution and incorporating a proper one into the core lang itself. 
* have to rely on some experimental features (caseStatementMacros) + addendum to the standard lib (fusion) for pattern matching, which I read is going to get revamped in some future RFC. 
* metaprogramming gets painful if you are transforming input before generating code. 
* I had to do compile time json serialization, and the built-in marshal module didn't work for my type, so I had to try a bunch of libraries. In the end I found I can use std/jsonutils, but before I reached that point I tried a bunch of BUGGY json libs out there. 

Rough edges aside, Nim is definitely a hacker's language. It lets you build things. It doesn't prevent you from doing low-level stuff. You can get stuff done if you know what you are doing. It is orders of magnitude more terse and expressive than anything in the C family. The required type annotations also significantly aids code reading (no guessing what a parameter's type is like you might on a python codebase, or a ML one) and makes it super easy to grep for particular patterns during refactor. 

I haven't had the need to do any C/C++ interops so far, so just taking people's words that it's supposed to be easy. Some day we will cross that bridge. 