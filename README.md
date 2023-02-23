# About
Implementation of the tiger language specified in Appel's "Modern Compiler Implementation in ML", in the Nim language. 

# Naming
tiger-land was a typo when I made the repo, but it sounds way cooler than tiger-lang, so I kept it. 

# Progress
- [x] 2 Lexical analysis
- [x] 3 Parsing
- [x] 4 Abstract syntax
- [x] 5 Semantic analysis
- [x] 6 Activation records
- [x] 7 Translation to IR 
- [x] 8 Basic blocks and traces 
- [ ] 9 Instruction selection
- [ ] 10 Liveness analysis
- [ ] 11 Register allocation
- [ ] 12 Checkpoint 

# Other topics
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

# Thoughts on using the Nim language
Nim was chosen because 
* I needed a hobby language that have performance profile close to C. 
* I like whitespace significant syntax. 
* I don't want to look at or write C++. I am not a masochist. 
* Rust looks promising but besides the default move semantics and enforced lifetime tracking, it doesn't feel particularly exciting to use (meh syntax). Otoh, traits look nice and I wish Nim has a proper support for interfaces (concepts are the closest thing but it's experimental and tooling for it is not great). 

Overall, the Nim language has been a solid choice for this project. 

Pros:
* case objects are mostly like algebraic data types. 
* syntax looks clean and very concise 
* initially having to annotate everything feels annoying, but it makes the code very readable, no head scratching a week later wondering what the type of the function is.
* language community is active and people do respond to questions on forum. 
* language power to syntax is very high. 
* has the usual systems language feature (assembly, c/c++ interop), although I haven't tried them to see what it's like. 
* language is big, reminescent of c++, but mostly well thought out, and gives you _choices_ on how you write your software. it feels like designer respects your intelligence as a programmer. 

Cons:
* experimental features are...experimental. lots of them, and docs never say which ones actually work properly. for example, code reordering doesn't support mutually recursive procs, and there's some rfc somewhere about replacing it. 
* depending on how you look at it, this could be a pro, but when you encounter a rough edge, you need to temper your expectations, and there's (so far) always some workaround. 
* lacks pattern matching built-in, but you can use caseStatementMacros, experimental feature...
* concepts type checking done at instantiation site, and vscode tooling doesn't pick up errors. 
* concepts are not well integrated into generics. if you define a concept in module A, have a bunch of implementations, have module B define some procs that take the concept, and B does not directly reference the implementations, then the concept requires static dispatching to compile, otherwise it complains about missing procs. 
* procs with same name seem to shadow each other and compiler doesn't warn you
* having to rely on some experimental features (caseStatementMacros, concepts) to do pattern matching, interfaces is shitty. 
* metaprogramming gets real painful if you are trying to write some generic code. 
* compile time computation is painfully slow. it can time out. then you have to offload it to external process. except it's also a pain to marshal/unmarshal data for that purpose. see next item. 
* I had to do compile time json serialization, and the built-in marshal module didn't work for my type, so I had to try a bunch of libraries. In the end I found I can use std/jsonutils, but before I reached that point I tried a bunch of BUGGY-ass json libs out there. 
* Lack of incremental compilation - anything that `import parse` ends up running the grammar generation again which takes many seconds. 
