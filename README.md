# About
Implementation of the tiger language specified in Appel's "Modern Compiler Implementation in ML", in the Nim language. 

# Status

Abandoned to switch implementation language. Nim was not a good choice. It was fun for a while until it became a PITA to keep using. 

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

I gave up on the language when I realized, concepts is a half-assed "feature" and i couldn't straightforwardly declare that i want a generic parameter to be of this concept, and then call the procs of the concept, without a compiler error saying it couldn't find the proc. and i already experienced many paper cuts before this of random shit not working or not working all the way. 

Switching to Rust, as that language has escape velocity, has a future, is so much more user friendly (documentation for everything, super user friendly compiler error messages, linter, code formatter, and actually runs without a gc, unlike the weird shit Nim tries to pull off trying to pass off as a language for hard realtime systems but still use GC), and actually makes shared-memory threading so much easier. Nim has next to none documentation on how to do parallelism, because the people who know how to don't write documentation, and you are left to just figure shit out on your own. 

This was fun for a bit until the language died under the interaction of its incoherent features and complexity. 

Pros:
* case objects are basically C tag unions. case statement on the basic case object kinds does check for exhaustiveness. 
* syntax looks clean and very concise. high s/n ratio. 
* has the usual systems language feature (assembly, c/c++ interop), although I haven't tried them to see what it's like. 

Cons:
* having to annotate everything is stupid. it inherited the Pascal way of verbosity. 
* still memory unsafe, unclear how to do multithreaded code. 
* the memory model is a joke. a gazillion GC. then a patchwork of move semantics and destructors added to imitate RAII. 
* experimental features are...experimental. lots of them, and docs never say which ones actually work properly. for example, code reordering doesn't support mutually recursive procs, and there's some rfc somewhere about replacing it. so much cruft. 
* there are so many little things that make using Nim for non-trivial projects a death by thousand cuts. for example, all symbols are imported and conflicting names resolve depending on import order (and i am not even sure which names follow this rule because it does complain about conflicting overloads). 
* lacks real pattern matching built-in, but you can use caseStatementMacros, experimental feature...cough...
* concepts type checking done at instantiation site, and vscode tooling doesn't pick up errors. 
* concepts are very hard to use. 
* concept doesn't play with generics. if you define a concept in module A, have a bunch of implementations, have module B define some procs that take the concept, and B does not directly reference the implementations, then the concept requires static dispatching to compile, otherwise it complains about missing procs. 
* procs with same name seem to shadow each other and compiler doesn't warn you
* metaprogramming gets real painful real fucking quick, if you are trying to generate a bunch of non-trivial code. this tries so hard to be lisp but fails so badly because it has syntax. 
* compile time computation is painfully slow. it can time out. then you have to offload it to external process. except it's also a pain to marshal/unmarshal data for that purpose. for example, my parser generator times out on tiger grammer, so i had to rewrite the shit to call an exernal process, and i had to try half a dozen serialization libs before finding a way to do it. 
* there is no ecosystem. most libraries are 1 man projects and half-abandoned, bug-ridden pieces of shit. see above comment about json. 
* lack of incremental compilation - anything that `import parse` ends up running the grammar generation again which takes many seconds. 
* community initially looks active and kind of welcoming, but upon digging, you discover Nimskull fork, and the scathing complains from ex-communicated members who have a grudge against the core members. if so mature, why much drama? 
* araq may be a competent language designer but he def is weak in type theory. why couldn't the type system support interfaces/protocols in some kind of bounded polymorphism? 
* to continue, the whole language dev process lacks structure and transparency. 
* i don't see any effort to promote industry use. so this is doomed to be forever some language playground for araq that a few people find cool and use. 
