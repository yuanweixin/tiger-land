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

# Thoughts on Nim
Overall, the Nim language is a pretty solid choice for this kind of work. For the most part it gets out of the way and lets me get stuff done. There are rough edges but none that make me want to stop using the language. It is definitely a hacker's language. 