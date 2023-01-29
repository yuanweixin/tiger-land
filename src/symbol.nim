import tables, options

type
    Symbol* = (string, int)
    SymbolSet* = object
        created: Table[string, int]
        nextIdx: int

proc newSymbolSet*(): SymbolSet =
    # initiates a new set of symbols for use in compiler.
    # only one symbol set should be created per invocation of tiger.
    result = SymbolSet(created: initTable[string, int](), nextIdx: 0)

var symset = newSymbolSet()

func name*(s: Symbol): string =
    ## extracts the name part of the symbol.
    result = s[0]

proc symbol*(name: string): Symbol =
    ## Creates a new symbol with the given string in the global symbol set if it does not exist.
    ## If a symbol with the given name exists, it is returned.
    if name in symset.created:
        result = (name, symset.created[name])
    else:
        let idx = symset.nextIdx
        inc symset.nextIdx
        symset.created[name] = idx
        result = (name, idx)

const markerSym: Symbol = ("", -1) # marks the beginning of scope.
type
    Symtab*[T] = object ## symbol table object. interact with it using the procs.
        tbl: Table[Symbol, seq[T]]
        stack: seq[Symbol]

proc newSymtab*[T](): Symtab[T] =
    result = Symtab(tbl: initTable[Symbol, seq[T]](), stack: @[])

proc enter*[T](symtab: var Symtab[T], sym: Symbol, binding: T) =
    ## adds the given symbol to the symtab.
    if sym notin symtab.tbl:
        symtab.tbl[sym] = @[binding]
    else:
        symtab.tbl[sym].add binding
    symtab.stack.add sym

proc look*[T](symtab: var Symtab, sym: Symbol): Option[T] =
    ## locate the last binding of the given symbol in the symtab.
    if sym in symtab.tbl:
        let bindings = symtab.tbl[sym]
        return some[T](bindings[^1])
    return none[T]()

proc beginScope*(symtab: var Symtab) =
    ## this must be called at start of scope before adding symbols
    symtab.stack.add markerSym

proc endScope*(symtab: var Symtab) =
    ## this must be called at the end of a scope to clean up that scope's symbols
    while symtab.stack[^1] != markerSym:
        let sym = symtab.stack.pop
        symtab.tbl[sym].pop()
    symtab.stack.pop

