import tables, options

type
    Symbol* = (string, int)
    SymbolSet* = object
        created: Table[string, int]
        nextIdx: int

proc newSymbolSet*(): SymbolSet =
    result = SymbolSet(created: initTable[string, int](), nextIdx: 0)

func name*(s: Symbol): string =
    ## extracts the name part of the symbol.
    result = s[0]

proc symbol*(symset: var SymbolSet, name: string): Symbol =
    ## creates a new symbol with the given string.
    ## the existing symbol is returned.
    if name in symset.created:
        result = (name, symset.created[name])
    else:
        let idx = symset.nextIdx
        inc symset.nextIdx
        symset.created[name] = idx
        result = (name, idx)



const markerSym: Symbol = ("", -1) # marks the beginning of scope.
type
    Symtab*[T] = object
        tbl: Table[Symbol, seq[T]]
        stack: seq[Symbol]

proc newSymtab*[T](): Symtab[T] =
    result = Symtab(tbl: initTable[Symbol, seq[T]](), stack: @[])

proc enter*[T](symtab: var Symtab[T], sym: Symbol, binding: T) =
    if sym notin symtab.created:
        symtab.created[sym] = @[binding]
    else:
        symtab.created[sym].add binding
    symtab.stack.add sym

proc look*[T](symtab: var Symtab, sym: Symbol): Option[T] =
    if sym in symtab.created:
        let bindings = symtab.created[sym]
        return some[T]bindings[bindings.high]
    return none[T]()

proc beginScope*(symtab: var Symtab) =
    symtab.stack.add markerSym

proc endScope*(symtab: var Symtab) =
    while symtab.stack[symtab.stack.high] != markerSym:
        let sym = symtab.stack.pop
        symtab[sym].pop()
    symtab.stack.pop

