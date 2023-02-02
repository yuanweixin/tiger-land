import tables, options

type
    Symbol* = (string, int)
    SymbolSet* = object
        created: Table[string, int]
        nextIdx: int

proc `$`*(s: Symbol): string =
    result.add "Symbol("
    result.add s[0]
    result.add ", "
    result.add $s[1]
    result.add ")"

# only one instance is created per invocation of tiger.
var symset = SymbolSet(created: initTable[string, int](), nextIdx: 0)

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

proc `$`*[T](st: Symtab[T]): string =
    result.add "Symtab table\n"
    for (k, v) in st.tbl.pairs():
        if v.len > 0:
            result.add "\t"
            result.add $k
            result.add "\n"
            for t in v:
                result.add "\t\t"
                result.add $t
                result.add "\n"
    result.add "Symtab stack (top down)\n"
    for i in countdown(st.stack.high, 0):
        result.add "\t"
        result.add $st.stack[i]
        result.add "\n"

proc newSymtab*[T](): Symtab[T] =
    result = Symtab[T](tbl: initTable[Symbol, seq[T]](), stack: @[])

proc enter*[T](symtab: var Symtab[T], sym: Symbol, binding: T) =
    when defined(tigerdevel):
        doAssert symtab.stack[0] == markerSym,
                "missing beginScope() call on symtab\n" & $symtab
    ## adds the given symbol to the symtab.
    if sym notin symtab.tbl:
        symtab.tbl[sym] = @[binding]
    else:
        symtab.tbl[sym].add binding
    symtab.stack.add sym
    when defined(tigerdevel):
        echo "entering symbol ", sym, " in symtab\n", symtab

proc look*[T](symtab: var Symtab[T], sym: Symbol): Option[T] =
    when defined(tigerdevel):
        echo "looking up symbol ", sym, " in symtab\n", symtab
    ## locate the last binding of the given symbol in the symtab.
    if sym in symtab.tbl:
        let bindings = symtab.tbl[sym]
        if bindings.len == 0:
            return none[T]()
        return some[T](bindings[^1])
    return none[T]()

proc beginScope*[T](symtab: var Symtab[T]) =
    when defined(tigerdevel):
        echo "beginScope called on symtab\n", symtab
    ## this must be called at start of scope before adding symbols
    symtab.stack.add markerSym

proc endScope*[T](symtab: var Symtab[T]) =
    when defined(tigerdevel):
        echo "endScope called on symtab\n", symtab
    ## this must be called at the end of a scope to clean up that scope's symbols
    while symtab.stack[^1] != markerSym:
        let sym = symtab.stack.pop
        when defined(tigerdevel):
            echo "popped symbol ", sym
        discard symtab.tbl[sym].pop()
    discard symtab.stack.pop

