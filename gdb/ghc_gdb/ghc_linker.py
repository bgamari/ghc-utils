import gdb

null = gdb.Value(0).cast(gdb.lookup_type('void').pointer())

def walk_hashtable(table):
    assert table.type == gdb.lookup_type('HashTable')
    HSEGSIZE = 1024 # Must match rts/Hash.c

    # Derived from Hash.c:mapHashTable
    segment = int(table['max'] + table['split'] - 1) / HSEGSIZE
    index = int(table['max'] + table['split'] - 1) % HSEGSIZE
    while segment >= 0:
        while index >= 0:
            hl = table['dir'][segment][index]
            while hl != null:
                h = hl.dereference()
                yield (h['key'], h['data'])
                hl = hl['next']

            index -= 1

        segment -= 1
        index = HSEGSIZE - 1

class LinkerSymbols(object):
    def __init__(self):
        RtsSymbolInfo = gdb.lookup_type('RtsSymbolInfo')
        SymbolName = gdb.lookup_type('char').pointer()
        addrToSymbol = []
        self.symbolToAddr = {}
        for k,v in walk_hashtable(gdb.parse_and_eval('symhash').dereference()):
            name = k.cast(SymbolName).string()
            info = v.cast(RtsSymbolInfo.pointer()).dereference()
            addr = int(info['value'])
            addrToSymbol.append((addr, name))
            self.symbolToAddr[name] = addr

        addrToSymbol.sort()
        self.addrs = [addr for (addr,x) in addrToSymbol]
        self.symbols = [sym for (x,sym) in addrToSymbol]

    def lookupSymbol(self, addr):
        (foundAddr, sym) = self.lookupNearestSymbol(addr)
        return None if addr != foundAddr else sym

    def lookupNearestSymbol(self, addr):
        import bisect
        i = bisect.bisect_left(self.addrs, addr)
        if i is None:
            return None
        if self.addrs[i] > addr:
            i -= 1
        return (self.addrs[i], self.symbols[i])

    def lookupAddr(self, symbol):
        return self.symbolToAddr.get(symbol)

_linkerSymbols = None
def getLinkerSymbols():
    global _linkerSymbols
    if not _linkerSymbols:
        _linkerSymbols = LinkerSymbols()
    return _linkerSymbols

class LookupGhcSymbolCmd(gdb.Command):
    """ Lookup the symbol an address falls with (assuming the symbol was loaded
        by the RTS linker) """
    def __init__(self):
        super(LookupGhcSymbolCmd, self).__init__ ("ghc symbol", gdb.COMMAND_USER)

    def invoke(self, args, from_tty):
        addr = int(gdb.parse_and_eval(args))
        foundAddr, sym = getLinkerSymbols().lookupNearestSymbol(addr)
        print("%d bytes into %s (starts at 0x%x)" % (addr - foundAddr, sym, foundAddr))

class LookupGhcAddrCmd(gdb.Command):
    """ Lookup the address of a symbol loaded by the RTS linker """
    def __init__(self):
        super(LookupGhcAddrCmd, self).__init__ ("ghc address", gdb.COMMAND_USER)

    def invoke(self, args, from_tty):
        sym = args
        foundAddr = getLinkerSymbols().lookupAddr(sym)
        if foundAddr is None:
            print("Failed to find %s" % sym)
        else:
            print("%s starts at 0x%x" % (sym, foundAddr))

    def complete(self, text, word):
        syms = getLinkerSymbols().symbolToAddr.keys()
        matches = [ sym for sym in syms if text in sym ]
        return matches


LookupGhcSymbolCmd()
LookupGhcAddrCmd()
