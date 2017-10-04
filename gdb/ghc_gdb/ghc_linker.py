import gdb

null = gdb.Value(0).cast(gdb.lookup_type('void').pointer())

def walk_hashtable(table):
    assert table.type == gdb.lookup_type('HashTable')
    HSEGSIZE = 1024

    # Derived from Hash.c:mapHashTable
    segment = int((table['max'] - table['split'] - 1) / HSEGSIZE)
    index = int((table['max'] - table['split'] - 1) % HSEGSIZE)
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
        return self.symbolToAddr[symbol]

_linkerSymbols = None
def getLinkerSymbols():
    global _linkerSymbols
    if not _linkerSymbols:
        _linkerSymbols = LinkerSymbols()
    return _linkerSymbols

class LookupGhcSymbolCmd(gdb.Command):
    def __init__(self):
        super(LookupGhcSymbolCmd, self).__init__ ("ghc symbol", gdb.COMMAND_USER)

    def invoke(self, args, from_tty):
        addr = int(gdb.parse_and_eval(args))
        foundAddr, sym = getLinkerSymbols().lookupNearestSymbol(addr)
        print("%d bytes into %s (starts at 0x%x)" % (addr - foundAddr, sym, foundAddr))


LookupGhcSymbolCmd()
