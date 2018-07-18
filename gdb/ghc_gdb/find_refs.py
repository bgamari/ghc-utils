from typing import List, Optional, Tuple
from . import ghc_heap
import gdb

word_size = 8
if word_size == 8:
    TAG_MASK = 7
else:
    TAG_MASK = 3

class Ptr(int):
    def __str__(self):
        return hex(self)
    def __repr__(self):
        return hex(self)
    def untagged(self):
        return self & ~TAG_MASK

# This is a terrible hack but it's better than searching through 1TB of address space.
# Unfortunately while gdb's `find` command seems to be smart enough to only search
# mapped memory, the same can't be said of Python's search_memory.
heap_start = 0x4200000000
heap_end = 0x4210000000

def search_memory_many(inferior: gdb.Inferior, start: Ptr, end: Ptr, val: Ptr) -> List[Ptr]:
    #print('Searching for %s' % val)
    while True:
        addr = inferior.search_memory(start, end-start, val)
        if addr is None:
            break
        else:
            yield Ptr(addr)
            start = addr + 1

def pack_pointer(ptr: Ptr) -> bytes:
    import struct
    return struct.pack('@P', ptr)

def unpack_pointer(bs: bytes) -> Ptr:
    import struct
    return struct.unpack('@P', bs)[0]

def find_symbol(addr: Ptr) -> Optional[gdb.Symbol]:
    """ Find the top-level Symbol associated with an address. """
    try:
        block = gdb.block_for_pc(addr)
    except RuntimeError:
        return None

    if block is None:
        return None

    while block and not block.function:
        block = block.superblock
    return block.function

def find_refs(closure_ptr: Ptr) -> List[Ptr]:
    """ Find all references to a closure. """
    inf = gdb.selected_inferior()
    closure_ptr = Ptr(closure_ptr).untagged()
    for tag in range(0,7):
        val = pack_pointer(closure_ptr | tag)
        yield from search_memory_many(inf, 0, 0x1000000, val) # static mappings
        yield from search_memory_many(inf, heap_start, heap_end, val) # heap

RecRefs = List[Tuple[Ptr, 'RecRefs']]

def find_refs_rec(closure_ptr: Ptr, depth: int) -> RecRefs:
    """
    Recursively search for references to a closure up to the given depth.
    """
    inf = gdb.selected_inferior()
    if depth == 0:
        return []
    else:
        return [(ref, find_refs_rec(ref_start, depth-1))
                for ref in find_refs(closure_ptr)
                for ref_start in [find_containing_closure(inf, ref)]
                if ref_start is not None]

def find_containing_closure(inferior: gdb.Inferior, ptr: Ptr) -> Optional[Ptr]:
    """
    Try to identify the beginning of the closure containing the given address.
    Note that this is quite heuristic (looking for something that looks like an
    info table)
    """
    max_closure_size = 4096
    ptr = Ptr(ptr).untagged()
    for i in range(max_closure_size // word_size):
        start = ptr - i * word_size
        bs = inferior.read_memory(start, word_size)
        addr = unpack_pointer(bs)
        sym = find_symbol(addr)
        # Is this an info table pointer?
        if sym is not None and sym.print_name.endswith('_info'): # and sym.value == addr:
            info = ghc_heap.get_itbl(gdb.parse_and_eval('(StgClosure *) %d' % start))
            nptrs = int(info['layout']['payload']['ptrs'])
            if i <= nptrs:
                return Ptr(start)
            else:
                print('suspicious info table: too far (p=0x%08x, info=%s, nptrs=%d, i=%d)' % (start, sym.print_name, nptrs, i))

    return None

def render_tree(rec_refs: RecRefs) -> str:
    def gos(xs, n):
        return [r
                for x in xs
                for r in go(x, n)]

    def go(x, n):
        ptr, xs = x
        return ['%s0x%x:' % (' '*(2*n), ptr)] + gos(xs, n+1)

    return '\n'.join(gos(rec_refs, 0))
