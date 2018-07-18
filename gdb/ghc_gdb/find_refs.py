import typing
from typing import List, Optional, Tuple, TypeVar, Callable, NamedTuple
from . import ghc_heap
from .types import *
import gdb

T = TypeVar('T')

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

class Tree(typing.Generic[T]):
    def __init__(self, node: T, children: List['Tree[T]']):
        self.node = node
        self.children = children

    def _render(self, render_node: Callable[[T], str], indent: int) -> str:
        indentation = ' ' * (2*indent)
        return '\n'.join([indentation + render_node(self.node)] +
                         [child._render(render_node, indent+1)
                          for child in self.children])

    def render(self, render_node: Callable[[T], str]) -> str:
        return self._render(render_node, 0)

    def edges(self) -> List[Tuple[T, T]]:
        return [ (self.node, child.node)
                 for child in self.children ] + \
               [ edge
                 for child in self.children
                 for edge in child.edges() ]

    def to_dot(self, node_name: Callable[[T], str]) -> str:
        lines = ['digraph {'] + ['"%s" -> "%s";' % (node_name(a), node_name(b)) for (a,b) in self.edges()] + ['}']
        return '\n'.join(lines)


class ClosureRef(NamedTuple):
    referring_field: Ptr
    referring_closure: Ptr

def find_refs_rec(closure_ptr: Ptr, depth: int) -> Tree[ClosureRef]:
    """
    Recursively search for references to a closure up to the given depth.
    """
    inf = gdb.selected_inferior()
    def go(closure_ptr, seen, depth) -> List[Tree[ClosureRef]]:
        if depth == 0 or closure_ptr in seen:
            return []
        else:
            return [Tree(ClosureRef(ref, find_containing_closure(inf, ref)),
                         go(ref_start, seen.union({closure_ptr}), depth-1))
                    for ref in find_refs(closure_ptr)
                    for ref_start in [find_containing_closure(inf, ref)]
                    if ref_start is not None]

    return Tree(ClosureRef(0,0), go(closure_ptr, set(), depth))

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
            if i <= nptrs + 5: # A bit of fudge for the headers
                return Ptr(start)
            else:
                print('suspicious info table: too far (p=0x%08x, info=%s, nptrs=%d, i=%d)' % (start, sym.print_name, nptrs, i))

    return None
