import typing
from typing import List, Optional, Tuple, TypeVar, Callable, NamedTuple, Set, Dict, Iterator, Any
from . import ghc_heap
from .types import *
from . import closure
from .utils import CommandWithArgs
from .block import get_bdescr, heap_start, heap_end
import gdb

T = TypeVar('T')

stg_STACK_info = gdb.parse_and_eval('&stg_STACK_info')

def search_memory_many(inferior: gdb.Inferior, start: Ptr, end: Ptr, val: bytes) -> Iterator[Ptr]:
    #print('Searching for %s' % val)
    while True:
        addr = inferior.search_memory(start.addr(), end.addr() - start.addr(), val)
        if addr is None:
            break
        else:
            addr = Ptr(addr)
            yield addr
            start = addr.offset_bytes(1)

def find_symbol(addr: Ptr) -> Optional[gdb.Symbol]:
    """
    Find the top-level Symbol associated with an address.
    Due to limitations of the GDB Python bindings this will only find
    symbols with debug information (not what gdb calls "minimal symbols").
    """
    try:
        block = gdb.block_for_pc(addr.addr())
    except RuntimeError:
        return None

    while block is not None and not block.function:
        block = block.superblock

    return block.function if block is not None else None

def find_symbol_name(addr: Ptr) -> Optional[str]:
    sym = find_symbol(addr)
    if sym:
        return sym.print_name
    else:
        # hack to support non-code symbols
        # see https://cygwin.com/ml/gdb/2017-12/msg00015.html
        s = str(gdb.parse_and_eval('(void*) %s' % addr))
        start = s.find('<')
        end = s.find('>')
        if start is not None and end is not None:
            sym_name = s[start+1:end]
            return sym_name
        else:
            return None

def find_refs(closure_ptr: Ptr, include_static=True) -> Iterator[Ptr]:
    """ Find all references to a closure. """
    inf = gdb.selected_inferior()
    closure_ptr = closure_ptr.untagged()
    for tag in range(0,7):
        val = closure_ptr.tagged(tag).pack() # type: bytes

        if include_static:
            yield from search_memory_many(inf, Ptr(0), Ptr(0x10000000), val)
        yield from search_memory_many(inf, heap_start, heap_end, val) # heap


class Edge(NamedTuple):
    referring_field: Ptr
    referring_closure: Optional[Ptr]
    referree_closure: Ptr

def find_refs_rec(closure_ptr: Ptr,
                  max_depth: int,
                  max_closure_size: int,
                  include_static = True
) -> Iterator[Edge]:
    """
    Recursively search for references to a closure up to the given depth.
    """
    inf = gdb.selected_inferior()

    # Objects we have found references to
    seen_closures = set() # type: Set[Ptr]
    # Objects we have yet to search for references to
    todo = [(0, closure_ptr)] # type: List[Tuple[int, Ptr]]
    while len(todo) > 0:
        d, ptr = todo.pop()
        seen_closures |= {ptr}
        found_refs = find_refs(ptr, include_static=include_static)
        for ref in set(found_refs):
            ref_start = find_containing_closure(inf, ref,
                                                max_closure_size=max_closure_size)
            if ref_start is not None:
                print('%s -> %s' % (ref, ptr))
                if d < max_depth and ref_start not in seen_closures:
                    todo.append((d+1, ref_start))
            else:
                print('Failed to find beginning of %s' % ref)

            edge = Edge(referring_field=ref,
                        referring_closure=ref_start,
                        referree_closure=ptr)
            yield edge

def find_containing_stack(inferior: gdb.Inferior,
                          ptr: Ptr
                          ) -> Optional[Ptr]:
    ptr = ptr.untagged()
    for i in range(0x10000):
        start = ptr.offset_bytes(- i * word_size)
        if start <= Ptr(0):
            break

        bs = None
        try:
            bs = inferior.read_memory(start.addr(), word_size)
        except gdb.MemoryError:
            continue

        addr = Ptr.unpack(bs)
        if addr.addr() == stg_STACK_info.address:
            return start

    return None

def find_containing_closure(inferior: gdb.Inferior,
                            ptr: Ptr,
                            max_closure_size: int
) -> Optional[Ptr]:
    """
    Try to identify the beginning of the closure containing the given address.
    Note that this is quite heuristic (looking for something that looks like an
    info table)
    """
    ptr = ptr.untagged()
    for i in range(max_closure_size):
        start = ptr.offset_bytes(- i * word_size)
        bs = None
        try:
            bs = inferior.read_memory(start.addr(), word_size)
        except gdb.MemoryError:
            continue

        addr = Ptr.unpack(bs)
        sym = find_symbol_name(addr)
        # Is this an info table pointer?
        if sym is not None and sym.endswith('_info'): # and sym.value == addr:
            info = ghc_heap.get_itbl(gdb.parse_and_eval('(StgClosure *) %d' % start.addr()))
            if int(info['type']) in closure.stack_frame_types:
                return find_containing_stack(inferior, addr)

            nptrs = int(info['layout']['payload']['ptrs'])
            #print(ptr, i, info, nptrs)
            if int(info['type']) == closure.ClosureType.IND_STATIC:
                if ptr.addr() - start.addr() > 8:
                    # Only trace the indirectee of IND_STATICs
                    return None
            elif i <= nptrs + 5: # A bit of fudge for the headers
                return start
            else:
                print('suspicious info table: too far (field=%s, info@%s=%s, nptrs=%d, i=%d)' % (ptr, start, sym, nptrs, i))
                return None

    return None

closureTypeDict = { v: str(ty) for ty, v in closure.ClosureType.__dict__.items() }

BF_NONMOVING = 1024 # gdb.parse_and_eval('BF_NONMOVING')
NONMOVING_SEGMENT_MASK = (1 << 15) - 1

def get_nonmoving_segment(ptr: Ptr) -> Optional[Tuple[gdb.Value, int]]:
    bd = get_bdescr(ptr)
    if bd is not None and bd['flags'] & BF_NONMOVING:
        seg_base = ptr.addr() & ~NONMOVING_SEGMENT_MASK
        block_idx = int(gdb.parse_and_eval('nonmovingGetBlockIdx(%s)' % ptr))
        seg = gdb.parse_and_eval('(struct NonmovingSegment *) %s' % seg_base).dereference()
        return (seg, block_idx)
    else:
        return None

def refs_dot(edges: List[Edge]) -> str:
    def node_name(ref: Edge) -> str:
        return str(ref.referring_field)

    def node_attrs(ref: Edge) -> Dict[str,str]:
        try:
            if ref.referring_closure is not None:
                itbl_ptr = ghc_heap.get_itbl(gdb.parse_and_eval('(StgClosure *) %d' % (ref.referring_closure.addr(),)))
                itbl = itbl_ptr.dereference()
                closure_type = closureTypeDict.get(int(itbl['type']), 'unknown')
                closure_name = find_symbol(Ptr(int(itbl_ptr)))
                if closure_name is None:
                    closure_name = 'unknown symbol'
            else:
                closure_type = 'invalid'
                closure_name = "unknown"
        except gdb.MemoryError:
            closure_type = 'invalid itbl'
            closure_name = ""

        extra = ''
        seg_blkIdx = get_nonmoving_segment(ref.referring_field)
        if seg_blkIdx is not None:
            seg, blk = seg_blkIdx
            snap = int(seg['next_free_snap'])
            mark = seg['bitmap'][blk]
            if mark != 0:
                color = 'blue'
                extra += '\nepoch=%d' % mark
            else:
                color = 'red'
                extra += '\nunmarked'
            if snap <= blk:
                # Allocated since the last time we took a snapshot
                extra += " (new)"
        else:
            color = 'black'

        # ! means that we couldn't find the start of the containing closure;
        # pointer identifies field
        label = []
        if ref.referring_closure is not None:
            label += [
                "%s (fld %d)" % (ref.referring_closure,
                (ref.referring_field.addr() - ref.referring_closure.addr()) / 8)
            ]
        else:
            label += ['! %s' % ref.referring_field]

        label += ['%s%s' % (closure_type, extra)]
        label += [closure_name]
        print(label)
        label = '\n'.join(label)
        return {'label': label,
                'fontcolor': color}

    def format_attrs(attrs: Dict[str,str]) -> str:
        return ', '.join('"%s"="%s"' % (name, val)
                         for name, val in attrs.items())

    lines  = ['digraph {']
    lines += ['  "%s" -> "%s";' %
              (e.referring_closure, e.referree_closure)
              for e in edges]
    lines += ['  "%s" [%s];' % (e.referring_closure, format_attrs(node_attrs(e)))
                for e in edges]
    lines += ['}']
    return '\n'.join(lines)

def collect(iterator: Iterator[T]) -> List[T]:
    xs = []  # type: List[T]
    try:
        for x in iterator:
            xs.append(x)
    except KeyboardInterrupt:
        pass

    return xs

class ExportClosureDepsDot(CommandWithArgs):
    """ Search for references to a closure """
    command_name = "ghc closure-deps"

    def build_parser(self, parser):
        parser.add_argument('-d', '--depth', default=5, type=int, help='Maximum search depth')
        parser.add_argument('-o', '--output', default='deps.dot',
                            metavar='FILE', type=str, help='Output dot file path')
        parser.add_argument('-n', '--no-static', action='store_true', help="Don't search static maps")
        parser.add_argument('-S', '--max-closure-size', type=int, help="The maximum distance in words to search for a closure header", default=4*4096)
        parser.add_argument('closure_ptr', type=str, help='A pointer to a closure')

    def run(self, opts, from_tty) -> None:
        closure_ptr = Ptr(gdb.parse_and_eval(opts.closure_ptr))
        edges = collect(find_refs_rec(closure_ptr, max_depth=opts.depth,
                                      include_static=not opts.no_static,
                                      max_closure_size=opts.max_closure_size))
        with open(opts.output, 'w') as f:
            f.write(refs_dot(edges))

        print('Found %d reference edges' % len(edges))
        print('Written to %s' % opts.output)

ExportClosureDepsDot()
