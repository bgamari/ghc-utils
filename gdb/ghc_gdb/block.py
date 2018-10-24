import gdb
from enum import IntFlag
from typing import Optional, Any
from .utils import CommandWithArgs
from .types import *

Bdescr = gdb.lookup_type('bdescr')
BdescrPtr = Bdescr.pointer()

# This is a terrible hack but it's better than searching through 1TB of address space.
# Unfortunately while gdb's `find` command seems to be smart enough to only search
# mapped memory, the same can't be said of Python's search_memory.
heap_start = Ptr(0x4200000000)
heap_end = Ptr(0x4210000000)

def block_chain_elems(bd):
    assert bd.type == BdescrPtr
    while int(bd) != 0:
        yield bd
        bd = bd['link']

def is_heap_alloced(ptr: Ptr):
    return heap_start <= ptr and ptr < heap_end
    #return gdb.parse_and_eval('HEAP_ALLOCED(%d)' % ptr.addr)

def get_bdescr(ptr: Ptr) -> Optional[Any]:
    if ptr < heap_start: return None # XXX

    if is_heap_alloced(ptr):
        # _bdescr is only provided by the debug RTS
        return gdb.parse_and_eval('_bdescr(%d)' % ptr.addr).dereference()
    else:
        return None

class BlockFlags(IntFlag):
    BF_EVACUATED  = 1
    BF_LARGE      = 2
    BF_PINNED     = 4
    BF_MARKED     = 8
    BF_EXEC       = 32
    BF_FRAGMENTED = 64
    BF_KNOWN      = 128
    BF_SWEPT      = 256
    BF_COMPACT    = 512
    BF_NONMOVING  = 1024

def format_bdescr(bd):
    assert bd.type == BdescrPtr
    gen = int(bd.dereference()['gen_no'])
    blocks = int(bd.dereference()['blocks'])
    flags = int(bd.dereference()['flags'])
    flag_desc = ','.join(f.name
                         for f in BlockFlags
                         if flags & f.value)
    return "Bdescr @ 0x%x { gen=%d, blocks=%d, flags=%s }" % \
        (int(bd), gen, blocks, flag_desc)

class PrintBlock(CommandWithArgs):
    """ Print a block descriptor """
    command_name = "ghc block"

    def build_parser(self, parser):
        parser.add_argument('closure', type=str)

    def run(self, opts, from_tty):
        bd = gdb.parse_and_eval('Bdescr(%s)' % opts.closure)
        print(format_bdescr(bd))

class PrintBlockChain(CommandWithArgs):
    """ Print a chain of block descriptors """
    command_name = "ghc block-chain"

    def build_parser(self, parser):
        parser.add_argument('bdescr', type=str)

    def run(self, opts, from_tty):
        bd = gdb.parse_and_eval(opts.bdescr)
        assert bd.type == BdescrPtr
        for b in block_chain_elems(bd):
            print(format_bdescr(b))

PrintBlock()
PrintBlockChain()
