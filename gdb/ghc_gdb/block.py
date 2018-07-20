import gdb
from enum import Enum
from .utils import CommandWithArgs

Bdescr = gdb.lookup_type('bdescr')
BdescrPtr = Bdescr.pointer()

def block_chain_elems(bd):
    assert bd.type == BdescrPtr
    while int(bd) != 0:
        yield bd
        bd = bd['link']

class BlockFlags(Enum):
    BF_EVACUATED  = 1
    BF_LARGE      = 2
    BF_PINNED     = 4
    BF_MARKED     = 8
    BF_EXEC       = 32
    BF_FRAGMENTED =  64
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

PrintBlockChain()
