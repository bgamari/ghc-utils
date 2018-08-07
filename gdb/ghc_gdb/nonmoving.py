import gdb
from .types import Ptr
from .utils import CommandWithArgs
from .block import BlockFlags

class PrintNonmovingClosure(CommandWithArgs):
    """ Print the segment descriptor of an object in the nonmoving heap """
    command_name = "ghc nonmoving-closure"

    def build_parser(self, parser):
        parser.add_argument('closure_ptr', type=str, help='A pointer to a closure')

    def run(self, opts, from_tty):
        closure_ptr = Ptr(gdb.parse_and_eval(opts.closure_ptr))
        bd = gdb.parse_and_eval('*Bdescr(%s)' % closure_ptr)
        flags = int(bd['flags'])
        if not flags & BlockFlags.BF_NONMOVING:
            print("Not in the nonmoving heap")
        elif flags & BlockFlags.BF_LARGE:
            print("Large object in the nonmoving heap")
        else:
            segment = gdb.parse_and_eval('nonmoving_get_segment(%s)' % closure_ptr)
            block = gdb.parse_and_eval('nonmoving_get_block_idx(%s)' % closure_ptr)
            block_start = gdb.parse_and_eval('nonmoving_segment_get_block(%s, %d)' %
                                             (segment, block))
            print('Closure %s: segment 0x%x, block %d @ 0x%x' % (closure_ptr, int(segment), int(block), block_start))
            print(segment.dereference())
            mark_bit = segment.dereference()['bitmap'][block]
            print('Mark bit @ 0x%x: %d' % (int(mark_bit.address), mark_bit))

PrintNonmovingClosure()
