import gdb
from .types import Ptr
from .utils import CommandWithArgs

class PrintNonmovingClosure(CommandWithArgs):
    """ Print the segment descriptor of an object in the nonmoving heap """
    command_name = "ghc nonmoving-closure"

    def build_parser(self, parser):
        parser.add_argument('closure_ptr', type=str, help='A pointer to a closure')

    def run(self, opts, from_tty):
        closure_ptr = Ptr(gdb.parse_and_eval(opts.closure_ptr))
        segment = gdb.parse_and_eval('nonmoving_get_segment(%s)' % closure_ptr)
        if int(segment) == 0:
            print('Not in nonmoving heap')
            return
        else:
            block = gdb.parse_and_eval('nonmoving_get_block_idx(%s)' % closure_ptr)
            print('Closure %s: segment 0x%x, block %d' % (closure_ptr, int(segment), int(block)))
            print(segment.dereference())
            mark_bit = segment.dereference()['bitmap'][block]
            print('Mark bit @ 0x%x: %d' % (int(mark_bit.address), mark_bit))

PrintNonmovingClosure()
