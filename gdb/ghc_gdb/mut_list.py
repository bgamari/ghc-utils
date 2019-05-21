from typing import List, Optional, Tuple, TypeVar, Callable, NamedTuple, Set, Dict, Iterator, Any
from .types import Ptr
import gdb

def collect_mut_list() -> Iterator[Ptr]:
    ncaps = int(gdb.parse_and_eval('n_capabilities'))
    ngens = int(gdb.parse_and_eval('RtsFlags.GcFlags.generations'))
    for c in range(ncaps):
        for g in range(ngens):
            bd = gdb.parse_and_eval('capabilities[%d].mut_lists[%d]' % (c, g))
            while Ptr(bd) != Ptr(0):
                p = bd.dereference()['start']
                end = bd.dereference()['free']
                while p < end:
                    yield Ptr(p.dereference()).untagged()
                    p += 1

                bd = bd['link']
