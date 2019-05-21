from typing import List, Optional, Tuple, TypeVar, Callable, NamedTuple, Set, Dict, Iterator, Any
from .types import Ptr
from .utils import get_num_capabilities, get_num_generations
import gdb

def collect_mut_list(gen: int) -> Iterator[Ptr]:
    assert gen < get_num_generations()
    ncaps = get_num_capabilities()
    for c in range(ncaps):
        bd = gdb.parse_and_eval('capabilities[%d].mut_lists[%d]' % (c, gen))
        while Ptr(bd) != Ptr(0):
            p = bd.dereference()['start']
            end = bd.dereference()['free']
            while p < end:
                yield Ptr(p.dereference()).untagged()
                p += 1

            bd = bd['link']
