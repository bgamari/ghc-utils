from typing import List, Optional, Tuple, TypeVar, Callable, NamedTuple, Set, Dict, Iterator, Any
from .types import Ptr
import gdb

def collect_mut_list() -> Iterator[Ptr]:
    ncaps = int(gdb.parse_and_eval('n_capabilities'))
    ngens = int(gdb.parse_and_eval('RtsFlags.GcFlags.n_generations'))
    for c in range(ncaps):
        for g in range(ngens):
            bd = gdb.parse_and_eval('capabilities[%d].mut_lists[%d]', ngens)
            while Ptr(bd) != Ptr(0):
                p = bd['start']
                end = bd['scan']
                while p < end:
                    yield Ptr(p.deref()).untagged()
