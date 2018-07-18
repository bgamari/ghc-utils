from .types import *

class ClosureType(object):
    INVALID_OBJECT                = 0
    CONSTR                        = 1
    CONSTR_1_0                    = 2
    CONSTR_0_1                    = 3
    CONSTR_2_0                    = 4
    CONSTR_1_1                    = 5
    CONSTR_0_2                    = 6
    CONSTR_NOCAF                  = 7
    FUN                           = 8
    FUN_1_0                       = 9
    FUN_0_1                       = 10
    FUN_2_0                       = 11
    FUN_1_1                       = 12
    FUN_0_2                       = 13
    FUN_STATIC                    = 14
    THUNK                         = 15
    THUNK_1_0                     = 16
    THUNK_0_1                     = 17
    THUNK_2_0                     = 18
    THUNK_1_1                     = 19
    THUNK_0_2                     = 20
    THUNK_STATIC                  = 21
    THUNK_SELECTOR                = 22
    BCO                           = 23
    AP                            = 24
    PAP                           = 25
    AP_STACK                      = 26
    IND                           = 27
    IND_STATIC                    = 28
    RET_BCO                       = 29
    RET_SMALL                     = 30
    RET_BIG                       = 31
    RET_FUN                       = 32
    UPDATE_FRAME                  = 33
    CATCH_FRAME                   = 34
    UNDERFLOW_FRAME               = 35
    STOP_FRAME                    = 36
    BLOCKING_QUEUE                = 37
    BLACKHOLE                     = 38
    MVAR_CLEAN                    = 39
    MVAR_DIRTY                    = 40
    TVAR                          = 41
    ARR_WORDS                     = 42
    MUT_ARR_PTRS_CLEAN            = 43
    MUT_ARR_PTRS_DIRTY            = 44
    MUT_ARR_PTRS_FROZEN0          = 45
    MUT_ARR_PTRS_FROZEN           = 46
    MUT_VAR_CLEAN                 = 47
    MUT_VAR_DIRTY                 = 48
    WEAK                          = 49
    PRIM                          = 50
    MUT_PRIM                      = 51
    TSO                           = 52
    STACK                         = 53
    TREC_CHUNK                    = 54
    ATOMICALLY_FRAME              = 55
    CATCH_RETRY_FRAME             = 56
    CATCH_STM_FRAME               = 57
    WHITEHOLE                     = 58
    SMALL_MUT_ARR_PTRS_CLEAN      = 59
    SMALL_MUT_ARR_PTRS_DIRTY      = 60
    SMALL_MUT_ARR_PTRS_FROZEN0    = 61
    SMALL_MUT_ARR_PTRS_FROZEN     = 62
    COMPACT_NFDATA                = 63

class Closure(object):
    def __init__(self, addr: Ptr, info_ptr: Ptr, type_desc = None):
        self.addr = addr
        self.type_desc = type_desc if type_desc else "Closure"
        self.info_ptr = info_ptr

    def describe(self):
        return r'{type_desc}@{addr}'.format(**self.__dict__)

class Constr(Closure):
    def __init__(self, addr: Ptr, info_ptr: Ptr, ptrs: List[Ptr], nptrs: List[Word]):
        Closure.__init__(self, addr, info_ptr, "Constr(%s)" % constr_name)
        self.ptrs = ptrs
        self.nptrs = nptrs
