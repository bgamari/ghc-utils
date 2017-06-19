import gdb
import gdb.printing
from collections import namedtuple
import traceback

bits = 64
showSpAddrs = False


# tables-next-to-code?
tntc = True

StgClosure = gdb.lookup_type('StgClosure')
StgClosurePtr = StgClosure.pointer()
StgInfoTable = gdb.lookup_type('StgInfoTable')
StgInfoTablePtr = StgInfoTable.pointer()
StgConInfoTable = gdb.lookup_type('StgConInfoTable')
StgRetInfoTable = gdb.lookup_type('StgRetInfoTable')
StgFunInfoTable = gdb.lookup_type('StgFunInfoTable')
StgClosureInfo = gdb.lookup_type('StgClosureInfo')
StgWord = gdb.lookup_type('uintptr_t')
StgPtr = gdb.lookup_type('void').pointer()

# profiled rts?
profiled = 'prof' in (f.name for f in StgInfoTable.fields())

if bits == 32:
    BITMAP_SIZE_MASK = 0x1f
    BITMAP_SIZE_SHIFT = 5
else:
    BITMAP_SIZE_MASK = 0x3f
    BITMAP_SIZE_SHIFT = 6

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

def build_closure_printers():
    C = ClosureType
    p = {}
    for ty, v in ClosureType.__dict__.items():
        if isinstance(v, int):
            # damn you python and your terrible scoping
            def get_printer(ty):
                return lambda closure: str(ty)
            p[v] = get_printer(ty)

    def print_small_bitmap(bitmap, payload, depth):
        assert payload.type == StgWord.pointer()
        s = ''
        for i, isWord in enumerate(iter_small_bitmap(bitmap)):
            w = payload + i
            if showSpAddrs:
                s += '  field %d (%x): ' % (i, w.cast(StgWord))
            else:
                s += '  field %d: ' % i
            if isWord:
                s += 'Word %d' % (w.dereference())
            else:
                ptr = w.dereference().cast(StgClosurePtr)
                s += 'Ptr  0x%-12x: %s' % (ptr, print_closure(untag(ptr), depth))
            s += '\n'
        return s

    def invalid_object(closure, depth):
        raise RuntimeError('invalid object')
    p[C.INVALID_OBJECT] = invalid_object

    def constr(closure, depth):
        con_info = get_con_itbl(closure)
        s = 'constr'
        prof_info = get_prof_info(get_itbl(closure))
        if prof_info is not None:
            s += '(closure_desc=%s, type=%s)' % prof_info
        return s
    for ty in [C.CONSTR,
               C.CONSTR_1_0, C.CONSTR_0_1,
               C.CONSTR_1_1, C.CONSTR_0_2, C.CONSTR_2_0,
               C.CONSTR_NOCAF]:
        p[ty] = constr

    def fun(closure, depth):
        s = 'FUN'
        prof_info = get_prof_info(get_itbl(closure))
        if prof_info is not None:
            s += '(closure_desc=%s, type=%s)' % prof_info
        return s
    for ty in [C.FUN, C.FUN_1_0, C.FUN_0_1, C.FUN_1_1,
               C.FUN_0_2, C.FUN_2_0, C.FUN_STATIC]:
        p[ty] = fun

    def thunk(closure, depth):
        s = 'THUNK'
        info = get_itbl(closure)
        prof_info = get_prof_info(info)
        if prof_info is not None:
            s += '(%s)' % str(prof_info)
        s += '\n'
        s += print_small_bitmap(bitmap=info['layout']['bitmap'],
                                payload=closure.cast(StgWord.pointer()),
                                depth=depth-1)
        return s
    for ty in [C.THUNK, C.THUNK_1_0, C.THUNK_0_1, C.THUNK_1_1,
               C.THUNK_0_2, C.THUNK_2_0, C.THUNK_STATIC]:
        p[ty] = thunk

    def thunk_sel(closure, depth):
        ty = gdb.lookup_type('StgSelector').pointer()
        selectee = closure.cast(ty)['selectee']
        return 'THUNK_SELECTOR(%s, %s)' % (selectee, print_closure(selectee, depth-1))
    p[C.THUNK_SELECTOR] = thunk_sel

    def application(ty, closure, depth):
        ap_ = closure.cast(ty).dereference()
        things = [str(ap_['fun'])]
        for i in range(int(ap_['n_args'])):
            things.append(str((ap_['payload'] + i).dereference()))
        return 'AP(%s)' % ', '.join(things)
    p[C.AP] = lambda closure, depth: application(gdb.lookup_type('StgAP').pointer(), closure, depth)
    p[C.PAP] = lambda closure, depth: application(gdb.lookup_type('StgPAP').pointer(), closure, depth)

    def ap_stack(closure, depth):
        ty = gdb.lookup_type('StgAP_STACK').pointer()
        ap_ = closure.cast(ty).dereference()
        return 'AP_STACK(size=%s, fun=%s, payload=%s)' % \
            (ap_['size'], print_closure(ap_['fun'], depth-1), ap_['payload'])
    p[C.AP_STACK] = ap_stack

    def update_frame(closure, depth):
        ty = gdb.lookup_type('StgUpdateFrame').pointer()
        updatee = closure.cast(ty)['updatee']
        return 'UPDATE_FRAME(%s: %s)' % (updatee, print_closure(updatee, depth-1))
    p[C.UPDATE_FRAME] = update_frame

    def indirect(closure, depth):
        ty = gdb.lookup_type('StgInd').pointer()
        ind = closure.cast(ty)['indirectee']
        return 'BLACKHOLE(%s: %s)' % (ind, print_closure(untag(ind), depth-1))
    p[C.IND] = indirect
    p[C.IND_STATIC] = indirect
    p[C.BLACKHOLE] = indirect

    def ret_small(closure, depth, showSpAddrs=True):
        c = closure.cast(StgWord.pointer().pointer()).dereference()
        info = get_itbl(closure)
        s = 'RET_SMALL'
        s += '  return = %s (%s)\n' % (c, gdb.find_pc_line(int(c.cast(StgWord))))
        s += print_small_bitmap(bitmap=info['layout']['bitmap'],
                                payload=closure.cast(StgWord.pointer()) + 1,
                                depth=depth-1)
        return s
    p[C.RET_SMALL] = ret_small

    return p

closurePrinters = build_closure_printers()
closureTypeDict = { v: str(ty) for ty, v in ClosureType.__dict__.items() }

def heap_alloced(ptr):
    w = ptr.cast(StgWord)
    aspace = gdb.parse_and_eval('mblock_address_space')
    return (w >= aspace['begin']) and (w < aspace['end'])

class InfoTablePrinter(object):
    def __init__(self, val):
        self.val = val

    def to_string(self):
        v = self.val
        s = ""
        #s += '(ptrs=%08x  nptrs=%08x)' % (v['payload']['ptrs'], v['payload']['nptrs'])
        return s

    def display_hint(self):
        return 'info table'

def get_itbl(closure):
    assert closure.type == StgClosurePtr
    info_ptr = closure.dereference()['header']['info']
    return info_ptr.cast(StgInfoTable.pointer()) - 1
    #return (gdb.parse_and_eval('get_itbl(%s)' % closure))

def get_con_itbl(closure):
    assert closure.type == StgClosurePtr
    info_ptr = closure.dereference()['header']['info']
    return info_ptr.cast(StgConInfoTable.pointer()) - 1
    #return (gdb.parse_and_eval('get_con_itbl(%s)' % closure))

def get_ret_itbl(closure):
    assert closure.type == StgClosurePtr
    info_ptr = closure.dereference()['header']['info']
    return info_ptr.cast(StgRetInfoTable.pointer()) - 1

def get_fun_itbl(closure):
    assert closure.type == StgClosurePtr
    info_ptr = closure.dereference()['header']['info']
    return info_ptr.cast(StgFunInfoTable.pointer()) - 1

def print_closure(closure, depth=1):
    if not heap_alloced(closure):
        return 'off-heap'

    try:
        assert closure.type == StgClosurePtr
        closure = untag(closure)
        info = get_itbl(closure)
        ty = int(info['type'])
        if depth > 0:
            printer = closurePrinters.get(ty)
            if printer is None:
                raise RuntimeError("Invalid closure type %d: closure=%s" % (ty, closure))
            else:
                return printer(closure, depth)
        else:
            return closureTypeDict[ty]

    except Exception as e:
        #print(traceback.format_exc(10))
        return 'Error(%s)' % e

ProfInfo = namedtuple('ProfInfo', 'closure_desc,type')

def get_prof_info(info_tbl_ptr):
    assert info_tbl_ptr.type == StgInfoTablePtr
    if not profiled:
        return None
    else:
        char_ptr_ty = gdb.lookup_type('char').pointer()
        if tntc:
            closure_desc = ((info_tbl_ptr + 1).cast(StgWord) + info_tbl_ptr.dereference()['prof']['closure_desc_off']).cast(char_ptr_ty).string()
            closure_type = ((info_tbl_ptr + 1).cast(StgWord) + info_tbl_ptr.dereference()['prof']['closure_type_off']).cast(char_ptr_ty).string()
        else:
            closure_type = info_tbl_ptr.dereference()['prof']['closure_type'].string()
            closure_desc = info_tbl_ptr.dereference()['prof']['closure_desc'].string()

        return ProfInfo(closure_desc, closure_type)

def iter_small_bitmap(bitmap):
    """ Iterate over fields of a small bitmap, yield True for fields which are
    words and False for pointers. """
    size = bitmap & BITMAP_SIZE_MASK
    bits = bitmap >> BITMAP_SIZE_SHIFT
    for i in range(size):
        isWord = bits & (1 << i) != 0
        yield isWord

class PrintInfoCmd(gdb.Command):
    def __init__(self):
        super(PrintInfoCmd, self).__init__ ("print_info", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        print(get_itbl(gdb.parse_and_eval(arg)))

class PrintGhcClosureCmd(gdb.Command):
    def __init__(self):
        super(PrintGhcClosureCmd, self).__init__ ("closure", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        closure = gdb.parse_and_eval('$rbp').cast(StgClosurePtr)
        print(print_closure(untag(closure)))

class PrintGhcStackCmd(gdb.Command):
    def __init__(self):
        super(PrintGhcStackCmd, self).__init__ ("ghc-backtrace", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        sp = gdb.parse_and_eval('$rbp').cast(StgPtr)
        maxDepth = 10
        depth=2
        if arg:
            maxDepth = int(arg)
        for i in range(maxDepth):
            print('%d: ' % i, end='')
            #info = (sp.cast(StgInfoTable.pointer().pointer()).dereference() - 1).dereference()
            #info = get_info_table(sp.cast(StgClosurePtr.pointer()).dereference())
            info = get_itbl(sp.cast(StgClosurePtr))
            ty = info['type']
            if ty in [ ClosureType.UPDATE_FRAME,
                       ClosureType.CATCH_FRAME ]:
                print(print_closure(sp.cast(StgClosurePtr), depth))

            elif ty == ClosureType.UNDERFLOW_FRAME:
                print(print_closure(sp.cast(StgClosurePtr), depth))
                break

            elif ty == ClosureType.STOP_FRAME:
                print(print_closure(sp.cast(StgClosurePtr), depth))
                break

            elif ty == ClosureType.RET_SMALL:
                print(print_closure(sp.cast(StgClosurePtr), depth))

            elif ty == ClosureType.RET_BCO:
                print('RET_BCO')
                raise NotImplemented

            elif ty == ClosureType.RET_FUN:
                print('RET_FUN')
                raise NotImplemented
            else:
                raise RuntimeError('unknown stack frame type %d' % ty)

            size = stack_frame_size(sp.cast(StgClosurePtr))
            sp = sp + StgPtr.sizeof * size

def untag(ptr):
    assert ptr.type == StgClosurePtr
    return (ptr.cast(StgWord) & ~7).cast(StgClosurePtr)

def stack_frame_size(frame):
    assert frame.type == StgClosurePtr
    #return gdb.parse_and_eval('stack_frame_sizeW(0x%x)' % sp)
    info = get_ret_itbl(frame)
    ty = info.dereference()['i']['type']
    if ty == ClosureType.RET_FUN:
        size = frame.cast(StgRetFun.pointer()).dereference()['size']
        return gdb.parse_and_eval('sizeof(StgRetFun)') + size
    elif ty == ClosureType.RET_BIG:
        raise NotImplemented
    elif ty == ClosureType.RET_BCO:
        raise NotImplemented
    else:
        bitmap = info.dereference()['i']['layout']['bitmap']
        size = bitmap & BITMAP_SIZE_MASK
        return 1 + size

def build_pretty_printer():
    pp = gdb.printing.RegexpCollectionPrettyPrinter("ghc")
    #pp.add_printer('StgInfoTable', '^StgInfoTable$', InfoTablePrinter)
    return pp

PrintGhcClosureCmd()
PrintGhcStackCmd()
PrintInfoCmd()
gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pretty_printer(), replace=True)
