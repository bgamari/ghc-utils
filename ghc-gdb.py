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

def doc(thing):
    if isinstance(thing, str):
        return Text(thing)
    elif isinstance(thing, Doc):
        return thing
    else:
        print(thing)
        raise ArgumentError("Unknown thing")

class Doc(object):
    def render(self, indentation=0):
        return '\n'.join(self._render_lines())

    def _render_lines(self):
        raise NotImplementedError('%s' % self.__class__)

    def __str__(self):
        return self.render()

    def indented(self, indentation=2):
        return Indent(indentation, self)

class HSep(Doc):
    def __init__(self, *children):
        self.children = [doc(x) for x in children]

    def append(self, thing):
        self.children += [doc(thing)]

    def __iadd__(self, thing):
        self.append(thing)
        return self

    def _render_lines(self):
        accum = []
        last = ''
        for child in self.children:
            lines = child._render_lines()
            if len(lines) == 0:
                continue
            elif len(lines) == 1:
                last += lines[0] + ' '
            if len(lines) > 1:
                accum += [last + lines[0]]
                accum += lines[1:-1]
                last = lines[-1]

        if last != '':
            accum += [last]
        return accum

class VSep(Doc):
    def __init__(self, *children):
        self.children = [doc(x) for x in children]

    def append(self, thing):
        assert isinstance(thing, Doc)
        self.children += [doc(thing)]

    def __iadd__(self, thing):
        self.append(thing)
        return self

    def _render_lines(self):
        return [ line
                 for child in self.children
                 for line in child._render_lines()
               ]

class Indent(Doc):
    def __init__(self, indentation, child):
        self.indentation = indentation
        self.child = child

    def _render_lines(self):
        return [' '*self.indentation + x for x in self.child._render_lines()]

class Text(Doc):
    def __init__(self, text):
        self.text = text

    def _render_lines(self):
        return [self.text]

class Hang(Doc):
    def __init__(self, head, indentation=2, *children):
        self.head = doc(head)
        self.indentation = int(indentation)
        self.children = VSep(*(doc(c) for c in children))

    def _render_lines(self):
        return self.head._render_lines() + Indent(self.indentation, self.children)._render_lines()

    def append(self, thing):
        self.children += doc(thing)

    def __iadd__(self, thing):
        self.append(thing)
        return self

def build_closure_printers():
    C = ClosureType
    p = {}
    for ty, v in ClosureType.__dict__.items():
        if isinstance(v, int):
            # damn you python and your terrible scoping
            def get_printer(ty):
                return lambda closure, depth: str(ty)
            p[v] = get_printer(ty)

    def print_small_bitmap(bitmap, payload, depth):
        assert payload.type == StgWord.pointer()
        doc = VSep()
        for i, isWord in enumerate(iter_small_bitmap(bitmap)):
            w = payload + i
            if showSpAddrs:
                s = 'field %d (%x): ' % (i, w.cast(StgWord))
            else:
                s = 'field %d: ' % i

            if isWord:
                s += 'Word %d' % (w.dereference())
            else:
                ptr = w.dereference().cast(StgClosurePtr)
                s += 'Ptr  0x%-12x: %s' % (ptr, print_closure(untag(ptr), depth))

            doc += Text(s)

        return doc

    def invalid_object(closure, depth):
        raise RuntimeError('invalid object')
    p[C.INVALID_OBJECT] = invalid_object

    def constr(closure, depth):
        con_info = get_con_itbl(closure)
        s = 'constr'
        prof_info = get_prof_info(get_itbl(closure))
        if prof_info is not None:
            s += '(closure_desc=%s, type=%s)' % prof_info
        return Text(s)
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
        return Text(s)
    for ty in [C.FUN, C.FUN_1_0, C.FUN_0_1, C.FUN_1_1,
               C.FUN_0_2, C.FUN_2_0, C.FUN_STATIC]:
        p[ty] = fun

    def thunk(closure, depth):
        info = get_itbl(closure)
        s = 'THUNK (%s)' % print_addr(closure.dereference().cast(StgPtr))
        prof_info = get_prof_info(info)
        if prof_info is not None:
            s += '(%s)' % str(prof_info)

        hang = Hang(s)
        ptrs = int(info['layout']['payload']['ptrs'])
        for i in range(ptrs):
            hang += HSep('Ptr:', print_closure(closure.dereference()['payload'][i], depth=depth-1))
        for i in range(int(info['layout']['payload']['nptrs'])):
            hang += Text('Word: %s' % closure.dereference()['payload'][ptrs + i])
        return hang
    for ty in [C.THUNK, C.THUNK_1_0, C.THUNK_0_1, C.THUNK_1_1,
               C.THUNK_0_2, C.THUNK_2_0, C.THUNK_STATIC]:
        p[ty] = thunk

    def thunk_sel(closure, depth):
        ty = gdb.lookup_type('StgSelector').pointer()
        selectee = closure.cast(ty)['selectee']
        return Text('THUNK_SELECTOR(%s, %s)' % (selectee, print_closure(selectee, depth-1)))
    p[C.THUNK_SELECTOR] = thunk_sel

    def application(ty, closure, depth):
        ap_ = closure.cast(ty).dereference()
        things = [str(ap_['fun'])]
        for i in range(int(ap_['n_args'])):
            things.append(str((ap_['payload'] + i).dereference()))
        return Text('AP(%s)' % ', '.join(things))
    p[C.AP] = lambda closure, depth: application(gdb.lookup_type('StgAP').pointer(), closure, depth)
    p[C.PAP] = lambda closure, depth: application(gdb.lookup_type('StgPAP').pointer(), closure, depth)

    def ap_stack(closure, depth):
        ty = gdb.lookup_type('StgAP_STACK').pointer()
        ap_ = closure.cast(ty).dereference()
        return Text('AP_STACK(size=%s, fun=%s, payload=%s)' % \
            (ap_['size'], print_closure(ap_['fun'], depth-1), ap_['payload']))
    p[C.AP_STACK] = ap_stack

    def update_frame(closure, depth):
        ty = gdb.lookup_type('StgUpdateFrame').pointer()
        updatee = closure.cast(ty)['updatee']
        return Text('UPDATE_FRAME(%s: %s)' % (updatee, print_closure(updatee, depth-1)))
    p[C.UPDATE_FRAME] = update_frame

    def indirect(closure, depth):
        ty = gdb.lookup_type('StgInd').pointer()
        ind = closure.cast(ty)['indirectee']
        return Text('BLACKHOLE(%s: %s)' % (ind, print_closure(untag(ind), depth-1)))
    p[C.IND] = indirect
    p[C.IND_STATIC] = indirect
    p[C.BLACKHOLE] = indirect

    def ret_small(closure, depth, showSpAddrs=True):
        c = closure.cast(StgWord.pointer().pointer()).dereference()
        info = get_itbl(closure)
        s = Hang('RET_SMALL')
        s += Text('return=%s' % print_addr(c))
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

def print_addr(ptr):
    ptr = ptr.cast(StgPtr)
    sym = gdb.find_pc_line(int(ptr.cast(StgWord)))
    if sym.symtab:
        return Text('%s (%s:%d)' % (ptr, sym.symtab.filename, sym.line))
    else:
        return Text('%s' % (ptr))

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
    assert closure.type == StgClosurePtr
    if not heap_alloced(closure):
        return Text('off-heap(%s)' % print_addr(closure))

    try:
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
        return 'Error(%d: %s)' % (closure, e)

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

    def invoke(self, args, from_tty):
        import argparse
        parser = argparse.ArgumentParser()
        parser.add_argument('-d', '--depth', type=int, default=2)
        parser.add_argument('closure')
        opts = parser.parse_args(args.split())

        closure = gdb.parse_and_eval(opts.closure).cast(StgClosurePtr)
        print(print_closure(untag(closure), depth=opts.depth))

class PrintGhcStackCmd(gdb.Command):
    def __init__(self):
        super(PrintGhcStackCmd, self).__init__ ("ghc-backtrace", gdb.COMMAND_USER)

    def invoke(self, args, from_tty):
        import argparse
        parser = argparse.ArgumentParser()
        parser.add_argument('-d', '--depth', type=int, default=1)
        parser.add_argument('-n', '--frames', type=int, default=10)
        opts = parser.parse_args(args.split())

        sp = gdb.parse_and_eval('$rbp').cast(StgPtr)
        print(print_stack(sp, depth=opts.depth, max_frames=opts.frames))

def print_stack(sp, max_frames, depth=1):
    assert sp.type == StgPtr
    doc = VSep()
    for i in range(max_frames):
        d = HSep()
        d += Text('%d:' % i)
        stop = False
        #info = (sp.cast(StgInfoTable.pointer().pointer()).dereference() - 1).dereference()
        #info = get_info_table(sp.cast(StgClosurePtr.pointer()).dereference())
        info = get_itbl(sp.cast(StgClosurePtr))
        ty = info['type']
        if ty in [ ClosureType.UPDATE_FRAME,
                    ClosureType.CATCH_FRAME ]:
            frame = print_closure(sp.cast(StgClosurePtr), depth)

        elif ty == ClosureType.UNDERFLOW_FRAME:
            frame = print_closure(sp.cast(StgClosurePtr), depth)
            stop = True

        elif ty == ClosureType.STOP_FRAME:
            frame = print_closure(sp.cast(StgClosurePtr), depth)
            stop = True

        elif ty == ClosureType.RET_SMALL:
            frame = print_closure(sp.cast(StgClosurePtr), depth)

        elif ty == ClosureType.RET_BCO:
            frame = Text('RET_BCO')
            raise NotImplementedError()

        elif ty == ClosureType.RET_FUN:
            frame = Text('RET_FUN')
            raise NotImplementedError()
        else:
            raise RuntimeError('unknown stack frame type %d' % ty)

        d += frame
        doc += d
        if stop:
            break
        size = stack_frame_size(sp.cast(StgClosurePtr))
        sp = sp + StgPtr.sizeof * size

    return doc

def all_threads():
    gens = int(gdb.parse_and_eval('RtsFlags.GcFlags.generations'))
    end_tso_queue = int(gdb.parse_and_eval('&stg_END_TSO_QUEUE_closure'))
    for i in range(gens):
        t_ptr = gdb.parse_and_eval('generations[%d].threads' % i)
        while t_ptr != end_tso_queue:
            yield t_ptr
            t_ptr = t_ptr.dereference()['_link']

class PrintGhcThreadsCmd(gdb.Command):
    def __init__(self):
        super(PrintGhcThreadsCmd, self).__init__ ("ghc-threads", gdb.COMMAND_USER)

    def invoke(self, args, from_tty):
        import argparse
        parser = argparse.ArgumentParser()
        parser.add_argument('-n', '--frames', type=int, default=5)
        opts = parser.parse_args(args.split())

        blocked_reasons = {
            0: lambda tso: 'nothing',
            1: lambda tso: 'MVar@%s' % tso['block_info']['closure'],
            2: lambda tso: 'blackhole@%s' % tso['block_info']['bh'],
        }
        for tso_ptr in all_threads():
            tso = tso_ptr.dereference()
            why_blocked = blocked_reasons.get(int(tso['why_blocked']), lambda tso: 'unknown')(tso)
            print('id=%d\tTSO=0x%08x\tblocked on %s' % (tso['id'], int(tso_ptr), why_blocked))
            sp = tso['stackobj'].dereference()['sp'].cast(StgPtr)
            print(print_stack(sp, max_frames=opts.frames, depth=1).indented())
            print()

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
PrintGhcThreadsCmd()
gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pretty_printer(), replace=True)
