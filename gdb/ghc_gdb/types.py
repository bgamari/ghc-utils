word_size = 8
if word_size == 8:
    TAG_MASK = 7
else:
    TAG_MASK = 3

class Ptr(object):
    def __init__(self, addr: int) -> None:
        self.addr = addr

    def __gt__(self, a: "Ptr"):
        return self.addr > a.addr

    def __lt__(self, a: "Ptr"):
        return self.addr < a.addr

    def __ge__(self, a: "Ptr"):
        return self.addr >= a.addr

    def __le__(self, a: "Ptr"):
        return self.addr <= a.addr

    def __str__(self):
        return hex(self.addr)

    def __repr__(self):
        return hex(self.addr)

    def offset_bytes(self, n: int) -> "Ptr":
        return Ptr(self.addr + n)

    def untagged(self):
        return Ptr(self.addr & ~TAG_MASK)

    def tagged(self, tag: int) -> "Ptr":
        assert tag >= 0 and tag < TAG_MASK+1
        return Ptr((self.addr & ~TAG_MASK) | tag)

    def pack(self) -> bytes:
        import struct
        return struct.pack('@P', self.addr)

    @staticmethod
    def unpack(bs: bytes) -> "Ptr":
        import struct
        return Ptr(struct.unpack('@P', bs)[0])

Word = int

