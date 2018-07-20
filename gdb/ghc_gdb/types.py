word_size = 8
if word_size == 8:
    TAG_MASK = 7
else:
    TAG_MASK = 3

class Ptr(int):
    def __init__(self, addr: int):
        self.addr = addr

    def __str__(self):
        return hex(self.addr)

    def __repr__(self):
        return hex(self.addr)

    def untagged(self):
        return Ptr(self.addr & ~TAG_MASK)

    def tagged(self, tag: int) -> "Ptr":
        assert tag < TAG_MASK+1
        return Ptr(self.addr | ~tag)

    def pack(self) -> bytes:
        import struct
        return struct.pack('@P', self.addr)

    @staticmethod
    def unpack(bs: bytes) -> "Ptr":
        import struct
        return Ptr(struct.unpack('@P', bs)[0])

Word = int

