word_size = 8
if word_size == 8:
    TAG_MASK = 7
else:
    TAG_MASK = 3

class Ptr(int):
    def __str__(self):
        return hex(self)

    def __repr__(self):
        return hex(self)

    def untagged(self):
        return self & ~TAG_MASK

    def tagged(self, tag: int) -> "Ptr":
        assert tag < 8
        return self | ~Ptr(tag)

    def pack(self) -> bytes:
        import struct
        return struct.pack('@P', ptr)

    @staticmethod
    def unpack(bs: bytes) -> "Ptr":
        import struct
        return Ptr(struct.unpack('@P', bs)[0])

Word = int

