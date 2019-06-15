word_size = 8
if word_size == 8:
    TAG_MASK = 7
else:
    TAG_MASK = 3

class Ptr(object):
    __slots__ = ("_addr",)

    def __init__(self, addr: int) -> None:
        self._addr = int(addr)

    def __eq__(self, a: object) -> bool:
        if isinstance(a, Ptr):
            return self._addr == a._addr
        else:
            return NotImplemented

    def __gt__(self, a: "Ptr") -> bool:
        return self._addr > a._addr

    def __lt__(self, a: "Ptr") -> bool:
        return self._addr < a._addr

    def __ge__(self, a: "Ptr") -> bool:
        return self._addr >= a._addr

    def __le__(self, a: "Ptr") -> bool:
        return self._addr <= a._addr

    def __str__(self) -> str:
        return hex(self._addr)

    def __repr__(self) -> str:
        return hex(self._addr)

    def __hash__(self) -> int:
        return self._addr

    def offset_bytes(self, n: int) -> "Ptr":
        return Ptr(self._addr + n)

    def untagged(self) -> "Ptr":
        return Ptr(self._addr & ~TAG_MASK)

    def tagged(self, tag: int) -> "Ptr":
        assert tag >= 0 and tag < TAG_MASK+1
        return Ptr((self._addr & ~TAG_MASK) | tag)

    def pack(self) -> bytes:
        import struct
        return struct.pack('@P', self._addr)

    def addr(self) -> int:
        return self._addr

    @staticmethod
    def unpack(bs: bytes) -> "Ptr":
        import struct
        return Ptr(struct.unpack('@P', bs)[0])

Word = int

