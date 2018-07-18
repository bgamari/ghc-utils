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

Word = int
