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
