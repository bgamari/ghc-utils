# reload in gdb with,
#
# python import importlib; importlib.reload(ghc_gdb.ghc_linker)

import gdb

class GhcCmd(gdb.Command):
    def __init__(self):
        super(GhcCmd, self).__init__ ("ghc", gdb.COMMAND_USER, prefix=True)

GhcCmd()

from . import ghc_heap
from . import ghc_linker
from . import block
