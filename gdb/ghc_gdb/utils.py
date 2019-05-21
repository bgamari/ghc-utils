import gdb

class CommandWithArgs(gdb.Command):
    def __init__(self):
        super(CommandWithArgs, self).__init__ (self.__class__.command_name, gdb.COMMAND_USER)
        import argparse
        self._parser = argparse.ArgumentParser()
        self.build_parser(self._parser)
        self.__class__.__doc__ += '\n' + self._parser.format_help()

    def invoke(self, args, from_tty):
        try:
            opts = self._parser.parse_args(args.split())
        except:
            # we don't know what exceptions parse_args() throws so we have to
            # catch all
            # ignore the exception because  parse_args() already prints an
            # error message
            return
        self.run(opts, from_tty)

    def build_parser(self, parser):
        raise NotImplementedError()

    def run(self, opts, from_tty):
        raise NotImplementedError()

def get_num_capabilities():
    return int(gdb.parse_and_eval('n_capabilities'))

def get_num_generations():
    return int(gdb.parse_and_eval('RtsFlags.GcFlags.generations'))

