from prompt_toolkit.buffer import Buffer
from xonsh.built_ins import XSH
from typing import Callable
from functools import reduce

import subprocess


def bin(name: str, cmd: list[str], decorators: list[Callable] = [], **overrides):
    assert XSH.aliases is not None

    def _impl(args, stdin=None, stdout=None, stderr=None):
        assert XSH.env is not None
        with XSH.env.swap(**overrides):
            subprocess.run(
                [*cmd, *args],
                env=XSH.env.detype(),
                stdin=stdin,
                stdout=stdout,
                stderr=stderr,
            )

    XSH.aliases[name] = reduce(lambda f, g: g(f), reversed(decorators), _impl)


def string(name: str, cmdline: str):
    """Simple string abbreviations, uses xontrib-abbrevs"""
    assert XSH.builtins is not None
    XSH.builtins.abbrevs[name] = match_beginning(cmdline)

def suffix(name: str, cmdline: str):
    """Suffix string abbreviations, uses xontrib-abbrevs"""
    assert XSH.builtins is not None
    XSH.builtins.abbrevs[name] = cmdline

def match_beginning(expansion: str):
    def _impl(buffer: Buffer, word: str):
        return (
            expansion
            if buffer.text.startswith(word) and buffer.cursor_position == len(word)
            else word
        )

    return _impl
