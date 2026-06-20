from prompt_toolkit.buffer import Buffer
from xonsh.built_ins import XSH
from typing import Callable, Iterable
from functools import reduce

import subprocess


def bin(name: str, cmd: list[str], decorators: list[Callable] = [], **overrides):
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


def first(name: str, cmdline: str):
    """Simple string abbreviations, uses xontrib-abbrevs"""
    assert XSH.builtins is not None
    XSH.builtins.abbrevs[name] = _match_cmd_beginning(cmdline)


def any(name: str, cmdline: str):
    """Global string abbreviations, uses xontrib-abbrevs"""
    assert XSH.builtins is not None
    XSH.builtins.abbrevs[name] = cmdline


def prefix(name: str, cmdline: str, prefix: str):
    """Expand alias if its prefixed with `prefix`, uses xontrib-abbrevs"""
    assert XSH.builtins is not None
    XSH.builtins.abbrevs[name] = _match_prefix(cmdline, prefix)


def _match_cmd_beginning(expansion: str):
    def _impl(buffer: Buffer, word: str):
        expansions = ("$(", "$[", "!(", "![")
        return expansion if _at_expr_beginning(buffer, word, expansions) else word

    return _impl


def _match_prefix(expansion: str, prefix: str):
    def _impl(buffer: Buffer, word: str):
        return expansion if buffer.text.startswith(prefix) else word

    return _impl


def _at_expr_beginning(
    buffer: Buffer, word: str, expression_markers: Iterable[str]
) -> bool:
    start = buffer.cursor_position - len(word)
    if start == 0:
        return True

    if start >= 3 and buffer.text[start - 3 : start - 1] in expression_markers:
        return True
    return False


# TODO delete
def _match_beginning(expansion: str):
    def _impl(buffer: Buffer, word: str):
        return (
            expansion
            if buffer.text.startswith(word) and buffer.cursor_position == len(word)
            else word
        )

    return _impl


function = XSH.aliases.register
command = XSH.aliases.return_command
