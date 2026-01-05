from xonsh.built_ins import XSH, subproc_captured_stdout
from xonsh.tools import unthreadable, uncapturable
import xonsh.procs.executables as exes
import xonsh.api.subprocess
import xonsh.api.os

from functools import reduce
from typing import Callable, Any
import subprocess

def binalias(name: str, cmd: list[str], decorators: list[Callable]=[], **overrides):
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

def stralias(name: str, cmdline: str):
    assert XSH.aliases is not None
    XSH.aliases[name] = cmdline

def find_exe(exe: str) -> str | None:
    return exes.locate_executable(exe)
