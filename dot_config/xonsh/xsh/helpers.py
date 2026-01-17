from xonsh.built_ins import XSH as session, XonshPathLiteral, subproc_captured_stdout
from xonsh.aliases import source_alias_fn
from xonsh.tools import unthreadable, uncapturable
import xonsh.procs.executables as exes

def which(exe: str) -> str | None:
    full = exes.locate_executable(exe)
    return str(XonshPathLiteral(full).name)


def source(*paths: list[XonshPathLiteral]):
    source_alias_fn([str(path.expanduser()) for path in paths])
