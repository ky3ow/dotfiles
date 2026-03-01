from xonsh.built_ins import XSH, XonshPathLiteral, subproc_captured_stdout
from xonsh.aliases import source_alias_fn
from xonsh.tools import unthreadable, uncapturable
import xonsh.procs.executables as exes

from dataclasses import dataclass
from xonsh.xontribs import xontribs_load, ExitCode
from xonsh.built_ins import subproc_uncaptured
from typing import Optional


def which(exe: str) -> str | None:
    if located := exes.locate_executable(exe):
        return str(XonshPathLiteral(located).name)

def source(*paths: XonshPathLiteral):
    source_alias_fn([str(path.expanduser()) for path in paths])


@dataclass
class Xontrib:
    module: str
    package: Optional[str] = None

    def ensure(self):
        package = self.package or f"xontrib-{self.module}"
        _, _, status = xontribs_load([self.module])
        if status == ExitCode.NOT_FOUND:
            print(f"{self.module} not installed. Installing {package}")
            subproc_uncaptured(["xpip", "install", package])
        xontribs_load([self.module])
