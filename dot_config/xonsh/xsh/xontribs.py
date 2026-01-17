from dataclasses import dataclass
from xonsh.xontribs import xontribs_load, ExitCode
from xonsh.built_ins import subproc_uncaptured
from typing import Optional

@dataclass
class Xontrib:
    module: str
    package: Optional[str] = None

    def ensure(self):
        package = self.package or f"xontrib-{self.module}"

        _, _, status = xontribs_load([self.module])

        if(status == ExitCode.NOT_FOUND):
            print(f"{self.module} not installed. Installing {package}")
            subproc_uncaptured(["xpip", "install", package])

        xontribs_load([self.module])
