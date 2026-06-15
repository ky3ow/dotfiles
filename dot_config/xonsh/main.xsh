import xsh.aliases as alias
import xsh.helpers as h
import re
from typing import Callable, TextIO

# https://xon.sh/tutorial.html#decorator-aliases
if bin := h.which("nvim"):
    alias.bin(
        "mini",
        [bin],
        decorators=[h.unthreadable, h.uncapturable],
        NVIM_APPNAME="minivim",
    )

if bin := h.which("eza"):
    alias.string("ls", bin)

if h.which("zoxide"):
    zoxide_init = h.subproc_captured_stdout(["zoxide", "init", "xonsh"])
    assert h.XSH.execer is not None
    h.XSH.execer.exec(zoxide_init, "exec", h.XSH.ctx, filename="zoxide")

    alias.string("cd", "z")

for exe in ["clip.exe", "xclip", "wl-copy", "pbcopy"]:
    if existing := h.which(exe):
        alias.string("@L", f"echo @(@.lastcmd.output) | {existing}")
        alias.suffix("@0", f"| {existing}")
        break

if h.which("carapace"):
    script = h.subproc_captured_stdout(["carapace", "_carapace", "xonsh"])
    assert h.XSH.execer is not None
    h.XSH.execer.exec(script, "exec", h.XSH.ctx, filename="carapace")

alias.suffix("@1", "out>/dev/null")
alias.suffix("@2", "err>/dev/null")
alias.suffix("@3", "all>/dev/null")

assert h.XSH.builtins is not None

def _print_regular(_, stdin: TextIO):
    for line in stdin:
        if line != "\n":
            print(line, end="")

alias.func("?p", _print_regular)

@h.XSH.builtins.events.on_ptk_create
def custom_keybindings(bindings, **_):
    big_word = re.compile(r'("[^"]*"|\'[^\']*\'|\S+)')

    @bindings.add("escape", "c-f")
    def _(event):
        buf = event.current_buffer
        if match := big_word.search(buf.document.text_after_cursor):
            buf.cursor_position += match.end()

    @bindings.add("escape", "c-b")
    def _(event):
        buf = event.current_buffer
        if matches := list(big_word.finditer(buf.document.text_before_cursor)):
            # Move to the start of the last match before the cursor
            buf.cursor_position = matches[-1].start()

    @bindings.add("escape", "w")
    def _(event):
        buf = event.current_buffer
        buf.text = f"$({buf.text})"
