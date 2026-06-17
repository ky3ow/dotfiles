import xsh.aliases as alias
import xsh.bigwords as bw
import xsh.helpers as h
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

@h.XSH.builtins.events.on_ptk_create
def custom_keybindings(bindings, **_):
    @bindings.add("escape", "c-f")
    def _(event):
        buf = event.current_buffer
        buf.cursor_position = bw.next_big_word_end(buf.document.text, buf.cursor_position)

    @bindings.add("escape", "c-b")
    def _(event):
        buf = event.current_buffer
        buf.cursor_position = bw.prev_big_word_start(buf.document.text, buf.cursor_position)

    @bindings.add("escape", "w")
    def _(event):
        buf = event.current_buffer
        buf.text = f"lastcmd = !({buf.text}); echo @(lastcmd.errors or lastcmd.output)"
        buf.cursor_position = len(buf.text)

    @bindings.add("c-a")
    def _(event):
        buf = event.current_buffer
        doc = buf.document
        line_start = doc.translate_row_col_to_index(doc.cursor_position_row, 0)
        if doc.cursor_position == line_start:
            buf.cursor_position = 0
        else:
            buf.cursor_position = line_start

    @bindings.add("c-e")
    def _(event):
        buf = event.current_buffer
        doc = buf.document
        suggestion = buf.suggestion
        if suggestion and suggestion.text and doc.is_cursor_at_the_end:
            buf.insert_text(suggestion.text)
            return
        line_end = doc.translate_row_col_to_index(
            doc.cursor_position_row, len(doc.current_line)
        )
        if doc.cursor_position == line_end:
            buf.cursor_position = len(doc.text)
        else:
            buf.cursor_position = line_end

    @bindings.add("c-b")
    def _(event):
        buf = event.current_buffer
        if buf.cursor_position > 0:
            buf.cursor_position -= 1

    @bindings.add("c-f")
    def _(event):
        buf = event.current_buffer
        if buf.cursor_position < len(buf.document.text):
            buf.cursor_position += 1

    @bindings.add("escape", "k")
    def _(event):
        buf = event.current_buffer
        text_after = buf.document.text_after_cursor
        if text_after:
            event.app.clipboard.set_text(text_after)
            buf.text = buf.document.text[:buf.cursor_position]
