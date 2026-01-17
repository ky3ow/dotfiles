import xsh.aliases as alias
import xsh.helpers as h
import re

assert h.XSH.env is not None

h.XSH.env["XONSH_COPY_ON_DELETE"] = True

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


assert h.XSH.builtins is not None

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
