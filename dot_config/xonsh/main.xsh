from __future__ import annotations
from textwrap import dedent
from typing import TYPE_CHECKING

import xsh.aliases as alias
import xsh.bigwords as bw
import xsh.helpers as h

from xonsh.tools import unthreadable, uncapturable

if TYPE_CHECKING:
    import prompt_toolkit.key_binding as ptk_kb
    Ev = ptk_kb.KeyPressEvent

# https://xon.sh/tutorial.html#decorator-aliases
if cmd := alias.command("nvim"):
    alias.bin(
        "mini",
        [cmd.name],
        decorators=[unthreadable, uncapturable],
        NVIM_APPNAME="minivim",
    )

if cmd := alias.command("eza"):
    cmd.as_("ls")

if cmd := alias.command("zoxide"):
    cmd.init_script(cmd.name, "init", "xonsh")
    cmd.with_name("z").as_("cd")

for exe in ["clip.exe", "xclip", "wl-copy", "pbcopy"]:
    if cmd := alias.command(exe):
        alias.first("@L", f"echo @(@.lastcmd.output) | {cmd.name}")
        alias.any("@0", f"| {cmd.name}")
        break

if cmd := alias.command("carapace"):
    cmd.init_script(cmd.name, "_carapace", "xonsh")

if cmd := alias.command("git"):
    cmd.with_("]s", "--staged")

    cmd.sub("status").as_("gs")
    cmd.sub("diff").as_("gd")

    cmd.sub("add").as_("ga")
    cmd.sub("commit").as_("gc")

    cmd.sub("push").as_("gp").with_("]f", "--force-with-lease")
    cmd.sub("pull").as_("gP")

    cmd.sub("switch").as_("gco").with_("]d", "--detached")

if cmd := alias.command("chezmoi"):
    cmd.sub("add").as_("chad")
    cmd.sub("re-add").as_("ched")
    cmd.sub("git pull").as_("cheg")
    cmd.sub("apply").as_("chap")
    alias.first("ch", "cd ~/.local/share/chezmoi")

alias.any("@1", "out>/dev/null")
alias.any("@2", "err>/dev/null")
alias.any("@3", "all>/dev/null")

assert h.XSH.builtins is not None

@h.XSH.builtins.events.on_ptk_create
def custom_keybindings(bindings: ptk_kb.KeyBindings, **_):
    @bindings.add("escape", "c-f")
    def _(event: Ev):
        buf = event.current_buffer
        buf.cursor_position = bw.next_big_word_end(buf.document.text, buf.cursor_position)

    @bindings.add("escape", "c-b")
    def _(event: Ev):
        buf = event.current_buffer
        buf.cursor_position = bw.prev_big_word_start(buf.document.text, buf.cursor_position)

    @bindings.add("escape", "w")
    def _(event: Ev):
        buf = event.current_buffer
        buf.text = f"lastcmd = !({buf.text}); echo @(lastcmd.errors or lastcmd.output)"
        buf.cursor_position = len(buf.text)

    @bindings.add("c-a")
    def _(event: Ev):
        buf = event.current_buffer
        doc = buf.document
        line_start = doc.translate_row_col_to_index(doc.cursor_position_row, 0)
        if doc.cursor_position == line_start:
            buf.cursor_position = 0
        else:
            buf.cursor_position = line_start

    @bindings.add("c-e")
    def _(event: Ev):
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
    def _(event: Ev):
        buf = event.current_buffer
        if buf.cursor_position > 0:
            buf.cursor_position -= 1

    @bindings.add("c-f")
    def _(event: Ev):
        buf = event.current_buffer
        if buf.cursor_position < len(buf.document.text):
            buf.cursor_position += 1

    @bindings.add("escape", "k")
    def _(event: Ev):
        buf = event.current_buffer
        text_after = buf.document.text_after_cursor
        if text_after:
            event.app.clipboard.set_text(text_after)
            buf.text = buf.document.text[:buf.cursor_position]

    @bindings.add("c-v")
    def _(event: Ev):
        """Paste with textwrap.dedent applied."""
        buf = event.current_buffer
        data = event.app.clipboard.get_data()
        buf.insert_text(dedent(data.text))

    @bindings.add("escape", "]")
    def _(event: Ev):
        """Indent all lines except first by 2 spaces."""
        buf = event.current_buffer
        doc = buf.document
        if doc.text.count("\n") < 1:
            return

        first = doc.text.find("\n")
        head = doc.text[:first]
        tail = doc.text[first:].replace("\n", "\n  ")

        buf.text = head + tail
        if doc.cursor_position_row != 0:
            buf.cursor_position = buf.cursor_position + 2 * doc.cursor_position_row

    @bindings.add("escape", "[")
    def _(event: Ev):
        """Dedent all lines except first by 2 spaces."""
        buf = event.current_buffer
        doc = buf.document
        if doc.text.count("\n") < 1:
            return

        first = doc.text.find("\n")
        head = doc.text[:first]
        tail = doc.text[first:].replace("\n  ", "\n")

        buf.text = head + tail
        if doc.cursor_position_row != 0 and doc.cursor_position_col >= 2:
            buf.cursor_position = buf.cursor_position - 2 * doc.cursor_position_row

