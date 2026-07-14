import xonsh

TESTED_VER = xonsh.__version__ == "0.24.0"

def patch(run=True):
    def decorator(func):
        if run:
            func()

    return decorator

@patch(TESTED_VER)
def _fix_async_multiprompt():
    """Async prompt handle multipromptfield ?"""
    from xonsh.shells.ptk_shell.updator import Executor
    from xonsh.style_tools import style_as_faded

    def _patched_run_func(self, func, field):
        result = func()
        # patched this if for git thing
        if hasattr(result, "value"):
            result = result.value
        self.thread_results[field] = (
            result if result is None else style_as_faded(result)
        )
        return result

    _orig_run_func = Executor._run_func
    Executor._run_func = _patched_run_func


@patch(TESTED_VER)
def _add_force_sync_fields():
    """Some prompt fields like CWD better to be"""
    from xonsh.shells.ptk_shell.formatter import PTKPromptFormatter
    import functools

    def _patched_get_field(self, field, async_prompt=None, idx=None, spec=None, conv=None, **_):
        func = functools.partial(super(PTKPromptFormatter, self)._get_field_value, field)
        if (
            async_prompt is not None
            and self.fields.needs_calling(field)
            and not getattr(self.fields.get(field), "sync_only", False)
           ):
            return async_prompt.submit_section(func, field, idx, spec, conv)
        return func()

    _orig_get_field = PTKPromptFormatter._get_field_value
    PTKPromptFormatter._get_field_value = _patched_get_field

@patch(TESTED_VER)
def _fix_kb():
    from prompt_toolkit.input.ansi_escape_sequences import ANSI_SEQUENCES
    from prompt_toolkit.keys import Keys
    ANSI_SEQUENCES["\x1b[27;2;32~"] = " " # ty: ignore[invalid-assignment]
    ANSI_SEQUENCES["\x1b[27;2;9~"] = Keys.BackTab
    ANSI_SEQUENCES["\x1b[27;7;13~"] = Keys.ControlJ

@patch(TESTED_VER)
def _fix_colors_in_async():
    """When new sections arrive from async thread - can lack colors if they 
    weren't present in prompt."""
    import threading

    from prompt_toolkit.formatted_text import PygmentsTokens
    from xonsh.built_ins import XSH
    from xonsh.shells.ptk_shell import tokenize_ansi
    from xonsh.shells.ptk_shell.updator import AsyncPrompt
    from xonsh.style_tools import partial_color_tokenize


    def _patched_invalidate(self):
      # NOTE: self.get_lazy_ptk_kwargs <- search this
      if self.timer:
          self.timer.cancel()

      def _invalidate():
          # This discovers any colors emitted only by completed async fields.
          new_prompt = self.tokens.process()
          formatted_tokens = tokenize_ansi(
              PygmentsTokens(partial_color_tokenize(new_prompt))
          )

          app = self.session.app

          def _update_in_main():
              # PATCHED HERE
              self.session.style = XSH.shell.shell.get_prompt_style()
              setattr(self.session, self.name, formatted_tokens)
              app.invalidate()

          if app.loop is not None:
              app.loop.call_soon_threadsafe(_update_in_main)
          else:
              _update_in_main()


      self.timer = threading.Timer(XSH.env["ASYNC_INVALIDATE_INTERVAL"], _invalidate)
      self.timer.start()

    _orig_invalidate = AsyncPrompt.invalidate
    AsyncPrompt.invalidate = _patched_invalidate
