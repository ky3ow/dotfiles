"""Let your pipe lines flow thru the Python code in xonsh. """

import os
import sys
import traceback
import ast
from xonsh.tools import print_color
from xonsh.built_ins import XonshSession

def _preset_drop(line, num, args):
    """Drop empty lines."""
    if args:
        return None if line == args[0] else line
    else:
        return line if line.strip() else None

def _preset_fromlist(line, num, args):
    """Read python list representation and return the element by index."""
    lst = eval(line)
    if lst:
        if args:
            n = int(args[0])
            return lst[n] if n >= 0 and n < len(lst) else ""
        return str(lst[0])
    else:
        return ""

PIPELINE_PRESETS = {
    "drop": _preset_drop,
    "len": "len(line)",
    "pass": "line",
    "strip": "line.strip()",
    "lstrip": "line.lstrip()",
    "rstrip": "line.rstrip()",
    "split": lambda line, num, args: line.split(args[0] if args else None),
    "fromlist": _preset_fromlist,
    "lower": "line.lower()",
    "upper": "line.upper()",
    "title": "line.title()",
    "startswith": lambda line, num, args: line.startswith(args[0]),
    "endswith": lambda line, num, args: line.endswith(args[0]),
}

def _pipeline(args, stdin, stdout):
    """
    pl "line, num, args"
    Example: echo "123" | pl "line[::-1]"
    Example: echo " 123 " | pl strip
    """
    lambda_body = 'lambda line, num, args: %s'

    if len(args) == 0:
        fn = eval(lambda_body % PIPELINE_PRESETS["pass"], __xonsh__.ctx)
    elif args[0] in PIPELINE_PRESETS:
        preset = PIPELINE_PRESETS[args[0]]
        if callable(preset):
            fn = preset
        elif isinstance(preset, str):
            fn = eval(lambda_body % preset, __xonsh__.ctx)
        else:
            print_color('{YELLOW}'+f'Unsupported type: {preset!r}'+'{RESET}', file=sys.stderr)
            return
    else:
        fn = eval(lambda_body % args[0], __xonsh__.ctx)

    fn_args = args[1:]
    
    if stdin is None:
        try:
            print(fn(None, 0, fn_args))
        except:
            print_color('{YELLOW}' + str(traceback.format_exc()) + '{RESET}', file=sys.stderr)
        return

    num = 0
    for line in stdin.readlines():
        try:
            res = fn(line.rstrip(os.linesep), num, fn_args)
        except Exception as e:
            print_color('{YELLOW}' + f'Error line {num+1}: {line!r}: {e}' + '{RESET}', file=sys.stderr)
            return
        num += 1
        if res is not None:
            print(res, file=stdout, flush=True)


def _load_xontrib_(xsh: XonshSession, **_):
    xsh.aliases["%"] = _pipeline
