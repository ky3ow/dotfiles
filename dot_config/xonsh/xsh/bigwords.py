"""
bash-like shell words (ctrl+alt+f/b), but extended to substitutions
jumps over non-whitespace characters, strings, substitutions

Examples (|cursor| is placeholder, not actual pipe character):

|cursor|a/bc def -> a/bc|cursor| def
|cursor|"a bc" def -> "a bc"|cursor| def
|cursor|$(a b c) def -> $(a b c)|cursor| def
|cursor|$(a $( b ) c) -> $(a $( b ) c )|cursor|
$(a |cursor|$( b ) c) -> $(a $( b )|cursor| c )
"""

_SUBSTITUTIONS = (
    ("@!(", ")"),
    ("$(", ")"),
    ("!(", ")"),
    ("@(", ")"),
    ("$[", "]"),
    ("![", "]"),
)


def _find_expansion_end(text: str, pos: int, prefix: str, close: str) -> int:
    """Return position after the closing delimiter of an expansion starting at pos."""
    open_char = prefix[-1]
    i = pos + len(prefix)
    n = len(text)
    depth = 1
    while i < n and depth:
        if text[i] == open_char:
            depth += 1
        elif text[i] == close:
            depth -= 1
        i += 1
    return i


def _tokenize_range(text: str, begin: int, limit: int, tokens: list):
    """Tokenize text[begin:limit], appending (start, end) pairs to tokens.
    Nested expansions are tokenized recursively — inner tokens come before outer."""
    i = begin
    while i < limit:
        if text[i].isspace():
            i += 1
            continue
        start = i
        for prefix, close in _SUBSTITUTIONS:
            if text.startswith(prefix, i):
                end = _find_expansion_end(text, i, prefix, close)
                # Recursively tokenize contents (between opening and closing delimiters)
                _tokenize_range(text, i + len(prefix), end - 1, tokens)
                tokens.append((start, end))
                i = end
                break
        else:
            if text[i] in ('"', "'"):
                quote = text[i]
                i += 1
                while i < limit and text[i] != quote:
                    i += 1
                if i < limit:
                    i += 1
            else:
                while i < limit and not text[i].isspace():
                    i += 1
            tokens.append((start, i))


def _tokenize_big_words(text: str) -> list[tuple[int, int]]:
    tokens: list[tuple[int, int]] = []
    _tokenize_range(text, 0, len(text), tokens)
    return tokens


def next_big_word_end(text: str, pos: int) -> int:
    tokens = _tokenize_big_words(text)
    
    # Find the tightest token containing the cursor (start <= pos < end)
    best_token = None
    for start, end in tokens:
        if start <= pos < end:
            if best_token is None or end < best_token[1]:
                best_token = (start, end)
    
    if best_token is None:
        # Cursor is not inside any token. Find the next token after cursor.
        next_token = None
        for start, end in tokens:
            if start > pos:
                if next_token is None or start < next_token[0]:
                    next_token = (start, end)
        return next_token[1] if next_token else pos
    
    # Cursor is at or inside best_token
    if pos == best_token[0]:
        # At start of token. Jump to its end.
        return best_token[1]
    else:
        # Strictly inside. Find the next sub-token after cursor (within best_token).
        next_sub = None
        for start, end in tokens:
            if start > pos and end <= best_token[1]:
                if next_sub is None or start < next_sub[0]:
                    next_sub = (start, end)
        return next_sub[1] if next_sub else best_token[1]


def prev_big_word_start(text: str, pos: int) -> int:
    tokens = _tokenize_big_words(text)
    
    # Find the tightest token containing the cursor (start < pos <= end)
    best_token = None
    for start, end in tokens:
        if start < pos <= end:
            if best_token is None or start > best_token[0]:
                best_token = (start, end)
    
    if best_token is None:
        # Cursor is not inside any token. Find the previous token before cursor.
        prev_token = None
        for start, end in tokens:
            if end < pos:
                if prev_token is None or end > prev_token[1]:
                    prev_token = (start, end)
        if prev_token:
            return prev_token[0]
        return 0 if pos > 0 else 0
    
    # Cursor is at or inside best_token
    if pos == best_token[1]:
        # At end of token. Jump to its start.
        return best_token[0]
    else:
        # Strictly inside. Find the previous sub-token before cursor (within best_token).
        prev_sub = None
        for start, end in tokens:
            if end < pos and start >= best_token[0]:
                if prev_sub is None or end > prev_sub[1]:
                    prev_sub = (start, end)
        return prev_sub[0] if prev_sub else best_token[0]
