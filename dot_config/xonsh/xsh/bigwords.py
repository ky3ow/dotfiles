_EXPANSIONS = (
    ("@!(", ")"),
    ("$(", ")"),
    ("!(", ")"),
    ("@(", ")"),
    ("$[", "]"),
    ("![", "]"),
)


def _tokenize_big_words(text: str):
    i = 0
    n = len(text)
    while i < n:
        if text[i].isspace():
            i += 1
            continue
        start = i
        for prefix, close in _EXPANSIONS:
            if text.startswith(prefix, i):
                open_char = prefix[-1]
                i += len(prefix)
                depth = 1
                while i < n and depth:
                    if text[i] == open_char:
                        depth += 1
                    elif text[i] == close:
                        depth -= 1
                    i += 1
                yield start, i
                break
        else:
            if text[i] in ('"', "'"):
                quote = text[i]
                i += 1
                while i < n and text[i] != quote:
                    i += 1
                if i < n:
                    i += 1
                yield start, i
            else:
                while i < n and not text[i].isspace():
                    i += 1
                yield start, i


def next_big_word_end(text: str, pos: int) -> int:
    for start, end in _tokenize_big_words(text):
        if start <= pos < end:
            return end
        if start >= pos:
            return end
    return pos


def prev_big_word_start(text: str, pos: int) -> int:
    result = pos
    for start, end in _tokenize_big_words(text):
        if start < pos <= end:
            result = start
        elif end <= pos:
            result = start
        else:
            break
    # If we're before the first word but not at the absolute start, jump to 0.
    if result == pos and pos > 0:
        return 0
    return result
