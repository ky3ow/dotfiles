$XONSH_COPY_ON_DELETE = True
# $XONSH_STORE_STDOUT = True

if p"/run/WSL".exists():
    for _dir in ["/mnt/c/Windows", "/mnt/c/Windows/system32"]:
        $PATH.append(_dir)
