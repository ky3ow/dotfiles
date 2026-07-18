#!/usr/bin/env sh
if ! emacsclient -e 't'; then
  emacs --daemon
fi

EXIST=$(emacsclient -e '(if (filtered-frame-list (lambda (f) (string= (frame-parameter f '\''name) "notes"))) 1 0 )')

if [ $EXIST -eq 1 ]; then
  emacsclient -e '
    (let ((frame (car (filtered-frame-list (lambda (f) (string= (frame-parameter f '\''name) "notes"))))))
        (make-frame-invisible frame)
        (make-frame-visible frame)
        (find-file "~/Notes/scratchpad.org")
        (end-of-buffer))'
else
  emacsclient -c -n -F "((name . \"notes\") (width . 100) (height . 40))" -e '
      (find-file "~/Notes/scratchpad.org")
      (end-of-buffer)'
fi
