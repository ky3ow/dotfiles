;;; unclutter.el --- My twist on no-littering.el with xdg directories  -*- lexical-binding: t -*-

(defcustom unclutter-use-customize nil
  "Specifies behavior of custom file.
When set to `nil', custom file is simply ignored(but kept in `emacs-state-home' to avoid cluttering init.el).
Otherwise when set to truthy value - use it and place it into user configuration")

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))

(when (and unclutter-use-customize (file-exists-p custom-file))
  (load custom-file))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(make-directory (expand-file-name "auto-save-list" user-emacs-directory) t)
(make-directory (expand-file-name "lock" user-emacs-directory) t)
(add-to-list 'auto-save-file-name-transforms `(".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t) :append) ;; add transformation for all files(by default only remote files)
(add-to-list 'lock-file-name-transforms `(".*" ,(expand-file-name "lock/" user-emacs-directory) t) :append) ;; same as autosave(by default nil)

(provide 'unclutter)
