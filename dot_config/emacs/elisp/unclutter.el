;;; unclutter.el --- My twist on no-littering.el with xdg directories  -*- lexical-binding: t -*-

(defcustom unclutter-use-customize nil
  "Specifies behavior of custom file.
When set to `nil', custom file is simply ignored(but kept in `emacs-state-home' to avoid cluttering init.el).
Otherwise when set to truthy value - use it and place it into user configuration")

(setq custom-file (expand-file-name "customize.el"
				    (if unclutter-use-customize emacs-config-home emacs-state-home)))

(when (and unclutter-use-customize (file-exists-p custom-file))
  (load custom-file))

(setq user-emacs-directory (expand-file-name emacs-cache-home))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups" emacs-state-home))))

(make-directory (expand-file-name "autosaves" emacs-state-home) t)
(make-directory (expand-file-name "lock" emacs-state-home) t)
(setq auto-save-list-file-prefix (expand-file-name "autosaves/sessions/" emacs-state-home)) ;; mapping of autosave files
(add-to-list 'auto-save-file-name-transforms `(".*" ,(expand-file-name "autosaves/" emacs-state-home) t) :append) ;; add transformation for all files(by default only remote files)
(add-to-list 'lock-file-name-transforms `(".*" ,(expand-file-name "lock/" emacs-state-home) t) :append) ;; same as autosave(by default nil)

(provide 'unclutter)
