;;; (setq use-short-answers t) ;;
;;; (setq inhibit-x-resources t) ;; was set for some reason

(require 'xdg)
(defcustom emacs-state-home (expand-file-name "emacs" (xdg-state-home))
  "Specifies where to store state files(undo, backups)")
(defcustom emacs-cache-home (expand-file-name "emacs" (xdg-cache-home))
  "Specifies where to store cache files(eln cache)")
(defcustom emacs-data-home (expand-file-name "emacs" (xdg-data-home))
  "Specifies where to store data files(packages from elpa)")
(defcustom emacs-config-home (expand-file-name "emacs/elisp" (xdg-config-home))
  "Specifies where are user config files(your elisp configuration)")

(make-directory emacs-state-home t)
(make-directory emacs-cache-home t)
(make-directory emacs-data-home t)
(make-directory emacs-config-home t)

(setopt package-user-dir (expand-file-name "packages" emacs-data-home))
(startup-redirect-eln-cache emacs-cache-home)
(add-to-list 'load-path emacs-config-home) ;; packages dir
