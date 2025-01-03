(scroll-bar-mode -1) ;; scroll
(tool-bar-mode -1) ;; tool bar
(menu-bar-mode -1) ;; menu bar

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      frame-title-format '("%b")
      use-file-dialog nil)

;; (setq use-short-answers t)
(setq inhibit-x-resources t
      inhibit-startup-echo-area-message t)

;; Temporary changes
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(defvar my-emacs--file-name-handler-alist file-name-handler-alist)
(defvar my-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my-emacs--file-name-handler-alist
                  vc-handled-backends my-emacs--vc-handled-backends)))

(require 'xdg)

(defcustom my-emacs-state (expand-file-name "emacs" (xdg-state-home))
  "Specifies where to store state files(undo, backups)")
(defcustom my-emacs-cache (expand-file-name "emacs" (xdg-cache-home))
  "Specifies where to store cache files(eln cache)")
(defcustom my-emacs-data (expand-file-name "emacs" (xdg-data-home))
  "Specifies where to store data files(packages from elpa)")

(make-directory my-emacs-state t)
(make-directory my-emacs-cache t)
(make-directory my-emacs-data t)

(startup-redirect-eln-cache my-emacs-cache)
(setq package-user-dir (expand-file-name "packages" my-emacs-data))
