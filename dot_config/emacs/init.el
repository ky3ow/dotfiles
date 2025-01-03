(setq column-number-mode t) ;; in bar
(setq line-number-mode t) ;; in bar
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; in column

(require 'ef-themes)
(load-theme 'ef-elea-dark :no-confirm)
;;(load-theme 'ef-elea-light :no-confirm)

(global-hl-line-mode 1)
(hl-line-mode 1)

;; FILES

(setq user-emacs-directory (expand-file-name ky3ow-emacs-cache))
(setq custom-file (expand-file-name "customize" ky3ow-emacs-state))
;; if i want to actually use customize
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(setq backup-directory-alist `(("." . ,(expand-file-name "backups" ky3ow-emacs-state))))

(make-directory (expand-file-name "autosaves" ky3ow-emacs-state) t)
(make-directory (expand-file-name "lock" ky3ow-emacs-state) t)
(setq auto-save-list-file-prefix (expand-file-name "autosaves/sessions/" ky3ow-emacs-state)) ;; mapping of autosave files
(add-to-list 'auto-save-file-name-transforms `(".*" ,(expand-file-name "autosaves/" ky3ow-emacs-state) t) :append) ;; add transformation for all files(by default only remote files)
(add-to-list 'lock-file-name-transforms `(".*" ,(expand-file-name "lock/" ky3ow-emacs-state) t) :append) ;; same as autosave(by default nil)
