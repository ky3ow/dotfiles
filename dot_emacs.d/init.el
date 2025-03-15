(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory)) ;; packages dir
(add-to-list 'package-archives '("mepla" . "https://melpa.org/packages/") t) ;; add melpa

(use-package unclutter
  :custom (unclutter-use-customize nil))

(use-package emacs
  :custom
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  
  ;;; (use-short-answers t)
  ;;; (inhibit-x-resources t) ;; was set for some reason

  (inhibit-startup-screen t)
  (ring-bell-function 'ignore)
  (frame-title-format '("%b"))
  (use-file-dialog nil)
  (inhibit-startup-echo-area-message (user-login-name))
  
  (column-number-mode t)
  (line-number-mode t)
  (tab-bar-show 1)
  (isearch-lazy-count t)

  (savehist-mode t)

  (sentence-end-double-space nil)

  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 1)
  (completion-detailed t)
  (tab-always-indent 'complete)
  (completion-auto-help 'always)
  (completions-format 'one-column)
  (completions-group t)
  ;;(completion-auto-select 'second-tab)
  (completions-max-height 20)

  (x-underline-at-descent-line nil)
  (switch-to-buffer-obey-display-actions t)

  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
  (pixel-scroll-precision-mode t)

  :config
  ;; fonts  
  (set-face-attribute 'default nil :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")

  ;; general hooks
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook 'visual-line-mode)

  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

  ;; tab-bar-select-tab
  (dotimes (i 9)
    (let ((keybind (format "C-c C-%d" (1+ i))))
      (global-set-key (kbd keybind)
		      `(lambda ()
			(interactive)
			(tab-bar-select-tab ,(1+ i))))))

  (when (display-graphic-p)
    (context-menu-mode)))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark :no-confirm))
;;(load-theme 'ef-elea-light :no-confirm)

(use-package which-key
  :ensure t
  :custom
  (which-key-side-window-location '(right bottom))
  (which-key-popup-type 'side-window)
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

(use-package org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)

  (org-pretty-entities t)
  (org-agenda-tags-column 0)
  (org-ellipsis "…")

  :hook
  (org-mode . org-indent-mode))

(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars " ")
  (org-modern-star 'replace)
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  :hook
  (org-mode . org-modern-mode))

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 0.7)
  :hook
  (org-mode . olivetti-mode))

(use-package eat
  :ensure t)
