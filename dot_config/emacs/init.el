(use-package unclutter
  :custom (unclutter-use-customize nil))

(use-package emacs
  :custom
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  
  (inhibit-startup-screen t)
  (ring-bell-function 'ignore)
  (frame-title-format '("%b"))
  (use-file-dialog nil)
  (inhibit-startup-echo-area-message (user-login-name))
  
  (column-number-mode t)
  (line-number-mode t)
  (tab-bar-show 1)
  (isearch-lazy-count t)
  (global-hl-line-mode t)
  
  :config
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark :no-confirm))
;;(load-theme 'ef-elea-light :no-confirm)
