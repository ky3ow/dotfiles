(add-to-list 'load-path emacs-config-home)

(setq column-number-mode t) ;; in bar
(setq line-number-mode t) ;; in bar
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; in column

(global-hl-line-mode 1)
(hl-line-mode 1)

(use-package unclutter)

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark :no-confirm))
;;(load-theme 'ef-elea-light :no-confirm)

(use-package emacs
  :config
  (setq isearch-lazy-count t))
