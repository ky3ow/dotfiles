(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory)) ;; packages dir
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
 ("mepla" . "https://melpa.org/packages/")))

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

  ;; tab-bar-things
  (defun my-tab-bar-tab-name-format-function (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
	  (bufname (alist-get 'name tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format "%d " i) "")
               bufname
	       (if (and (buffer-modified-p (get-buffer bufname))
			(not (string-match "\\*.*\\*" bufname)))
		   "*"
		 "")
               (or (and tab-bar-close-button-show
			(not (eq tab-bar-close-button-show
				 (if current-p 'non-selected 'selected)))
			tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))

  (setopt tab-bar-tab-name-format-function 'my-tab-bar-tab-name-format-function)
  
  (when (display-graphic-p)
    (context-menu-mode))

  (defun wslp()
    (and
     (eq system-type 'gnu/linux)
     (file-exists-p "/run/WSL")))

  (when (wslp)
    (setq select-active-regions nil
        select-enable-clipboard 't
        select-enable-primary nil
        interprogram-cut-function #'gui-select-text))
  )

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
  (org-modern-block-fringe nil)
  (org-modern-replace-stars "◉○◈◇✿")
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  (defcustom org-modern-replace-stars-cycle t
    "Cycle replace stars instead of repeating past max defined level"
    :type 'boolean)
  
  (defun org-modern--star ()
    "Prettify headline stars."
    (let* ((beg (match-beginning 1))
           (end (match-end 1))
           (level (- end beg)))
      (when (and org-modern--hide-stars-cache (not (eq beg end)))
	(cl-loop for i from beg below end do
		 (put-text-property i (1+ i) 'display
                                    (nth (logand i 1)
					 org-modern--hide-stars-cache))))
      (when org-modern-star
	(when (and (eq org-modern-hide-stars 'leading) org-hide-leading-stars)
          (put-text-property beg (1+ end) 'face (get-text-property end 'face)))
	(put-text-property
	 (if (eq org-modern-hide-stars 'leading) beg end)
	 (1+ end) 'display
	 (let* ((cache (if (and org-modern--folded-star-cache
				(org-invisible-p (pos-eol)))
                           org-modern--folded-star-cache
			 org-modern--expanded-star-cache))
		(cache-index (if org-modern-replace-stars-cycle
				 (mod level (length cache))
			       (min (1- (length cache)) level))))
           (aref cache cache-index))))))

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
