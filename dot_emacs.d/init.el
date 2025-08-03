;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory)) ;; packages dir
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))

(use-package unclutter
  :custom (unclutter-use-customize nil))

(use-package emacs
  :preface
  (defun my-toggle-truncate-lines ()
	"Toggle the `truncate-lines` setting."
	(interactive)
	(setopt truncate-lines (not truncate-lines))
	(if truncate-lines
	(message "Truncate lines: On")
	  (message "Truncate lines: Off")))

  (defun wslp()
	(and
	 (eq system-type 'gnu/linux)
	 (file-exists-p "/run/WSL")))

  (defun smarter-move-beginning-of-line (arg)
	(interactive "^p")
	(setq arg (or arg 1))
	;; Move lines first
	(when (/= arg 1)
	  (let ((line-move-visual nil))
	(forward-line (1- arg))))
	(let ((orig-point (point)))
	  (back-to-indentation)
	  (when (= orig-point (point))
	(move-beginning-of-line 1))))

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

  (delete-selection-mode t)
  (electric-pair-mode t)

  (dired-kill-when-opening-new-dired-buffer t)
  (recentf-mode t)

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

  ;; hide commands in -x which do not work in current mode
  (read-extended-command-predicate #'command-completion-default-include-p)

  (x-underline-at-descent-line nil)
  (switch-to-buffer-obey-display-actions t)

  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
  (pixel-scroll-precision-mode t)
  (scroll-conservatively 100)
  (scroll-margin 8)

  (select-enable-clipboard nil)
  (truncate-lines t)
  (help-window-select t)

  (mode-line-position-column-line-format '(" [%l/%c]" ))

  (mode-line-format '("%e" mode-line-front-space
			  (:propertize
			   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
			   display
			   (min-width
			(5.0)))
			  mode-line-frame-identification mode-line-buffer-identification
			  "   "
			  mode-line-position
			  (:eval (format "(%d/%d)"
					 (count-lines (point-min) (point-max))
					 (length (buffer-substring (line-beginning-position) (line-end-position)))))
			  (vc-mode vc-mode)
			  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

  (face-font-family-alternatives '(("Iosevka" "DejaVu Sans")
				   ("Monospace" "courier" "fixed")
				   ("Monospace Serif" "Courier 10 Pitch" "Consolas" "Courier Std" "FreeMono" "Nimbus Mono L" "courier" "fixed")
				   ("courier" "CMU Typewriter Text" "fixed")
				   ("Sans Serif" "helv" "helvetica" "arial" "fixed")
				   ("helv" "helvetica" "arial" "fixed")))

  (tab-width 4)

  (context-menu-mode t)
  (xterm-mouse-mode (not (display-graphic-p)))

  :custom-face
  (default ((t (:family "Iosevka" :height 150))))
  (variable-pitch ((t (:family "Iosevka Aile" :height 140))))
					;(symbol ((t (:family "DejaVu Sans"))))

  :bind (:prefix-map clipboard-map
			 :prefix "C-c c"
			 ("c" . clipboard-kill-ring-save)
			 ("v" . clipboard-yank)
			 ("x" . clipboard-kill-region))

  :bind (("C-S-c" . clipboard-kill-ring-save)
	 ("C-S-v" . clipboard-yank)
	 ("C-S-x" . clipboard-kill-region)
	 ("<remap> <move-beginning-of-line>" . smarter-move-beginning-of-line))

  :bind (:prefix-map option-toggles
			 :prefix "C-c t"
			 ("w" . my-toggle-truncate-lines)
			 ("s" . visual-line-mode))

  :config
  (put 'narrow-to-region 'disabled nil)

  (when (wslp)
	(setq select-active-regions nil)
	(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
	  (cmd-args '("/c" "start")))
	  (when (file-exists-p cmd-exe)
	(setq browse-url-generic-program  cmd-exe
		  browse-url-generic-args     cmd-args
		  browse-url-browser-function 'browse-url-generic
		  search-web-default-browser 'browse-url-generic))))

  :hook

  (prog-mode . whitespace-mode)
  (prog-mode . display-line-numbers-mode)
  (text-mode . visual-line-mode)
  ((prog-mode text-mode) . hl-line-mode)
  ((prog-mode text-mode) . whitespace-mode))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark :no-confirm)
  (ef-themes-with-colors
	(custom-set-faces
	 `(whitespace-space ((,c :foreground ,bg-dim)))
	 `(whitespace-indent ((,c :foreground ,bg-alt))))))

(use-package whitespace
  :custom
  (whitespace-style
   '(face trailing tabs spaces newline missing-newline-at-eof empty
	  indentation space-after-tab space-before-tab space-mark
	  tab-mark))
  (whitespace-action '(auto-cleanup)))

(use-package hideshow
  :custom
  (outline-blank-line t)
  :bind-keymap ("C-c o" . hs-minor-mode-map)
  :bind (:map hs-minor-mode-map
			  ("C-c o TAB" . hs-toggle-hiding)
			  ("C-c o o" . hs-show-block)
			  ("C-c o c" . hs-hide-block)
			  ("<backtab>" . hs-toggle-hiding))
  :hook
  (prog-mode . hs-minor-mode))

(use-package tab-bar
  :custom
  (tab-bar-tab-name-format-function 'my-tab-bar-tab-name-format-function)
  (tab-bar-tab-hints t)
  (tab-bar-separator "")
  (tab-bar-close-button-show nil)
  :preface
  (defun my-tab-bar-tab-name-format-function (tab i)
	(let ((current-p (eq (car tab) 'current-tab))
	  (bufname (alist-get 'name tab)))
	  (propertize
	   (concat (if tab-bar-tab-hints (format " %d " i) "")
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
  :config
  (dotimes (i 9)
	(let ((keybind (format "C-c c %d" (1+ i))))
	  (global-set-key (kbd keybind)
			  `(lambda ()
			 (interactive)
			 (tab-bar-select-tab ,(1+ i))))))
  )

(use-package which-key
  :ensure t
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-popup-type 'side-window)
  (which-key-idle-delay 1)
  (which-key-mode t))

(use-package org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)

  (org-pretty-entities t)
  (org-agenda-tags-column 0)
  (org-ellipsis "…")

  ;;(org-return-follows-link t)

  :hook
  (org-mode . org-indent-mode))

(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars " ")
  (org-modern-star 'replace)
  (org-modern-block-fringe nil)
  (org-modern-replace-stars "◉○◈◇✿")
  :custom-face
  (org-modern-symbol ((t :family "DejaVu Sans")))

  :preface
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
  :ensure t
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-maximum-latency 0.005)
  (eat-minimum-latency 0.001))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-mode t))

(use-package xclip
  :ensure t
  :if (not (display-graphic-p))
  :custom
  (xclip-mode (not (display-graphic-p))))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :ensure t
  :commands (magit-status magit-diff))

(use-package treesit
  :custom
  (treesit-language-source-alist '((lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"
					:commit "88e446476a1e97a8724dff7a23e2d709855077f2")
				   (python "https://github.com/tree-sitter/tree-sitter-python" :commit
					   "bffb65a8cfe4e46290331dfef0dbf0ef3679de11"))))
