;;; init --- My init file -*- lexical-binding: t; -*-
;;; Commentary:
;;; tries to make everything the way that does not annoy me
;;; Code:
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory)) ;; packages dir

(use-package package
  :custom
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/"))))

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

  (defun skip-these-buffers (_window buffer _bury-or-kill)
	"Function for `switch-to-prev-buffer-skip'."
	(string-match "\\*[^*]+\\*" (buffer-name buffer)))

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
  (electric-indent-mode nil)

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
  (pixel-scroll-precision-use-momentum nil)
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

  (treesit-font-lock-level 4)

  (switch-to-prev-buffer-skip 'skip-these-buffers)

  (ad-redefinition-action 'accept)

  (kill-do-not-save-duplicates t)

  (kill-region-dwim 'unix-word)

  (use-short-answers t)

  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)

  :custom-face
  (default ((t (:family "Iosevka" :height 150))))
  (variable-pitch ((t (:family "Iosevka Aile" :height 140))))
					;(symbol ((t (:family "DejaVu Sans"))))

  :bind
  (:prefix-map clipboard-map
			   :prefix "C-c c"
			   ("c" . clipboard-kill-ring-save)
			   ("v" . clipboard-yank)
			   ("x" . clipboard-kill-region))

  :bind
  ("C-S-c" . clipboard-kill-ring-save)
  ("C-S-v" . clipboard-yank)
  ("C-S-x" . clipboard-kill-region)
  ("<remap> <move-beginning-of-line>" . smarter-move-beginning-of-line)
  ("C-x C-b" . ibuffer)
  ("C-M-y" . duplicate-dwim)
  ("RET" . newline-and-indent)
  ("C-x C-z" . nil)
  ("C-z" . nil)
  ("C-x C-p" . previous-buffer)
  ("C-x C-n" . next-buffer)

  :bind
  (:prefix-map option-toggles
			   :prefix "C-c t"
			   ("w" . my-toggle-truncate-lines)
			   ("s" . visual-line-mode))

  :config
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

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
  (prog-mode . display-line-numbers-mode)
  ((prog-mode yaml-mode) . whitespace-mode)
  (text-mode . visual-line-mode)
  ((prog-mode text-mode) . hl-line-mode))


(use-package delight
  :ensure t)

(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
	 ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
	 (".*" "open" "xdg-open")))
  :bind
  ("C-x f" . find-name-dired))

(use-package ibuffer
  :custom
  (ibuffer-human-readable-size t))

(use-package recentf
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 15))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark :no-confirm)
  (ef-themes-with-colors
	(custom-set-faces
	 `(whitespace-space ((,c :foreground ,bg-dim)))
	 `(whitespace-indent ((,c :foreground ,bg-alt))))))

(use-package whitespace
  :delight
  :custom
  (whitespace-style
   '(face trailing tabs spaces newline missing-newline-at-eof empty
	  indentation space-after-tab space-before-tab space-mark
	  tab-mark)))

(use-package hideshow
  :delight (hs-minor-mode)
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
  :delight
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

  (org-edit-src-content-indentation 4)

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

(use-package toc-org ;; probably don't need that
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 0.7)
  :hook
  (org-mode . olivetti-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "multimarkdown"))

(use-package eat
  :ensure t
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-maximum-latency 0.005)
  (eat-minimum-latency 0.001)
  :hook
  (eshell-load . eat-eshell-mode))

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

(use-package smerge-mode
  :ensure nil
  :defer t
  :bind (:map smerge-mode-map
			  ("C-c m u" . smerge-keep-upper)
			  ("C-c m l" . smerge-keep-lower)
			  ("C-c m n" . smerge-next)
			  ("C-c m p" . smerge-previous)))

(use-package eldoc
  :delight
  :custom
  (eldoc-idle-delay 0)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :ensure t
  :bind
  ("C-h C-k" . eldoc-box-help-at-point)
  ("C-h C-b" . eldoc-doc-buffer))

(use-package flymake
  :custom
  (flymake-mode-line-lighter " FM")
  :hook
  (prog-mode . flymake-mode))

(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-margin-mode t)
  (diff-hl-disable-on-remote t)
  :hook
  (dired-mode . diff-hl-dired-mode-unless-remote)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  ((prog-mode conf-mode) . diff-hl-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-mode t)
  (corfu-popuinfo-delay 0.1)
  (corfu-separator ?\s)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 3)

  (completion-ignore-case t)

  (text-mode-ispell-word-completion nil)
  (global-corfu-mode t)
  :bind
  (:map corfu-map
		("M-RET" . corfu-insert))
  :config
  (dolist (key '("TAB" "RET" "<down>" "<up>"
				 "<remap> <next-line>" "<remap> <previous-line>"))
	(keymap-unset corfu-map key)))

(use-package cape
  :ensure t
  :bind
  ("M-/" . dabbrev-completion)
  ("C-M-/" . dabbrev-expand)
  (:prefix-map capes-map
			   :prefix "C-c /"
			   ("f" . #'cape-file)
			   ("d" . #'cape-dict)
			   ("b" . #'cape-elisp-block)
			   ("l" . #'cape-line)
			   ("n" . #'cape-keyword)))

(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-mode t))

(use-package projectile
  :ensure t
  :custom
  (projectile-mode t)
  (projectile-run-use-comint-mode t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/code/" ("~/.config" . 1)))
  (projectile-mode-line-prefix " Dir"))

(use-package transient)

(use-package consult
  :ensure t
  :config
  (advice-add #'register-preview :override #'consult-register-preview)

  (cl-defmacro my-consult-define-prefix (name &key doc flags command)
	`(transient-define-prefix ,name ()
       ,doc
       [
		["Flags" ,@flags]
		["Actions"
		 ("s" "Save options" transient-save)
		 ("q" "Quit" transient-quit-one)
		 ("c" "Invoke command" (lambda ()
								 (interactive)
								 (transient-quit-one)
								 (,command)))
		 ]
		]))

  (transient-define-infix my-consult-rg-case ()
	:description "Ignore case"
	:key "-i"
	:argument "--ignore-case")

  (transient-define-infix my-consult-rg-toggle-hidden ()
	:description "Hidden"
	:key "-."
	:argument "--hidden")

  (defvar my-consult-ripgrep-args
	;; --smart-case
	"rg --null --line-buffered --color=never --max-columns=1000 --path-separator /  --no-heading --with-filename --line-number --search-zip")

  (defun my--consult-rg-impl (&optional my-consult-input)
	(let* ((base my-consult-ripgrep-args)
		   (extra-flags-list (transient-args 'my-transient-consult-rg))
		   (new-flags (mapconcat #'identity extra-flags-list " "))
		   (new-command (concat base " " new-flags)))
	  (let ((consult-ripgrep-args new-command))
		(consult-ripgrep nil my-consult-input))))

  (defun my-consult-rg ()
	(interactive)
	(if (minibufferp)
		(let ((my-consult-input (minibuffer-contents)))
		  (run-at-time 0.01 nil #'my--consult-rg-impl my-consult-input)
		  (minibuffer-quit-recursive-edit))
	  (my--consult-rg-impl)))

  (my-consult-define-prefix my-transient-consult-rg
							:doc "Ripgrep flags"
							:flags
							((my-consult-rg-toggle-hidden)
							(my-consult-rg-case))
							:command my-consult-rg)

  (transient-define-prefix my-consult-main-transient ()
  "Main menu for consult search commands."
  [["Commands"
    ("r" "Consult-ripgrep" my-transient-consult-rg)
	]
   ["Actions"
	("q" "Quit" transient-quit-one)
	]
   ])

  ;; need calling consult ripgrep first for some reason
  ;; (defun my-transient-auto-save (&rest _)
  ;; 	"Save state only for `my-consult-rg-flags-transient`."
  ;; 	(when (and (boundp 'transient--prefix)
  ;;              (object-of-class-p transient--prefix transient-prefix)
  ;;              (eq (oref transient--prefix command)
  ;;                  'my-consult-rg-flags-transient))
  ;; 	  (message "SAVING")
  ;;     (transient-save)))
  ;; (advice-add 'transient-infix-set :after #'my-transient-auto-save)

  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-function #'projectile-project-root)
  :bind
  ("M-s f" . consult-find)
  ("M-s g" . consult-ripgrep)
  ("M-s r" . consult-recent-file)
  ("M-s b" . consult-buffer)
  ("M-s i" . consult-info)
  ("M-s /" . consult-line)
  ("M-s G" . grep)
  ("C-c m" . my-consult-main-transient)
  (:map minibuffer-local-map
   ("C-." . my-consult-main-transient)))


(use-package embark
  :ensure t
  :preface
  (defun embark-select-and-go ()
	(interactive)
	(embark-select)
	(forward-line 1))
  :bind
  ("C-c ." . embark-act)
  (:map embark-collect-mode-map
		("m" . embark-select)
		("M-m" . embark-select-and-go)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package sideline
  :delight
  :ensure t)

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'point)
  (sideline-backends-right '(sideline-flymake)))

(use-package treesit
  :custom
  (treesit-language-source-alist
   '((lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"
		  :commit "88e446476a1e97a8724dff7a23e2d709855077f2")
	 (python "https://github.com/tree-sitter/tree-sitter-python"
			 :commit "bffb65a8cfe4e46290331dfef0dbf0ef3679de11"))))

(use-package kmacro-x
  :ensure t
  :delight
  (kmacro-x-mc-atomic-undo-mode)
  (kmacro-x-atomic-undo-mode)
  :preface
  (defun my-macro-on-lines ()
	"Apply macro on region lines and amalgamate undo"
	(interactive)
	(with-undo-amalgamate
	  (apply-macro-to-region-lines (region-beginning) (region-end))))

  :custom
  (kmacro-x-mc-atomic-undo-mode t)
  (kmacro-x-atomic-undo-mode t)
  :bind
  (:map kmacro-x-mc-mode-map
		("RET" . nil)
		("M-RET" . nil)
		("C-g" . kmacro-x-mc-quit)
		("M-o" . kmacro-x-mc-apply-one))

  (:repeat-map my-kmacro-x-mc-keymap
			   ("n" . kmacro-x-mc-mark-next)
			   ("p" . kmacro-x-mc-mark-previous)
			   :exit
			   ("l" . my-macro-on-lines)
			   ("a" . kmacro-x-mc-apply))

  :bind-keymap
  ("C-c k" . my-kmacro-x-mc-keymap))

(use-package repeat
  :custom
  (repeat-mode t))

(use-package wgrep
  :ensure t)

(use-package chezmoi
  :ensure t)

(provide 'init.el)
;;; init.el ends here
