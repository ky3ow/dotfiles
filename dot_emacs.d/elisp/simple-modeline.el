;;; simple-modeline.el --- Simple modeline  -*- lexical-binding: t -*-

(defgroup simple-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup simple-modeline-faces nil
  "Faces for my custom modeline."
  :group 'simple-modeline)

(defface simple-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package)."
  :group 'simple-modeline-faces)

;;; KBD macro module

(defface simple-modeline-indicator-blue-bg
  `((default :inherit (bold simple-modeline-indicator-button)
	     :background ,(face-attribute 'ansi-color-blue :background)
	     :foreground "black"))
  "Face for modeline indicators"
  :group 'simple-modeline-faces)

(defvar-local simple-modeline-kmacro-module
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " @recording " 'face 'simple-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(put 'simple-modeline-kmacro-module 'risky-local-variable t)

;;; Narrow indicator

(defface simple-modeline-indicator-cyan-bg
  `((default :inherit (bold simple-modeline-indicator-button)
	     :background ,(face-attribute 'ansi-color-cyan :background)
	     :foreground "black"))
  "Face for modeline indicators"
  :group 'simple-modeline-faces)

(defvar-local simple-modeline-narrow-module
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " narrow " 'face 'simple-modeline-indicator-cyan-bg)))
  "Mode line construct to report the narrowed state of the current buffer.")

(put 'simple-modeline-narrow-module 'risky-local-variable t)

;;; Remote buffer

(defface simple-modeline-indicator-red-bg
  `((default :inherit (bold simple-modeline-indicator-button)
	     :background ,(face-attribute 'ansi-color-red :background)
	     :foreground "black"))
  "Face for modeline indicators"
  :group 'simple-modeline-faces)

(defvar-local simple-modeline-remote-module
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'simple-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

(put 'simple-modeline-remote-module 'risky-local-variable t)

;;; Input method

(defface simple-modeline-indicator-green-bg
  `((default :inherit (bold simple-modeline-indicator-button)
	     :background ,(face-attribute 'ansi-color-green :background)
	     :foreground "black"))
  "Face for modeline indicators"
  :group 'simple-modeline-faces)

(defvar-local simple-modeline-input-module
    '(:eval
      (propertize (format " %s " (or current-input-method-title "EN"))
                  'face 'simple-modeline-indicator-green-bg
                  'mouse-face 'mode-line-highlight))
  "Mode line construct to report the multilingual environment.")

(put 'simple-modeline-input-module 'risky-local-variable t)

;;; Buffer name

(defun simple-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `simple-modeline-buffer-identification'."
  (let ((faces))
    (when (mode-line-window-selected-p)
      (push 'mode-line-buffer-id faces))
    (when (and (buffer-file-name) (buffer-modified-p))
      (push 'italic faces))
    (if faces faces nil)))

(defun simple-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((filename (buffer-file-name)))
    (format "%s%s%s" (if buffer-read-only
			 (format "%s " (char-to-string #xE0A2)) "")
	    (if filename (abbreviate-file-name filename) (buffer-name))
	    (if (and filename (buffer-modified-p)) "*" ""))))

(defun simple-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `simple-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local simple-modeline-buffer-module
    '(:eval
      (propertize (simple-modeline-buffer-name)
                  'face (simple-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (simple-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

(put 'simple-modeline-buffer-module 'risky-local-variable t)

;;; Major mode

(defface simple-modeline-indicator-red
  `((default :inherit (bold simple-modeline-indicator-button)
	     :foregound ,(face-attribute 'ansi-color-red :foreground)))
  "Face for modeline indicators"
  :group 'simple-modeline-faces)

(defun simple-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "🞊"))))
    (propertize indicator 'face 'shadow)))

(defun simple-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun simple-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `simple-modeline-major-mode'."
  (if-let* ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local simple-modeline-major-module
    (list
     (propertize "%[" 'face 'simple-modeline-indicator-red)
     '(:eval
       (concat
        (simple-modeline-major-mode-indicator)
        " "
        (propertize
	 (simple-modeline-major-mode-name)
         'mouse-face 'mode-line-highlight
         'help-echo (simple-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'simple-modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local simple-modeline-process-module
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

(put 'simple-modeline-major-module 'risky-local-variable t)
(put 'simple-modeline-process-module 'risky-local-variable t)

;;; LSP

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local simple-modeline-lsp-module
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

(put 'simple-modeline-lsp-module 'risky-local-variable t)

;;; Misc

(defvar-local simple-modeline-misc-module
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

(put 'simple-modeline-misc-module 'risky-local-variable t)

;;; Lines

(defvar-local simple-modeline-lines-module
    '(:eval
       (propertize (concat " [%c][%l/" (number-to-string (line-number-at-pos (point-max))) "] ")
                    'face 'simple-modeline-indicator-blue-bg
                    'mouse-face 'mode-line-highlight))
  "Display lines")

(put 'simple-modeline-lines-module 'risky-local-variable t)

;;; Encoding

(defface simple-modeline-indicator-magenta-bg
  `((default :inherit (bold simple-modeline-indicator-button)
	     :background ,(face-attribute 'ansi-color-magenta :background)
	     :foreground "black"))
  "Face for modeline indicators"
  :group 'simple-modeline-faces)

(defvar-local simple-modeline-encoding-module
    '(:eval
       (propertize (format " %s " (symbol-name (or buffer-file-coding-system 'utf-8-unix)))
                    'face 'simple-modeline-indicator-magenta-bg
                    'mouse-face 'mode-line-highlight))
  "File encoding")

(put 'simple-modeline-encoding-module 'risky-local-variable t)

;;; Minor

(defface simple-modeline-indicator-yellow-bg
  `((default :inherit (bold simple-modeline-indicator-button)
	     :background ,(face-attribute 'ansi-color-yellow :background)
	     :foreground "black"))
  "Face for modeline indicators"
  :group 'simple-modeline-faces)

(defvar-local simple-modeline-minor-module
    '(:eval
       (propertize (format-mode-line minor-mode-alist)
                    'face 'simple-modeline-indicator-yelow-bg
                    'mouse-face 'mode-line-highlight))
  "File encoding")

(put 'simple-modeline-minor-module 'risky-local-variable t)

;;; Align

(defvar-local simple-modeline-left-elements
  '("%e"
    simple-modeline-input-module
    simple-modeline-kmacro-module
    simple-modeline-narrow-module
    simple-modeline-remote-module
    " "
    simple-modeline-buffer-module
    simple-modeline-minor-module
    )
  "List of mode line constructs for the left side.")

(defvar-local simple-modeline-right-elements
  '(""
    simple-modeline-lsp-module
    simple-modeline-major-module
    " "
    simple-modeline-process-module
    simple-modeline-misc-module
    simple-modeline-lines-module
    simple-modeline-encoding-module)
  "List of mode line constructs for the right side.")

(put 'simple-modeline-left-elements 'risky-local-variable t)
(put 'simple-modeline-right-elements 'risky-local-variable t)

(defun simple-format-mode-line-aligned ()
  (let* (;; Get the available width for the mode line text
         (available-width (window-body-width))
         ;; Format the left and right parts using the standard formatter
         (left-str (format-mode-line simple-modeline-left-elements))
         (right-str (format-mode-line simple-modeline-right-elements))
         ;; Calculate the displayed width of each part
         (left-width (string-width left-str))
         (right-width (string-width right-str))
         ;; Calculate the padding needed. Ensure it's not negative.
         (padding-width (max 0 (1+ (- available-width left-width right-width))))
         ;; Create the padding string
         (padding-str (make-string padding-width ?\s)))
    ;; Concatenate left, padding, and right parts
    (concat left-str padding-str right-str)))

(setq-default mode-line-format '((:eval (simple-format-mode-line-aligned))))

;;; Setting the modeline

;; (setq-default mode-line-format
;; 	      '("%e"
;; 		simple-modeline-input-module
;; 		simple-modeline-kmacro-module
;; 		simple-modeline-narrow-module
;; 		simple-modeline-remote-module
;; 		" "
;; 		simple-modeline-buffer-module
;; 		" "
;; 		simple-modeline-minor-module
;; 		;; mode-line-format-right-align ; Emacs 30
;;                 simple-modeline-lsp-module
;;                 " "
;; 		simple-modeline-major-module
;; 		" "
;; 		simple-modeline-process-module
;; 		simple-modeline-misc-module
;; 		simple-modeline-lines-module
;; 		simple-modeline-encoding-module
;; 		))

(provide 'simple-modeline)
