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
	    (or (abbreviate-file-name filename) (buffer-name))
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

;;; Setting the modeline

(setq-default mode-line-format
	      '("%e"
		simple-modeline-kmacro-module
		simple-modeline-narrow-module
		simple-modeline-remote-module
		simple-modeline-input-module
		" "
		simple-modeline-buffer-module
		))

(provide 'simple-modeline)
