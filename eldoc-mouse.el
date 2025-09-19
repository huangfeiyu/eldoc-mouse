;;; eldoc-mouse.el --- Display eldoc documentation on mouse hover -- lexical-binding: t; --

;; Copyright (C) 2025 Huang Feiyu

;; Author: Huang Feiyu sibadake1@163.com
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (eldoc-box "2.1.1") (eglot "1.8"))
;; Keywords: tools, languages, convenience, emacs, mouse, hover
;; URL: https://github.com/huangfeiyu/eldoc-mouse
;; License: GPL-3.0-or-later

;;; Commentary:

;; This package enhances eldoc' by displaying documentation in a child frame
;; when the mouse hovers over a symbol in eglot'-managed buffers. It integrates
;; with eldoc-box' for popup documentation and provides a debounced mouse hover
;; mechanism to avoid spamming the LSP server. Enable it in prog-mode' buffers
;; to show documentation for the symbol under the mouse cursor.

;; To use, ensure eldoc-box are installed, then add:
;; (require 'eldoc-mouse)
;; to your Emacs configuration.

;;; Code:

(require 'eldoc)
(require 'eldoc-box)
(require 'eglot)

(defgroup eldoc-mouse nil
  "Dispay document for mouse hover"
  :prefix "eldoc-mouse-"
  :group 'eldoc)

(defcustom eldoc-mouse-idle-time 0.2
  "The minimum amount of seconds that the mouse hover on a symbol before
   triggering eldoc to get the document of the symbol, default to 0.2 second."
  :type 'number
  :group 'eldoc-mouse)

(defvar eldoc-mouse-mouse-timer nil
  "Idle timer for mouse hover eldoc.")

(defvar-local eldoc-mouse-last-symbol-bounds nil
  "Bounds of the last symbol processed for eldoc.")

(defun eldoc-mouse-show-doc-at (pos)
  "Ask eldoc to show documentation for symbol at POS.
POS is the buffer position under the mouse cursor."
  (when (and pos
             (number-or-marker-p pos)
             (or (eq eldoc-mouse-last-symbol-bounds nil)
                 (< pos (car eldoc-mouse-last-symbol-bounds))
                 (> pos (cdr eldoc-mouse-last-symbol-bounds))))
    (save-excursion
      (add-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function nil t)
      (goto-char pos)
      (setq-local eldoc-mouse-last-symbol-bounds (bounds-of-thing-at-point 'symbol))
      (when (thing-at-point 'symbol)
        (eldoc-print-current-symbol-info))
      (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t))))

(defun eldoc-mouse-doc-on-mouse (event)
  "Show eldoc documentation when mouse hovers over EVENT."
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (when (and pos (number-or-marker-p pos) (eglot-managed-p))
      ;; Debounce to avoid spamming eglot
      (when eldoc-mouse-mouse-timer
        (cancel-timer eldoc-mouse-mouse-timer))
      (setq eldoc-mouse-mouse-timer
            (run-with-idle-timer
             eldoc-mouse-idle-time nil #'eldoc-mouse-show-doc-at pos)))))

(defun eldoc-mouse-handle-eglot-hooks ()
  "remove the hooks that display doc on cursor hover, keep highlighting on cursor."
  ;; Avoid unnecessary document of signatures that clutters the document.
  (remove-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function t)
  ;; Avoid show document for the cursor.
  (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t)
  ;; Enable highlight symbol under the cursor.
  ;; In the future the following line is no longer necessary, as emacs use a specific function eglot-highlight-eldoc-function for highlighting.
  ;; And here, we want to keep the highlight at cursor.
  ;; see details: https://cgit.git.savannah.gnu.org/cgit/emacs.git/commit/?id=60166a419f601b413db86ddce186cc387e8ec269
  (when (fboundp 'eglot--highlight-piggyback)
    (add-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback nil t)))

(defun eldoc-mouse-setup ()
  "Set up eldoc-mouse for the current buffer."
  ;; Enable eldoc-box hover mode
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-hook 'eglot-managed-mode-hook #'eldoc-mouse-handle-eglot-hooks t)
  ;; Bind mouse movement to documentation display
  (local-set-key [mouse-movement] #'eldoc-mouse-doc-on-mouse))

;;;###autoload
(defun eldoc-mouse-enable ()
  "Enable eldoc-mouse in all `prog-mode' buffers."
  (interactive)
  ;; Enable mouse tracking
  (setq track-mouse t)
  ;; make sure the eldoc-mouse also enabled to the current buffer.
  (when (eglot-managed-p)
    (eldoc-box-hover-mode)
    (eldoc-mouse-handle-eglot-hooks)
    (local-set-key [mouse-movement] #'eldoc-mouse-doc-on-mouse))
  (add-hook 'prog-mode-hook #'eldoc-mouse-setup))

;;;###autoload
(defun eldoc-mouse-disable ()
  "Disable eldoc-mouse in all `prog-mode' buffers."
  (interactive)
  (setq track-mouse nil)
  (remove-hook 'prog-mode-hook #'eldoc-mouse-setup)
  (remove-hook 'eglot-managed-mode-hook #'eldoc-mouse-handle-eglot-hooks)
  (when eldoc-mouse-mouse-timer
    (cancel-timer eldoc-mouse-mouse-timer)
    (setq eldoc-mouse-mouse-timer nil))
  (local-unset-key [mouse-movement]))

(provide 'eldoc-mouse)

;;; eldoc-mouse.el ends here
