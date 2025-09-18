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
      (setq-local eldoc-mouse-last-symbol-bounds (bounds-of-thing-at-point 'symbol))
      (goto-char pos)
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
             0.2 nil #'eldoc-mouse-show-doc-at pos)))))

(defun eldoc-mouse-setup ()
  "Set up eldoc-mouse for the current buffer."
  (when (eglot-managed-p)
    ;; Enable eldoc-box hover mode
    (eldoc-box-hover-mode 1)
    ;; Avoid unnecessary document of signatures that clutters the document.
    (remove-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function t)
    ;; Avoid show document for the cursor.
    (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t)
    ;; Enable highlight symbol under the cursor.
    (add-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback nil t)
    ;; Enable mouse tracking
    (setq track-mouse t)
    ;; Bind mouse movement to documentation display
    (local-set-key [mouse-movement] #'eldoc-mouse-doc-on-mouse)))

;;;###autoload
(defun eldoc-mouse-enable ()
  "Enable eldoc-mouse in all `prog-mode' buffers."
  (interactive)
  (add-hook 'prog-mode-hook #'eldoc-mouse-setup))

;;;###autoload
(defun eldoc-mouse-disable ()
  "Disable eldoc-mouse in all `prog-mode' buffers."
  (interactive)
  (remove-hook 'prog-mode-hook #'eldoc-mouse-setup)
  (when eldoc-mouse-mouse-timer
    (cancel-timer eldoc-mouse-mouse-timer)
    (setq eldoc-mouse-mouse-timer nil))
  (local-unset-key [mouse-movement]))

(eldoc-mouse-enable)

(provide 'eldoc-mouse)

;;; eldoc-mouse.el ends here
