;;; eldoc-mouse.el --- Display documentation for mouse hover -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Huang Feiyu

;; Author: Huang Feiyu sibadake1@163.com
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (posframe "1.4.0") (eglot "1.8"))
;; Keywords: tools, languages, convenience, emacs, mouse, hover
;; URL: https://github.com/huangfeiyu/eldoc-mouse
;;
;; This file is part of eldoc-mouse.
;;
;; eldoc-mouse is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.
;;
;; eldoc-mouse is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with eldoc-mouse; if not, see <http://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:

;; This package enhances eldoc' by displaying documentation in a child frame
;; when the mouse hovers over a symbol in eglot'-managed buffers. It integrates
;; with posframe' for popup documentation and provides a debounced mouse hover
;; mechanism to avoid spamming the LSP server. Enable it in prog-mode' buffers
;; to show documentation for the symbol under the mouse cursor.

;; To use, ensure posframe is installed, then add:
;; (require 'eldoc-mouse)
;; to your Emacs configuration.

;;; Code:

(require 'eldoc)
(require 'posframe)
(require 'eglot)
(require 'cl-lib)

(defgroup eldoc-mouse nil
  "Dispay document for mouse hover."
  :prefix "eldoc-mouse-"
  :group 'eldoc)

(defcustom eldoc-mouse-idle-time 0.2
  "The minimum amount of seconds.
 that the mouse hover on a symbol before
   triggering eldoc to get the document of the symbol, default to 0.2 second."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-max-width 70
  "The maximum number of charactersthe posframe may contain in each line."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-min-height 3
  "The minimum number of lines posframe may contain."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-buffer-name "*doc-posframe-buffer*"
  "The name of the hidden buffer used by posframe."
  :type 'string
  :group 'eldoc-mouse)

(defvar eldoc-mouse-mouse-timer nil
  "Idle timer for mouse hover eldoc.")

(defvar-local eldoc-mouse-mouse-overlay nil
  "The overlay for the symbol under the mouse.")

(defvar-local eldoc-mouse-last-symbol-bounds nil
  "Bounds of the last symbol processed for eldoc.")

(defun eldoc-mouse-show-doc-at (pos)
  "Ask eldoc to show documentation for symbol at POS.
POS is the buffer position under the mouse cursor."
  (when (and pos
             (number-or-marker-p pos)
             (not (eldoc-mouse-is-mouse-hovering-posframe? eldoc-mouse-posframe-buffer-name))
             (or (null eq eldoc-mouse-last-symbol-bounds)
                 (< pos (car eldoc-mouse-last-symbol-bounds))
                 (> pos (cdr eldoc-mouse-last-symbol-bounds))))
    (posframe-hide eldoc-mouse-posframe-buffer-name)
    (when (fboundp 'eglot--highlight-piggyback)
      (remove-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback t))
    (when eldoc-mouse-mouse-overlay
      (delete-overlay eldoc-mouse-mouse-overlay))
    (save-excursion
      (add-hook 'eldoc-documentation-functions #'eldoc-mouse-hover-eldoc-function nil t)
      (goto-char pos)
      (setq-local eldoc-mouse-last-symbol-bounds (bounds-of-thing-at-point 'symbol))
      (when (thing-at-point 'symbol)
        (eldoc-print-current-symbol-info)
        (setq-local eldoc-mouse-mouse-overlay (make-overlay (car eldoc-mouse-last-symbol-bounds) (cdr eldoc-mouse-last-symbol-bounds)))
        (overlay-put eldoc-mouse-mouse-overlay 'face 'highlight))
      (remove-hook 'eldoc-documentation-functions #'eldoc-mouse-hover-eldoc-function t)
      (when (fboundp 'eglot--highlight-piggyback)
        (add-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback nil t)))))

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

(defun eldoc-mouse-hover-eldoc-function (cb)
  "Modify the `eglot-hover-eldoc-function', so it won't call `eglot--highlight-piggyback', `CB'."
  (if (fboundp 'eglot--highlight-piggyback)
      (cl-letf (((symbol-function 'eglot--highlight-piggyback) (lambda (&rest args) (message ""))))
        (eglot-hover-eldoc-function cb))
    (eglot-hover-eldoc-function cb)))

(defun eldoc-mouse-handle-eglot-hooks ()
  (setq-local eldoc-display-functions (list #'eldoc-display-in-buffer #'eldoc-mouse-display-in-posframe))
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

(defun eldoc-mouse-is-mouse-hovering-posframe? (posframe-name)
  "Check if the mouse is hovering over the given posframe `POSFRAME-NAME'."
  (let* ((posframe (get-buffer posframe-name))   ;; Get the posframe buffer
         (frame (get-buffer-window posframe)))
    ;; keep the child frame when it is clicked, need a better way to determine if the mouse is overing the child frame.
    (if (and posframe (windowp frame))
        t
      nil)))

(defun eldoc-mouse-display-in-posframe (docs interactive)
  "Display `DOCS' STRING in a posframe at the current mouse position."
  (when docs
    ;; output the document for *eldoc* buffer
    (eldoc--format-doc-buffer docs)
    (let* ((eldoc-buffer (get-buffer (car (seq-filter (lambda (buf) (string-match-p ".*\\*eldoc.*\\*" (buffer-name buf))) (buffer-list))))))
      (when eldoc-buffer
        (let ((text (with-current-buffer eldoc-buffer
                      (buffer-string)))
              (border-color (face-foreground 'default)))
          (when text
            (posframe-show eldoc-mouse-posframe-buffer-name
                           :position (car eldoc-mouse-last-symbol-bounds)
                           :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
                           :max-width eldoc-mouse-posframe-max-width
                           :min-height eldoc-mouse-posframe-min-height
                           :border-width 1
                           :border-color border-color
                           :string text))))
      
      t)))  ;; non-nil => suppress other display functions


(defun eldoc-mouse-setup ()
  "Set up eldoc-mouse for the current buffer."
  (add-hook 'eglot-managed-mode-hook #'eldoc-mouse-handle-eglot-hooks t)
  ;; Bind mouse movement to documentation display
  (local-set-key [mouse-movement] #'eldoc-mouse-doc-on-mouse))

;;;###autoload
(defun eldoc-mouse-enable ()
  "Enable eldoc-mouse in all `prog-mode' buffers."
  (interactive)

  ;; (setq eldoc-message-function #'eldoc-mouse-message-function)
  ;; Enable mouse tracking
  (setq track-mouse t)

  ;; (add-to-list 'eldoc-display-functions #'eldoc-mouse-display-in-posframe)
  ;; make sure the eldoc-mouse also enabled to the current buffer.
  (when (eglot-managed-p)
    ;; (eldoc-box-hover-mode)
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
