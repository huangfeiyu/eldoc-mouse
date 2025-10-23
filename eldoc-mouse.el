;;; eldoc-mouse.el --- Display documentation for mouse hover -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Huang Feiyu

;; Author: Huang Feiyu sibadake1@163.com
;; Package-Version: 20251015.1027
;; Package-Revision: 9ca2aec75f72
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
;; when the mouse hovers over a symbol in eglot'-managed buffers.  It integrates
;; with posframe' for popup documentation and provides a debounced mouse hover
;; mechanism to avoid spamming the LSP server.  Enable it in prog-mode' buffers
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

(defcustom eldoc-mouse-idle-time 0.4
  "The minimum amount of seconds.
that the mouse hover on a symbol before
   triggering eldoc to get the document of the symbol, default to 0.2 second."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-max-width 70
  "The maximum number of characters the posframe may contain in each line."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-min-height 3
  "The minimum number of lines posframe may contain."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-max-height 15
  "The maximum number of lines posframe may contain.
It could be nil, means no limit, in practical, I found that set this too big or
no limit, the popup may affect writing."
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

(defvar-local eldoc-mouse-unsupress-posframe nil
  "Temporarily un-suppress the posframe.
By default, posframe will not used by eldoc.")

(defvar-local eldoc-mouse--original-display-functions nil
  "Store the original `eldoc-display-functions'.")

;;;###autoload
(define-minor-mode eldoc-mouse-mode
  "Toggle the `eldoc-mouse-mode'."
  :lighter " eldoc-mouse"

  (if eldoc-mouse-mode
      (eldoc-mouse-enable)
    (eldoc-mouse-disable)))

;;;###autoload
(defun eldoc-mouse-pop-doc-at-cursor ()
  "Show document at the cursor."
  (interactive)
  (eldoc-mouse--hide-posframe)
  (when-let* ((symbol-bounds (bounds-of-thing-at-point 'symbol)))
    (cond
     (eldoc-mouse-mode
      (add-hook
       'eldoc-documentation-functions #'eldoc-mouse-hover-eldoc-function
       nil t)
      (setq-local eldoc-mouse-last-symbol-bounds symbol-bounds)
      (setq-local eldoc-mouse-unsupress-posframe t)

      ;; Make sure eldoc always send the request to get doc.
      (setq eldoc--last-request-state nil)

      (eldoc-print-current-symbol-info)
      (remove-hook
       'eldoc-documentation-functions #'eldoc-mouse-hover-eldoc-function
       t))
     (t
      (when (eglot-managed-p)
        (remove-hook
         'eldoc-documentation-functions #'eglot-signature-eldoc-function
         t))
      
      (setq-local eldoc-mouse-last-symbol-bounds symbol-bounds)
      (unless eldoc-mouse--original-display-functions
        (setq-local eldoc-mouse--original-display-functions
                    eldoc-display-functions))
      (setq-local eldoc-display-functions
                  (append
                   eldoc-display-functions '(eldoc-mouse-display-in-posframe)))
      (setq-local eldoc-mouse-unsupress-posframe t)

      ;; Make sure eldoc always send the request to get doc.
      (setq eldoc--last-request-state nil)

      (eldoc-print-current-symbol-info)
      (when (eglot-managed-p)
        (add-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function
                  nil
                  t))))))
(defun eldoc-mouse-enable ()
  "Enable eldoc-mouse in all `eglot-managed-p' buffers."
  (when (eglot-managed-p)
    ;; Enable mouse tracking.
    (setq track-mouse t)
    (setq-local eldoc-mouse--original-display-functions eldoc-display-functions)
    (setq-local eldoc-display-functions
                (append
                 eldoc-display-functions '(eldoc-mouse-display-in-posframe)))
    ;; Avoid unnecessary document of signatures that clutters the document.
    (remove-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function
                 t)
    ;; Avoid show document for the cursor.
    (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t)
    ;; Enable highlight symbol under the cursor.
    ;; In the future the following line is no longer necessary,
    ;; as emacs use a specific function `eglot-highlight-eldoc-function'
    ;; for highlighting.
    ;; And here, we want to keep the highlight at cursor.
    ;; See details:
    ;; https://cgit.git.savannah.gnu.org/cgit/emacs.git/commit/?id=60166a419f601b413db86ddce186cc387e8ec269
    (when (fboundp 'eglot--highlight-piggyback)
      (add-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback
                nil
                t))
    (local-set-key [mouse-movement] #'eldoc-mouse-doc-on-mouse)))

(defun eldoc-mouse-disable ()
  "Disable eldoc-mouse in all `eglot-managed-p' buffers."
  (when eldoc-mouse--original-display-functions
    (setq-local eldoc-display-functions
                eldoc-mouse--original-display-functions))
  (when (fboundp 'eglot--highlight-piggyback)
    (remove-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback t))

  (unless (memq #'eglot-signature-eldoc-function eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function
              nil
              t))
  (unless (memq #'eglot-hover-eldoc-function eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function
              nil
              t))

  (when eldoc-mouse-mouse-timer
    (cancel-timer eldoc-mouse-mouse-timer)
    (setq eldoc-mouse-mouse-timer nil))
  (eldoc-mouse--hide-posframe)
  (local-unset-key [mouse-movement])
  (when (y-or-n-p "Disable mouse-tracking (may impact other modes)?")
    (setq track-mouse nil)))

(defun eldoc-mouse--post-command-hook ()
  "The hook of post-command used by eldoc-mouse.
Support close the popup when the cursor is moved away."
  (when (not (eq 'eldoc-mouse-doc-on-mouse this-command))
    (let ((pos (point)))
      (when (or (< pos (car eldoc-mouse-last-symbol-bounds))
                (> pos (cdr eldoc-mouse-last-symbol-bounds)))
        (eldoc-mouse--hide-posframe)))))

(defun eldoc-mouse--change-buffer-hook ()
  "The hook when changing buffer.
Support close the popup when user switch buffer."
  (eldoc-mouse--hide-posframe))

(defun eldoc-mouse-show-doc-at (pos)
  "Ask eldoc to show documentation for symbol at POS.
POS is the buffer position under the mouse cursor."
  (when (and pos
             (number-or-marker-p pos)
             (not
              (eldoc-mouse-is-mouse-hovering-posframe?
               eldoc-mouse-posframe-buffer-name))
             (or (null eldoc-mouse-last-symbol-bounds)
                 (< pos (car eldoc-mouse-last-symbol-bounds))
                 (> pos (cdr eldoc-mouse-last-symbol-bounds))))
    (eldoc-mouse--hide-posframe)
    (when (fboundp 'eglot--highlight-piggyback)
      (remove-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback
                   t))
    (when eldoc-mouse-mouse-overlay
      (delete-overlay eldoc-mouse-mouse-overlay))
    (save-excursion
      (add-hook
       'eldoc-documentation-functions #'eldoc-mouse-hover-eldoc-function
       nil t)
      (goto-char pos)
      (setq-local eldoc-mouse-last-symbol-bounds
                  (bounds-of-thing-at-point 'symbol))
      ;; Use (nth 4 (syntax-ppss)) to check if the mouse is over a code comment.
      ;; based on the answer from
      ;; https://emacs.stackexchange.com/questions/14269/14270#14270
      (when (and eldoc-mouse-last-symbol-bounds
                 (not (eolp))
                 (not (nth 4 (syntax-ppss))))
        (setq-local eldoc-mouse-unsupress-posframe t)
        (eldoc-print-current-symbol-info)
        (setq-local eldoc-mouse-mouse-overlay
                    (make-overlay
                     (car eldoc-mouse-last-symbol-bounds)
                     (cdr eldoc-mouse-last-symbol-bounds)))
        (overlay-put eldoc-mouse-mouse-overlay 'face 'highlight))
      (remove-hook
       'eldoc-documentation-functions #'eldoc-mouse-hover-eldoc-function
       t)
      (when (fboundp 'eglot--highlight-piggyback)
        (add-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback
                  nil
                  t)))))

(defun eldoc-mouse--hide-posframe ()
  "Hide the posframe."
  (remove-hook 'buffer-list-update-hook #'eldoc-mouse--change-buffer-hook t)
  (remove-hook 'post-command-hook #'eldoc-mouse--post-command-hook t)
  (advice-remove 'keyboard-quit #'eldoc-mouse--hide-posframe)
  (posframe-hide eldoc-mouse-posframe-buffer-name))

(defun eldoc-mouse-doc-on-mouse (event)
  "Show eldoc documentation when mouse hovers over EVENT."
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (when (and pos (number-or-marker-p pos) (eglot-managed-p))
      ;; Debounce to avoid spamming eglot.
      (when eldoc-mouse-mouse-timer
        (cancel-timer eldoc-mouse-mouse-timer))
      (setq eldoc-mouse-mouse-timer
            (run-with-idle-timer
             eldoc-mouse-idle-time nil #'eldoc-mouse-show-doc-at
             pos)))))

(defun eldoc-mouse-hover-eldoc-function (cb)
  "Modify the `eglot-hover-eldoc-function'.
So it won't call `eglot--highlight-piggyback` with `CB`."
  (if (fboundp 'eglot--highlight-piggyback)
      (cl-letf (((symbol-function 'eglot--highlight-piggyback)
                 (lambda (&rest _args) (message ""))))
        (eglot-hover-eldoc-function cb))
    (eglot-hover-eldoc-function cb)))

(defun eldoc-mouse-handle-eglot-hooks ()
  "Handle the eldoc eglot hooks.
Remove all eglot hooks and keep highlighting on cursor,
add eldoc-mouse's `eldoc-display-functions'."
  (setq-local eldoc-display-functions
              (append
               eldoc-display-functions '(eldoc-mouse-display-in-posframe)))
  ;; Avoid unnecessary document of signatures that clutters the document.
  (remove-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function
               t)
  ;; Avoid show document for the cursor.
  (remove-hook 'eldoc-documentation-functions #'eglot-hover-eldoc-function t)
  ;; Enable highlight symbol under the cursor.
  ;; In the future the following line is no longer necessary,
  ;; as emacs use a specific function eglot-highlight-eldoc-function
  ;; for highlighting.
  ;; And here, we want to keep the highlight at cursor.
  ;; See details:
  ;; https://cgit.git.savannah.gnu.org/cgit/emacs.git/commit/?id=60166a419f601b413db86ddce186cc387e8ec269
  (when (fboundp 'eglot--highlight-piggyback)
    (add-hook 'eldoc-documentation-functions #'eglot--highlight-piggyback
              nil
              t)))

(defun eldoc-mouse-is-mouse-hovering-posframe? (posframe-name)
  "Check if the mouse is hovering over the given posframe `POSFRAME-NAME'."
  (let* ((posframe (get-buffer posframe-name)) ;; Get the posframe buffer
         (frame (get-buffer-window posframe)))
    ;; keep the child frame when it is clicked, need a better
    ;; way to determine if the mouse is over the child frame.
    (if (and posframe (windowp frame))
        t
      nil)))

(defun eldoc-mouse-display-in-posframe (docs _interactive)
  "Display `DOCS` STRING in a posframe at the current mouse position."
  (when (and docs eldoc-mouse-unsupress-posframe)
    (setq-local eldoc-mouse-unsupress-posframe nil)
    ;; Output the document for *eldoc* buffer.
    ;; (eldoc--format-doc-buffer docs)
    (let* ((eldoc-buffer
            (get-buffer
             (car
              (seq-filter
               (lambda (buf)
                 (string-match-p ".*\\*eldoc.*\\*" (buffer-name buf)))
               (buffer-list))))))
      (when eldoc-buffer
        (let ((text
               (with-current-buffer eldoc-buffer
                 (buffer-string)))
              (border-color (face-foreground 'default)))
          (when text
            (eldoc-mouse--pop-doc text border-color))))
      ;; non-nil => suppress other display functions.
      t)))

(defun eldoc-mouse--pop-doc (doc border-color)
  "Pop up the document DOC on posframe with BORDER-COLOR."
  (when (and eldoc-mouse--original-display-functions (not eldoc-mouse-mode))
    (setq-local eldoc-display-functions eldoc-mouse--original-display-functions)
    (setq-local eldoc-mouse--original-display-functions nil))
  (posframe-show
   eldoc-mouse-posframe-buffer-name
   :position (car eldoc-mouse-last-symbol-bounds)
   :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
   :max-width eldoc-mouse-posframe-max-width
   :min-height eldoc-mouse-posframe-min-height
   :max-height eldoc-mouse-posframe-max-height
   :border-width 1
   :border-color border-color
   :string doc)
  (advice-add 'keyboard-quit :before #'eldoc-mouse--hide-posframe)
  (add-hook 'post-command-hook #'eldoc-mouse--post-command-hook nil t)
  (add-hook 'buffer-list-update-hook #'eldoc-mouse--change-buffer-hook nil t))

(provide 'eldoc-mouse)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; eldoc-mouse.el ends here
