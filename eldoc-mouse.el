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
;; when the mouse hovers over a symbol.  It integrates with posframe' for popup
;; documentation.  Enable it in buffers that you want to show documentation using
;; eldoc for the symbol under the mouse cursor.

;; To use, ensure posframe is installed, then add the following:

;; The following two lines are both optional, but you would like to add at least one of them to your Emacs configuration.
;; (use-package eldoc-mouse :hook (eglot-managed-mode emacs-lisp-mode)) ;; enable mouse hover for eglot managed buffers, and Emacs Lisp buffers.
;; (global-set-key (kbd "<f1> <f1>") 'eldoc-mouse-pop-doc-at-cursor) ;; replace <f1> <f1> to a key you like.  Displaying document on a popup when you press a key.

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

(defcustom eldoc-mouse-posframe-max-width 120
  "The maximum number of characters the posframe may contain in each line."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-min-height 1
  "The minimum number of lines posframe may contain."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-max-height 30
  "The maximum number of lines posframe may contain.
It could be nil, means no limit, in practical, I found that set this too big or
no limit, the popup may affect writing."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-fringe-width 8
  "The width of the posframe fringe."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-border-width 1
  "The width of the posframe border."
  :type 'number
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-border-color (face-foreground 'default)
  "The color of the posframe border."
  :type 'string
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-override-parameters '((drag-internal-border     . t))
  "This is very powful, *all* the valid frame parameters used by posframe’s frame can be overridden by it."
  :type '(alist :key-type symbol :value-type (choice integer boolean))
  :group 'eldoc-mouse)

(defcustom eldoc-mouse-posframe-buffer-name " *doc-posframe-buffer*"
  "The name of the hidden buffer used by posframe.
A leading space make the buffer hidden."
  :type 'string
  :group 'eldoc-mouse)

(defvar eldoc-mouse-mouse-timer nil
  "Idle timer for mouse hover eldoc.")

(defvar-local eldoc-mouse-mouse-overlay nil
  "The overlay for the symbol under the mouse.")

(defvar-local eldoc-mouse-last-symbol-bounds nil
  "Bounds of the last symbol processed for eldoc.")

(defvar-local eldoc-mouse--original-display-functions nil
  "Store the original `eldoc-display-functions'.")

(defvar-local eldoc-mouse--doc-identifier "*^eldoc-mouse*^"
  "The identifier used for distinguish the doc triggered by eldoc-mouse.")

(defvar-local eldoc-mouse--eldoc-documentation-functions
    (list #'eldoc-mouse--eglot-eldoc-documentation-function #'eldoc-mouse--elisp-eldoc-documentation-function)
  "The `eldoc-documentation-functions' for `eldoc-mouse-mode'.")

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
  (when-let* ((symbol-bounds (bounds-of-thing-at-point 'symbol))
              (eldoc-documentation-functions (eldoc-mouse--eldoc-documentation-functions)))
    (setq-local eldoc-mouse-last-symbol-bounds symbol-bounds)
    (when (not eldoc-mouse-mode)
      (unless eldoc-mouse--original-display-functions
        (setq-local eldoc-mouse--original-display-functions eldoc-display-functions))
      (setq-local eldoc-display-functions
                  (append
                   eldoc-display-functions '(eldoc-mouse-display-in-posframe))))
    (setq eldoc--last-request-state nil)
    (eldoc-print-current-symbol-info)))

(defun eldoc-mouse-enable ()
  "Enable eldoc-mouse in buffers."
  ;; Enable mouse tracking.
  (setq track-mouse t)
  (setq-local eldoc-mouse--original-display-functions eldoc-display-functions)
  (setq-local eldoc-display-functions
              (append
               eldoc-display-functions '(eldoc-mouse-display-in-posframe)))
  (local-set-key [mouse-movement] #'eldoc-mouse-doc-on-mouse))

(defun eldoc-mouse-disable ()
  "Disable eldoc-mouse in buffers."
  (when eldoc-mouse--original-display-functions
    (setq-local eldoc-display-functions
                eldoc-mouse--original-display-functions))

  (when eldoc-mouse-mouse-timer
    (cancel-timer eldoc-mouse-mouse-timer)
    (setq eldoc-mouse-mouse-timer nil))
  (eldoc-mouse--hide-posframe)
  (local-unset-key [mouse-movement])
  (when (y-or-n-p "eldoc-mouse-mode has been turned off.  Also disable mouse-tracking (may impact other modes)?")
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
    (when eldoc-mouse-mouse-overlay
      (delete-overlay eldoc-mouse-mouse-overlay))
    (save-excursion
      (let ((eldoc-documentation-functions (eldoc-mouse--eldoc-documentation-functions)))
        (goto-char pos)
        (setq-local eldoc-mouse-last-symbol-bounds
                    (bounds-of-thing-at-point 'symbol))
        ;; Use (nth 4 (syntax-ppss)) to check if the mouse is over a code comment.
        ;; based on the answer from
        ;; https://emacs.stackexchange.com/questions/14269/14270#14270
        (when (and eldoc-mouse-last-symbol-bounds
                   (not (eolp))
                   (not (nth 4 (syntax-ppss))))
          (eldoc-print-current-symbol-info)
          (setq-local eldoc-mouse-mouse-overlay
                      (make-overlay
                       (car eldoc-mouse-last-symbol-bounds)
                       (cdr eldoc-mouse-last-symbol-bounds)))
          (overlay-put eldoc-mouse-mouse-overlay 'face 'secondary-selection))))))

(defun eldoc-mouse--hide-posframe ()
  "Hide the posframe."
  (remove-hook 'buffer-list-update-hook #'eldoc-mouse--change-buffer-hook t)
  (remove-hook 'post-command-hook #'eldoc-mouse--post-command-hook t)
  (advice-remove 'keyboard-quit #'eldoc-mouse--hide-posframe)
  (posframe-hide eldoc-mouse-posframe-buffer-name))

(defun eldoc-mouse-doc-on-mouse (event)
  "Show eldoc documentation when mouse hovers over EVENT."
  (interactive "e")
  (when eldoc-mouse-mode
    (let ((pos (posn-point (event-start event))))
    (when (and pos (number-or-marker-p pos))
      ;; Debounce to avoid spamming eglot.
      (when eldoc-mouse-mouse-timer
        (cancel-timer eldoc-mouse-mouse-timer))
      (setq eldoc-mouse-mouse-timer
            (run-with-idle-timer
             eldoc-mouse-idle-time nil #'eldoc-mouse-show-doc-at
             pos))))))

(defun eldoc-mouse--eglot-eldoc-documentation-function (cb)
  "Modify the `eglot-hover-eldoc-function'.
So it won't call `eglot--highlight-piggyback` with `CB`."
  (if (eglot-managed-p)
      (if (fboundp 'eglot--highlight-piggyback)
          (cl-letf (((symbol-function 'eglot--highlight-piggyback)
                     (lambda (&rest _args) (message ""))))
            (eglot-hover-eldoc-function cb))
        (eglot-hover-eldoc-function cb))
    nil))

(defun eldoc-mouse--elisp-eldoc-documentation-function (_cb)
  "The `eldoc-documentation-functions' implementation for elisp."
  (if (eq major-mode 'emacs-lisp-mode)
      (let ((sym (symbol-at-point)))
        (cond
         ;; If the symbol is a function
         ((and sym (fboundp sym))
          (documentation sym))
         ;; If the symbol is a variable
         ((and sym (boundp sym))
          (let ((doc (documentation-property sym 'variable-documentation)))
            (if doc
                doc
              nil)))
         ;; If no symbol or not a function/variable
         (t nil)))
    nil))

(defun eldoc-mouse--hover-edloc-function-advise (orig-fn fn)
  "Wrap FN argument of ORIG-FN so that it append indentifier."
  (let ((result (funcall orig-fn
           (lambda (s &rest r)
             (funcall fn (if (and s (not (string-empty-p (string-trim s))))
                             (concat s eldoc-mouse--doc-identifier)
                           s) r)))))
    (if (stringp result)
        (concat result eldoc-mouse--doc-identifier)
      result)))

(defun eldoc-mouse--eldoc-documentation-functions ()
  "Get the eldoc documentation functions defined by eldoc-mouse."
  (let* ((fun-list1 (seq-filter (lambda (f)
                                  (and (not (function-equal f #'eglot-hover-eldoc-function))
                                       (or (not (fboundp 'eglot-highlight-eldoc-function))
                                           (not (function-equal f #'eglot-highlight-eldoc-function)))
                                       (not (function-equal f #'eglot-signature-eldoc-function))))
                                eldoc-documentation-functions))
         (fun-list2 (append eldoc-mouse--eldoc-documentation-functions fun-list1)))
    (mapcar (lambda (fun) (if (functionp fun)
                              (lambda (&rest args) (apply #'eldoc-mouse--hover-edloc-function-advise fun args))
                            fun))
            fun-list2)))

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
  (when (and docs (string-match-p (regexp-quote eldoc-mouse--doc-identifier) (car (car docs))))
    (get-buffer-create eldoc-mouse-posframe-buffer-name)
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
                 (buffer-string))))
          (when text
            (eldoc-mouse--pop-doc (replace-regexp-in-string (regexp-quote eldoc-mouse--doc-identifier) "" text)))))
      ;; non-nil => suppress other display functions.
      t)))

(defun eldoc-mouse--pop-doc (doc)
  "Pop up the document DOC on posframe with BORDER-COLOR."
  (when (and eldoc-mouse--original-display-functions (not eldoc-mouse-mode))
    (setq-local eldoc-display-functions eldoc-mouse--original-display-functions)
    (setq-local eldoc-mouse--original-display-functions nil))
  (posframe-show
   eldoc-mouse-posframe-buffer-name
   :initialize #'eldoc-mouse--posframe-init
   :position (car eldoc-mouse-last-symbol-bounds)
   :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
   :max-width eldoc-mouse-posframe-max-width
   :min-height eldoc-mouse-posframe-min-height
   :max-height eldoc-mouse-posframe-max-height
   :border-width eldoc-mouse-posframe-border-width
   :border-color eldoc-mouse-posframe-border-color
   :left-fringe eldoc-mouse-posframe-fringe-width
   :right-fringe eldoc-mouse-posframe-fringe-width
   :override-parameters eldoc-mouse-posframe-override-parameters
   :string doc)
  (advice-add 'keyboard-quit :before #'eldoc-mouse--hide-posframe)
  (add-hook 'buffer-list-update-hook #'eldoc-mouse--change-buffer-hook nil t)
  (add-hook 'post-command-hook #'eldoc-mouse--post-command-hook nil t))

(defun eldoc-mouse--posframe-init ()
  "Initialize the posframe buffer."
  (visual-line-mode t)
  (setq-local fringe-indicator-alist
              (assq-delete-all 'continuation fringe-indicator-alist))
  (setq-local visual-line-fringe-indicators '(nil nil)))

(provide 'eldoc-mouse)

;;; eldoc-mouse.el ends here
