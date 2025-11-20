[![melpa badge][melpa-badge]][melpa-link]

# eldoc-mouse

`eldoc-mouse` is an Emacs package that enhances the `eldoc` functionality by displaying documentation in a popup at the mouse point using [posframe](https://github.com/tumashu/posframe) when the mouse hovers over a symbol. It integrates with `posframe` to provide popping up documentation.
[Eldoc-mouse Demo at Youtube](https://youtu.be/XFAc4WyiJjI)
<video src="https://github.com/user-attachments/assets/5622cfd2-de0c-46e8-9276-d67615671932" controls></video>

## Features
- **It is specifically designed for mouse hover interactions and has a small codebase**.
- Displays documentation in a popup when hovering over symbols.
- Integrates with `posframe` for popup documentation.
- Support moving mouse to the popup by move mouse on it and click.
- Automatically hide the popup when mouse moved away from the symbol, also supoort pressing `C-g` to hide the popup.
- The following enhancements are made for eglot managed buffers.
   - Removed the unnecessary signatures from the document to make doucment more clear.
   - Still keep highlighting the symbol under the cursor.
- An interactive command to pop up document at cursor `eldoc-mouse-pop-doc-at-cursor`, this is helpful when you don't bother to move mouse, but want to check document. I bind it to key sequence `F1 F1`, press Ctrl-G or moving cursor away from the current symbol to close the popup.
- More modes can be easily supported by extends `eldoc-mouse`.
- So far, `eldoc-mouse` works only for GUI Emacs.

## Installation

`eldoc-mouse` is available on [MELPA](https://melpa.org/)

You can install `eldoc-mouse` with the following command.

<kbd>M-x package-install [RET] eldoc-mouse [RET]</kbd>

## Usage
### Enable eldoc-mouse:
Add the following in your Emacs configuration:

``` elisp
(use-package eldoc-mouse
  ;; replace <f1> <f1> to a key you like, "C-h ." maybe. Displaying document on a popup when you press a key.
  :bind (:map eldoc-mouse-mode-map
         ("<f1> <f1>" . eldoc-mouse-pop-doc-at-cursor)) ;; optional
  ;; enable mouse hover for eglot managed buffers, and emacs lisp buffers. ;; optional
  :hook (eglot-managed-mode emacs-lisp-mode))
```
### Supported modes
* eglot-managed-mode
* emacs-lisp-mode
## Customization

You can customize the behavior of eldoc-mouse by adjusting the variables. For instance, you can adjust the delay time between mouse hover and displaying the documentation by changing the eldoc-mouse-mouse-timer settings.

## Extend (Add new mode)
`eldoc-mouse` can be easily extended to support more major/minor modes, finish the following 3 steps to extend it to support a new mode.

1. write an impementation of `eldoc-documentation-functions`. see https://www.gnu.org/software/emacs/manual/html_node/emacs/Programming-Language-Doc.html#index-eldoc_002ddocumentation_002dfunctions. Here's an example implementation for `emacs-lisp-mode`
   ```elisp
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
        nil)) ;; if the expected mode is not available, nil should be returned.
   ```
2. add the function name to the `eldoc-mouse` variable `eldoc-mouse--eldoc-documentation-functions`. for example:
   ```elisp
   (defvar-local eldoc-mouse--eldoc-documentation-functions
    (list #'eldoc-mouse--eglot-eldoc-documentation-function #'eldoc-mouse--elisp-eldoc-documentation-function)
   "The `eldoc-documentation-functions' for `eldoc-mouse-mode'.")
   ```
3. submit a pull request. I'd love to merge it.

## Requirements

    Emacs 30.1 or higher
    posframe version 1.4.0 or higher
    eglot version 1.8 or higher

## License

This package is licensed under the GNU General Public License v3 (GPL-3.0-or-later). See the LICENSE file for details.
Contributing

## Contribution
Feel free to open issues and pull requests for improvements. If you encounter any bugs or have feature requests, please create an issue on the GitHub Issues page.
## TODO 
* make moving mouse to the posframe easier, currently, it requires moving mouse quickly and with a click.
* *(done)* make showing document for mouse hover more generic, not only for eglot managed buffers,  but also for buffers that it makes sense to show something on a posframe for mouse hover. (truly lives up to its name)
* *(done)* an interactive command to popup document on a posframe for the symbol of the cursor.

## Acknowledgments

    lsp-ui: inspiration, the most mouse friendly tool in Emacs.
    eldoc-box: inspiration, the first package for display eglot-eldoc-document to a child frame.
    posframe: for popup document in beautiful child frame. 
    eglot: for offering Language Server Protocol (LSP) support in Emacs.
    Emacs: for being an amazing, extensible text editor.

Author

Huang Feiyu sibadake1@163.com

[melpa-link]: https://melpa.org/#/eldoc-mouse
[melpa-badge]: https://melpa.org/packages/eldoc-mouse-badge.svg
