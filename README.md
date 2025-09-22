# eldoc-mouse

`eldoc-mouse` is an Emacs package that enhances the `eldoc` functionality by displaying documentation in a child frame at the mouse point using [posframe](https://github.com/tumashu/posframe) when the mouse hovers over a symbol in an `eglot` managed buffer. It integrates with `posframe` to provide popup documentation and features a debounced hover mechanism to prevent excessive requests to the LSP server.
[Eldoc-mouse Demo at Youtube](https://youtu.be/XFAc4WyiJjI)


## Features
- Displays documentation in a child frame when hovering over symbols in `eglot` managed buffers.
- Integrates with `posframe` for popup documentation.
- Avoids spamming the LSP server by debouncing hover events.
- Works in `prog-mode` buffers to show documentation for the symbol under the mouse point.
- Removed the unnecessary signatures from the document to make doucment more clear.
- Still keep highlighting the symbol under the cursor.

## Installation

Clone this repository and load it into your Emacs configuration:

```sh
git clone https://github.com/huangfeiyu/eldoc-mouse.git
```
Then, in your Emacs configuration:

``` elisp
(add-to-list 'load-path "/path/to/eldoc-mouse/")
(require 'eldoc-mouse)
```

## Usage
### Enable eldoc-mouse:
Add the following in your Emacs configuration:
```
(eldoc-mouse-enable)
```
alternatively, run the command interactively:
```
M-X: eldoc-mouse-enable
```
### Disable eldoc-mouse:
If you want to disable eldoc-mouse, you can do so with:
```
M-X: eldoc-mouse-disable
```
## Customization

You can customize the behavior of eldoc-mouse by adjusting the variables or adding additional hooks. For instance, you can adjust the delay time between mouse hover and displaying the documentation by changing the eldoc-mouse-mouse-timer settings.
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
* make showing document for mouse hover more generic, not only for eglot managed buffers,  but also for buffers that it makes sense to show something on a posframe for mouse hover.
* an interactive command to popup document on a posframe for the symbol of the cursor.

## Acknowledgments

    lsp-ui: inspiration, the most mouse friendly tool in Emacs.
    eldoc-box: inspiration, the first package for display eglot-eldoc-document to a child frame.
    posframe: for popup document in beautiful child frame. 
    eglot: for offering Language Server Protocol (LSP) support in Emacs.
    Emacs: for being an amazing, extensible text editor.

Author

Huang Feiyu sibadake1@163.com
