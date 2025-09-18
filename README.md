# eldoc-mouse

`eldoc-mouse` is an Emacs package that enhances the `eldoc` functionality by displaying documentation in a child frame when the mouse hovers over a symbol in an `eglot`-managed buffer. It integrates with `eldoc-box` to provide popup documentation and features a debounced hover mechanism to prevent excessive requests to the LSP server.
[Screencast_20250918_110121.webm](https://github.com/user-attachments/assets/6bb80bee-dc2b-4d36-b8a4-4d416e0a6100)


## Features
- Displays documentation in a child frame when hovering over symbols in `eglot`-managed buffers.
- Integrates with `eldoc-box` for popup documentation.
- Avoids spamming the LSP server by debouncing hover events.
- Works in `prog-mode` buffers to show documentation for the symbol under the mouse cursor.

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
Enabled by default, alternatively, run the command:
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
    eldoc-box version 2.1.1 or higher
    eglot version 1.8 or higher

## License

This package is licensed under the GNU General Public License v3 (GPL-3.0-or-later). See the LICENSE file for details.
Contributing

## Contribution
Feel free to open issues and pull requests for improvements. If you encounter any bugs or have feature requests, please create an issue on the GitHub Issues page.
## Acknowledgments

    eldoc-box: for providing popup documentation frames.
    eglot: for offering Language Server Protocol (LSP) support in Emacs.
    Emacs: for being an amazing, extensible text editor.

Author

Huang Feiyu sibadake1@163.com
