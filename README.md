# pyregexp

pyregexp is a command for emacs which enables you to use Python regular expressions and either a Python string or a Python expression for doing replacements.

While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches:

![entering regexp](https://github.com/benma/pyregexp/raw/master/screenshots/pyregexp0A.png)

While constructing the replacement in the minibuffer, you get live visual feedback for the replacements:

![entering replacement](https://github.com/benma/pyregexp/raw/master/screenshots/pyregexp0B.png)

## Installation

You need a Python interpreter.
Add the following code to your init file. Of course you can select your own key.

```Lisp
(add-to-list 'load-path "which-folder-pyregexp-file-in/")
(require 'pyregexp)
(define-key global-map (kbd "C-c r") 'pyregexp-replace)
```

To customize, use `M-x customize-group [RET] pyregexp`. You can specify how to invoke the Python interpreter by modifying the `pyregexp-command-prefix` variable. The default is `python ...`.

## Examples

### Example 1
![Example 1](https://github.com/benma/pyregexp/raw/master/screenshots/pyregexp1.png)
### Example 2
![Example 2](https://github.com/benma/pyregexp/raw/master/screenshots/pyregexp2.png)
### Example 3
![Example 3](https://github.com/benma/pyregexp/raw/master/screenshots/pyregexp3.png)
### Example 4
![Example 4](https://github.com/benma/pyregexp/raw/master/screenshots/pyregexp4.png)