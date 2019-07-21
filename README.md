# key-quiz
[![Melpa Status](http://melpa.milkbox.net/packages/key-quiz-badge.svg)](http://melpa.milkbox.net/#/key-quiz)
[![Build Status](https://travis-ci.org/federicotdn/key-quiz.svg?branch=master)](https://travis-ci.org/federicotdn/key-quiz)
![Melpa Status](https://img.shields.io/github/license/federicotdn/key-quiz.svg)

Key Quiz is a game for GNU Emacs (26+) where the player must type in key sequences corresponding to different Emacs commands, chosen at random.

The game includes a variant, called "reverse mode", where the player is given a key sequence and then must answer with the corresponding command.

## Installation
You can install Key Quiz by using the `package-install` command (make sure [MELPA](https://melpa.org/) is included in your package sources):

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `key-quiz` <kbd>[RET]</kbd>

Alternatively, you can just add `key-quiz.el` to your `load-path`.

## Playing
To play Key Quiz, use <kbd>M-x</kbd> `key-quiz` <kbd>[RET]</kbd>.

By default, Key Quiz will quiz the player on keys set in Fundamental mode. This can be changed by setting the variable `key-quiz-use-mode` to another major mode command, such as `'org-mode`. Other aspects of the game can also be customized by configuring variables; see <kbd>M-x</kbd> `customize-group` <kbd>[RET]</kbd> `key-quiz` <kbd>[RET]</kbd>.

You can also play Key Quiz by calling the `key-quiz` function directly. By doing this, it is possible to pass a custom list of key-command pairs from which the player will be quizzed with. For example:
```elisp
(key-quiz nil '(("C-x f b"   . "foo-bar")
                ("C-c b f f" . "bar-foo-foo")
                ("C-c q b"   . "quux-baz")
                ("C-x C-f"   . "find-file")
                ("C-c q f"   . "quuz-foo")
                ("C-c b q"   . "bar-quuz")))
```

More detailed instructions can be found at <kbd>C-h f</kbd> `key-quiz` <kbd>[RET]</kbd>.

## Screenshots
<p align="center">
  <img src="https://user-images.githubusercontent.com/6868935/59036677-1f8e0480-8846-11e9-9087-93058a35ce71.png" alt="Key Quiz" title="" />
</p>

<p align="center">
  <img src="https://user-images.githubusercontent.com/6868935/59046014-89fb7080-8857-11e9-9d19-eed1465c2bac.png" alt="Key Quiz" title="" />
</p>

## Alternatives
- [keywiz.el](https://github.com/deestan/emacs/blob/master/emacs-goodies-el/keywiz.el) - Emacs key sequence quiz by Jesper Harder.
- [shortcutFoo](https://www.shortcutfoo.com/) - Learn shortcuts and commands.

## License
Copyright © 2019 Federico Tedin.

Distributed under the GNU General Public License, version 3.
