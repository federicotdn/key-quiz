# key-quiz
[![Melpa Status](http://melpa.milkbox.net/packages/key-quiz-badge.svg)](http://melpa.milkbox.net/#/key-quiz)
[![Build Status](https://travis-ci.org/federicotdn/key-quiz.svg?branch=master)](https://travis-ci.org/federicotdn/key-quiz)
![Melpa Status](https://img.shields.io/github/license/federicotdn/key-quiz.svg)

Key Quiz is a game for GNU Emacs (26+) where the player must type in key sequences corresponding to different Emacs commands, chosen at random.

The game includes a variant, called "reverse mode", where the user is given a key sequence and then must answer with the corresponding command.

## Installation
You can install Key Quiz by using the `package-install` command (make sure [MELPA](https://melpa.org/) is included in your package sources):

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `key-quiz` <kbd>[RET]</kbd>

Alternatively, you can just add `key-quiz.el` to your `load-path`.

## Playing
To play Key Quiz, use <kbd>M-x</kbd> `key-quiz` <kbd>[RET]</kbd>. More detailed instructions can be found at <kbd>C-h f</kbd> `key-quiz` <kbd>[RET]</kbd>.

## Screenshots
<p align="center">
  <img src="https://user-images.githubusercontent.com/6868935/59036677-1f8e0480-8846-11e9-9087-93058a35ce71.png" alt="Key Quiz" title="" />
</p>

<p align="center">
  <img src="https://user-images.githubusercontent.com/6868935/59046014-89fb7080-8857-11e9-9d19-eed1465c2bac.png" alt="Key Quiz" title="" />
</p>

## Alternatives
- [keywiz.el](https://github.com/deestan/emacs/blob/master/emacs-goodies-el/keywiz.el) - Emacs key sequence quiz by Jesper Harder.

## License
Copyright © 2019 Federico Tedin.

Distributed under the GNU General Public License, version 3.
