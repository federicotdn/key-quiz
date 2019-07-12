NOOUTPUT = { ! grep '^'; }

define CUSTOM_GAME
(key-quiz nil (quote (("C-f o o" . "foo-cmd")
                      ("C-b a r" . "bar-cmd")
                      ("C-M-t e s t" . "test-cmd")
	              ("M-m e t a" . "meta-cmd"))))
endef
export CUSTOM_GAME

check:
	emacs --batch --eval '(byte-compile-file "key-quiz.el")' 2>&1 | $(NOOUTPUT)
	emacs --batch --eval '(find-file "key-quiz.el")' \
		      --eval '(checkdoc-current-buffer)' 2>&1 | $(NOOUTPUT)

run:
	emacs -q -l key-quiz.el --eval '(key-quiz)'

run-reverse:
	emacs -q -l key-quiz.el --eval '(key-quiz t)'

run-custom:
	emacs -q -l key-quiz.el --eval "$$CUSTOM_GAME"
