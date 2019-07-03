NOOUTPUT = { ! grep '^'; }

check:
	emacs --batch --eval '(byte-compile-file "key-quiz.el")' 2>&1 | $(NOOUTPUT)
	emacs --batch --eval '(find-file "key-quiz.el")' \
		      --eval '(checkdoc-current-buffer)' 2>&1 | $(NOOUTPUT)

run:
	emacs -q -l key-quiz.el
