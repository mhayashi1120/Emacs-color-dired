EMACS = emacs

check: compile
	$(EMACS) -q -batch -eval "(check-declare-file \"color-dired.el\")" 2>&1 | grep -e "Checking"
	$(EMACS) -q -batch -L . -l color-dired.el -l color-dired-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l color-dired.elc -l color-dired-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile color-dired.el

clean:
	@rm -f color-dired.elc
