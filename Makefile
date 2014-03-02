# Test with emacs 24
EMACS     = emacs --batch --quick --directory .
WGET      = wget --timestamping
DIRECTORY = http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/

testtargets  = pow-core.el
testlogs = test-pow-app-el.log
compiledfiles = pow-core.elc pow-app-list.elc
mostlyclean += $(testlogs)

.PHONY: all test mostlyclean clean

all: test compile

test: $(testlogs)

compile: $(compiledfiles)

test-%-el.log: test/test-%.el $(testtargets)
	$(EMACS) --eval '(load-file "$<")' 2>&1 | tee $@

.el.elc:
	$(EMACS) --batch --eval '(byte-compile-file "$<")'

mostlyclean:
	-rm -f $(mostlyclean)

clean: mostlyclean
	-rm -f $(clean)
