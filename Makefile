# Test with emacs 24
EMACS     = emacs --batch --quick --directory .
testtargets  = pow-core.el
testlogs = test-pow-app-el.log
compiledfiles = pow-core.elc pow-app-list.elc
mostlyclean += $(testlogs)
clean += $(compiledfiles)

.PHONY: all test mostlyclean clean

all: test

test: compile $(testlogs)

compile: $(compiledfiles)

test-%-el.log: test/test-%.el $(testtargets)
	$(EMACS) --eval '(load-file "$<")' 2>&1 | tee $@

.el.elc:
	$(EMACS) --batch --eval '(byte-compile-file "$<")'

mostlyclean:
	-rm -f $(mostlyclean)

clean: mostlyclean
	-rm -f $(clean)
