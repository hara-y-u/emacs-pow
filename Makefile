# Test with emacs 24
EMACS     = emacs --batch --quick --directory .
testtargets  = pow-core.el
testlogs = test-pow-core-el.log
compiledfiles = pow-core.elc pow-app-list.elc
mostlyclean += $(testlogs)
clean += $(compiledfiles)

.PHONY: all test compile emacs-version mostlyclean clean

all: test

test: emacs-version compile $(testlogs)

emacs-version:
	@echo **TESTING ENVIRONMENT**
	@$(EMACS) --version
	@echo

test-%-el.log: test/test-%.el $(testtargets)
	@echo **TEST START**
	$(EMACS) --eval '(load-file "$<")' 2>&1 | tee $@

compile: $(compiledfiles)

.el.elc:
	@echo **BYTE COMPILE**
	$(EMACS) --batch --eval '(byte-compile-file "$<")'
	@echo

mostlyclean:
	-rm -f $(mostlyclean)

clean: mostlyclean
	-rm -f $(clean)
