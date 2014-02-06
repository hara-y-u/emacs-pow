EMACS     = emacs --batch --quick --directory .
WGET      = wget --timestamping
DIRECTORY = http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/
cached    = ert.el
clean    += $(cached)

testtargets  = pow.el
testlogs = test-pow-app-el.log
mostlyclean += $(testlogs)

.PHONY: all test mostlyclean clean

all: test

# Testing with emacs 22
test: $(testlogs)

test-%-el.log: test/test-%.el ert.el cl.el $(testtargets)
	$(EMACS) --eval '(load-file "$<")' 2>&1 | tee $@

ert.el:
	$(WGET) --output-document=$@ \
	$(DIRECTORY)ert.el?id=221b77ae6ed02721c1a5356fbf2d44856c0f3f6b
cl.el:
	$(WGET) --output-document=$@ \
	$(DIRECTORY)cl.el?h=old-branches/EMACS_22_BASE

mostlyclean:
	-rm -f $(mostlyclean)

clean: mostlyclean
	-rm -f $(clean)
