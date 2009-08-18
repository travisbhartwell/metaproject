EMACS23 = /usr/bin/emacs
EMACS22 = /usr/bin/emacs22

clean :
	rm -f lisp/*.elc

byte-compile : clean
	$(EMACS23) --batch -q --no-site-file --eval '(byte-compile-file "lisp/metaproject.el")'

byte-compile22 : clean
	$(EMACS22) --batch -q --no-site-file --eval '(byte-compile-file "lisp/metaproject.el")'

.PHONY: test
test : byte-compile
	test/run-test.sh $(EMACS23)

test22 : byte-compile22
	test/run-test.sh $(EMACS22)
