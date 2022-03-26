all: gzip
	git archive --format=tar version-`cat quicklisp/version.txt` quicklisp/ > quicklisp.tar
	gzip -fnk9 quicklisp.tar

gzip:
	gzip -fnk9 setup.lisp asdf.lisp

clean:
	rm -f quicklisp.tar quicklisp.tar.gz setup.lisp.gz asdf.lisp.gz

tag:
	test -z "`git status -s quicklisp/ asdf.lisp setup.lisp`"
	git tag version-`cat quicklisp/version.txt`
