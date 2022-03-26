
SOURCES=drei.texi docstrings

.PHONY: all clean distclean

all: drei.pdf drei.info drei.html
drei.info: $(SOURCES)
	makeinfo drei.texi -I docstrings --output drei.info
drei.pdf: $(SOURCES)
	texi2pdf drei.texi -I docstrings
drei.html: $(SOURCES)
	makeinfo drei.texi -I docstrings --html
clean:
	rm -rf drei.cp drei.cps drei.fn drei.fns drei.toc drei.tp drei.tps drei.vr drei.vrs
distclean: clean
	rm -rf drei.info drei.pdf drei.html
