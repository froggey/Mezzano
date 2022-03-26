SOURCES=mcclim.texi user-manual.texi reference-manual.texi developer-manual.texi extensions.texi auxiliary-material.texi chap-debugger.texi chap-inspector.texi chap-listener.texi docstrings

INCLUDES=-I docstrings -I examples -I figures

RESULTS=mcclim.pdf mcclim.info mcclim mcclim.html

VPATH=./figures

.PHONY: all clean distclean

all: $(RESULTS)

mcclim.info: $(SOURCES)
	makeinfo $(INCLUDES) --output mcclim.info mcclim.texi

mcclim.pdf: $(SOURCES)
	texi2pdf $(INCLUDES) mcclim.texi

mcclim: $(SOURCES)
	makeinfo $(INCLUDES) --html --css-include=style-multi.css mcclim.texi

mcclim.html: $(SOURCES)
	makeinfo $(INCLUDES) --html --no-split --css-include=style-common.css --css-include=style-single.css mcclim.texi

clean:
	rm -rf mcclim.cp mcclim.cps mcclim.fn mcclim.fns mcclim.toc \
		mcclim.tp mcclim.tps mcclim.vr mcclim.vrs mcclim.aux mcclim.log

distclean: clean
	rm -rf $(RESULTS)
