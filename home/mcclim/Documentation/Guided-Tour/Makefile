SOURCES=guided-tour.tex

GENERATED=hello-world.stamp hello-world-def-app.cut hello-world-defclass.cut hello-world-handle-repaint.cut     \
          scheduler.stamp scheduler-part1.cut scheduler-part2.cut                                               \
          file-browser.stamp file-browser-all.cut                                                               \
          draw-frame.stamp draw-frame-interfacing.cut draw-frame-def-app.cut draw-frame-commands.cut            \
          techno-dep.pstex techno-dep.pstex_t

RESULTS=guided-tour.dvi guided-tour.pdf

.PHONY: all clean

all: $(RESULTS)

guided-tour.dvi: $(SOURCES) $(GENERATED)
	latex guided-tour.tex
	bibtex guided-tour
	latex guided-tour.tex
	latex guided-tour.tex

guided-tour.pdf: $(SOURCES) $(GENERATED)
	pdflatex guided-tour.tex
	bibtex guided-tour
	pdflatex guided-tour.tex
	pdflatex guided-tour.tex

%.pstex: %.fig
	fig2dev -L pstex $(value $@) -b 0 $< $@

%.pstex_t: %.pstex %.fig
	fig2dev -L pstex_t $(value $@) -E 1 -p $^ $@

%.stamp: %.lisp
	awk '/LTAG-end/   { found=found " " active; active="" }     \
                          { if (active!="") print > active ".cut" } \
             /LTAG-start/ { split($$2,foo,":"); active=foo[2] }     \
             END          { print found }' $<
	touch $@

clean:
	rm -f guided-tour.aux guided-tour.bbl guided-tour.log guided-tour.dvi guided-tour.blg   \
           $(GENERATED)                                                                         \
           $(RESULTS)
