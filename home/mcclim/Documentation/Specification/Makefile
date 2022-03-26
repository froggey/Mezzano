NAME=clim

TEXFILES=*.tex
PDF=$(shell ./find-graphics $(TEXFILES))

all : $(NAME).pdf

%.pdf: %.epsi
	epstopdf $<

$(NAME).pdf: $(TEXFILES) $(PDF)
	pdflatex $(NAME).tex
	makeindex $(NAME)
	pdflatex $(NAME).tex
	pdflatex $(NAME).tex

clean:
	rm -f *.pdf *.toc *.out *.log *~ *.idx *.ilg *.ind *.aux
