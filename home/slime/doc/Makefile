# This file has been placed in the public domain.
#
# Where to put the info file(s). NB: the GNU Coding Standards (GCS)
# and the Filesystem Hierarchy Standard (FHS) differ on where info
# files belong. The GCS says /usr/local/info; the FHS says
# /usr/local/share/info. Many distros obey the FHS, but people who
# installed their emacs from source probably have a GCS-ish file
# hierarchy.
infodir=/usr/local/info

# What command to use to install info file(s)
INSTALL_CMD=install -m 644

# Info files generated here.
infofiles=slime.info

TEXI = slime.texi contributors.texi

help:
	@echo -e "\
Most important targets:\n\
all               generate info, pdf, and html documents\n\
slime.info        generate the slime.info file\n\
slime.html        generate a single html file\n\
html/index.html   generate on html file per node in html/ directory\n\
html.tgz          create a tarball of all html files\n\
clean             remove generated files"

all: slime.info slime.pdf html/index.html

slime.dvi: $(TEXI)
	texi2dvi slime.texi

slime.ps: slime.dvi
	dvips -o $@ $<

slime.info: $(TEXI)
	makeinfo $<

slime.html: $(TEXI)
	texi2html --css-include=slime.css $<

html/index.html: $(TEXI)
	makeinfo -o html --css-include=slime.css --html $<

html.tgz: html/index.html
	tar -czf $@ html

DOCDIR=/project/slime/public_html/doc
# invoke this like: make CLUSER=heller publish
publish: slime.pdf html.tgz
	scp slime.pdf html.tgz $(CLUSER)@common-lisp.net:$(DOCDIR)
	ssh $(CLUSER)@common-lisp.net "cd $(DOCDIR); tar -zxf html.tgz"

slime.pdf: $(TEXI)
	texi2pdf $<

slime-refcard.pdf: slime-refcard.tex
	texi2pdf $<

install: install-info

uninstall: uninstall-info

# Create contributors.texi, a texinfo table listing all known
# contributors of code.
#
# The gist of this horror show is that the contributor list is piped
# into texinfo-tabulate.awk with one name per line, sorted
# by number of contributions.
LAST_CHANGELOG_COMMIT=ab6d1bd5c9d3c5b4a6299b8c864ce4acfd25cbcc
contributors.texi: ../slime.el Makefile texinfo-tabulate.awk
	git show $(LAST_CHANGELOG_COMMIT):ChangeLog \
	         $(LAST_CHANGELOG_COMMIT):contrib/ChangeLog | \
	sed -ne '/^[0-9]/{s/^[^ ]* *//; s/ *<.*//; p;}' | \
	(cat; git log $(LAST_CHANGELOG_COMMIT).. --format='%aN') | \
	sort | \
	uniq -c | \
	LC_ALL=C sort -nr | \
	sed -e 's/^[^A-Z]*//; /^$$/d' | \
	LC_ALL=C awk -f texinfo-tabulate.awk \
	> $@

#.INTERMEDIATE: contributors.texi

# Debian's install-info wants a --section argument.
install-info: section=$(shell grep INFO-DIR-SECTION $(infofiles) | sed 's/INFO-DIR-SECTION //')
install-info: slime.info
	mkdir -p $(infodir)
	$(INSTALL_CMD) $(infofiles) $(infodir)/$(infofiles)
	@if (install-info --version && \
		install-info --version 2>&1 | sed 1q | grep -i -v debian) >/dev/null 2>&1; then \
		echo "install-info --info-dir=$(infodir) $(infodir)/$(infofiles)";\
		install-info --info-dir="$(infodir)" "$(infodir)/$(infofiles)" || :;\
	else \
		echo "install-info --infodir=$(infodir) --section $(section) $(section) $(infodir)/$(infofiles)" && \
		install-info --infodir="$(infodir)" --section $(section) ${section} "$(infodir)/$(infofiles)" || :; fi

uninstall-info:
	@if (install-info --version && \
		install-info --version 2>&1 | sed 1q | grep -i -v debian) >/dev/null 2>&1; then \
		echo "install-info --info-dir=$(infodir) --remove $(infodir)/$(infofiles)";\
		install-info --info-dir="$(infodir)" --remove "$(infodir)/$(infofiles)" || :;\
	else \
		echo "install-info --infodir=$(infodir) --remove $(infodir)/$(infofiles)";\
		install-info --infodir="$(infodir)" --remove "$(infodir)/$(infofiles)" || :; fi
	rm -f $(infodir)/$(infofiles)

clean:
	rm -f contributors.texi
	rm -f slime.aux slime.cp slime.cps slime.fn slime.fns slime.ky
	rm -f slime.kys slime.log slime.pg slime.tmp slime.toc slime.tp
	rm -f slime.vr slime.vrs
	rm -f slime.info slime.pdf slime.dvi slime.ps slime.html
	rm -f slime-refcard.pdf slime-refcard.log slime-refcard.aux
	rm -rf html html.tgz
