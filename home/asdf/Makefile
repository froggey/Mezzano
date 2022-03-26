# Configuring this Makefile for your personal use:
# Set environment variable ASDF_TEST_LISPS to a space-separated list of values
# (see "defaultlisps" below, for an example).
# If you have a special way to find libraries that are used in the build and
# test process, you may bind ASDF_DEVEL_SOURCE_REGISTRY to a source registry to
# use (using the environment variable syntax), or bind it to "override" to use
# your normal CL source registry. Otherwise, it will use local copies of
# everything.

system		:= "asdf"
webhome_private := common-lisp.net:/project/asdf/public_html/
webhome_public	:= "http://common-lisp.net/project/asdf/"
clnet_home      := "/project/asdf/public_html/"
sourceDirectory := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

#### Common Lisp implementations available for testing.
## export ASDF_TEST_LISPS to override the default list of such implementations,
## or specify a lisps= argument at the make command-line
defaultLisps = ccl clisp sbcl ecl ecl_bytecodes cmucl abcl scl allegro lispworks allegromodern gcl xcl mkcl
ifdef ASDF_TEST_LISPS
lisps ?= ${ASDF_TEST_LISPS}
else
lisps ?= ${defaultLisps}
endif
ifdef ASDF_UPGRADE_TEST_LISPS
  ulisps ?= ${ASDF_UPGRADE_TEST_LISPS}
else
  ifdef ASDF_TEST_LISPS
    ulisps ?= ${ASDF_TEST_LISPS}
  else
    ulisps ?= ${defaultLisps}
  endif
endif

version := $(shell cat "version.lisp-expr")
#$(info $$version is [${version}])
version := $(patsubst "%",%,$(version))
#$(info $$version is [${version}])
fullversion := $(shell git describe --tags --match "[0-9][.][0-9]*" 2> /dev/null || echo $(version))

## grep for #+/#- features in the test/ directory to see plenty of disabled tests on some platforms
## NOT SUPPORTED BY OUR AUTOMATED TESTS:
##	cormancl genera lispworks-personal-edition rmcl
## Some are manually tested once in a while.
ifdef ASDF_TEST_SYSTEMS
s ?= ${ASDF_TEST_SYSTEMS}
endif

ifdef ASDF_DEVEL_SOURCE_REGISTRY
ifeq ($(ASDF_DEVEL_SOURCE_REGISTRY), override)
# do nothing... Use the user's CL_SOURCE_REGISTRY
else
export CL_SOURCE_REGISTRY = ${ASDF_DEVEL_SOURCE_REGISTRY}
endif
else # no ASDF_DEVEL_SOURCE_REGISTRY
export CL_SOURCE_REGISTRY = ${sourceDirectory}/:${sourceDirectory}/uiop/:${sourceDirectory}/ext//
endif
#$(error "CL_SOURCE_REGISTRY is ${CL_SOURCE_REGISTRY}")
sys := $(shell uname -s)
ifneq (,$(findstring CYGWIN,$(sys)))
CL_SOURCE_REGISTRY := $(shell cygpath -pw "${CL_SOURCE_REGISTRY}")
endif


l ?= sbcl

ABCL ?= abcl
ALLEGRO ?= alisp
ALLEGROMODERN ?= mlisp
CCL ?= ccl
CLISP ?= clisp
CMUCL ?= cmucl
ECL ?= ecl
GCL ?= gcl
LISPWORKS ?= lispworks
MKCL ?= mkcl
SBCL ?= sbcl
SCL ?= scl
XCL ?= xcl

header_lisp := header.lisp
driver_lisp := uiop/package.lisp uiop/common-lisp.lisp uiop/utility.lisp uiop/version.lisp uiop/os.lisp uiop/pathname.lisp uiop/filesystem.lisp uiop/stream.lisp uiop/image.lisp uiop/lisp-build.lisp uiop/launch-program.lisp uiop/run-program.lisp uiop/configuration.lisp uiop/backward-driver.lisp uiop/driver.lisp
defsystem_lisp := upgrade.lisp session.lisp component.lisp operation.lisp system.lisp system-registry.lisp action.lisp lisp-action.lisp find-component.lisp forcing.lisp plan.lisp operate.lisp find-system.lisp parse-defsystem.lisp bundle.lisp concatenate-source.lisp package-inferred-system.lisp output-translations.lisp source-registry.lisp backward-internals.lisp backward-interface.lisp interface.lisp user.lisp footer.lisp
all_lisp := $(header_lisp) $(driver_lisp) $(defsystem_lisp)

print-%  : ; @echo $* = $($*)

# Making ASDF itself should be our first, default, target:
build/asdf.lisp: $(all_lisp)
	mkdir -p build
	rm -f $@
	cat $(all_lisp) > $@

ext:
	git submodule update --init

noext:
	git submodule deinit .

# This quickly locates such mistakes as unbalanced parentheses:
load: build/asdf.lisp
	./test/run-tests.sh -t $l $(all_lisp)

install: archive

bump: bump-version
	git commit -a -m "Bump version to $$(eval a=$$(cat version.lisp-expr) ; echo $$a)"
	temp=$$(cat version.lisp-expr); temp="$${temp%\"}"; temp="$${temp#\"}"; git tag $$temp

bump-version: build/asdf.lisp
	./bin/bump-version ${v}

driver-files:
	@echo $(driver_lisp)

defsystem-files:
	@echo $(defsystem_lisp)

# FIXME: needs rewrite
#archive: build/asdf.lisp
#	./bin/asdf-builder make-and-publish-archive

archive: build/asdf.lisp
	$(eval UIOPDIR := "uiop-$(version)")
	mkdir -p build/$(UIOPDIR) 	# UIOP tarball
	cp -pHux uiop/README.md uiop/uiop.asd uiop/asdf-driver.asd ${driver_lisp} uiop/contrib/debug.lisp build/$(UIOPDIR)
	tar zcf "build/uiop-${version}.tar.gz" -C build $(UIOPDIR)
	rm -r build/$(UIOPDIR)
	$(eval ASDFDIR := "asdf-$(version)")
	mkdir -p build/$(ASDFDIR) # asdf-defsystem tarball
	cp -pHux build/asdf.lisp asdf.asd version.lisp-expr header.lisp README.md ${defsystem_lisp} build/$(ASDFDIR)
	tar zcf "build/asdf-defsystem-${version}.tar.gz" -C build $(ASDFDIR)
	rm -r build/$(ASDFDIR)
	git archive --worktree-attributes --prefix="asdf-$(version)/" --format=tar -o "build/asdf-${version}.tar" ${version} #asdf-all tarball
	gzip "build/asdf-${version}.tar"
	cp "build/asdf.lisp" "build/asdf-${version}.lisp"

publish-archive:
	rsync --times --chmod=a+rX,ug+w "build/uiop-${version}.tar.gz" "build/asdf-defsystem-${version}.tar.gz" \
"build/asdf-${version}.tar.gz" "build/asdf-${version}.lisp" common-lisp.net:/project/asdf/public_html/archives/
	ssh common-lisp.net "cd /project/asdf/public_html/archives/; ln -sf uiop-${version}.tar.gz uiop.tar.gz; ln -sf asdf-defsystem-${version}.tar.gz asdf-defsystem.tar.gz; ln -sf asdf-${version}.tar.gz asdf.tar.gz; ln -sf asdf-${version}.lisp asdf.lisp"

### Count lines separately for asdf-driver and asdf itself:
wc:
	@wc $(driver_lisp) | sort -n ; echo ; \
	wc $(header_lisp) $(defsystem_lisp) | sort -n ; \
	echo ; \
	wc $(header_lisp) $(driver_lisp) $(defsystem_lisp) | tail -n 1

push:
	git status
	git push --tags cl.net release master
	git push --tags github release master
	git fetch
	git status

# doc:
# 	${MAKE} -C doc
# don't have the toolchain to build docs installed...
doc: ;
website:
	${MAKE} -C doc website
	${MAKE} -C uiop/doc website

clean_dirs = $(sourceDirectory)
clean_extensions = fasl dfsl cfsl fasl fas lib dx32fsl lx64fsl lx32fsl ufasl o bak x86f vbin amd64f sparcf sparc64f hpf hp64f

clean:
	@for dir in $(clean_dirs); do \
	     if test -d $$dir; then \
		 echo Cleaning $$dir; \
		 for ext in $(clean_extensions); do \
		     find $$dir \( -name "*.$$ext" \) \
		    -and -not -path \""*/.git/*"\" \
			  -and -not -path \""*/_darcs/*"\" \
			  -and -not -path \""*/tags/*"\" -delete; \
		done; \
	     fi; \
	    echo "Cleaned $$dir"; \
	done
	echo "Done with cleaning loop."
	rm -rf build/ LICENSE test/try-reloading-dependency.asd test/hello-world-example asdf.lisp
	rm -rf test/hello-world-example.exe test/mkcl_*.dll # needed only on MS-Windows
	${MAKE} -C doc clean
	${MAKE} -C uiop/doc clean

mrproper:
	git clean -xfd

test-upgrade: build/asdf.lisp show-version
	./test/run-tests.sh -u ${l}
u: test-upgrade

test-clean-load: build/asdf.lisp show-version
	./test/run-tests.sh -c ${l}

show-version:
	@echo "Building and testing asdf $(fullversion)"

# test-glob has been replaced by t, and lisp by l, easier to type
test-lisp: build/asdf.lisp show-version
	@cd test; ./run-tests.sh ${l} ${t}

t: test-lisp

test: doc test-lisp test-clean-load test-load-systems

test-load-systems: build/asdf.lisp show-version
	./test/run-tests.sh -l ${l} ${s}

test-all-lisps: test-load-systems test-all-clean-load test-all-lisp test-all-upgrade

test-all-clean-load:
	@for lisp in ${lisps} ; do ${MAKE} test-clean-load l=$$lisp || exit 1 ; done

test-all-lisp:
	@for lisp in ${lisps} ; do ${MAKE} test-lisp l=$$lisp || exit 1 ; done

test-all-upgrade:
	@for lisp in ${ulisps} ; do ${MAKE} test-upgrade l=$$lisp || exit 1 ; done

test-all-no-upgrade: doc test-load-systems test-all-clean-load test-all-lisp

test-all: test-all-no-upgrade test-all-upgrade

test-all-lisp-no-stop:
	@for lisp in ${lisps} ; do ${MAKE} test-lisp l=$$lisp ; done ; :

test-all-upgrade-no-stop:
	@for lisp in ${ulisps} ; do ${MAKE} test-upgrade l=$$lisp ; done ; :

test-all-no-upgrade-no-stop: doc test-load-systems test-all-clean-load test-all-lisp-no-stop
	make --quiet check-all-test-results

test-all-no-stop: doc test-load-systems test-all-clean-load test-all-lisp-no-stop test-all-upgrade-no-stop
	make --quiet check-all-results

check-all-test-results:
	@A="`grep -L '[5-9][0-9] passing and 0 failing' build/results/*-test.text`" ; \
	if [ -n "$$A" ] ; then \
		echo "Unexpected test failures on these implementations:" ; \
		echo "$$A" ; \
		exit 1 ; \
	fi

check-all-upgrade-results:
	@A="`grep -L 'Upgrade test succeeded for ' build/results/*-upgrade.text`" ; \
	if [ -n "$$A" ] ; then \
		echo "Unexpected upgrade failures on these implementations:" ; \
		echo "$$A" ; \
		exit 1 ; \
	fi

check-all-results:
	@r=0 ; \
	make --quiet check-all-test-results || r=1 ; \
	make --quiet check-all-upgrade-results || r=1 ; \
	exit $r

extract: extract-all-tagged-asdf
extract-all-tagged-asdf: build/asdf.lisp
	./test/run-tests.sh -H


# Delete wrongful tags from local repository
fix-local-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git tag -d $$i ; done

# Delete wrongful tags from remote repository
fix-remote-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git push $${REMOTE:-cl.net} :refs/tags/$$i ; done

release-push:
	git checkout master
	git merge release
	git checkout release
	git merge master
	git checkout master

TODO:
	exit 2

release: TODO test-all test-on-other-machines-too debian-changelog debian-package send-mail-to-mailing-lists

.PHONY: install archive push doc website clean mrproper show-version \
	test-forward-references test test-lisp test-upgrade test-forward-references \
	test-all test-all-lisps test-all-no-upgrade \
	debian-package release \
	replace-sbcl-asdf replace-ccl-asdf \
	fix-local-git-tags fix-remote-git-tags wc wc-driver wc-asdf \
	list-source-registry \
	ext noext

# debug the source registry that will be used to execute commands from this Makefile.
#list-source-registry:
#	${sourceDirectory}/bin/asdf-builder re '(uiop:writeln (sort (alexandria:hash-table-alist asdf::*source-registry*) `string< :key `car))'

# RELEASE or PUSH checklist:
# make test-all
# make test-load-systems s=fare-all
# make bump v=3.0
# edit debian/changelog # RELEASE only...
# git commit
# git tag 3.0 # for example ...
# make debian-package
# git push
# git push origin 3.0 # for example...
# everything from here for RELEASE only
# make release-push archive website debian-package
# dput mentors ../*.changes
# send debian mentors request
# send announcement to asdf-announce, asdf-devel, etc.
# Move all fixed bugs from Fix Committed -> Fix Released on launchpad
#
## Users don't release as above, only maintainers do.
## Users, all you need to do is: make
## Vendors, you may want to test your implementation with: make test l=sbcl
