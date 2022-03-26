#!/bin/sh

# run-tests {lisp invocation} {scripts-regex}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

usage () {
    echo "$0 [lisp invocation] [scripts-regex]"
    echo " - read lisp forms one at a time from matching scripts"
    echo " - quit with exit status 0 on getting eof"
    echo " - quit with exit status >0 if an unhandled error occurs"
    echo " you need to supply the .script in the second argument"
    echo " lisps include abcl, ccl (clozure),"
    echo "    allegro, allegro8, allegromodern, allegromodern8,"
    echo "    allegro_s, allegro8_s, allegromodern_s, allegromodern8_s (SMP variants)"
    echo "    allegro_64, allegro8_64, allegromodern_64, allegromodern8_64 (64-bit variants),"
    echo "    allegro_64_s, allegro8_64_s, allegromodern_64_s, allegromodern8_64_s, (SMP, 64-bit variants)"
    echo "    clasp, clisp, cmucl, ecl, gcl, sbcl, scl and xcl."
    echo " To configure the script, you may set environment variables to point to the various lisp runtimes."
    echo " Allegro CL is a special case: instead of setting environment variables for the specific runtime"
    echo "   locations, you may simply specify the Allegro install directories using these variables:"
    echo "     ALLEGRO64DIR, ALLEGRO64SDIR (64-bit Allegro and SMP Allegro, respectively), ALLEGRODIR, and"
    echo "     ALLEGROSDIR."
    echo "OPTIONS:"
    echo "    -c -- clean load test."
    echo "    -d -- debug mode."
    echo "    -t -- test interactively."
    echo "    -h -- show this message."
    echo "    -u -- upgrade tests."
    echo "    -l -- load systems tests."
    echo "    -H -- extract all asdf versions to upgrade from."
    echo "    -u -- upgrade tests, we already told you."
}

unset DEBUG_ASDF_TEST upgrade clean_load load_systems test_interactively extract_all

SHELL=/bin/sh
export SHELL DEBUG_ASDF_TEST GCL_ANSI ASDF_OUTPUT_TRANSLATIONS

if [ -n "$ALLEGRO64DIR" ] ; then
    ALLEGRO_64=${ALLEGRO64DIR}/alisp
    ALLEGRO8_64=${ALLEGRO64DIR}/alisp8
    ALLEGROMODERN_64=${ALLEGRO64DIR}/mlisp
    ALLEGROMODERN8_64=${ALLEGRO64DIR}/mlisp8
fi
if [ -n "$ALLEGRO64SDIR" ] ; then
    ALLEGRO_64_S=${ALLEGRO64SDIR}/alisp
    ALLEGRO8_64_S=${ALLEGRO64SDIR}/alisp8
    ALLEGROMODERN_64_S=${ALLEGRO64SDIR}/mlisp
    ALLEGROMODERN8_64_S=${ALLEGRO64SDIR}/mlisp8
fi
if [ -n "$ALLEGRODIR" ] ; then
    ALLEGRO=${ALLEGRODIR}/alisp
    ALLEGRO8=${ALLEGRODIR}/alisp8
    ALLEGROMODERN=${ALLEGRODIR}/mlisp
    ALLEGROMODERN8=${ALLEGRODIR}/mlisp8
fi
if [ -n "$ALLEGROSDIR" ] ; then
    ALLEGRO_S=${ALLEGROSDIR}/alisp
    ALLEGRO8_S=${ALLEGROSDIR}/alisp8
    ALLEGROMODERN_S=${ALLEGROSDIR}/mlisp
    ALLEGROMODERN8_S=${ALLEGROSDIR}/mlisp8
fi


while getopts "cdtHulhu" OPTION
do
    case $OPTION in
        c)
            clean_load=t
            ;;
        d)
            DEBUG_ASDF_TEST=t
            ;;
        t)
            test_interactively=t
            ;;
        h)
            usage
            exit 1
            ;;
        u)
            upgrade=t
            ;;
        l)
            load_systems=t
            ;;
        H)
            extract_all=t
            ;;
    esac
done
shift $(($OPTIND - 1))

if [ x"$1" = "xhelp" ]; then
    usage
    exit 1
fi
lisp=${1:-sbcl} ; shift


ECHO () { printf '%s\n' "$*" ;}
ECHOn () { printf '%s' "$*" ;}
DBG () { ECHO "$*" >& 2 ;}
simple_term_p () {
  case "$1" in *[!a-zA-Z0-9-+_,.:=%/]*) return 1 ;; *) return 0 ;; esac
}
kwote0 () { ECHOn "$1" | sed -e "s/\([\\\\\"\$\`]\)/\\\\\\1/g" ;}
kwote1 () { if simple_term_p "$1" ; then ECHOn "$1"
  else ECHOn "\"$(kwote0 "$1")\"" ; fi ;}
kwote () { ( set +x
  k="" ; for i ; do ECHOn "$k" ; kwote1 "$i" ; k=" " ; done ; echo
) }
DO () { kwote "$@" ; "$@" ; }

do_tests () {
  if [ -z "$*" ]; then
       scripts="*.script"
  else
       scripts="$*"
  fi
  env | grep -i asdf
  ## We go through great lengths to avoid " in the command line,
  ## the quoting of which many Windows implementations get wrong.
  ## While we're at it, we also avoid spaces and backslashes.
  ( DO $bcmd $eval '(or`,#.(load(string`|script-support.lisp|))#.(asdf-test::compile-asdf-script))' )
  if [ $? -ne 0 ] ; then
    echo "Compilation FAILED" >&2
    echo "you can retry compilation with:" >&2
    echo ./test/run-tests.sh $lisp >&2
    echo "or more interactively (and maybe with rlwrap or in emacs), start with:" >&2
    echo "$icmd" >&2
    echo "then copy/paste:" >&2
    echo '(load "test/script-support.lisp") (asdf-test::compile-asdf-script)' >&2
  else
    echo "Compiled OK" >&2
    test_count=0
    test_pass=0
    test_fail=0
    failed_list=""
    for i in $scripts ;
    do
      echo "Testing: $i" >&2
      test_count=`expr "$test_count" + 1`
      rm -f ~/.cache/common-lisp/"`pwd`"/* || true
      if DO $bcmd $eval "'(#.(load(string'|script-support.lisp|))#.(asdf-test::load-asdf)#.(asdf-test::frob-packages)#.(asdf-test:run-test-script'|$i|))" ; then
        echo "Using $command, $i passed" >&2
	test_pass=`expr "$test_pass" + 1`
      else
        echo "Using $command, $i failed" >&2
	test_fail=`expr "$test_fail" + 1`
	failed_list="$failed_list $i"
        echo "you can retry compilation with:" >&2
        echo ./test/run-tests.sh $lisp $i >&2
        echo "or more interactively (and maybe with rlwrap or in emacs), start with:" >&2
        echo "(cd test ; $icmd )" >&2
        echo "then copy/paste:" >&2
        echo "'(#.(load \"script-support.lisp\") #.(asdf-test::da) #.(load-asdf) #.(frob-packages) #.(load \"$i\"))" >&2
      fi
      echo >&2
      echo >&2
    done
    echo >&2
    echo "-#---------------------------------------" >&2
    echo "Using $command" >&2
    echo "Ran $test_count tests: " >&2
    echo "  $test_pass passing and $test_fail failing" >&2
    if [ $test_fail -eq 0 ] ; then
	echo "all tests apparently successful" >&2
        echo success > ../build/results/status
    else
	echo "failing test(s): $failed_list" >&2
    fi
    echo "-#---------------------------------------" >&2
    echo >&2
  fi
}

#
# not used currently but leave here for future reference.
#
case $(uname) in
    CYGWIN*|MSYS_NT*) os=windows ;;
    Darwin*) os=macos ;;
    Linux*) os=linux ;;
    *) os=unknown ;;
esac



# terminate on error
set -e

command= flags= nodebug= eval= bcmd= icmd=
case "$lisp" in
  abcl)
    command="${ABCL:-abcl}"
    flags="--noinit --nosystem --noinform"
    eval="--eval"
    ;;
  allegro*)
    case "$lisp" in
      allegro) command="${ALLEGRO:-alisp}" ;;
      allegro8) command="${ALLEGRO8:-alisp8}" ;;
      allegromodern) command="${ALLEGROMODERN:-mlisp}" ;;
      allegromodern8) command="${ALLEGROMODERN8:-mlisp8}" ;;
      allegro_s) command="${ALLEGRO_S:-alisp_s}" ;;
      allegro8_s) command="${ALLEGRO8_S:-alisp8_s}" ;;
      allegromodern_s) command="${ALLEGROMODERN_S:-mlisp_s}" ;;
      allegromodern8_s) command="${ALLEGROMODERN8_S:-mlisp8_s}" ;;
      allegro_64) command="${ALLEGRO_64:-alisp_64}" ;;
      allegro8_64) command="${ALLEGRO8_64:-alisp8_64}" ;;
      allegromodern_64) command="${ALLEGROMODERN_64:-mlisp_64}" ;;
      allegromodern8_64) command="${ALLEGROMODERN8_64:-mlisp8_64}" ;;
      allegro_64_s) command="${ALLEGRO_64_S:-alisp_64_s}" ;;
      allegro8_64_s) command="${ALLEGRO8_64_S:-alisp8_64_s}" ;;
      allegromodern_64_s) command="${ALLEGROMODERN_64_S:-mlisp_64_s}" ;;
      allegromodern8_64_s) command="${ALLEGROMODERN8_64_S:-mlisp8_64_s}" ;;
    esac
    # For the sake of the lisp-invocation library, re-export these
    # ALLEGRO=$command ; export ALLEGRO ;
    # echo ALLEGRO=$ALLEGRO
    flags="-q"
    nodebug="-batch"
    if [ "$os" = windows ] ; then
        adir=$(dirname "${command}") ;
        allegroName=$(basename "${command}" ".exe") ;
        if [[ ${allegroName: -1} == "8" ]] ; then build=build ; else build=buildi ; fi ;
        # this takes somewhat unjustifiable advantage of the fact that
        # the Allegro images have the same name (with .dxl extension)
        # as the corresponding executables.  the "build" executable
        # runs an ACL image in the current terminal instead of a
        # separate window, as is normal on Windows.
        bcmd="${adir}/${build}.exe -I ${adir}/${allegroName}.dxl $flags" ;
    fi
    eval="-e" ;;
  ccl)
    command="${CCL:-ccl}"
    flags="--no-init --quiet"
    nodebug="--batch"
    eval="--eval" ;;
  clasp)
    command="${CLASP:-clasp}"
    flags="--norc --noinit"
    eval="--eval" ;;
  clisp)
    command="${CLISP:-clisp}"
    flags="-norc --silent -ansi -I "
    nodebug="-on-error exit"
    eval="-x" ;;
  cmucl)
    # cmucl likes to have its executable called lisp, but so does scl
    # Please use a symlink or an exec ... "$@" trampoline script.
    command="${CMUCL:-cmucl}"
    flags="-noinit"
    nodebug="-batch"
    eval="-eval" ;;
  ecl)
    command="${ECL:-ecl}"
    flags="-norc -load sys:cmp"
    eval="-eval" ;;
  ecl_bytecodes)
    command="${ECL:-ecl}"
    flags="-norc -eval (ext::install-bytecodes-compiler)"
    eval="-eval" ;;
  gcl)
    GCL_ANSI=t
    command="${GCL:-gcl}"
    flags=""
    nodebug="-batch"
    eval="-eval" ;;
  lispworks)
    command="${LISPWORKS:-lispworks-console}"
    # If you have a licensed copy of lispworks,
    # you can obtain the "lispworks" binary with, e.g.
    # echo '(hcl:save-image "/lispworks" :environment nil)' > /tmp/build.lisp ;
    # ./lispworks-6-0-0-x86-linux -siteinit - -init - -build /tmp/build.lisp
    flags="-siteinit - -init -"
    eval="-eval" ;;
  mkcl)
    command="${MKCL:-mkcl}"
    flags="-norc"
    eval="-eval" ;;
  sbcl)
    command="${SBCL:-sbcl}"
    flags="--no-userinit --no-sysinit"
    # flags="--noinform --no-userinit --no-sysinit"
    nodebug="--disable-debugger"
    eval="--eval" ;;
  scl)
    command="${SCL:-scl}"
    flags="-noinit"
    nodebug="-batch"
    eval="-eval" ;;
  xcl)
    command="${XCL:-xcl}"
    flags="--no-userinit --no-siteinit --noinform"
    eval="--eval" ;;
  *)
    echo "Unsupported lisp: $1" >&2
    echo "Please add support to run-tests.sh" >&2
    exit 42 ;;
esac

if ! type "$command" > /dev/null ; then
    echo "lisp implementation not found: $command" >&2
    exit 43
fi

ASDFDIR="$(cd $(dirname $0)/.. ; command pwd)"
: ${bcmd:=$command $flags} ${icmd:=$command $flags} # batch and interactive
if [ -z "${DEBUG_ASDF_TEST}" ] ; then
  bcmd="$bcmd $nodebug"
fi


create_config () {
    cd ${ASDFDIR}
    mkdir -p build/results/ build/test-source-registry-conf.d build/test-asdf-output-translations-conf.d
}
upgrade_tags () {
    if [ -n "$ASDF_UPGRADE_TEST_TAGS" ] ; then
        echo $ASDF_UPGRADE_TEST_TAGS ; return
    fi
    # REQUIRE is a magic tag meaning whatever your implementation provides, if anything
    #
    # 1.85 (2004-05-16) is the last release by Daniel Barlow (not 1.37, which is the README revision!)
    # 1.97 (2006-05-14) is the last release before Gary King takes over
    # 1.369 (2009-10-27) is the last release by Gary King
    #
    # 2.000 to 2.019 and 2.20 to 2.26 are Faré's "stable" ASDF 2 releases
    #   2.000 (2010-05-31) was the first ASDF 2 release
    #   2.008 (2010-09-10) was a somewhat stable ASDF 2 release
    #   2.011 (2010-11-28) was used by CLISP 2.49, Debian squeeze, Ubuntu 10.04 LTS
    #   2.014.6 (2011-04-06) was used by Quicklisp in 2011
    #   2.019 (2011-11-27) was stable and used by LispWorks since 2012.
    #   2.20 (2012-01-18) was in CCL 1.8, Ubuntu 12.04 LTS
    #   2.22 (2012-06-12) was used by debian wheezy
    #   2.26 (2012-10-30) was used by Quicklisp in 2013
    #
    # 2.26.x is where the refactoring that begat ASDF 3 took place.
    # 2.26.61 is the last single-file, single-package ASDF.
    # 2.27 to 2.33 are Faré's "stable" ASDF 3 pre-releases
    #   2.27 (2013-02-01) is the first ASDF 3 pre-release
    #   2.32 (2013-03-05) is the first really stable ASDF 3 pre-release
    #
    # The 3.0 series is a stable release of ASDF 3
    # with Robert Goldman taking over maintainership at 3.0.2.
    # 3.0.0 was just 2.33.10 promoted, but version-satisfies meant it was suddenly
    # not compatible with ASDF2 anymore, so we immediately released 3.0.1
    #   3.0.1 (2013-05-16) is the first stable ASDF 3 release
    #   3.0.2 (2013-07-02) is the first ASDF 3 in SBCL
    #   3.0.3 (2013-10-22) is the last in the ASDF 3.0 series
    #
    # The 3.1 series provides the 3.1 feature, meaning users can rely on
    # all the stabilization work done in 3.0 so far, plus extra developments
    # in UIOP, package-inferred-system, and more robustification.
    #   3.1.2 (2014-05-06) is the first ASDF 3.1 release
    #   3.1.3 (2014-07-24) a bug fix release for 3.1.2
    #   3.1.4 (2014-10-09) more bug fixes, source-registry cache, in LispWorks 7
    #   3.1.5 (2015-07-21) more bug fixes, what SBCL sports (as of 1.3.14, 2017-02-04)
    #   3.1.6 (2015-10-17) more bug fixes
    #   3.1.7 (2016-03-23) more bug fixes, last in 3.1 series
    #
    # The 3.2 series provides the asdf3.2 feature, meaning users can rely on
    # all its new features (launch-program, improved bundle support), as well as
    # the improvements done in 3.1 (e.g. XDG support).
    #   3.2.0 (2017-01-08) first in 3.2 series
    #   3.2.1 (2017-04-03) bug fixes, second and last in 3.2 series
    #
    # The 3.3 series provides the asdf3.3 feature, meaning users can rely on
    # all its new features (proper phase separation) as well as earlier features.
    #   3.3.0 (2017-10-06) first in 3.3 series
    #   3.3.1 (2017-11-14) bug fixes, second
    #   3.3.2 (2018-05-03) bug fixes, third and latest in 3.3 series
    #
    # We return the above designated versions in order of decreasing relevance,
    # which pretty much means REQUIRE and most recent first.
    # We picked the last and first in each relevant series, plus 2.26.

    if [ "$lisp" = cmucl ]; then
        echo REQUIRE 3.3.2 3.3.1 3.3.0
    else
        echo REQUIRE 3.3.2 3.3.1 3.3.0 3.2.1 3.2.0 3.1.7 3.1.2 3.0.3 2.26
    fi

    #echo 3.1.7 3.1.6 3.1.5 3.1.4 3.1.3 3.1.2
    #echo 3.0.3 3.0.2 3.0.1
    #echo 2.32 2.27
    #echo 2.26 2.22 2.20 2.019 2.014.6 2.011 2.008 2.000
    #echo 1.369 1.97 1.85
}
upgrade_methods () {
    if [ -n "$ASDF_UPGRADE_TEST_METHODS" ] ; then
        echo $ASDF_UPGRADE_TEST_METHODS ; return
    fi
    cat <<EOF
'load-asdf-lisp'load-asdf-lisp-clean
'load-asdf-lisp'load-asdf-system
'load-asdf-lisp'compile-load-asdf-upgrade
'load-asdf-lisp'load-asdf-fasl
()'load-asdf-fasl
'load-asdf-lisp-and-test-uiop'load-asdf-fasl
EOF
}
extract_tagged_asdf () {
    cd ${ASDFDIR}
    mkdir -p build/
    tag=$1
    if [ REQUIRE = "$tag" ] ; then return 0 ; fi
    file=build/asdf-${tag}.lisp ;
    if [ ! -f $file ] ; then
        case $tag in
            1.*|2.0*|2.2[0-6]|2.26.61)
                git show ${tag}:asdf.lisp > $file ;;
            2.2[7-9]*|2.[3-9][0-9]*|3.*)
                mkdir -p build/old/build
                git archive ${tag} | (cd build/old/ ; tar xf -)
                make -C build/old
                mv build/old/build/asdf.lisp build/asdf-${tag}.lisp
                rm -rf build/old ;;
             *)
                echo "Don't know how to extract asdf.lisp for version $tag"
                exit 55
                ;;
        esac
    fi
}
extract_all_tagged_asdf () {
    for i in `upgrade_tags` ; do
      extract_tagged_asdf $i
    done
}
valid_upgrade_test_p () {
    case "${1}:${2}:${3}" in
        # It's damn slow. Also, for some reason, we punt on anything earlier than 2.25,
        # and only need to test it once, below for 2.24.
        abcl:1.*|abcl:2.00[0-9]:*|abcl:201[0-9]:*|abcl:2.2[0-3]:*) : ;;
        # ccl fasl numbering broke loading of old asdf 2.0
        ccl:2.0[01]*|ccl:2.2[0-6]*) : ;;
        # Allegro ships with versions 3*, so give up testing 2
        # Also, unpatched Allegro 10 has bug updating from 2.26 and before
        allegro*:[12].*) : ;;
        # My old ubuntu 10.04LTS clisp 2.44.1 came wired in
        # with an antique ASDF 1.374 from CLC that can't be downgraded.
        # More recent CLISPs work.
        # 2.00[0-7] use UID, which fails on some old CLISPs.
        # Note that for the longest time, CLISP has included 2.011 in its distribution.
        # Now its hg repository includes 3.0.2.29, but clisp hasn't released in many years(!)
        # We don't punt on upgrade anymore, so we can go at it!
        #clisp:2.00[0-7]:*|clisp:1.*|clisp:2.0[01]*|clisp:2.2[0-5]:*) : ;;
        # CMUCL has problems with 2.32 and earlier because of
        # the redefinition of system's superclass component.
        cmucl:1.*|cmucl:2.[012]*|cmucl:2.3[012]*) : ;;
        # Skip many ECL tests, for various ASDF issues
        ecl*:1.*|ecl*:2.0[01]*|ecl*:2.20:*) : ;;
        # GCL 2.7.0 from late November 2013 is required, with ASDF 3.1.2
        gcl:REQUIRE:*|gcl:1.*|gcl:2.*|gcl:3.0*) : ;;
        # LispWorks is broken at ASDF 3.0.3, but can upgrade from earlier and later ASDFs.
        lispworks:3.0.3:*) : ;;
        # MKCL is only supported starting with specific versions 2.24, 2.26.x, 3.0.3.0.x, so skip.
        mkcl:[12]*|mkcl:3.0*) : ;;
        # XCL support starts with ASDF 2.014.2
        # — It also dies during upgrade trying to show the backtrace.
        xcl:1.*|xcl:2.00*|xcl:2.01[0-4]:*|xcl:*) : ;;
        *) return 0 ;;
   esac
   return 1
}
run_upgrade_tests () {
    cd ${ASDFDIR}
    mkdir -p build/results/
    rm -f build/*.*f* uiop/*.*f* test/*.*f* ## Remove stale FASLs from ASDF 1.x, especially when different implementations have same name
    ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${ASDFDIR}\" (\"${ASDFDIR}/build/fasls/\" :implementation \"asdf/\")) (t (\"${ASDFDIR}/build/fasls/\" :implementation \"root/\")) :ignore-inherited-configuration)"
    su=test/script-support.lisp
    tags="`upgrade_tags`"
    methods="`upgrade_methods`"
    {
    for tag in $tags ; do
        for method in $methods ; do
            if valid_upgrade_test_p $lisp $tag $method ; then
                echo "Testing ASDF upgrade from ${tag} using method $method"
                extract_tagged_asdf $tag
                $bcmd $eval \
                "'(#.(load(string'|$su|))#.#.\`(in-package,:asdf-test)#.(test-upgrade$method\`|$tag|))" ||
                { echo "upgrade FAILED for $lisp from $tag using method $method" ;
                  echo "you can retry just that test with:" ;
                  echo ASDF_UPGRADE_TEST_TAGS=\"$tag\" ASDF_UPGRADE_TEST_METHODS=\"$method\" ./test/run-tests.sh -u $lisp ;
                  echo "or more interactively (and maybe with rlwrap or in emacs), start with:"
                  echo "$icmd"
                  echo "then copy/paste:"
                  echo "(load \"$su\") (asdf-test::da) (test-upgrade $method \"$tag\")"
                  exit 1 ;}
    fi ; done ; done
    echo "Upgrade test succeeded for ${lisp}"
    } 2>&1 | tee build/results/${lisp}-upgrade.text
}
run_tests () {
  create_config
  cd ./test/
  echo failure > ../build/results/status
    thedate=`date "+%Y-%m-%d"`
    rm -f "../build/results/${lisp}-test.text" || :
    do_tests "$@" 2>&1 | \
	tee "../build/results/${lisp}-test.text" "../build/results/${lisp}-test-${thedate}.save"
    read a < ../build/results/status
  clean_up
  if [ success = "$a" ] ; then ## exit code
      return 0
  else
     echo "To view full results and failures, try the following command:" >&2
     echo "     less -p ABORTED build/results/${lisp}-test.text" >&2
     return 1
  fi
}
clean_up () {
    rm -rf ../build/test-source-registry-conf.d ../build/test-asdf-output-translations-conf.d
}
test_clean_load () {
    case $lisp in
        gcl|cmucl) return 0 ;; # These are hopeless
    esac
    cd ${ASDFDIR}
    mkdir -p build/results/
    nop=build/results/${lisp}-nop.text
    load=build/results/${lisp}-load.text
    $bcmd $eval \
      "(or'#.(load(string'|test/script-support.lisp|):verbose():print())#.(asdf-test:exit-lisp'0))" \
      > $nop 2>&1
    $bcmd $eval \
      "(or'#.(load(string'|test/script-support.lisp|):verbose():print())#.(asdf-test:verbose())#.(load(string'|build/asdf.lisp|):verbose())#.(uiop/image:quit'0))" \
      > $load 2>&1
    if diff $nop $load ; then
      echo "GOOD: Loading ASDF on $lisp produces no message" >&2 ; return 0
    else
      echo "BAD: Loading ASDF on $lisp produces messages" >&2 ; return 1
    fi
}
test_load_systems () {
    cd ${ASDFDIR}
    mkdir -p build/results/
    echo "Loading all these systems: $*"
    $bcmd $eval \
      "(or #.(load(string'|test/script-support.lisp|))#.(asdf-test:with-test()(asdf-test:test-load-systems $*)))" \
        2>&1 | tee build/results/${lisp}-systems.text
}
test_interactively () {
    cd ${ASDFDIR}
    mkdir -p build/results/
    rlwrap $icmd $eval "(or'#.(load(string'|test/script-support.lisp|))#.(asdf-test:interactive-test'($*)))"
}

if [ -z "$command" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
elif [ -n "$test_interactively" ] ; then
    test_interactively "$@"
elif [ -n "$clean_load" ] ; then
    test_clean_load
elif [ -n "$load_systems" ] ; then
    test_load_systems "$@"
elif [ -n "$upgrade" ] ; then
    run_upgrade_tests
elif [ -n "$extract_all" ] ; then
    extract_all_tagged_asdf
else
    run_tests "$@"
fi ; exit # NB: "; exit" makes it robust wrt the script being modified while running.
