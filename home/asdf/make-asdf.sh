#!/bin/sh
# Usage: ./make-asdf.sh [keyword=argument ...] <command>
# See the Makefile for the list of acceptable keyword arguments

here="$(dirname $0)"

header_lisp="header.lisp"
driver_lisp="uiop/package.lisp uiop/common-lisp.lisp uiop/utility.lisp uiop/version.lisp uiop/os.lisp uiop/pathname.lisp uiop/filesystem.lisp uiop/stream.lisp uiop/image.lisp uiop/lisp-build.lisp uiop/launch-program.lisp uiop/run-program.lisp uiop/configuration.lisp uiop/backward-driver.lisp uiop/driver.lisp"
defsystem_lisp="upgrade.lisp session.lisp component.lisp operation.lisp system.lisp system-registry.lisp action.lisp lisp-action.lisp find-component.lisp forcing.lisp plan.lisp operate.lisp find-system.lisp parse-defsystem.lisp bundle.lisp concatenate-source.lisp package-inferred-system.lisp output-translations.lisp source-registry.lisp backward-internals.lisp backward-interface.lisp interface.lisp user.lisp footer.lisp"

all () {
  # Default action: bootstrap asdf.lisp
  build_asdf
}
build_asdf () {
  # That's the only thing that we really need before we may invoke asdf-tools.
  mkdir -p build
  a=build/asdf.lisp
  cat ${header_lisp} ${driver_lisp} ${defsystem_lisp} > ${a}.tmp
  if [ -f ${a} ] && cmp -s ${a} ${a}.tmp ; then
    rm -rf ${a}.tmp
  else
    mv -f ${a}.tmp ${a}
  fi
}
build_asdf_tools () {
  if [ -x build/asdf-tools ] ; then
    : "Reusing existing asdf-tools."
  else
    : "Building asdf-tools."
    build_asdf
    ${here}/tools/asdf-tools build-asdf-tools
  fi
}
ext () {
  # Download all the development-time dependencies of ASDF:
  git submodule update --init
}
noext () {
  # Remove all the development-time dependencies of ASDF:
  git submodule deinit .
}
driver_files () {
  # These targets are used during tests to ensure the Makefile is in synch with the .asd files.
  echo ${driver_lisp}
}
defsystem_files () {
  # These targets are used during tests to ensure the Makefile is in synch with the .asd files.
  echo ${defsystem_lisp}
}

case "$1" in
  "") all ;;
  all|build_asdf|build_asdf_tools|ext|noext|driver_files|defsystem_files) "$@" ;;
  *) build_asdf_tools ; exec ${here}/build/asdf-tools env "$@" ;;
esac ; exit
