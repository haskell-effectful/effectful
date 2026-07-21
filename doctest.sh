#!/bin/sh
#
# For this to work you need to:
#
# - Put "write-ghc-environment-files: always" in your cabal.project.local.
#
# - Compile doctest with the same GHC version the project currently uses.
#

set -eu

run_doctest() {
  pushd "${1}"
  doctest \
    "${2}" \
    -XGHC2021 \
    -XDataKinds \
    -XDeepSubsumption \
    -XDerivingStrategies \
    -XDuplicateRecordFields \
    -XLambdaCase \
    -XNoFieldSelectors \
    -XNoStarIsType \
    -XOverloadedRecordDot \
    -XRoleAnnotations \
    -XTypeFamilies \
    -XUndecidableInstances
  popd
}

run_doctest effectful-core src
run_doctest effectful-th src
run_doctest effectful src
