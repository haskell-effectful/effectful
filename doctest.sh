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
    -XBangPatterns \
    -XConstraintKinds \
    -XDataKinds \
    -XDeriveFunctor \
    -XFlexibleContexts \
    -XFlexibleInstances \
    -XGADTs \
    -XGeneralizedNewtypeDeriving \
    -XLambdaCase \
    -XMultiParamTypeClasses \
    -XNoStarIsType \
    -XRankNTypes \
    -XRecordWildCards \
    -XRoleAnnotations \
    -XScopedTypeVariables \
    -XStandaloneDeriving \
    -XTupleSections \
    -XTypeApplications \
    -XTypeFamilies \
    -XTypeOperators
  popd
}

run_doctest effectful src
run_doctest effectful-core src
