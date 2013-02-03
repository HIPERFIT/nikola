#!/bin/bash

if [ x$HSENV != x ]; then
  deactivate_hsenv
fi

if [ x$ROOT = x ]; then
  cd ~/vectorprogramming
  source sourceme.sh
  cd $OLDPWD
fi

loadHSENV "nikola"

ghci -hide-package nikola -package-conf dist_nikola-GHC7.4.2/package.conf.inplace -XTypeOperators -XTypeFamilies -XScopedTypeVariables Playground.hs

deactivate_hsenv
