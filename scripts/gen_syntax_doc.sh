#!/bin/bash

set -e -u

ROOT=$(dirname $(dirname $(readlink -f "${BASH_SOURCE:-$0}")))
LIB=$ROOT/src/lib
MLIS=$LIB/*.mli
TGT=$LIB/syntax.mld

echo -n "Generate $TGT... " 1>&2
echo -e "{0 kkmarkdown syntax}

It supports a subset of {{: https://daringfireball.net/projects/markdown/syntax }the original
markdown syntax}, in order to avoid XSS attack.

The following rules are listed in alphabetical order.  To see the actual application order, read the definitions:

- {{: https://github.com/kkeundotnet/kkmarkdown/blob/main/src/lib/AllSpanRules.ml}AllSpanRules.ml}
- {{: https://github.com/kkeundotnet/kkmarkdown/blob/main/src/lib/AllBlockRules.ml}AllBlockRules.ml}
" > $TGT
for file in $MLIS; do
    base=$(basename ${file%.mli})
    awk "/SYNTAX: START/{ syntax=1; }
         /\(\*\* /{ if (syntax) { flag=1; print \"\n{1 $base}\n\"; } }
         / \*\)/{ unset_flag=1; }
    	 /SYNTAX: END/{ syntax=0; }
         { ret=flag; if (unset_flag) { flag=0; unset_flag=0; } }
         ret" $file \
    | sed -E -e 's/\(\*\* | \*\)//g' \
    >> $TGT
done
echo -e "\nThat's it.  Enjoy kkmarkdown!" >> $TGT
echo "DONE" 1>&2
