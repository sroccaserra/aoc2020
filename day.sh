#!/usr/bin/env bash

if test "$2" != "test"
then
    sed "s/DayXX/Day$1/" "src/DayXX.hs" > "src/Day${1}.hs"
else
    sed "s/DayXX/Day$1/" "test/DayXXSpec.hs" > "test/Day${1}Spec.hs"
fi

stack build
