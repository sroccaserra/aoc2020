#!/usr/bin/env bash

sed "s/DayXX/Day$1/" src/DayXX.hs > "src/Day${1}.hs"
sed "s/DayXX/Day$1/" src/DayXXSpec.hs > "src/Day${1}Spec.hs"
stack build
