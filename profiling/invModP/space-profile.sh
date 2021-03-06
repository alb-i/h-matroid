#!/bin/bash

cd $(dirname $0)

stack build --profile

for x in matroid lazy strict ; do
stack run --profile -- $x +RTS -hy -p -K100M
hp2ps -e8in -c invModP-exe.hp
cp invModP-exe.ps $x.ps
done
