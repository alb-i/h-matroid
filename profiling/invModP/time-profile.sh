#!/bin/bash

cd $(dirname $0)

stack build

for x in matroid lazy strict ; do
echo Profiling $x
stack run  -- $x +RTS -sstderr 2>$x-time.txt
done
