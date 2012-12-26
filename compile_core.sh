#!/bin/sh

reltmp=../tmp/

cd core-elm

for file in *elm
do
	elm --only-js --output-directory=$reltmp $file
done
