#!/bin/sh

cd tmp

for file in *js
do
	echo "Compiled: $file"
	tgts=`find ../core-js/ | grep $file`
	for tgt in $tgts
	do
		echo "Core: $tgt"
		out=`echo $tgt | sed 's/..\/core-js\/\///'`".diff"
		echo "Out: $out"
		git diff $file $tgt > $out
	done
done
