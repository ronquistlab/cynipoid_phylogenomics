#!/bin/zsh

for f in $(ls *.faa) 
do
    hmmcleaner.pl $f --ali > $f.tsv
done

