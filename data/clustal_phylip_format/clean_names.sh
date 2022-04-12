#!/bin/zsh

echo $(date) >> clean_names_log.txt

for f in $(ls *.faa)
do
    mv $f $f.phy
done

for f in $(grep -l "Orussus" *.phy) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Orussus/Orussusa/|wq" $f
done

for f in $(grep -l "Nasonia" *.phy) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Nasonia/Nasoniav/|wq" $f
done

for f in $(grep  -l "Micropl" *.phy) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Micropl/Micropld/|wq" $f
done

echo $(date) >> clean_names_log.txt

