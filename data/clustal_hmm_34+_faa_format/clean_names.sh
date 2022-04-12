#!/bin/zsh

echo $(date) >> clean_names_log.txt

for f in $(grep -l "Orussus-" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Orussus-/Orussusa/|wq" $f
done

for f in $(grep -l "Nasonia-" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Nasonia-/Nasoniav/|wq" $f
done

for f in $(grep  -l "Micropl-" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Micropl-/Micropld/|wq" $f
done

for f in $(grep -l "Phana_JH--" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Phana_JH--/Phana_JH/|wq" $f
done

for f in $(grep -l "Peric_JH--" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Peric_JH--/Peric_JH/|wq" $f
done

for f in $(grep -l "Gana_sp1--" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Gana_sp1--/Gana_sp1/|wq" $f
done

for f in $(grep -l "Callas_no-" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Callas_no-/Calla_no/|wq" $f
done

for f in $(grep -l "Calla_no" *.faa) 
do
    echo $f >> clean_names_log.txt
    vim -c "%s/Calla_no/Callas_no/|wq" $f
done

echo $(date) >> clean_names_log.txt

