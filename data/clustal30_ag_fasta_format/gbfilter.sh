#!/bin/bash

echo $(date)

for f in $(ls *.faa)
do
	../../Gblocks_0.91b/Gblocks $f -p=t -t=p -b5=h -e=-ag
done

wait
echo $(date)

