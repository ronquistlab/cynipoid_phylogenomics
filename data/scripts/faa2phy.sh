#!/bin/bash 

echo $(date)

module load bioinfo-tools
module load clustalw

#for f in $(cat complete_37.list)
#for f in $(cat at_least_36.list)
for f in $(cat at_least_34.list)
do
	clustalw2 -infile=$f -outfile=phy34/$f.phy -output=phylip -convert
done

wait
echo $(date)
