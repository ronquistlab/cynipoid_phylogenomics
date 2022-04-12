#!/bin/bash
#
#SBATCH -J gblocks
#SBATCH -t 10:00
#SBATCH -n 1
#SBATCH -o slurm_allgaps.log

echo $(date)

for f in $(ls *.phy)
do
	../Gblocks_0.91b/Gblocks $f -p=t -t=p -b5=h -e=-ag
done

wait
echo $(date)

