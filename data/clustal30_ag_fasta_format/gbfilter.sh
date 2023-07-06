#!/bin/bash -l

#SBATCH -A snic2017-7-283
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 10:00
#SBATCH -J Gblocks
#SBATCH -o allgaps.log

echo $(date)

for f in $(ls ../../aligned_buscos/fasta_format/*.faa)
do
	/proj/snic2017-7-283/aln_synergus/Gblocks_0.91b/Gblocks $f -p=t -t=p -b5=a -e=-ag
done

wait
echo $(date)
