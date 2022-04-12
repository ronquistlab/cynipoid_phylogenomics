#!/bin/bash
#
#SBATCH -J iq-ag5
#SBATCH -t 24:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o slurm_iq_clustal34_ag_0.47-0.59_C60_I_G5.log

echo $(date)
../../iqtree-1.6.12-Linux/bin/iqtree -s ../../data/clustal34_ag_0.47-0.59.phy -nt 32 -m C60+I+G5 -pre iq_clustal34_ag_0.47-0.59_C60_I_G5 --runs 2 -bb 2000
wait
echo $(date)

