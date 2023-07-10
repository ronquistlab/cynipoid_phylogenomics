#!/bin/bash
#
#SBATCH -J iq30ag10
#SBATCH -t 48:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o slurm_iq_clustal30_ag_lt0.10_C60_I_G5.log

echo $(date)
../../iqtree-1.6.12-Linux/bin/iqtree -s ../../data/clustal30_ag_lt0.10.phy -nt 32 -m C60+I+G5 -pre iq_clustal30_ag_lt0.10_C60_I_G5 --runs 2 -bb 2000
wait
echo $(date)

