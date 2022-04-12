#!/bin/bash
#
#SBATCH -J 34no-esc
#SBATCH -t 48:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o slurm_iq_clustal34_ag_lt0.26_no_escha_C60_I_G5.log

echo $(date)
../../iqtree-1.6.12-Linux/bin/iqtree -s ../../data/clustal34_ag_lt0.26_no_escha.phy -nt 32 -m C60+I+G5 -pre iq_clustal34_ag_lt0.26_no_escha_C60_I_G5 --runs 2 -bb 2000
wait
echo $(date)

