#!/bin/bash
#
#SBATCH -J iq35-c60
#SBATCH -t 24:00:00
#SBATCH -N 1
#SBATCH -C fat --exclusive
#SBATCH -o slurm_iq_35_C60_I_G5.log

echo $(date)
../../iqtree-1.6.12-Linux/bin/iqtree -s ../../data/allgaps35.phy -nt 32 -m C60+I+G5 -pre iq_35_C60_I_G5 --runs 2 -bb 2000
wait
echo $(date)
