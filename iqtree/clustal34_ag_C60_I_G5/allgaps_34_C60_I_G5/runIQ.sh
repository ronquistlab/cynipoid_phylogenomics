#!/bin/bash
#
#SBATCH -J iq34-c60
#SBATCH -t 24:00:00
#SBATCH -N 1
#SBATCH -C fat --exclusive
#SBATCH -o iq34_C60.log

echo $(date)
../../iqtree-1.6.12-Linux/bin/iqtree -s ../../data/allgaps34.phy -nt 32 -m C60+I+G5 -pre iq_34_c60 --runs 2 -bb 2000
wait
echo $(date)
