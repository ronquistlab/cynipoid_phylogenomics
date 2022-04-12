#!/bin/bash
#
#SBATCH -J iq36-c60
#SBATCH -t 24:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o iq36_C60.log

echo $(date)
../../iqtree-1.6.12-Linux/bin/iqtree -s ../../data/allgaps36.phy -nt 32 -m C60+I+G5 -pre iq_36_c60 --runs 2 -bb 2000
wait
echo $(date)
