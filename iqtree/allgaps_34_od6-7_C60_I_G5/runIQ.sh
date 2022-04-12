#!/bin/bash
#
#SBATCH -J iq-od6-7
#SBATCH -t 20:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o slurm_iq_34_od6-7_C60_I_G5.log

echo $(date)
../../iqtree-1.6.12-Linux/bin/iqtree -s ../../data/allgaps34_od6-7.phy -nt 32 -m C60+I+G5 -pre iq_34_od6-7_C60_I_G5 --runs 2 -bb 2000
wait
echo $(date)

