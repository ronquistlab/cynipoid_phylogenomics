#!/bin/bash
#
#SBATCH -J 34noe-f1
#SBATCH -t 72:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o clustal34_ag_lt0.26_no_outgr_escha_cat-f81_r1.log

mpprun -np 32 ../../pbmpi/data/pb_mpi -d ../../data/clustal34_ag_lt0.26_no_outgr_escha.phy -cat -f81 clustal34_ag_lt0.26_no_outgr_escha_cat-f81_r1

