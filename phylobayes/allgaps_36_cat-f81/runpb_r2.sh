#!/bin/bash
#
#SBATCH -J pb36f2
#SBATCH -t 72:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o allgaps_36_cat-f81_r2.log

mpprun -np 32 ../../pbmpi/data/pb_mpi -d ../../data/allgaps36.phy -cat -f81 allgaps_36_cat-f81_r2

