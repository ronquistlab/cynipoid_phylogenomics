#!/bin/bash
#
#SBATCH -J pb37f2
#SBATCH -t 72:00:00
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -o allgaps_37_cat-f81_r2.log

mpprun -np 32 ../../pbmpi/data/pb_mpi -d ../../data/allgaps37.phy -cat -f81 allgaps_37_cat-f81_r2
