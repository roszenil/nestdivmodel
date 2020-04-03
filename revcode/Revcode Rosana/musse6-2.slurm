#!/bin/bash
#SBATCH --job-name=musse6-2
#SBATCH --time=00-71:58:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --error=musse62error
#SBATCH --output=musse62out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
mpirun -np 10 rb-mpi musse6state.Rev
