#!/bin/bash
#SBATCH --job-name=musse3
#SBATCH --time=00-71:58:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=500mb
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --error=musse3error
#SBATCH --output=musse3out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
rb-mpi musse3state.Rev
