#!/bin/bash
#SBATCH --job-name=cid3
#SBATCH --time=00-71:58:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=500mb
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --error=cid3error
#SBATCH --output=cid3out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
rb-mpi cid3state.Rev
