#!/bin/bash
#SBATCH --job-name=muhisse3
#SBATCH --time=00-71:58:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=500mb
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --error=muhisse3error
#SBATCH --output=muhisse3out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
rb-mpi muhisse3state.Rev
