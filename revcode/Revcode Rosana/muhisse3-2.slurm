#!/bin/bash
#SBATCH --job-name=muhisse3-2
#SBATCH --time=06-23:58:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=500mb
#SBATCH --partition=shared-long
#SBATCH --ntasks=1
#SBATCH --error=muhisse32error
#SBATCH --output=muhisse32out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
rb-mpi muhisse3state-2.Rev
