#!/bin/bash
#SBATCH --job-name=cid6
#SBATCH --time=06-23:58:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --partition=shared-long
#SBATCH --ntasks=1
#SBATCH --error=cid6error
#SBATCH --output=cid6out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
rb-mpi cid6state.Rev
