#!/bin/bash
#SBATCH --job-name=musse6
#SBATCH --time=00-71:58:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --partition=shared
#SBATCH --ntasks=1
#SBATCH --error=musse6error
#SBATCH --output=musse6out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
rb-mpi musse6state.Rev
