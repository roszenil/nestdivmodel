#!/bin/bash
#SBATCH --job-name=musse6
#SBATCH --time=10-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=zenilab
#SBATCH --partition=zenilab
#SBATCH --ntasks=1
#SBATCH --error=musse6error
#SBATCH --output=musse6out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load bio/RevBayes
rb-mpi musse6state.Rev
