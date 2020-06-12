#!/bin/bash
#SBATCH --job-name=mkn3
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=20-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=mkn3error
#SBATCH --output=mkn3out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load revbayes

rb /ufrc/burleigh/rzenil/nido/mkn3state.Rev