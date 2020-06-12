#!/bin/bash
#SBATCH --job-name=mkn6
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=20-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=mkn6error
#SBATCH --output=mkn6out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load revbayes

rb /ufrc/burleigh/rzenil/nido/mkn6state.Rev