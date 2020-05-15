#!/bin/bash
#SBATCH --job-name=exp05
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=10-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=exp05error
#SBATCH --output=exp05out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load revbayes

rb /ufrc/burleigh/rzenil/nido/musse3min_exp.Rev