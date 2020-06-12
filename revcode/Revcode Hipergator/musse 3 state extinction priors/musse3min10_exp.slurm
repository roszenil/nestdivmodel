#!/bin/bash
#SBATCH --job-name=exp10
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=15-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=exp10error
#SBATCH --output=exp10out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load revbayes

rb /ufrc/burleigh/rzenil/nido/musse3min10_exp.Rev