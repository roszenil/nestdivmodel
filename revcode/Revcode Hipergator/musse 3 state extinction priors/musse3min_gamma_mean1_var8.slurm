#!/bin/bash
#SBATCH --job-name=gammavar8
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=14-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=gammavar8error
#SBATCH --output=gammavar85out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu


module load revbayes

rb /ufrc/burleigh/rzenil/nido/musse3min_gamma_mean1_var8.Rev