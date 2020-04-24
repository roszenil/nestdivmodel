#!/bin/bash
#SBATCH --job-name=muhisse6
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=10-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH  --account=burleigh
#SBATCH  --qos=burleigh
#SBATCH --error=muhisse6error
#SBATCH --output=muhisse6out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load gcc
module load revbayes

rb /ufrc/burleigh/rzenil/nido/muhisse6state.Rev