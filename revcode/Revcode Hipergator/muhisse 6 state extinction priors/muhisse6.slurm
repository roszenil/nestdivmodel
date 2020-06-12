#!/bin/bash
#SBATCH --job-name=muhisse6
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=30-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=5G
#SBATCH  --account=burleigh
#SBATCH  --qos=burleigh
#SBATCH --error=muhisse6error
#SBATCH --output=muhisse6out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load gcc
module load intel/2018.1.163  
module load openmpi/3.0.0
module load revbayes/1.0.7

rb /ufrc/burleigh/rzenil/nido/muhisse6state.Rev