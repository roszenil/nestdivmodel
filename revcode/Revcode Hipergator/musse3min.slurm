#!/bin/bash
#SBATCH --job-name=musse3min
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=04-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh-b
#SBATCH --error=musse3minerror
#SBATCH --output=musse3minout
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load gcc
module load revbayes

rb /ufrc/burleigh/rzenil/nido/musse3min.Rev