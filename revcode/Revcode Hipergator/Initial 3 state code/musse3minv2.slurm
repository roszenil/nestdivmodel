#!/bin/bash
#SBATCH --job-name=musse3minv2
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=10-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=musse3v2minerror
#SBATCH --output=musse3v2minout
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load gcc
module load revbayes

rb /ufrc/burleigh/rzenil/nido/musse3minv2.Rev