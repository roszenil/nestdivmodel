#!/bin/bash
#SBATCH --job-name=musse6-2
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=04-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH  --account=burleigh
#SBATCH  --qos=burleigh-b
#SBATCH --error=musse6-2error
#SBATCH --output=musse6-2out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load gcc
module load revbayes

rb /ufrc/burleigh/rzenil/nido/musse6state-2.Rev