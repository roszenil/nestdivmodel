#!/bin/bash
#SBATCH --job-name=cid3state
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=14-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=cid3error
#SBATCH --output=cid3out
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load revbayes

rb /blue/burleigh/rzenil/nido/cid3state.Rev
