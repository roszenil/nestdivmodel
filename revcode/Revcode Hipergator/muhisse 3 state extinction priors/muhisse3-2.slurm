#!/bin/bash
#SBATCH --job-name=hidden3state
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=30-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --account=burleigh
#SBATCH --qos=burleigh
#SBATCH --error=hidden3stateerror
#SBATCH --output=hidden3stateout
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=roszenil@hawaii.edu

module load revbayes

rb /ufrc/burleigh/rzenil/nido/muhisse3state-2.Rev
