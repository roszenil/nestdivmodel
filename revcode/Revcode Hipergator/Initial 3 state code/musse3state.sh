#!/bin/sh
#SBATCH --job-name=musse3state
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=jaymcentee@ufl.edu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2gb # Per processor memory
#SBATCH --time=96:00:00
#SBATCH --output=musse3state.out
#SBATCH --qos=burleigh-b

module load gcc
module load revbayes

rb /home/jaymcentee/ufrc/nest_musse/revcode/musse3state.Rev