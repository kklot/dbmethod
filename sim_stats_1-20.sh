#!/bin/bash
#SBATCH --job-name=dbmt_process
#SBATCH --partition=fuchs
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40
#SBATCH --mem-per-cpu=2000
#SBATCH --array=1-20
#SBATCH --mail-type=ALL
#SBATCH --time=00:05:00

srun Rscript sim_stats.R
exit 0
