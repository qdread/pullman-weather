#!/bin/bash
#SBATCH --job-name=pullmanRF
#SBATCH --ntasks=5
#SBATCH --mem=48gb
#SBATCH --partition=short
#SBATCH --time=2-00:00:00
#SBATCH --array=1-4

cd /home/quentin.read/GitHub/pullman-weather
module load r/4.1.2
Rscript2 rf_fits_remote.R
