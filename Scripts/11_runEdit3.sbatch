#!/bin/bash
#SBATCH --job-name=kmers
#SBATCH --array=0-19
#SBATCH --time=04:00:00
#SBATCH --mem=200G
#SBATCH --cpus-per-task=1

module load R/4.2.1-foss-2022a

FILES_LIST=($(ls FINAL_group2*.rds))
FILE="${FILES_LIST[$SLURM_ARRAY_TASK_ID]}"

Rscript editFiles2.R "$FILE"
