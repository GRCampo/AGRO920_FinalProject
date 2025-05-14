#!/bin/bash
#SBATCH --job-name=chunk_cv
#SBATCH --array=374-4505%100
#SBATCH --time=2-00:00:00
#SBATCH --mem=50G
#SBATCH --cpus-per-task=1

module load R/4.2.1-foss-2022a

Rscript kmers_CV_byChunck.R $SLURM_ARRAY_TASK_ID

