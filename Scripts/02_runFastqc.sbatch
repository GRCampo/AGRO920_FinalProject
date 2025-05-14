#!/bin/bash -l
#SBATCH --job-name=data920
#SBATCH --time=4-00:00:00
#SBATCH --mem=10G
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --array=1-14

# Load Fastqc 
module load FastQC/0.12.1-Java-11

# finding R1 and R2 files
FASTQ_FILES_R1=($(ls /bulk/eakhunov/grcampos/embeddings/fastq_filtered/*_1_clean.fastq.gz))
FASTQ_FILES_R2=($(ls /bulk/eakhunov/grcampos/embeddings/fastq_filtered/*_2_clean.fastq.gz))

# One file per array
FILE_R1=${FASTQ_FILES_R1[$SLURM_ARRAY_TASK_ID - 1]} #bash arrays starts with 0
FILE_R2=${FASTQ_FILES_R2[$SLURM_ARRAY_TASK_ID - 1]}

# Running Fatsqc
echo "Processing $FILE_R1 and $FILE_R2"
fastqc -t 10 -o /bulk/eakhunov/grcampos/embeddings/fastqc_reports "$FILE_R1" "$FILE_R2"

