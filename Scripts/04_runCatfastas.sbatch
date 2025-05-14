#!/bin/bash -l
#SBATCH --job-name=data920
#SBATCH --time=4-00:00:00
#SBATCH --mem=50G
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --array=1-14

FASTQ_LIST=($(ls ./fasta_files/*_1.fasta))
FASTQ1=${FASTQ_LIST[$SLURM_ARRAY_TASK_ID - 1]}
FASTQ2=$(echo $FASTQ1 | sed 's/_1.fasta/_2.fasta/')  
OUTPUT="$(basename "$FASTQ1" | sed 's/_1.fasta/_merged.fasta/')"

cat "$FASTQ1" "$FASTQ2" > "$OUTPUT"

echo "Done for $FASTQ1 e $FASTQ2!"

