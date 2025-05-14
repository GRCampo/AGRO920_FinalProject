#!/bin/bash -l
#SBATCH --job-name=data920
#SBATCH --time=4-00:00:00
#SBATCH --mem=20G
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --array=1-14

module load picard/2.25.1-Java-11

# List of paired-end FASTQ files
FASTQ_FILES_R1=($(ls data/*_1.fastq.gz))
FASTQ_FILES_R2=($(ls data/*_2.fastq.gz))

# One file per array index
FILE_R1=${FASTQ_FILES_R1[$SLURM_ARRAY_TASK_ID - 1]}
FILE_R2=${FASTQ_FILES_R2[$SLURM_ARRAY_TASK_ID - 1]}
SAMPLE_NAME=$(basename "$FILE_R1" | sed 's/_1.fastq.gz//')

echo "Processing sample: $SAMPLE_NAME"
java -jar $EBROOTPICARD/picard.jar FastqToSam F1="$FILE_R1" F2="$FILE_R2" O="/bulk/eakhunov/grcampos/embeddings/bam_files/${SAMPLE_NAME}_unaligned.bam" SM="$SAMPLE_NAME"
echo "FASTQ to BAM completed for $SAMPLE_NAME"

# Remove duplicates with MarkDuplicates
java -jar $EBROOTPICARD/picard.jar MarkDuplicates I="/bulk/eakhunov/grcampos/embeddings/bam_files/${SAMPLE_NAME}_unaligned.bam" O="/bulk/eakhunov/grcampos/embeddings/bam_files/${SAMPLE_NAME}_deduplicated.bam" M="/bulk/eakhunov/grcampos/embeddings/bam_files/${SAMPLE_NAME}_duplicate_metrics.txt" REMOVE_DUPLICATES=true VALIDATION_STRINGENCY=LENIENT #Lenient: Display warnings but will continue processing even with minor errors.
echo "MarkDuplicates completed for $SAMPLE_NAME"

# Convert BAM back to FASTQ
java -jar $EBROOTPICARD/picard.jar SamToFastq I="/bulk/eakhunov/grcampos/embeddings/bam_files/${SAMPLE_NAME}_deduplicated.bam" F="/bulk/eakhunov/grcampos/embeddings/fastq_filtered/${SAMPLE_NAME}_1_clean.fastq.gz" F2="/bulk/eakhunov/grcampos/embeddings/fastq_filtered/${SAMPLE_NAME}_2_clean.fastq.gz"
echo "BAM to FASTQ completed for $SAMPLE_NAME"

# Remove BAM files
rm "/bulk/eakhunov/grcampos/embeddings/bam_files/${SAMPLE_NAME}_unaligned.bam"
rm "/bulk/eakhunov/grcampos/embeddings/bam_files/${SAMPLE_NAME}_deduplicated.bam"

echo "Processing completed for $SAMPLE_NAME!"
