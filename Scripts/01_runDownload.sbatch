#!/bin/bash -l
#SBATCH --job-name=data920
#SBATCH --time=4-00:00:00
#SBATCH --mem=50G
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --array=1-14

RUNS=("SRR1542404" "SRR1542405" "SRR1542406" "SRR1542407" "SRR1542408" "SRR1542409" "SRR1542410" "SRR1542411" "SRR1542412" "SRR1542413" "SRR1542414" "SRR1542415" "SRR1542416" "SRR1542417")

RUN=${RUNS[$SLURM_ARRAY_TASK_ID-1]}

echo "Processing $RUN Job Array ID: $SLURM_ARRAY_TASK_ID"

fasterq-dump --split-files --threads 10 $RUN

gzip "${RUN}_1.fastq"
gzip "${RUN}_2.fastq"

echo "$RUN done!"

