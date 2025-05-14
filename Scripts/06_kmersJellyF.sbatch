#!/bin/bash
#SBATCH --job-name=kmer_count
#SBATCH --time=7-00:00:00
#SBATCH --array=0-1399%100   
#SBATCH --mem=25G
#SBATCH --cpus-per-task=10

module load Jellyfish/2.3.0-GCC-11.3.0

echo "Job started on $(date)"

# Step 1: Generate a list of all pseudo-fasta files
if [ "$SLURM_ARRAY_TASK_ID" -eq 0 ]; then
  echo "Generating list of pseudo-fasta files..."
  find ps_fasta -type f -name "*_ps*.fasta" | sort > all_pseudo_fastas.list
fi

sleep 10

# Step 2: file path 
FASTA=$(sed -n "$((SLURM_ARRAY_TASK_ID + 1))p" all_pseudo_fastas.list)
BASENAME=$(basename "$FASTA" .fasta)
DIRNAME=$(dirname "$FASTA")

echo "Processing: $FASTA"

# Step 3: Define output file paths
BCFILE="${DIRNAME}/${BASENAME}.bc"
JFFILE="${DIRNAME}/${BASENAME}.jf"
TSVFILE="${DIRNAME}/${BASENAME}_k41.tsv"

# Step 4: First pass - Bloom Counter
jellyfish bc -m 41 -s 300M -t 10 -o "$BCFILE" "$FASTA"

# Step 5: Second pass - count only k-mers with frequency >= 2
jellyfish count -m 41 -s 300M -t 10 --bc "$BCFILE" -C -o "$JFFILE" "$FASTA"

# Step 6: TSV format -> transformer
jellyfish dump -c "$JFFILE" > "$TSVFILE"

echo "Finished: $TSVFILE"

# Step 7: Rovome temporary Jellyfish files
rm -f "$BCFILE" "$JFFILE"
echo "Temporary files removed: $BCFILE and $JFFILE"
