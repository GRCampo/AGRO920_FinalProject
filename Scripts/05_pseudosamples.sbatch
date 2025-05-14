#!/bin/bash
#SBATCH --job-name=pseudo_split
#SBATCH --time=1-00:00:00
#SBATCH --array=0-13
#SBATCH --mem=25G
#SBATCH --cpus-per-task=1

echo "Job started on $(date)"
FASTA_LIST=($(ls fasta_files/*_merged.fasta))
FASTA="${FASTA_LIST[$SLURM_ARRAY_TASK_ID]}"
BASENAME=$(basename "$FASTA" _merged.fasta)

echo "Processing: $BASENAME"
mkdir -p ps_fasta/${BASENAME}/ids

# Step 1: Extract unique base read IDs
echo "Extracting read IDs..."
grep ">" "$FASTA" | sed 's/>//g' | sed 's/\/[12]$//' | sort -u > ps_fasta/${BASENAME}/ids/${BASENAME}_ID.txt

# Step 2: Shuffle
echo "Shuffling IDs..."
shuf ps_fasta/${BASENAME}/ids/${BASENAME}_ID.txt > ps_fasta/${BASENAME}/ids/${BASENAME}_shufID.txt

# Step 3: Split into 100 pseudo-ID files
echo "Splitting into 100 pseudo-ID files..."
total=$(wc -l < ps_fasta/${BASENAME}/ids/${BASENAME}_shufID.txt)
lines_per_file=$(( (total + 99) / 100 ))

split -l $lines_per_file -d -a 2 ps_fasta/${BASENAME}/ids/${BASENAME}_shufID.txt ps_fasta/${BASENAME}/ids/pseudo_

# Step 4: Generate paired-end ID files
echo "Creating paired-end ID files..."
for f in ps_fasta/${BASENAME}/ids/pseudo_*; do
    awk '{print $0"/1\n"$0"/2"}' "$f" > "${f}.txt"
done

# Step 5: Create new pseudo-fasta files
echo "Generating pseudo-fasta files..."
for i in $(seq -w 0 99); do
    seqkit grep -f ps_fasta/${BASENAME}/ids/pseudo_${i}.txt "$FASTA" -o ps_fasta/${BASENAME}/${BASENAME}_ps${i}.fasta
    echo "${BASENAME}_ps${i}.fasta done."
done

# Step 6: Create summary file
echo "Creating summary..."
SUMMARY_FILE="ps_fasta/${BASENAME}/pseudo_summary.txt"
> "$SUMMARY_FILE"

for f in ps_fasta/${BASENAME}/ids/pseudo_*; do
    IDFILE=$(basename "$f")
    GROUP="${IDFILE##*_}"
    GROUP="${GROUP/.txt/}"

    clean_ids=$(awk -F'/' '{print $1}' "$f" | sort -u)
    count=$(echo "$clean_ids" | wc -l)
    ids_line=$(echo "$clean_ids" | paste -sd "," -)

    echo "> pseudo_${GROUP} (${count} reads)" >> "$SUMMARY_FILE"
    echo "$ids_line" >> "$SUMMARY_FILE"
done

# Step 7: Clean up intermediate pseudo ID files
echo "Cleaning up intermediate ID files..."
rm ps_fasta/${BASENAME}/ids/pseudo_*

echo "All pseudo-samples for $BASENAME created successfully on $(date)"

