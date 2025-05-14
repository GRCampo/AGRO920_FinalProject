#!/bin/bash
#SBATCH --job-name=merge_kmers
#SBATCH --array=0-13
#SBATCH --mem=200G
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=15

# List of treatment IDs (folders)
TREAT_IDS=(SRR1542404 SRR1542405 SRR1542406 SRR1542407 SRR1542408 SRR1542409 SRR1542410 SRR1542411 SRR1542412 SRR1542413 SRR1542414 SRR1542415 SRR1542416 SRR1542417)


TREAT_ID=${TREAT_IDS[$SLURM_ARRAY_TASK_ID]}

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Treatment start: $TREAT_ID"

cd $TREAT_ID

# Step 1: Fix TSV files
#echo "[$(date '+%Y-%m-%d %H:%M:%S')] Fixing TSV files."
#for f in *.tsv; do
  # insert header before the original first line
#  sed -i '1s/^/kmer\tcount\n/' "$f"
  # from line 2 onward, replace first space with tab
#  sed -i '2,$s/ /\t/' "$f"
#done

# Step 2: Merge all pseudo‐samples ps00…ps99 into a single matrix
echo "[$(date '+%Y-%m-%d %H:%M:%S')] 10-19."
#~/Tools_Scripts/csvtk join *_ps{00..09}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}.tsv
~/Tools_Scripts/csvtk join *_ps{10..19}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_10-19.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 20-29."
~/Tools_Scripts/csvtk join *_ps{20..29}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_20-29.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 30-39."
~/Tools_Scripts/csvtk join *_ps{30..39}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_30-39.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 40-49."
~/Tools_Scripts/csvtk join *_ps{40..49}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_40-49.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 50-59."
~/Tools_Scripts/csvtk join *_ps{50..59}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_50-59.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 60-69."
~/Tools_Scripts/csvtk join *_ps{60..69}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_60-69.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 70-79."
~/Tools_Scripts/csvtk join *_ps{70..79}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_70-79.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 80-89."
~/Tools_Scripts/csvtk join *_ps{80..89}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_80-89.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] 90-99."
~/Tools_Scripts/csvtk join *_ps{90..99}_k41.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > ../${TREAT_ID}_90-99.tsv

# Done
echo "[$(date '+%Y-%m-%d %H:%M:%S')] $TREAT_ID completed."
