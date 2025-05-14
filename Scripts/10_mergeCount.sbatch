#!/bin/bash
#SBATCH --job-name=merge_count2
#SBATCH --array=0
#SBATCH --mem=200G
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=15

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 1"
~/Tools_Scripts/csvtk join count_00-09.tsv count_10-19.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 2"
~/Tools_Scripts/csvtk join FINAL.tsv count_20-29.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 3"
~/Tools_Scripts/csvtk join FINAL2.tsv count_30-39.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 4"
~/Tools_Scripts/csvtk join FINAL.tsv count_40-49.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 5"
~/Tools_Scripts/csvtk join FINAL2.tsv count_50-59.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 6"
~/Tools_Scripts/csvtk join FINAL.tsv count_60-69.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 7"
~/Tools_Scripts/csvtk join FINAL2.tsv count_70-79.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 8"
~/Tools_Scripts/csvtk join FINAL.tsv count_80-89.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 9"
~/Tools_Scripts/csvtk join FINAL2.tsv count_90-99.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

mv FINAL.tsv kmer_count.tsv
rm FINAL2.tsv
