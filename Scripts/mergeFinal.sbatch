#!/bin/bash
#SBATCH --job-name=merge_kmers
#SBATCH --array=0
#SBATCH --mem=350G
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=15

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 1"
~/Tools_Scripts/csvtk join FINAL_group1_00-09.tsv FINAL_group1_10-19.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 2"
~/Tools_Scripts/csvtk join FINAL.tsv FINAL_group1_20-29.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 3"
~/Tools_Scripts/csvtk join FINAL2.tsv FINAL_group1_30-39.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 4"
~/Tools_Scripts/csvtk join FINAL.tsv FINAL_group1_40-49.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 5"
~/Tools_Scripts/csvtk join FINAL2.tsv FINAL_group1_50-59.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 6"
~/Tools_Scripts/csvtk join FINAL.tsv FINAL_group1_60-69.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 7"
~/Tools_Scripts/csvtk join FINAL2.tsv FINAL_group1_70-79.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 8"
~/Tools_Scripts/csvtk join FINAL.tsv FINAL_group1_80-89.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL2.tsv

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 9"
~/Tools_Scripts/csvtk join FINAL2.tsv FINAL_group1_90-99.tsv -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > FINAL.tsv

mv FINAL.tsv FINAL_group1.tsv
