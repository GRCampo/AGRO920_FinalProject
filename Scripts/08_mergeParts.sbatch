#!/bin/bash
#SBATCH --job-name=merge_kmers
#SBATCH --array=0
#SBATCH --mem=200G
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=15

PARTS=(_00-09.tsv _10-19.tsv _20-29.tsv _30-39.tsv _40-49.tsv _50-59.tsv _60-69.tsv _70-79.tsv _80-89.tsv _90-99.tsv)
PART=${PARTS[$SLURM_ARRAY_TASK_ID]}

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 1"
~/Tools_Scripts/csvtk join "SRR1542404$PART" "SRR1542407$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final$PART"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 2"
~/Tools_Scripts/csvtk join "final$PART" "SRR1542409$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final2$PART"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 3"
~/Tools_Scripts/csvtk join "final2$PART" "SRR1542411$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final$PART"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 4"
~/Tools_Scripts/csvtk join "final$PART" "SRR1542413$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final2$PART"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 5"
~/Tools_Scripts/csvtk join "final2$PART" "SRR1542415$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final$PART"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 6"
~/Tools_Scripts/csvtk join "final$PART" "SRR1542416$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final2$PART"

mv "final2$PART" "FINAL_group1$PART"

#echo "-----Group 2------"
#echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 1"
#~/Tools_Scripts/csvtk join "SRR1542405$PART" "SRR1542406$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final$PART"

#echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 2"
#~/Tools_Scripts/csvtk join "final$PART" "SRR1542408$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final2$PART"

#echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 3"
#~/Tools_Scripts/csvtk join "final2$PART" "SRR1542410$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final$PART"

#echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 4"
#~/Tools_Scripts/csvtk join "final$PART" "SRR1542412$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final2$PART"

#echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 5"
#~/Tools_Scripts/csvtk join "final2$PART" "SRR1542414$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final$PART"

#echo "[$(date '+%Y-%m-%d %H:%M:%S')] step 6"
#~/Tools_Scripts/csvtk join "final$PART" "SRR1542417$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "final2$PART"

#mv "final2$PART" "FINAL_group2$PART"
