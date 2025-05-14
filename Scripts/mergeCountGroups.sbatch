#!/bin/bash
#SBATCH --job-name=merge_count
#SBATCH --array=0-9
#SBATCH --mem=200G
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=15

PARTS=(_00-09.tsv _10-19.tsv _20-29.tsv _30-39.tsv _40-49.tsv _50-59.tsv _60-69.tsv _70-79.tsv _80-89.tsv _90-99.tsv)
PART=${PARTS[$SLURM_ARRAY_TASK_ID]}

echo "[$(date '+%Y-%m-%d %H:%M:%S')]"
~/Tools_Scripts/csvtk join "countFINAL_group1$PART" "countFINAL_group2$PART" -t -f 1 -j 15 --outer-join --na 0 --prefix-filename --prefix-trim-ext > "count$PART"
echo "Done"
