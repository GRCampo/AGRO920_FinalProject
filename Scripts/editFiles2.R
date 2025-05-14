#!/usr/bin/env Rscript

library(data.table)

kmer_count <- fread('kmer_count/kmer_count.tsv') %>%
  rename_with(~ str_extract(., "group[12](?:_\\d{2}-\\d{2})?"), -1) %>%
  mutate(TOTAL = rowSums(across(-1))) %>%
  filter(TOTAL >= 10) %>%
  select(1, TOTAL) #45M

saveRDS(as.vector(kmer_filterMin$kmer), file = 'kmer_count/kmer_filterMin.rds')
