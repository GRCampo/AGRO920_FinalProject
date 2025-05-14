#!/usr/bin/env Rscript

library(data.table)
library(tidyverse)

file_paths <- paste0('kmer_count/chunks_cv/', list.files('kmer_count/chunks_cv/'))

kmer_cv <- lapply(file_paths, readRDS)
kmer_cv <- do.call(rbind, kmer_cv)

table(kmer_count$kmer %in% kmer_cv$kmer)

kmer_metrics <- left_join(kmer_cv, kmer_count, by = "kmer")

# top 25%
fasta_lines <-  kmer_metrics %>%
  filter(cv > quantile(cv, 0.75)) %>%
  arrange(desc(cv), desc(TOTAL)) %>%
  select(-mean, -sd)

fasta_lines <- paste0(">kmer", seq_len(nrow(kmer_metrics)), "\n", kmer_metrics$kmer)

write_lines(fasta_lines, "kmers_min_cv.fa")
