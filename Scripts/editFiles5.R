#!/usr/bin/env Rscript

library(data.table)
library(tidyverse)
library(Biostrings)

cdhit <- readDNAStringSet("kmers_clustered.fa")
seqs <- as.character(cdhit)
head(seqs)
length(seqs) 

kmer_metrics <- filter(kmer_metrics, kmer %in% seqs) #4.5M
head(kmer_metrics)
hist(kmer_metrics$cv)

# count matrices ----
path_count <- list.files("kmer_count", pattern = "_filter_min.rds$", full.names = TRUE)

count_clean <- data.table()

for(i in seq_along(path_count)){ #4h
  tmp <- readRDS(path_count[i]) %>% 
    filter(kmer %in% seqs2) %>% 
    arrange(factor(kmer, levels = seqs)) %>%
    column_to_rownames(var = 'kmer')
  
  count_clean <- cbind(count_clean, tmp)
}
rownames(count_clean) <- seqs
saveRDS('results/count_clean.rds')
