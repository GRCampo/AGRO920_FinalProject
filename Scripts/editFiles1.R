#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
print(args[1])

path_files <- args[1]

library(data.table)
library(stringr)

setwd('/bulk/eakhunov/grcampos/embeddings/')

tmp_mat <- fread(paste0('/bulk/eakhunov/grcampos/embeddings/kmer_count/', path_files))
colnames(tmp_mat)[-1] <- str_extract(colnames(tmp_mat)[-1], "SRR\\d+_ps\\d+")

kmer_check <- data.frame(kmer = tmp_mat$kmer,
                         count = rowSums(tmp_mat[, -1, with = FALSE]))

name_base <- tools::file_path_sans_ext(basename(path_files))

saveRDS(tmp_mat, file = paste0('/bulk/eakhunov/grcampos/embeddings/kmer_count/', name_base, '.rds'))
write.table(kmer_check, file = paste0('/bulk/eakhunov/grcampos/embeddings/kmer_count/count', name_base, '.tsv'), quote = F, row.names = F, sep = '\t')

print('Done!')
