#!/usr/bin/env Rscript

#args <- commandArgs(trailingOnly = TRUE)
#print(args[1])

#path_files <- args[1]
path_files <- 'FINAL_group1_50-59.rds'

library(data.table)
library(stringr)
library(tools)

setwd('/bulk/eakhunov/grcampos/embeddings/')


tmp_dt <- readRDS(paste0('kmer_count/', path_files))
kmers <- readRDS('kmer_count/kmer_filterMin.rds')

name_base <- file_path_sans_ext(basename(path_files))

cat("Min filter\n")
tmp_dt <- tmp_dt[kmer %in% kmers]

cat("Missing kmers\n")
missing_kmers <- setdiff(kmers, tmp_dt$kmer)

if (length(missing_kmers) > 0) {
  n_cols <- ncol(tmp_dt) - 1
  tmp_missing <- data.table(kmer = missing_kmers,
                            matrix(0, nrow = length(missing_kmers), ncol = n_cols))
  setnames(tmp_missing, colnames(tmp_dt))
  tmp_dt <- rbind(tmp_dt, tmp_missing)
}
gc()

cat("Ordering kmers\n")
tmp_dt <- tmp_dt[match(kmers, tmp_dt$kmer)]

cat("Saving\n")
saveRDS(tmp_dt, file = paste0('kmer_count/', name_base, '_filter_min.rds'))

cat("Done!\n")

