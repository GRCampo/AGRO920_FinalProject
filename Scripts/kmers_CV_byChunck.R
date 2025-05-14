#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
chunk_id <- as.integer(args[1])

library(data.table)

setwd('/bulk/eakhunov/grcampos/embeddings/')

files <- list.files("kmer_count/", pattern = "_filter_min.rds$", full.names = TRUE)
chunk_size <- 10000
n_total <- nrow(readRDS(files[1]))

rowSds <- function(x) sqrt(rowMeans((x - rowMeans(x))^2))

line_start <- (chunk_id - 1) * chunk_size + 1
line_end <- min(chunk_id * chunk_size, n_total)

cat("Processing chunk", chunk_id, ":", line_start, "-", line_end, "\n")

all_cols <- vector("list", length(files))
kmer_ref <- NULL

for (j in seq_along(files)) {
  dt <- readRDS(files[j])[line_start:line_end]
  if (j == 1) kmer_ref <- dt$kmer
  all_cols[[j]] <- dt[, -1, with = FALSE]
}

mat <- do.call(cbind, all_cols)

media <- rowMeans(mat)
desvio <- rowSds(mat)
cv <- desvio / media

result <- data.table(kmer = kmer_ref, mean = media, sd = desvio, cv = cv)

# saving
out_path <- sprintf("kmer_count/chunks_cv/chunk_%04d.rds", chunk_id)
dir.create("kmer_count/chunks_cv", showWarnings = FALSE)
saveRDS(result, out_path)

cat("Chunk", chunk_id, "saved to", out_path, "\n")

