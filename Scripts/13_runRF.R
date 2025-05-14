#!/usr/bin/env Rscript

cat("STARTING SCRIPT AT: ", as.character(Sys.time()), "\n")

## Data ----
library(tidyverse)
library(randomForest)
library(doParallel)
library(foreach)
library(stringr)

cat("Loading data...\n")

count_clean2 <- readRDS('~/AGRO920/results/count_clean2.rds')

treat_map <- c(SRR1542404 = "Control", SRR1542405 = "Control",
               SRR1542406 = "Drought_1h", SRR1542407 = "Drought_1h",
               SRR1542408 = "Drought_6h", SRR1542409 = "Drought_6h",
               SRR1542410 = "Heat_1h", SRR1542411 = "Heat_1h",
               SRR1542412 = "Heat_6h", SRR1542413 = "Heat_6h",
               SRR1542414 = "Drought_Heat_1h",SRR1542415 = "Drought_Heat_1h",
               SRR1542416 = "Drought_Heat_6h", SRR1542417 = "Drought_Heat_6h")

y <- data.frame(sample = sub("_.*", "", rownames(count_clean2)),
                ps = rownames(count_clean2))
y$treat <- factor(treat_map[y$sample])

set.seed(2310)
sort_data <- data.frame(Sample = sort(unique(y$sample)),
                        Rep = factor(1:2))
sort_data$Trat <- treat_map[sort_data$Sample] 

group1 <- sort_data %>% 
  group_by(Trat) %>% 
  slice_sample(n = 1) %>% 
  ungroup()

y_train <- filter(y, sample %in% group1$Sample) 
y_test <- filter(y, !(sample %in% group1$Sample)) 

train_idx <- rownames(count_clean2) %in% y_train$ps
X_train  <- count_clean2[train_idx, , drop = FALSE]

test_idx  <- rownames(count_clean2) %in% y_test$ps
X_test   <- count_clean2[test_idx,  , drop = FALSE]

treatments <- unique(y_train$treat)

labels <- c("drought_1h", "drought_6h", "heat_1h", "heat_6h")

y_multi <- y[, 1:3] %>%
  mutate(drought_1h  = ifelse(str_detect(treat, "Drought") & str_detect(treat, "1h"), 1, 0),
         drought_6h  = ifelse(str_detect(treat, "Drought") & str_detect(treat, "6h"), 1, 0),
         heat_1h     = ifelse(str_detect(treat, "Heat") & str_detect(treat, "1h"), 1, 0),
         heat_6h     = ifelse(str_detect(treat, "Heat") & str_detect(treat, "6h"), 1, 0))

y_train_multi <- y_multi %>% 
  filter(sample %in% group1$Sample) %>% 
  select(-sample, -treat)

y_test_multi <- y_multi %>% 
  filter(!(sample %in% group1$Sample)) %>% 
  select(-sample, -treat)

X_train <- as.data.frame(X_train)
X_train$control <- ifelse(y_train$treat == "Control", 1, 0)

X_test <- as.data.frame(X_test)
X_test$control <- ifelse(y_test$treat == "Control", 1, 0)

## Training ----
cat("TRAINING MODELS - START: ", as.character(Sys.time()), "\n")

cl <- makeCluster(30)
registerDoParallel(cl)

mtry_vals <- c(500, 1000, ncol(X_train))
nodesize_vals <- c(5, 10, 20)
grid <- expand.grid(mtry = mtry_vals, nodesize = nodesize_vals)

results_grid <- list()

for (label in labels) {
  results_grid[[label]] <- foreach(i = 1:nrow(grid), .packages = "randomForest") %dopar% {
    
    m <- grid$mtry[i]
    n <- grid$nodesize[i]
    
    result_model_list <- list()
    
    for (t in treatments) {
      idx_test <- which(y_train$treat == t)
      idx_train <- which(y_train$treat != t)
      
      X_tmp <- as.data.frame(X_train[idx_train, ])
      X_tmp$control <- ifelse(y_train$treat[idx_train] == "Control", 1, 0)
      
      y_tmp <- factor(y_train_multi[[label]][idx_train])
      
      rf_model <- randomForest(x = X_tmp,
                               y = y_tmp,
                               ntree = 500,
                               mtry = m,
                               nodesize = n,
                               importance = TRUE,
                               classwt = c("0" = 1, "1" = 2.5))
      
      pred_prob <- predict(rf_model, newdata = cbind(X_train[idx_test, ], control = ifelse(y_train$treat[idx_test] == "Control", 1, 0)),type = "prob")[, 2]
      pred <- ifelse(pred_prob > 0.5, 1, 0)
      obs <- y_train_multi[[label]][idx_test]
      
      result_model_list[[t]] <- data.frame(
        treatment = t,
        predicted = pred,
	prob = pred_prob,
        observed = obs,
        mtry = m,
        nodesize = n
      )
    }
    
    do.call(rbind, result_model_list)
  }
}

cat("TRAINING MODELS - END: ", as.character(Sys.time()), "\n")

## Test ----
cat("TESTING MODELS - START: ", as.character(Sys.time()), "\n")

rf_test_models <- list()

for (label in labels) {
  y_train_label <- factor(y_train_multi[[label]])
  y_test_label  <- factor(y_test_multi[[label]])
  
  rf_test_models[[label]] <- foreach(i = 1:nrow(grid), .packages = "randomForest") %dopar% {
    m <- grid$mtry[i]
    n <- grid$nodesize[i]
    
    rf_model <- randomForest(x = X_train, y = y_train_label,
                             ntree = 500,
                             mtry = m,
                             nodesize = n,
                             importance = TRUE,
                             classwt = c("0" = 1, "1" = 2.5))
    
    pred_prob <- predict(rf_model, X_test, type = "prob")[,2]
    pred <- ifelse(pred_prob > 0.5, 1, 0)
    
    acc <- mean(pred == y_test_label)
    
    list(
      model = rf_model,
      mtry = m,
      nodesize = n,
      acc = acc,
      predictions = pred,
      predicted_prob = pred_prob,
      observed = y_test_label
    )
  }
}

stopCluster(cl)
registerDoSEQ()

cat("TESTING MODELS - END: ", as.character(Sys.time()), "\n")

## Saving ----
RF_final <- list(training = results_grid,
                 test = rf_test_models)

saveRDS(RF_final, '~/AGRO920/results/RF_final.rds')

cat("SCRIPT FINISHED AT: ", as.character(Sys.time()), "\n")

