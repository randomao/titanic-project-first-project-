### =======================================================================================
### (3) RBF Neural network
### This script assesses predictions from a basic tree
### =======================================================================================

### (0) Preliminaries
rm(list = ls())

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)

# # Load data
# load('train1.Rdata')
# train_raw <- as.data.table(train)
# 
# # define child as age 12 and below 
# train_raw[, male_child := 0]
# train_raw[Sex == 'male' & Age <= 1, male_child := 1]
# train_raw[Sex == 'male' & title == 'Master', male_child := 1]
# 
# build <- sample_frac(train_raw, 0.8)
# val <- train_raw[!(PassengerId %in% build$PassengerId)]
# 
# save(build, val, file = 'build_val_data.Rdata')
load('build_val_data.Rdata')

### =======================================================================================
### (1) Baseline: all females survive, all males don't
### =======================================================================================
val <- rbind(build, val)
val1 <- val[, score := ifelse(Sex == 'female', 1, 0)]

with(val1, Hmisc::somers2(score, Survived))

### =======================================================================================
### (2) Using observations from previous
### =======================================================================================
val2 <- val

val2[, score := 0]

# val2[Sex == 'female' & Pclass %in% c(1,2), score := 1]
val2[Sex == 'female', score := 1]

val2[Sex == 'male' & male_child == 1 & Pclass %in% c(1,2), score := 1]

with(val2, Hmisc::somers2(score, Survived))

### =======================================================================================
### (3) Predict with prev
### =======================================================================================

