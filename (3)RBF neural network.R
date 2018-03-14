### =======================================================================================
### (3) RBF Neural network
### This script operationalises an RBF neural network for predicting titanic survivors
### =======================================================================================

### (0) Preliminaries
rm(list = ls())

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)

load('build_val_data.Rdata')


build_orig <- build
val_orig <- val
# [TODO] Fill in missing values for age as conditional averages
age_model <- lm(Age ~ 0 + factor(Sex) * factor(male_child) * factor(SibSp > 0) * 
                  factor(Parch > 0), data = build[!(is.na(Age))])
# summary(age_model)
build[is.na(Age), Age := predict(age_model, build[is.na(Age)], type = 'response')]
val[is.na(Age), Age := predict(age_model, val[is.na(Age)], type = 'response')]

# # check some stuff
# View(build[male_child == 1])

# Exclude groups which we can already predict
build <- build[!(Sex == 'female' & Pclass %in% c(1,2))]
build <- build[!(Sex == 'male' & male_child == 1 & Pclass %in% c(1,2))]

val <- val[!(Sex == 'female' & Pclass %in% c(1,2))]
val <- val[!(Sex == 'male' & male_child == 1 & Pclass %in% c(1,2))]

### =======================================================================================
### (1) Define hyper-parameters
### =======================================================================================

# Number of centres
# num_c <- 10
for(num_c in c(10, 20, 40, 80, 160)){

# beta: width of curve
for(beta_rbf in c(1,2,4,8,16)){

### =======================================================================================
### (2) Define centre vectors (using kmeans)
### =======================================================================================
# create data-table of independent variables
x <- build[, .(Pclass, male = ifelse(Sex == 'male',1,0), Age, SibSp, Parch, Fare
               , Embarked_ind = ifelse(Embarked == 'C', 1
                                      , ifelse(Embarked == 'S', 2
                                      , ifelse(Embarked == 'Q', 3, 4)))
               , male_child
               )]


# find centres
centres <- kmeans(x, num_c)[[2]]
# centres <- sample_n(x, num_c)

### =======================================================================================
### (3) Compute neuron outputs (radial basis functions)
### =======================================================================================

# initialise unit matrices
unit1 <- matrix(1, nrow = nrow(x), ncol = 1)
unit2 <- matrix(1, nrow = ncol(x), ncol = 1)

# loop over each centre
for(j in c(1:num_c)){
  
  ############################
  # Compute Euclidean distance using linear algebra
  cj <- centres[j,] %>% as.numeric()
  cj1 <- unit1 %*% cj
  
  d0 <- ((x - cj1)^2) %*% unit2
  d <- sqrt(d0)
  
  ###########################
  # Compute radial basis function
  
  phi_j <- exp(-(beta_rbf * (d^2)))
  
  ###########################
  # Update into data
  build[, paste0('phi', j) := phi_j]
  
  # print(j)
}


### =======================================================================================
### (4) Compute linear weights
### =======================================================================================

varlist <- paste0('phi', c(1:num_c))
varlist1 <- paste(varlist, collapse = '+')

model_weights <- lm(paste('Survived ~', varlist1), data = build)
summary(model_weights)

weights <- model_weights[[1]][c(1:num_c) + 1]

if(sum(is.na(weights)) > 0){next()}

# phi <- build[, .SD, .SDcols = (c(1:num_c) + ncol(build_orig))] %>% as.matrix()

# output <- phi %*% weights
# 
# build[, output := output]

### =======================================================================================
### (5) Score on holdout data
### =======================================================================================

x_val <- val[, .(Pclass, male = ifelse(Sex == 'male',1,0), Age, SibSp, Parch, Fare
                 , Embarked_ind = ifelse(Embarked == 'C', 1
                                         , ifelse(Embarked == 'S', 2
                                                  , ifelse(Embarked == 'Q', 3, 4)))
                 , male_child
                 )]


for(j in c(1:num_c)){
  
  ############################
  # Compute Euclidean distance using linear algebra
  cj <- centres[j,] %>% as.numeric()
  cj1 <- unit1 %*% cj
  
  d0 <- ((x_val - cj1)^2) %*% unit2
  d <- sqrt(d0)
  
  ###########################
  # Compute radial basis function
  
  phi_j <- exp(-(beta_rbf * (d^2)))
  
  ###########################
  # Update into data
  val[, paste0('phi', j) := phi_j]
  
  # print(j)
}

phi_val <- val[, .SD, .SDcols = (c(1:num_c) + ncol(build_orig))] %>% as.matrix()

output_val <- phi_val %*% weights

val1 <- val[, score := output_val]


somersD <- with(val, Hmisc::somers2(score, Survived))[1]

print(paste(num_c, beta_rbf, somersD))

}}