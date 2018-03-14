### =======================================================================================
### (5) Predict using observational rules and logistic regression
### =======================================================================================

### (0) Preliminaries
rm(list = ls())

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)

# Load data
train <- read.csv('train.csv') %>% as.data.table()
test <- read.csv('test.csv') %>% as.data.table()


### =======================================================================================
### (1) Cleaning data
### =======================================================================================
# Find male child using title

foo <- function(v, s, n){
  v <- as.character(v)
  x <- strsplit(v, split = s)
  for(i in 1:length(x)){
    if(i == 1){y <- x[[i]][n]
    }else{
      y <- c(y, x[[i]][n])
    }
  }
  return(y)
}

x1 <- foo(train$Name, ",", 2)
x2 <- foo(x1, "[.]", 1)
head(x2)
x2 <- sub(" ", "", x2)
train$title <- x2

x1 <- foo(test$Name, ",", 2)
x2 <- foo(x1, "[.]", 1)
head(x2)
x2 <- sub(" ", "", x2)
test$title <- x2

train[, male_child := 0]
train[Sex == 'male' & Age <= 12, male_child := 1 ]
train[Sex == 'male' & title == 'Master', male_child := 1 ]

test[, male_child := 0]
test[Sex == 'male' & Age <= 12, male_child := 1 ]
test[Sex == 'male' & title == 'Master', male_child := 1 ]



#################################################
# Fill in missing ages as conditional means
age_model <- lm(Age ~ 0 + factor(Sex) * factor(male_child) * factor(SibSp > 0) * 
                  factor(Parch > 0), data = train[!(is.na(Age))])

# summary(age_model)
train[is.na(Age), Age := predict(age_model, train[is.na(Age)], type = 'response')]
test[is.na(Age), Age := predict(age_model, test[is.na(Age)], type = 'response')]


### =======================================================================================
### (2) Build logistic regression model
### =======================================================================================
build <- train[!(Sex == 'female' & Pclass %in% c(1,2))]
build <- train[!(Sex == 'male' & male_child == 1 & Pclass %in% c(1,2))]

model <- glm(Survived ~ Sex + factor(Pclass), family = binomial, data = build)


### =======================================================================================
### (3) Predict
### =======================================================================================

test[Sex == 'female' & Pclass %in% c(1,2), score := 1]
test[Sex == 'male' & male_child == 1 & Pclass %in% c(1,2), score := 1]

test[is.na(score), score := predict(model, test[is.na(score)], type = 'response')]

test_final <- test[, .(PassengerId, Survived = ifelse(score >= 0.5 ,1, 0))]