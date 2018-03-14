### =======================================================================================
### (4) Logistic Regression (with model selection)
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
### (1) Define parameters to explore
### =======================================================================================
varlist <- c('factor(Pclass)', 'Sex', 'Age', 'SibSp', 'Parch'
             , 'factor(Embarked)', 'male_child', 'factor(SibSp > 0)'
             , 'factor(Parch > 0)'
             
             # interaction terms
             , 'factor(Pclass):Sex', 'factor(Pclass):Age'
             , 'factor(Pclass):factor(SibSp > 0 | Parch > 0)'
             , 'factor(Pclass):male_child'
             , 'Sex:Age'
             )


### =======================================================================================
### (2) Loop over number of parameters
### =======================================================================================
# Initialise data frame to collect data
df <- data.frame(num_var = numeric(), var_name = character(), somersD = numeric()
                 , stringsAsFactors = F)


# Initialise with one variable: sex
model <- glm(Survived ~ Sex, family = binomial, data = build)
val_temp <- val[, score := predict(model, val, type = 'response')]
somersD_last <- with(val_temp, Hmisc::somers2(score, Survived))[1]

df[1,] <- c(1, 'Sex', somersD_last)
print(paste(1, 'Sex', somersD_last))

varlist_in <- c('Sex')

for(n in 2:length(varlist)){
  
  varlist_to_test <- varlist[-which(varlist %in% varlist_in)]
  
  for(v in varlist_to_test){
    
    varlist_test_temp <- c(varlist, v) %>% paste(., collapse = '+')
    
    model <- glm(paste('Survived ~ ', varlist_test_temp), family = binomial, data = build)
    
    val_temp <- val[, score := predict(model, val, type = 'response')]
    somersD_temp <- with(val_temp, Hmisc::somers2(score, Survived))[1]
    
    if(somersD_temp < somersD_last){ next()
    }else{
      
      df <- rbind(df, c(n, v, somersD_temp))
      
    }
    
  }
  
  df_n <- df[df$num_var == n,]
  if(nrow(df_n) == 0){ next() 
  }else{
      vari <- df_n[which.max(df_n$somersD), 'var_name']
      varlist_in <- c(varlist_in, vari)
      somersD_last <- df_n[df_n$var_name == vari, 'somersD']
      
      print(paste(n, vari, somersD_last))
      
    }
  
}

### =======================================================================================
### (3) Find score cut-off for greatest accuracy
### =======================================================================================
train <- rbind(build, val[, score:=NULL])
model <- glm(Survived ~ Sex + factor(Pclass), family = binomial, data = train)

train[, score:=predict(model, type = 'response')]
train[order(score),]

prop <- train[, .(prop_survived = mean(Survived)), by = .(score)]
prop
