# This script explores and visualises the data
# (1) Treatment of data
# (2) Analysis

# =================================================================================
# (0) Preliminaries
# =================================================================================
rm(list = ls())

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)
# library(data.table)

# Load data
train <- read.csv(file = 'train.csv', header = T, stringsAsFactors = F)
test <- read.csv(file = 'test.csv', header = T, stringsAsFactors = F)

# =================================================================================
# (1) Treatment of data
# =================================================================================

str(train)

# Examine missing data
table(train$Cabin == "")
# Age: 177 missing; can use title (Master vs Mr) to distinguish children vs adults
# Cabin: 687 missing

# =================================================================================
# Family name
train$family_name <-  gsub(",.*$", "", train$Name)

# Find how many other family members were on board, and how many of them survived
train_list <- with(train, split(train, family_name))

f1 <- function(x){
  if(nrow(x) == 1){
    x <- mutate(x, family_aboard = 1, family_survived = NA)
  }
  
  if(nrow(x) > 1){
    x <- mutate(x, family_aboard = nrow(x), 
                family_survived = sum(x$Survived) >= 1)
  }
  return(x)
  
}

train_list <- lapply(train_list, f1)
train <- do.call(rbind, train_list)

# =================================================================================
# Title of passenger
foo <- function(v, s, n){
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

# =================================================================================
# (2) Analysis of data
# =================================================================================

# =================================================================================
# How important is gender in determining survival?
table(subset(train, Sex == 'male')$Survived) %>% prop.table()
table(subset(train, Sex == 'female')$Survived) %>% prop.table()

# 74.2% of females survived vs. 19% of males

# =================================================================================
# Higher class passengers had a higher chance of survival
d <- ddply(train, .(Pclass, Sex), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
d
# True for both genders
# Females in 1st and 2nd class have >90% survival rate, further analysis should focus
# on males and females in 3rd class

# =================================================================================
# Children vs adults
# H: Male children (defined as age 12 and below) have a much better chance of survival than male adults
train_m <- subset(train, Sex == 'male') %>% mutate(., child = ifelse(Age <= 12, 1, 0))
train_m[is.na(train_m$Age) & train_m$title == 'Master', 'child'] <- 1
train_m[is.na(train_m$Age) & train_m$title != 'Master', 'child'] <- 0

d <- ddply(train_m, .(Pclass, child), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
d

# In each class, male child has better odds than male adults
# This is 100% for male children in 1st and 2nd class, but data is fairly thin (2 and 9 obs)

# H: The same is true for female children in third class
# Note that we are unable to distinguish female children by title because they go by 'Miss'

train_f3 <- subset(train, Sex == 'female' & Pclass == 3) %>% mutate(., child = ifelse(Age <= 12, 1, 0))
table(is.na(train_f3$Age))
# Note 42 missing ages

d <- ddply(train_f3, .(child), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
subset(d, !is.na(child))

# Female children no more likely to survive than female adults (where we have age information)
# However, both probabilities are lower than the overall which includes females with missing age
#   this could indicate some bias with missing obs!

# =================================================================================
# Port of embarkation should make no difference to survival
d1 <- ddply(train, .(Pclass, Sex, Embarked), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
ggplot(data = d1, aes(x = interaction(Pclass, Sex))) + 
  geom_point(aes(y = P_survive, size = num_pass, colour = factor(Embarked)))
# There seems to be something, but unsure if noise

# =================================================================================
# Do families travelling together have a better chance of survival than people travelling alone?
# What about female family members?
d2 <- subset(train, family_aboard > 1) %>%
  ddply(., .(Pclass, Sex), summarise, P_survive = mean(Survived), num_pass = length(PassengerId))
d2

# Not much difference for female passengers because survival rate is already high
# For male passengers in 2nd class travelling with family offers a somewhat higher
# chance of survival

# Is the effect on males because they are children?
d2 <- subset(train, family_aboard > 1 & Age > 10) %>%
  ddply(., .(Pclass, Sex), summarise, P_survive = mean(Survived), num_pass = length(PassengerId))
d2
# Yes, effects disappear if we remove children (below age 10)

# =================================================================================
# Overall

