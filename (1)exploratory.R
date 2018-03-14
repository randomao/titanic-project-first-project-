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
train$family_name <- gsub(",.*$", "", train$Name)

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

save(train, file = 'train1.Rdata')


# =================================================================================
# (2) Analysis of data
# =================================================================================

# =================================================================================
# How important is gender in determining survival?
table(subset(train, Sex == 'male')$Survived) %>% prop.table()
table(subset(train, Sex == 'female')$Survived) %>% prop.table()

# 74.2% of females survived vs. 19% of males

# =================================================================================
# [H]: Higher class passengers had a higher chance of survival
d1 <- ddply(train, .(Pclass, Sex), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
d1
# True for both genders
# [C]: Females in 1st and 2nd class have >90% survival rate, further analysis should focus
#   on males and females in 3rd class

# =================================================================================
# Children vs adults
# [H]: Male children (defined as age 12 and below) have a much better chance of survival than male adults
train_m <- subset(train, Sex == 'male') %>% mutate(., child = ifelse(Age <= 12, 1, 0))
train_m[is.na(train_m$Age) & train_m$title == 'Master', 'child'] <- 1
train_m[is.na(train_m$Age) & train_m$title != 'Master', 'child'] <- 0

d2 <- ddply(train_m, .(Pclass, child), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
d2

# In each class, male child has better odds than male adults
# This is 100% for male children in 1st and 2nd class, but data is fairly thin (2 and 9 obs)
# Furthermore, male adults in 2nd class have < 10% chance of survival

# [C]: We can predict:
#   (i) survive for male children in 1st and 2nd class, (?)
#   (ii) not survive for male adults in 2nd class, and
#   focus attention on male adults in 1st and 3rd class

# [H]: The same is true for female children in third class
# Note that we are unable to distinguish female children by title because they go by 'Miss'

train_f3 <- subset(train, Sex == 'female' & Pclass == 3) %>% mutate(., child = ifelse(Age <= 12, 1, 0))
table(is.na(train_f3$Age))
# Note 42 missing ages

d3 <- ddply(train_f3, .(child), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
subset(d3, !is.na(child))

# Female children no more likely to survive than female adults (where we have age information)
# However, both probabilities are lower than the overall which includes females with missing age
#   this could indicate some bias with missing obs!

# =================================================================================
# [H]: Port of embarkation should make no difference to survival
d4 <- ddply(train, .(Pclass, Sex, Embarked), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
ggplot(data = d4, aes(x = interaction(Pclass, Sex))) + 
  geom_point(aes(y = P_survive, size = num_pass, colour = factor(Embarked)))

# Male passengers from Cherbourg seem to have small but consistent better odds than passengers from
# Southampton. This is true across sex and Pclass.
# Unlikely to be noise as the pattern is repeated for females.
# Most plausible explanation is that port of embarkation captures some variables
# not captured by sex and Pclass. Come back to this after everything else.

# =================================================================================
# Families
# [H]: Male adults in class 1/2 travelling with families are more likely to survive.
d5 <- subset(train_m, child ==0) %>% mutate(., family = ifelse((SibSp > 0 | Parch >0), 1, 0)) %>%
  ddply(., .(Pclass, family), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
d5

# No substantial difference in odds

# [H]: Male adults who are parents are less likely to survive
# [H]: Single males with siblings are less likely to survive
# [H]: What about interactions
d6 <- subset(train_m, child == 0) %>% mutate(., sibsp = SibSp > 0, parent = Parch > 0) %>%
  ddply(., .(Pclass, sibsp, parent), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))
subset(d6, num_pass > 10)

# 2nd and 3rd class fathers have 0% survival (but data is thin for 2nd class)

# In 1st class, males with sibsp (but no parch) are significantly more likely to survive (46% vs avg 33%)

# Majority of males travelled alone, in all classes
# Males who are both fathers and brothers are negligible except in 3rd class, and
# they have a lower chance of survival than all other groups


# [C]: Predict the following
#   (i) Male with family in 3rd class: survive = 0
#   
#   Date is reasonably thick (24)


# =================================================================================
# Continuous variable analysis for (i) female 3rd class, (ii) male 1st class travelling alone,
# (iii) male 1st/3rd class travelling alone
# =================================================================================
# 
# Male adults travelling alone
train_m_a <- subset(train_m, child == 0 & SibSp == 0 & Parch == 0) %>% 
  mutate(., age_range = ceiling(Age/10) * 10) %>%
  ddply(., .(Pclass, age_range), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))

ggplot(data = train_m_a, aes(x = age_range, colour = factor(Pclass))) + geom_line(aes(y = P_survive)) +
  geom_point(aes(y = P_survive, size = num_pass))

# =================================================================================
# Male adults with family, 1st class
train_m_1f <- subset(train_m, Pclass == 1 & !(child == 0 & SibSp == 0 & Parch == 0)) %>%
  mutate(., age_range = ceiling(Age/10) * 10) %>%
  ddply(., .(Pclass, age_range), summarise, P_survive = mean(Survived), num_pass = length(unique(PassengerId)))

ggplot(data = train_m_1f, aes(x = age_range, colour = factor(Pclass))) + geom_line(aes(y = P_survive)) +
  geom_point(aes(y = P_survive, size = num_pass))

