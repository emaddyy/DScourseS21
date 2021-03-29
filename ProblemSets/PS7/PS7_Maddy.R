# ECON 5253
# Ethan Maddy
# 3/25/2021
# PS7

library(magrittr)
library(tidyverse)
library(mice)
library(modelsummary)

# Question no.4
wages <- read_csv("ProblemSets/PS7/wages.csv")

# Question no.5
wages <- wages %>% drop_na(tenure, hgc)

# Question no.6
datasummary_skim(wages, histogram=F,output="markdown")
datasummary_skim(wages, histogram=F,output="latex")

# logwage is missing at 25%. I believe that logwage is MNAR. 


# Question no.7
wages <- read_csv("ProblemSets/PS7/wages.csv")

# MNAR
est1 <- lm(logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married), 
           data = wages)
summary(est1)

# MCAR
wages %<>% mutate(logwage2 = case_when(!is.na(logwage) ~ logwage, is.na(logwage) 
                                             ~ mean(wages$logwage, na.rm =T)))

est2 <- lm(logwage2 ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age 
           + as.factor(married), data = wages)
summary(est2)

modelsummary(list(est1,est2), output = "markdown")
modelsummary(list(est1,est2), output = "latex")

# MAR 

predval = predict(est1, newdata = wages)
head(predval)

wages %<>% mutate(logwage3 = case_when(!is.na(logwage) ~ logwage, is.na (logwage) ~ predval))

est3 <- lm(logwage3 ~hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married), data = wages)

modelsummary(list(est1,est2,est3), output = "markdown")
modelsummary(list(est1,est2,est3), output = "latex")

# mice (I <3 mice)
library(mice)

imputed <- mice(wages, m = 10, printFlag = FALSE,  seed = 12345)
fit <- with(imputed, lm(logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T)
                                 + age + as.factor(married)))
mouses <- mice::pool(fit)

# model summary of 4 models
modelsummary(list(est1, est2, est3, mouses),output="latex")
modelsummary(list(est1, est2, est3, mouses),output="markdown")