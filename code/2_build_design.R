# 2_initial_analysis.R
# Author: Sean Godwin
# Date: 2025-06-02
# Description: Play with Cole's pseudocode


## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # file referencing
library(tidyverse)   # data manipulation
library(data.table)  # faster data reading
library(rstan)       # R interface to Stan


## 1 [READ IN DATA] ------------------------------------------------------------
# Remember to change your path
# Read in recovery dataframe
rec <- as.data.frame(fread(here::here("./data/processed/recoveries.csv")))




## 00 [TEST CODE] --------------------------------------------------------------
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = here::here("./stan/models/test.stan"), data = schools_dat)


print(fit)
plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__"))

la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)