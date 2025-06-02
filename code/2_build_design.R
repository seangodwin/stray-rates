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

