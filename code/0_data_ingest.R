# 0_data_merging.R
# Author: Sean Godwin
# Date: 2025-05-09
# Description: Combine all the small RMIS files for salmon tag recoveries


## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # for file referencing
library(tidyverse)   # for data manipulation
library(data.table)  # for faster data reading/combining


## 1 [READ IN DATA] ------------------------------------------------------------
# Remember to change your path
# Most of the full datafiles and metadata can be found here:
#     https://www.rmpc.org/data-selection/rmis-files/

# First, read in recovery data, grouped by region
# This is the one data type you can't get a full dataset from the url above
# These files were generated at: 
#     https://www.rmis.org/cgi-bin/queryfrm.mpl?
#     Table=recoveriesbytagcode&tag_status=1&Version=4.2
# I only used the following in the 'Fishery' query criterion, for now
#     21 - Columbia River Gillnet
#     24 - Freshwater Net
#     27 - Freshwater Seine
#     44 - Columbia River Sport
#     46 - Freshwater Sport
#     47 - Freshwater Snag
#     50 - Hatchery
#     52 - Fish trap (freshwater)
#     53 - Wild Broodstock Collection
#     54 - Spawning Ground
#     57 - Mixed Wild Broodstock and Hatchery Returns
# And in the 'Recovery Location RMIS Region' query box, I selected an individual
#     domain, then all the regions within it
# For some domains and regions, there were too many observations, so I had to 
#     subset by year as well
# If you do it from this url instead, you miss out on some key release columns,
#     and need to remember to filter out tag status != 1 (i.e., recovered)
#     https://www.rmis.org/cgi-bin/queryfrm.mpl?Table=all_recoveries&Version=4.2

# Temporarily set root manually for Marty's cluster
set_here("/home/maddie/sean/")

# Find filenames
rec.files <- list.files(path = here::here("./data/raw/recovery-data-minifiles/"), 
                        pattern = "recoveries", 
                        full.names = TRUE)

# Make object names
rec.names <- paste("rec", 
                  sub(".*_(.*)\\.TXT", "\\1", rec.files), 
                  sep=".")

# Read in data
invisible(mapply(function(name, file) {
    assign(name, 
        as.data.frame(
             fread(file, 
                   # Drop columns I don't think I'll ever need, for smaller file
                   drop = c('record_code', 'format_version', 
                            'recovery_date_type', 'period',
                            'period_type', 'adclip_selective_fishery',
                            'estimation_level', 'sequential_number',
                            'sequential_column_number', 'sequential_row_number',
                            'sampled_run', 'sampled_length_range','sampled_sex',
                            'sampled_mark', 'number_cwt_estimated',
                            'record_origin'))), 
        envir = .GlobalEnv)
}, rec.names, rec.files))


## 2 [COMBINE DATA] ------------------------------------------------------------
# Combine way faster than do.call(rbind...)
rec <- as.data.frame(rbindlist(mget(rec.names)))

# Remove the couple commas in the dataset (ugly for now)
#    this means we won't be able to match a couple individual records to 
#    e.g., match location codes in RMIS database, but have to do to import csv
rec$recovery_location_name <- gsub(",", "", rec$recovery_location_name)
rec$hatchery_location_name <- gsub(",", "", rec$hatchery_location_name)
rec$stock_location_name <- gsub(",", "", rec$stock_location_name)


## 3 [WRITE DATA] --------------------------------------------------------------
write.csv(rec, here::here("./data/raw/recoveries_raw.csv"), quote=F, row.names=F)
