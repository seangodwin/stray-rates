# 1_data_wrangling.R
# Author: Sean Godwin
# Date: 2025-06-02
# Description: Get the recovery datafile ready to go


## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # file referencing
library(tidyverse)   # data manipulation
library(data.table)  # faster data reading
library(terra)       # spatial data manipulation
library(sf)          # old spatial functions


## 1 [READ IN NONSPATIAL DATA] -------------------------------------------------
# Remember to change your path
# Identify root directory
here::i_am("code/1_data_prep.R")

# Read in recovery data
rec <- as.data.frame(fread(here::here("./data/raw/recoveries.csv")))

# So much we don't need, and it gets annoying, so trim:
rec <- rec[,c('recovery_date', 'species', 'tag_code', 'brood_year', 'fishery', 
              'last_release_date_year', 'sampled_maturity', 
              'stock_location_code', 'recovery_location_code',
              'release_location_state', 'release_location_rmis_region',
              'release_location_rmis_basin')]
colnames(rec) <- c('rec.date', 'species', 'tag.code', 'brood.year', 'fishery',
                   'last.rel.year', 'samp.maturity', 
                   'stock.loc.code', 'rec.loc.code',
                   'rel.state', 'rel.region', 'rel.basin')

# Location data was downloaded from here:
#     https://www.rmis.org/cgi-bin/queryfrm.mpl?Table=locations&Version=4.2
# By checking off all options in the 'Location Type' query criterion
# For some reason this gives RMIS regions & basins, whereas the main file
#     ("LC042_ALL_FULLSET.csv") does not
loc <- read.csv(here::here("./data/raw/locations.csv"), 
                header=T, stringsAsFactors=F)

# Trim
loc <- loc[,c('location_code', 'location_type', 'name', 'rmis_region', 
              'rmis_basin', 'rmis_latitude', 'rmis_longitude'),]
colnames(loc) <- c('loc.code', 'loc.type', 'loc.name', 'region',
                   'basin', 'lat', 'lon')

# Release datafile was downloaded as "RL042_ALL_FULLSET.csv" from:
#     https://www.rmpc.org/data-selection/rmis-files/
rel <- read.csv(here::here("./data/raw/releases.csv"), 
                header=T, stringsAsFactors=F)
# Trim
rel <- rel[,c('submission_date', 'tag_code_or_release_id', 'tag_type', 
              'species', 'run', 'brood_year', 'first_release_date', 
              'last_release_date', 'release_location_code', 
              'hatchery_location_code', 'stock_location_code', 'rearing_type',
              'avg_weight', 'avg_length'),]
colnames(rel) <- c('sub.date', 'tag.code.or.release.id', 'tag.type',
                   'species', 'run', 'brood.year', 'first.rel.date',
                   'last.rel.date', 'rel.loc.code', 
                   'hatchery.loc.code', 'stock.loc.code', 'rear.type',
                   'avg.weight', 'avg.length')


## 2 [FORMAT NONSPATIAL DATA] --------------------------------------------------
# All this is ugly AF but I'm doing quickly
# Match in recovery locations
rec$rec.region <- 
        loc$region[match(rec$rec.loc.code, loc$loc.code)]
rec$rec.basin <- 
        loc$basin[match(rec$rec.loc.code, loc$loc.code)]

# Add age column
rec$rec.year <- as.numeric(substr(rec$rec.date,1,4))
rec$rec.date <- NULL
rec$age <- rec$rec.year - rec$brood.year

# Match tag code or release IDs with those in release file
rec$rel.loc.code <- rel$rel.loc.code[match(rec$tag.code, 
                                           rel$tag.code.or.release.id)]

# Same with lats/lons 
rec$rel.lat <- loc$lat[match(rec$rel.loc.code, loc$loc.code)]
rec$rel.lon <- loc$lon[match(rec$rel.loc.code, loc$loc.code)]
rec$rec.lat <- loc$lat[match(rec$rec.loc.code, loc$loc.code)]
rec$rec.lon <- loc$lon[match(rec$rec.loc.code, loc$loc.code)]

# Add rearing type
#     W = wild, H = hatchery-reared, M = mixed, U = unknown
rec$rear.type <- rel$rear.type[match(rec$tag.code, 
                                     rel$tag.code.or.release.id)]

##### FUTURE CHECK 
# 6.7% of the release basins in the recoveries dataset do NOT match up with
#     the basins grabbed using the release locations in the location dataset
rel.basin2 <- loc$basin[match(rec$rel.loc.code, loc$loc.code)]
check.basin <- data.frame(mismatch = rec$rel.basin != rel.basin2,
                          which = seq(1:nrow(rec)))
check.basin <- check.basin[check.basin$mismatch,]
nrow(check.basin)/nrow(rec)
# For now I guess we just overwrite the regions and basins in the recoveries 
#     dataset, but this is definitely something we need to ask about
rec$rel.region <- loc$region[match(rec$rel.loc.code, loc$loc.code)]
rec$rel.basin <- loc$basin[match(rec$rel.loc.code, loc$loc.code)]

# Reorder columns (this got silly, but w/e)
rec <- rec %>% 
          relocate(age, .after = species) %>%
          relocate(rec.year, .after = last.rel.year) %>%
          relocate(rel.loc.code, rel.state, rel.region, rel.basin, rel.lat, 
                   rel.lon, .after = stock.loc.code) %>%
          relocate(rec.loc.code, rec.region, rec.basin, rec.lat, rec.lon,
                   .after = rel.lon)

# Remove the ones we know came from juvenile surveys
# Sampled maturity: 1 = immature, 2 = jacks, 3 = adults, 4 = undetermined
rec %>% group_by(samp.maturity) %>% summarize(n=n())
rec <- rec[rec$samp.maturity != 1,]

##### FUTURE CHECK 
# For now, remove <=2 year old fish for all species except pink (<=1 year old)
#     to ensure we're not getting recaps of juveniles not gone to sea
# Also remove unrealistic ages
# Species: 1 = Chinook, 2 = coho, 3 = steelhead, 4 = sockeye, 5 = chum, 6= pink
#          7 = masu, 8 = cutthroat, 9 = Atlantic
rec %>% group_by(species, age) %>% summarize(n=n()) %>% print(n = Inf)
rec <- rec %>%
       filter((species == 6 & age ==2) | 
              (species != 6 & age >3 & age<=7))
       
# Remove everything pre-2018 brood year since we're going up to age 7
#     (to ensure full age classes)
rec <- rec[rec$brood.year <= 2018,]

# Only keep Chinook for now, as it's the only one with good sample size 
#     across the whole time series
rec %>% group_by(species, age) %>% summarize(n=n()) %>% print(n = Inf)
rec <- rec[rec$species == 1,]

# #########WILL NEED TO DO SOMETHING LIKE THIS IF WE'RE USING BASIN OR REGION
# ##### FUTURE CHECK 
# # Will do this cleaner later, but here's a list of general regins to exclude
# # From map: https://www.rmpc.org/wp-content/uploads/2022/07/RMIS_IndexDRB.pdf
# region.exc <- c("AKGN", "BCGN", "CAGN", "CRGN", "ORGN", "TRGN", "")
# # Remove these regions
# rec <- rec[is.na(rec$rel.region)==F & 
#              !(rec$rel.region %in% region.exc) & 
#              !(rec$rec.region %in% region.exc),]
# 
# ##### FUTURE CHECK
# # Remove some basins
# # Will do this cleaner later, but here's a list of general basins to exclude
# # From map: https://www.rmpc.org/wp-content/uploads/2022/07/RMIS_IndexDRB.pdf
# basin.exc <- c("CEAKG", "NOAKG", "SEAKG", "AKGNG", 
#                # Can keep QCIG I think because it's only basin in the region
#                "CEBCG", "GSTG", "JNSTG", "NOBCG", "WCVIG", "BCGNG",
#                "CECAG", "KLTRG", "NOCAG", "SAFAG", "SJOAG", "SOCAG", "CAGNG",
#                "CECRG", "LOCRG", "SNAKG", "UPCRG", "CRGNG",
#                "NOORG", "SOORG", "ORGNG",
#                # Same for thhe individual transboundary rivers, I think
#                "TRGNG", "")
# rec <- rec[is.na(rec$rel.basin)==F & 
#              !(rec$rel.basin %in% basin.exc) & 
#              !(rec$rec.basin %in% basin.exc),]


## 3 [READ IN SPATIAL DATA] ----------------------------------------------------
# States and provinces
    # From here: https://www.weather.gov/gis/AWIPSspatial
states <- vect(here::here("./data/raw/spatial/s_18mr25.shp"))
provinces <- vect(here::here("./data/raw/spatial/province.shp"))

# Rivers, from:
  # https://open.canada.ca/data/dataset/1c0e8a4e-952c-cd7a-1be8-29691aea2cd1
  # https://www.epa.gov/waterdata/nhdplus-national-data
riv.bc <- vect(here::here("./data/raw/spatial/c1w_rivers/c1w_NHN_NLFLOW_Strahler_pacific_3979.gpkg"))
riv.us <- vect(here::here("./data/raw/spatial/nhdplus_rivers/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"), 
               layer = "NHDFlowline_Network")

##### FUTURE CHECK
# Watersheds, from:
    # https://databasin.org/datasets/cd69ec510558421eb908d078f59e241c/
    # Unfortunately missing most of AK, so it would be good to expand
    # if possible to keep the northern Alaska recovery data
ws <- vect(here::here("./data/raw/spatial/na_bas_15s_beta.shp"))
names(ws) <- c("ws.id", "area")

# Bathymetry, from:
    # https://download.gebco.net/
bath <- rast(here::here("./data/raw/spatial/gebco_2024.tif"))


## 4 [FORMAT SPATIAL DATA] ------------------------------------------------------------
# CRS projection
crs.latlon <- "EPSG:4326"    # WGS84, in lat lon
crs.meters <- "EPSG:5070"    # Different CRS, in meters

# Remove everything except California and PNW
riv.us <- riv.us[riv.us$VPUID %in% c("18", "17"),]

# Do some formatting
riv.bc <- riv.bc[,c('RivID', 'Shreve', 'Strahler', 'Segment')]
names(riv.bc) <- c("riv.id", "shreve", "strahler", "segment.id")
riv.us <- riv.us[,c('GNIS_ID', 'StreamCalc')]
names(riv.us) <- c("riv.id", "strahler")

# Remove small streams for now
riv.bc <- riv.bc[riv.bc$strahler > 4,]
riv.us <- riv.us[riv.us$strahler > 4,]

# Make SpatVector for unique release locations
rel.summ <- as.data.frame(rec %>% 
                            group_by(rel.loc.code) %>%
                            summarize(lat = first(rel.lat),
                                      lon = first(rel.lon),
                                      n = n()))
rel.locs <- vect(rel.summ, geom=c("lon", "lat"), crs = crs.latlon)

# Make SpatVector for unique recovery locations
rec.summ <- as.data.frame(rec %>% 
                          group_by(rec.loc.code) %>%
                          summarize(lat = first(rec.lat),
                                    lon = first(rec.lon),
                                    n = n()))
rec.locs <- vect(rec.summ, geom=c("lon", "lat"), crs = crs.latlon)

# Make sure all SpatVectors are in latlon CRS (ugly for now)
states <- project(states, crs.latlon)
provinces <- project(provinces, crs.latlon)
riv.bc <- project(riv.bc, crs.latlon)
riv.us <- project(riv.us, crs.latlon)
ws <- project(ws, crs.latlon)
rel.locs <- project(rel.locs, crs.latlon)
rec.locs <- project(rec.locs, crs.latlon)

# Match watershed IDs to release and recovery locations
rel.locs$ws.id <- extract(ws["ws.id"], rel.locs)[,2]
rec.locs$ws.id <- extract(ws["ws.id"], rec.locs)[,2]

# Match those into the main dataframe
rec$rel.ws.id <- rel.locs$ws.id[match(rec$rel.loc.code, rel.locs$rel.loc.code)]
rec$rec.ws.id <- rec.locs$ws.id[match(rec$rec.loc.code, rec.locs$rec.loc.code)]

# Remove sampling location in one inland watershed
ws.remove <- "448914"
rec <- rec[rec$rec.ws.id != ws.remove,]

# Also remove that watershed for mapping recovery locations
ws <- ws[ws$ws.id != ws.remove,]
rec.locs <- rec.locs[rec.locs$ws.id != ws.remove]

# Remove recovery data when release or recovery watersheds are NA
rec <- rec[is.na(rec$rec.ws.id)==F & is.na(rec$rel.ws.id)==F,]

# Reorder columns, just 'cause
rec <- rec %>%
  relocate(rel.ws.id, .after = rel.basin) %>%
  relocate(rec.ws.id, .after = rec.basin)

# For the map, remove recovery locations not associated with a watershed
rec.locs <- rec.locs[is.na(rec.locs$ws.id)==F,]

# Remove rivers that aren't in watersheds with recovery lcoations
riv.bc$ws.id <- extract(ws, riv.bc)$ws.id
riv.us$ws.id <- extract(ws, riv.us)$ws.id
riv.bc <- riv.bc[riv.bc$ws.id %in% unique(rec.locs$ws.id),]
riv.us <- riv.us[riv.us$ws.id %in% unique(rec.locs$ws.id),]

# Add column for whether fish strayed or not
rec$strayed <- as.integer(rec$rel.ws.id != rec$rec.ws.id)


## 5 [WRITE DATA] --------------------------------------------------------------
# Non-spatial data
write.csv(rec, here::here("./data/processed/recoveries.csv"), 
          quote=F, row.names=F)
write.csv(rel, here::here("./data/processed/releases.csv"), 
          quote=F, row.names=F)
write.csv(loc, here::here("./data/processed/locations.csv"), 
          quote=F, row.names=F)

# Spatial data
writeVector(states, here::here("./data/processed/states.gpkg"), 
            overwrite = TRUE)
writeVector(provinces, here::here("./data/processed/provinces.gpkg"), 
            overwrite = TRUE)
writeVector(riv.bc, here::here("./data/processed/rivers_bc.gpkg"), 
            overwrite = TRUE)
writeVector(riv.us, here::here("./data/processed/rivers_us.gpkg"), 
            overwrite = TRUE)
writeVector(ws, here::here("./data/processed/watersheds.gpkg"), 
            overwrite = TRUE)
writeVector(rec.locs, here::here("./data/processed/recovery_locations.gpkg"), 
            overwrite = TRUE)
writeRaster(bath, here::here("./data/processed/bathymetry.tif"), 
            overwrite = TRUE, filetype  = "GTiff")

