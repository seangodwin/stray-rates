# 2_initial_analysis.R
# Author: Sean Godwin
# Date: 2025-06-02
# Description: Play with Cole's pseudocode


## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # file referencing
library(tidyverse)   # data manipulation
library(data.table)  # faster data reading
library(rstan)       # R interface to Stan
library(terra)       # spatial data manipulation
library(spdep)       # building edge list for neighbouring watersheds
library(sf)          # compatibility with spdep


## 1 [READ IN DATA] ------------------------------------------------------------
# Remember to change your path
# Temporarily set root manually for Marty's cluster
set_here("/home/maddie/sean/")

# Recovery data
rec <- as.data.frame(fread(here::here("./data/processed/recoveries.csv")))

# Watershed spatial data
ws <- vect(here::here("./data/processed/watersheds.gpkg"))


## 2 [SET-UP DATA] -------------------------------------------------------------
# Remove extraneous watersheds from spatial data
# Will need to change if we end up going with released watersheds
ws <- ws[ws$ws.id %in% rec$rec.ws.id,]

# Convert to sf for spdep compatibility (to compute neighbours)
ws.sf <- sf::st_as_sf(ws)

# Repair invalid geometries
ws.sf <- st_make_valid(ws.sf)

##### FUTURE CHECK
# Match latitude to wss.f (already aligned with spatial model order)
# Should we be using latitude at river mouth rather than centroid?
ws.sf$lat <- st_coordinates(st_centroid(ws.sf))[, 2]

# Compute neighbours (queen = TRUE means share any border point)
ws.nb <- poly2nb(ws.sf, queen = TRUE)

# Visual check
plot(st_geometry(ws.sf), border = "grey")
plot(ws.nb, st_coordinates(st_centroid(ws.sf)), add = TRUE, col = "red")

# Build edge list and nodes
# Do it in a way that handles isolated nodes by excluding them from edges, while still returning the full set of spatial units:
# now u[k] for an isolated watershed won’t be connected to any others
# v[k] still absorbs basin-specific noise
library(spdep)

# Extract safe neighbor edges
edge.list <- function(nb) {
  edges <- lapply(seq_along(nb), function(i) {
    neighbors <- nb[[i]]
    if (length(neighbors) > 0) {
      # Remove any 0s just in case
      neighbors <- neighbors[neighbors > 0]
      cbind(rep(i, length(neighbors)), neighbors)
    } else {
      NULL
    }
  })
  edges <- do.call(rbind, edges)
  edges <- unique(t(apply(edges, 1, sort)))  # Ensure each edge is unique
  edges
}
edges <- edge.list(ws.nb)

# Sanity check
stopifnot(all(edges > 0))  # should pass

# Set up for Stan
node1 <- edges[, 1]
node2 <- edges[, 2]
n.edges <- length(node1)

# Define number of spatial units
# Use # of rows in ws.nb, which includes ALL watersheds, even isolated ones
K <- length(ws.nb)

# Recalculate ws_idx based on the ordering in ws.sf
# This ensures that each fish’s watershed index aligns with the adjacency structure
rec <- rec %>%
       mutate(ws.idx = match(rec$rec.ws.id, ws.sf$ws.id))

# Sanity check: all ws indices must fall within 1:K for Stan to work
stopifnot(all(rec$ws.idx >= 1 & rec$ws.idx <= K))


## 4 [SET-UP MODEL] ------------------------------------------------------------
##### FUTURE CHECK
# Should we be using release watershed instead? I don't think so, but...
# Should we be using brood year instead of recovery year?
dat.list <- list(
  N = nrow(rec),
  y = rec$strayed,
  K = length(unique(rec$rec.ws.id)),
  ws = rec$ws.idx,
  T = length(unique(rec$rec.year)),
  year = as.integer(factor(rec$rec.year)),  # map years to 1:T, scale
  lat = ws.sf$lat,
  
  # BYM2 adjacency inputs
  n_edges = n.edges,
  node1 = node1,
  node2 = node2
)


## 5 [FIT MODEL] ---------------------------------------------------------------
fit <- stan(
  file = here::here("./stan/models/test.stan"),
  data = dat.list,
  chains = 4, iter = 500, warmup = 50,   
  cores = 8, 
  control = list(adapt_delta=0.95, max_treedepth=7), 
  refresh = 200                   
)

## edits for real run 
# adapt_delta = 0.99
# max_treedepth = 12
# iter = 2000
# warmup = 1000
# refresh = 500


## 6 [SAVE OUTPUTS] ------------------------------------------------------------
# Save the stanfit object (can be large)
saveRDS(fit, 
        here::here("./outputs/model-objects/fit_stray_model.rds"))

# Save the model input list for reproducibility
saveRDS(dat.list, 
        here::here("./outputs/model-objects/dat_list_stray_model.rds"))

# Save watershed spatial info (with latitudes & correct ordering)
saveRDS(ws.sf, 
        here::here("./outputs/model-objects/ws_sf.rds"))

##### FUTURE CHECK
# Save recovery dataframe (with ws.idx etc.)
# Need to come back to this because 1_data_prep.R also saves an earlier version
write.csv(rec, here::here("./data/processed/recoveries_post-model.csv"), 
          quote=F, row.names=F)
