# 1_data_wrangling.R
# Author: Sean Godwin
# Date: 2025-06-02
# Description: Get the recovery datafile ready to go


## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # file referencing
library(terra)       # spatial data manipulation


## 1 [READ IN DATA] ------------------------------------------------------------
# Load nonspatial dataframes
load(here::here("data/cleaned_nonspatial_data.RData"))

# Read in spatial data
states <- vect(here::here("data/spatial/processed_states.gpkg"))
provinces <- vect(here::here("data/spatial/processed_provinces.gpkg"))
riv.bc <- vect(here::here("data/spatial/processed_rivers_bc.gpkg"))
riv.us <- vect(here::here("data/spatial/processed_rivers_us.gpkg"))
ws <- vect(here::here("data/spatial/processed_watersheds.gpkg"))
rec.locs <- vect(here::here("data/spatial/processed_recovery_locations.gpkg"))
bath <- rast(here::here("./data/spatial/gebco_2024.tif"))


## 2 [MAP INPUTS] --------------------------------------------------------------
# Colours for watersheds
col.ws.full <- PNWColors::pnw_palette("Sunset", 
                                      n=length(unique(rec.locs$ws.id))*1.2, 
                                      type="continuous")
col.ws <- col.ws.full[1:length(unique(rec.locs$ws.id))]
col.pt <- col.ws.full[length(col.ws.full)]

# Bathymetry colours
col.bath <- rev(colorRampPalette(c("#E8F4F9", "#A3CDE2", "#31789D"))(100))

# Scale river line widths based on Strahler order
lwd.riv.bc <- scales::rescale(riv.bc$strahler, to = c(0.1, 1.5))  
lwd.riv.us <- scales::rescale(riv.us$strahler, to = c(0.1, 1.5))

# Other plot inputs
col.land <- "grey10"
col.border <- "grey80"   # state and province borders
col.riv <- "white"
col.label <- "white"
lwd.border <- 0.1
cex.pt <- 0.5
cex.ctry <- 1.4
cex.axis <- 0.8

# Extent of plot
x.range <- c(-137,-113)
y.range <- c(37,60)




## 3 [MAP] ---------------------------------------------------------------------
tiff(here::here("./plots/recovery_locations.tiff"), width=6, height=10, units="in",
     pointsize=20, res=600, compression="lzw")
par(mar=c(0,0,0,0), tck=-0.03, mgp=c(3,0.5,0), family="sans")
  
  # Empty plot
  plot(0, type="n", xlim=x.range, ylim=y.range, xaxs="i", yaxs="i", 
       axes=FALSE, xlab="", ylab="", frame.plot=FALSE)
  
  # Bathymetry
  plot(bath, add = TRUE,
       col = col.bath, legend=F)
  
  # States and provinces
  plot(states, add=T,
       col=col.land, border=col.border, lwd=lwd.border)
  plot(provinces, add=T,
       col=col.land, border=col.border, lwd=lwd.border)
  
  # 49th parallel
  lines(x = c(-122.758, -95), y = c(49, 49), col = "white", lwd = 0.8)
  
  # Watersheds that had recoveries
  plot(ws[ws$ws.id %in% rec.locs$ws.id,], add=T,
       col=col.ws, lwd=0.1)
  
  # Rivers
  plot(riv.bc, add=T,
       lwd=lwd.riv.bc, col=col.riv)
  plot(riv.us, add=T,
       lwd=lwd.riv.us, col=col.riv)
  
  # Recovery locations
  plot(rec.locs, add=T, cex=cex.pt, pch=21,
       bg=col.pt, col=col.land)
  
  # Longitude ticks (every 5 and 1 degrees)
  axis(1, at=seq(floor(x.range[1]/5)*5, ceiling(x.range[2]/5)*5, by=5), 
       labels=F, tck=0.025, cex.axis=cex.axis)
  axis(1, at=seq(floor(x.range[1]/5)*5, ceiling(x.range[2]/5)*5, by=1), 
       labels=F, tck=0.015, cex.axis=cex.axis)
  
  # Latitude ticks (every 5 and 1 degrees)
  axis(2, at=seq(floor(y.range[1]/5)*5, ceiling(y.range[2]/5)*5, by=5), 
       labels=F, tck=0.02, cex.axis=0.8)
  axis(2, at=seq(floor(y.range[1]/5)*5, ceiling(y.range[2]/5)*5, by=1), 
       labels=F, tck=0.01, cex.axis=0.8)
  
  # Country labels
  text(x=-120, y=56, labels = "Canada", family="sans", 
       cex=cex.ctry, col=col.label)
  text(x=-117, y=40, labels = "USA", family="sans", 
       cex=cex.ctry, col=col.label)
  
  # Boundary box
  box()

dev.off()