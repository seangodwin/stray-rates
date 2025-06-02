
library(PNWColors)   # colour palette

## 3 [REGION-LEVEL EXPLORATION] ------------------------------------------------
##### FUTURE CHECK 
# % of recoveries that were NOT from the region in which they were recovered
# Need to go region by region and basin by basin to check for errors
#     e.g., basin name change in AK in ~2020, causing spike in Chinook straying
stray.region.all <- as.data.frame(rec %>%
                                    filter(!is.na(rel.region), rel.region != "",
                                           !(rel.region %in% region.exc),
                                           !is.na(rec.region), rec.region != "",
                                           !(rec.region %in% region.exc)) %>%
                                    mutate(diff.region = 
                                             rel.region != rec.region) %>%
                                    group_by(rear.type, rel.region, brood.year) %>%
                                    summarise(n = n(),
                                              n.diff = sum(diff.region, 
                                                           na.rm = TRUE),
                                              perc.diff = (n.diff / n) * 100
                                    ) %>%
                                    ungroup())

# Average across all regions
stray.region.hatch.av <- as.data.frame(stray.region.all[
  stray.region.all$rear.type=="H",] %>%
    group_by(brood.year) %>%
    summarise(n.total = sum(n),
              n.diff.total = sum(n.diff),
              perc.diff = (n.diff.total / 
                             n.total) * 100
    ) %>%
    ungroup())

# Set colours
region.list <- unique(stray.region.all$rel.region)
colours <- PNWColors::pnw_palette(name="Sunset2", n=length(region.list), 
                                  type="continuous")

# Plot parameters
cex.axis <- 1.2
alpha <- 0.15
lwd.av <- 3.5
col.av <- PNWColors::pnw_palette(name="Starfish", n=100, type="continuous")[65]

# Plot hatchery only
tiff(here::here("./plots/region_hatchery.tiff"), width=8, height=5, units="in",
     pointsize=15, res=600, compression="lzw")
par(mar=c(3,3,0.4,1), tck=-0.02, mgp=c(3,0.5,0), family="sans")

# Set up an empty plot with proper axis limits
plot(1, xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=F, 
     xlim = c(1970,2020), ylim = c(0,100))

# Lines for each region, with gaps for years with no data
for (i in 1:length(region.list)) {
  #Subset
  reg.data <- subset(stray.region.all[stray.region.all$rear.type=="H",], 
                     rel.region == region.list[i])
  
  if (nrow(reg.data) == 0) next
  
  # Ensure years are sorted
  reg.data <- reg.data[order(reg.data$brood.year), ]
  
  # Compute difference between consecutive years
  year.diff <- c(Inf, diff(reg.data$brood.year))
  
  # Start a new group when the year gap is greater than 1
  group.id <- cumsum(year.diff > 1)
  
  # Add the group ID to your data
  reg.data$group.id <- group.id
  
  for(j in 1:length(unique(reg.data$group.id))) {
    lines(reg.data$brood.year[reg.data$group.id==j], 
          reg.data$perc.diff[reg.data$group.id==j],
          col = adjustcolor(colours[i], alpha.f=alpha), type = "l")
  }
}

# Lines for average
lines(stray.region.hatch.av$brood.year, stray.region.hatch.av$perc.diff, 
      col = col.av, 
      type="l", lwd=lwd.av, pch=16, cex=1.4)

# Legend
legend(2016.1, 95, legend = region.list, col = colours, pch = 16, 
       lty = 1, title = expression(bold("Region")), cex=0.4)

# Axis labels
axis(1, at=seq(1970,2020,10))
axis(2, at=seq(0,100,20), las=1)

# Axis titles
mtext("Brood year", side=1, line=1.7, cex=cex.axis)
mtext("Region-to-region stray rate (%)", side=2, line=1.8, cex=cex.axis)

box()

dev.off()

## 4 [BASIN-LEVEL EXPLORATION] ------------------------------------------------
##### FUTURE CHECK 
# % of recoveries that were NOT from the basin in which they were recovered
# Need to go region by region and basin by basin to check for errors
#     e.g., basin name change in AK in ~2020, causing spike in Chinook straying
stray.basin.all <- as.data.frame(rec %>%
                                   filter(!is.na(rel.basin), rel.basin != "",
                                          !(rel.basin %in% basin.exc),
                                          !is.na(rec.basin), rec.basin != "",
                                          !(rec.basin %in% basin.exc)) %>%
                                   mutate(diff.basin = 
                                            rel.basin != rec.basin) %>%
                                   group_by(rear.type, rel.basin, brood.year) %>%
                                   summarise(n = n(),
                                             n.diff = sum(diff.basin, 
                                                          na.rm = TRUE),
                                             perc.diff = (n.diff / n) * 100
                                   ) %>%
                                   ungroup())

# Average across all basins
stray.basin.hatch.av <- as.data.frame(stray.basin.all[
  stray.basin.all$rear.type=="H",] %>%
    group_by(brood.year) %>%
    summarise(n.total = sum(n),
              n.diff.total = sum(n.diff),
              perc.diff = (n.diff.total / 
                             n.total) * 100
    ) %>%
    ungroup())

# Set colours
basin.list <- unique(stray.basin.all$rel.basin)
colours <- PNWColors::pnw_palette(name="Sunset2", n=length(basin.list), 
                                  type="continuous")

# Plot hatchery only
tiff(here::here("./plots/basin_hatchery.tiff"), width=8, height=5, units="in",
     pointsize=15, res=600, compression="lzw")
par(mar=c(3,3,0.4,1), tck=-0.02, mgp=c(3,0.5,0), family="sans")

# Set up an empty plot with proper axis limits
plot(1, xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=F, 
     xlim = c(1970,2020), ylim = c(0,100))

# Lines for each basin, with gaps for years with no data
for (i in 1:length(basin.list)) {
  #Subset
  bas.data <- subset(stray.basin.all[stray.basin.all$rear.type=="H",], 
                     rel.basin == basin.list[i])
  
  if (nrow(bas.data) == 0) next
  
  # Ensure years are sorted
  bas.data <- bas.data[order(bas.data$brood.year), ]
  
  # Compute difference between consecutive years
  year.diff <- c(Inf, diff(bas.data$brood.year))
  
  # Start a new group when the year gap is greater than 1
  group.id <- cumsum(year.diff > 1)
  
  # Add the group ID to your data
  bas.data$group.id <- group.id
  
  for(j in 1:length(unique(bas.data$group.id))) {
    lines(bas.data$brood.year[bas.data$group.id==j], 
          bas.data$perc.diff[bas.data$group.id==j],
          col = adjustcolor(colours[i], alpha.f=alpha), type = "l")
  }
}

# Lines for average
lines(stray.basin.hatch.av$brood.year, stray.basin.hatch.av$perc.diff, 
      col = col.av, 
      type="l", lwd=lwd.av, pch=16, cex=1.4)

# Legend
# legend(2016.1, 95, legend = basin.list, col = colours, pch = 16, 
#        lty = 1, title = expression(bold("Basin")), cex=0.4)

# Axis labels
axis(1, at=seq(1970,2020,10))
axis(2, at=seq(0,100,20), las=1)

# Axis titles
mtext("Brood year", side=1, line=1.7, cex=cex.axis)
mtext("Basin-to-basin stray rate (%)", side=2, line=1.8, cex=cex.axis)

box()

dev.off()


## 5 [LAT DIFF EXPLORATION] ----------------------------------------------------
# Differences in latitudes between release and recovery
rec$lat.diff <- rec$rec.lat - rec$rel.lat
hist(rec$lat.diff, breaks=400, xlim=c(-3,3))

# Plot lat diffs for each fish

col.pt <- PNWColors::pnw_palette(name="Sunset2", n=100, type="continuous")[30]

tiff(here::here("./plots/lat-diff_over_time.tiff"), width=8, height=5, units="in",
     pointsize=15, res=600, compression="lzw")
par(mar=c(3,3,0.4,1), tck=-0.02, mgp=c(3,0.5,0), family="sans")

plot(1, xlim=c(1970,2020), ylim=c(-5,5))
points(jitter(rec$brood.year), jitter(rec$lat.diff), 
       pch=16, col=adjustcolor(col.pt, alpha.f=0.01))
abline(h=0)

dev.off()


# Variation in these release-to-recovery latitudinal differences
lat.diff.var <- rec %>% group_by(rel.state, brood.year) %>% 
  summarize(var = var(lat.diff, na.rm=T))

# Set colours
state.list <- unique(lat.diff.var$rel.state)
colours <- PNWColors::pnw_palette(name="Sunset2", n=length(state.list), 
                                  type="continuous")

tiff(here::here("./plots/lat-diff-variation_over_time.tiff"), width=8, height=8, units="in",
     pointsize=18, res=600, compression="lzw")
par(mfrow=c(3,2), mar=c(1.0,0.5,0.5,0.8), oma=c(2.0,3.1,0,0), 
    tck=-0.03, mgp=c(3,0.5,0), family="sans")

for(i in 1:length(state.list)) {     # for each panel
  plot(1, xlim=c(1970,2020), ylim=c(0,1))
  
  lines(lat.diff.var$brood.year[lat.diff.var$rel.state==state.list[i]], 
        lat.diff.var$var[lat.diff.var$rel.state==state.list[i]], 
        type="o", pch=21, bg="white", col=colours[i])
}

legend("topright", legend = state.list, col = colours, pch = 16, 
       lty = 1, title = expression(bold("State")), cex=0.8)

dev.off()


# Read in chinook production data
# From "SmoltProduction.xlsx" here: https://github.com/bchasco/COAST_WIDE
#   via Chasco et al. 2017: https://www.nature.com/articles/s41598-017-14984-8
prod <- as.data.frame(fread(here::here("./data/hatchery_production_raw.csv")))

plot(prod$year, prod$total, type="o")

prod$lat.diff.var <- lat.diff.var$var[match(prod$year, lat.diff.var$brood.year)]

plot(prod$total, prod$lat.diff.var)


## 6 [NEXT STEPS] --------------------------------------------------------------
# 0. Make lines not connect if missing brood years
# 1. QA/QC to figure out wtf is happening with the big spikes
# 2. Make plots split by region, basin, and hatchery vs wild
# 3. Why are region-to-region stray rates higher than basin-to-basin sometimes?
# 4. Something more fine-scale than basin level if possible (using stock
#    location, spawning ground surveys, recovery location, something...)
# 5. Add dead fish survey (fishery == 65), but figure out others first because
#    big job to re-download data


## 7. [ARCHIVED] ---------------------------------------------------------------
# # Average across all regions
# stray.basin.all.av <- as.data.frame(stray.basin.all %>%
#                                       group_by(brood.year) %>%
#                                       summarise(n.total = sum(n),
#                                                 n.diff.total = sum(n.diff),
#                                                 perc.diff = (n.diff.total / 
#                                                                n.total) * 100
#                                       ) %>%
#                                       ungroup())
# 
# # Plot all rearing types combined
# tiff(here::here("./plots/region_all.tiff"), width=8, height=5, units="in",
#      pointsize=15, res=600, compression="lzw")
# par(mar=c(3,3,0.4,1), tck=-0.02, mgp=c(3,0.5,0), family="sans")
#   
#   # Set up an empty plot with proper axis limits
#   plot(1, xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=F, 
#        xlim = c(1970,2020), ylim = c(0,100))
#   
#   # Lines for each region, with gaps for years with no data
#   for (i in 1:length(region.list)) {
#     #Subset
#     reg.data <- subset(stray.region.all, rel.region == region.list[i])
#     
#     # Ensure years are sorted
#     reg.data <- reg.data[order(reg.data$brood.year), ]
#     
#     # Compute difference between consecutive years
#     year.diff <- c(Inf, diff(reg.data$brood.year))
#     
#     # Start a new group when the year gap is greater than 1
#     group.id <- cumsum(year.diff > 1)
#     
#     # Add the group ID to your data
#     reg.data$group.id <- group.id
#     
#     for(j in 1:length(unique(reg.data$group.id))) {
#       lines(reg.data$brood.year[reg.data$group.id==j], 
#             reg.data$perc.diff[reg.data$group.id==j],
#             col = adjustcolor(colours[i], alpha.f=0.3), type = "l")
#     }
#   }
#   
#   # Lines for average
#   lines(stray.region.all.av$brood.year, stray.region.all.av$perc.diff, 
#         col = PNWColors::pnw_palette("Starfish", 1), 
#         type="l", lwd=3, pch=16, cex=1.4)
#   
#   # Legend
#   legend(2016.1, 95, legend = region.list, col = colours, pch = 16, 
#          lty = 1, title = expression(bold("Region")), cex=0.4)
#   
#   # Axis labels
#   axis(1, at=seq(1970,2020,10))
#   axis(2, at=seq(0,100,20), las=1)
#   
#   # Axis titles
#   mtext("Brood year", side=1, line=1.7, cex=cex.axis)
#   mtext("Region-to-region stray rate (%)", side=2, line=1.8, cex=cex.axis)
# 
#   box()
# 
# dev.off()
