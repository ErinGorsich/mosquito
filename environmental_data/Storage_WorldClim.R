
library(dismo)
library(rgdal)
rm(list = ls())
setwd("~/GitHub/mosquito/environmental_data")
env.dir <- "/Users/u1774615/GitHub/mosquito/environmental_data/climate_elevation/"


# Worldclim elevation 
elevation <- getData("alt", country = "ZAF", level = 0, path = env.dir)

# Worldclim (historical monthly: https://www.worldclim.org/data/monthlywth.html)
# tmin, tmax, prec, and 'bio' (19 bioclimactic varialbes) - all at 0.5 resolution
temperature <- getData('worldclim', country = "ZAF", level = 0, path = env.dir)


# 30s is ~ 1km2
test <- raster(file.choose())
