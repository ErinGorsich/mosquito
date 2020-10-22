library(dismo)
library(rgdal)
library(rgeos)
library(raster)
library(sp)
library(sf)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
setwd("~/GitHub/mosquito/environmental_data")
env.dir <- "/Users/u1774615/GitHub/mosquito/environmental_data/climate_elevation/"
loc.dir <- "/Users/u1774615/GitHub/mosquito/environmental_data/country/"

#################################################################
#################################################################
# Read in study sites spatial information
#################################################################
#################################################################
district <- readOGR(dsn = loc.dir, layer = "zaf_admbnda_adm2_2016SADB_OCHA")
district.red <- gSimplify(district, tol = 0.001, topologyPreserve = TRUE)
district.red <- SpatialPolygonsDataFrame(district.red, data = district@data)
district.sf <- as(district.red, "sf")

municipality<- readOGR(dsn = loc.dir, layer = "zaf_admbnda_adm3_2016SADB_OCHA")
municipality.red<- gSimplify(municipality, tol = 0.001, topologyPreserve = TRUE)
municipality.red <- SpatialPolygonsDataFrame(municipality.red, 
                                             data = municipality@data)
municipality.sf <- as(municipality.red, "sf")

# Read in study sites
peter <- read.csv("diptanks.csv")
df <- SpatialPointsDataFrame(coords = cbind(peter$long, peter$lat), 
                             proj4string = district@proj4string, data = peter)
df.sp <- SpatialPoints(coords = cbind(peter$long, peter$lat), 
                       proj4string = district@proj4string)
df.sf <- st_as_sf(peter, coords = c("long", "lat"), 
                  crs = st_crs(district@proj4string)) 

#################################################################
#################################################################
# Download / Read in WorldClim Data
#################################################################
#################################################################
# Worldclim elevation  (alt for altitude), at 90m resolution
elevation <- getData("alt", country = "ZAF", level = 0, path = env.dir)
plot(elevation[[1]])

# Worldclim (historical monthly: https://www.worldclim.org/data/monthlywth.html)
# tmin, tmax, prec,  - all at 0.5 resolution (minutes of degrees) ~1km
precip <- getData('worldclim', var = 'tmin', 
                  res = 0.5, lon = 32, lat = -27)

# Bioclimactic variables are average annual summary information 
# manual download from (https://www.worldclim.org/data/bioclim.html)
# BIO12 = Annual Precipitation; BIO13 = Precipitation of Wettest Month; 
# BIO14 = Precipitation of Driest Month; BIO15 = Precipitation Seasonality (CV)
bio <- getData('worldclim', var = 'bio', res = 0.5, lon = 32, 
                        lat = -27, path = env.dir)


#################################################################
#################################################################
# Quick plots to see... 
#################################################################
#################################################################
b <- data.frame(lat = c(-26.5, -26.5, -28.5, -28.5), 
                long = c(31.2, 33.2, 31.2, 33.2))
b <- SpatialPoints(coords = cbind(b$long, b$lat), proj4string=df.sp@proj4string)

cl <- list(elevation[[1]], bio[[12]], bio[[13]], bio[[14]], bio[[15]])
croplist <- lapply(cl, crop, y = b)
mun.plot <- st_crop(municipality.sf, b) 

# Elevation
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[1]], xy = TRUE),
                aes(x = x, y = y, fill = ZAF1_msk_alt)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Elevation (m)") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Annual Precipitation
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[2]], xy = TRUE),
                aes(x = x, y = y, fill = bio12_37)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Annual precipitation") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Precipitation of Wettest Month
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[3]], xy = TRUE),
                aes(x = x, y = y, fill = bio13_37)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Wettest month precipitation") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Precipitation of Driest Month
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[4]], xy = TRUE),
                aes(x = x, y = y, fill = bio14_37)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Driest month precipitation") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)


# BIO15 = Precipitation Seasonality (CV)
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[5]], xy = TRUE),
                aes(x = x, y = y, fill = bio15_37)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "CV precipitation") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

#################################################################
#################################################################
# Compare with Peter's data and save... 
#################################################################
#################################################################
# want Peter's data points at this projection... they should be
crs(df.sp); crs(elevation[[1]])
names <- c("elevation_in_meters", "annual_precipitation", 
           "precipitation_wettest_month", "precipitation_wettest_month", 
           "precipitation_cv")

get_buffer = function(raster, location, size){
    med <- extract(x = raster, y = location, buffer = size, fun = median)
    var <- extract(x = raster, y = location, buffer = size, 
                   fun = function(x) if (!is.null(x)) var(x, na.rm=TRUE))
    return(c(med, var))
}

get_one_village = function(diptank, rastlist){
    loc <- subset(df.sf, Diptank == diptank)
    temp <- data.frame(matrix(ncol = 12, nrow = length(rastlist)))
    colnames(temp) <- c("Diptank","approx.date", "ras.0", "name",
                        "ras.med.1k", "ras.var.1k", "ras.med.2k", "ras.var.2k", 
                        "ras.med.3k", "ras.var.3k", "ras.med.4k", "ras.var.4k")
    for (i in 1:length(rastlist)){
        ras <- rastlist[[i]]
        ras.value <- extract(ras, loc) 
        approx.date <- "long term average- 1970-2000"
        med.var.1000 <- get_buffer(ras, loc, 1000)
        med.var.2000 <- get_buffer(ras, loc, 2000)
        med.var.3000 <- get_buffer(ras, loc, 3000)
        med.var.4000 <- get_buffer(ras, loc, 4000)
        temp[i, ] <- c(diptank, approx.date, ras.value, names[i],
                       med.var.1000, med.var.2000, med.var.3000, med.var.4000)
    }
    return(temp)
}

old <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(old) <- c("Diptank", "date", "ras.0", "name",
                   "ras.med.1k", "ras.var.1k", "ras.med.2k", "ras.var.2k", 
                   "ras.med.3k", "ras.var.3k", "ras.med.4k", "ras.var.4k")
store <- old


for (i in 1:length(df.sf$Diptank)){
    diptank <- df.sf$Diptank[i]
    store <- rbind(store, get_one_village(diptank, cl))
    print(i)
}
write.csv(store, "worldclimdata.csv")


