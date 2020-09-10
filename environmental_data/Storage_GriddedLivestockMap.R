# Prep grided livestock map of the world data
rm(list = ls())
setwd("~/GitHub/mosquito/environmental_data")
loc.dir <- "/Users/u1774615/GitHub/mosquito/environmental_data/country/"
cow.dir <- "/Users/u1774615/GitHub/mosquito/environmental_data/livestock/"
x <- c("rgdal", "rgeos", "maptools", "sp", "sf", "raster", 
       "ggplot2", "RColorBrewer")
lapply(x, library, character.only = TRUE)

#################################################################
#################################################################
# Read in livestock maps (Gilbert et al. 2018)
#################################################################
#################################################################
# spatial resolution of 5 minutes of arc= 0.083333 decimal degrees ~10km at equator
# Check average spatial resolution (ASR) and census year; ASR is course for SA
# PS = indicator for whether data comes from a census or predicted
# AW vs DA: in AW, when pixels are smaller than the census polygon, all are 
# given equal weight and animals are distributed evenly within the census polygon
# excluding natural areas.
cl <- list()
cl[[1]] <- cattle_raster <- raster(paste0(cow.dir, "5_Ct_2010_Da.tif")) 
cl[[2]] <- cattle_aw_raster <- raster(paste0(cow.dir, "6_Ct_2010_Aw.tif"))
cl[[3]] <- cattle_ps_raster <- raster(paste0(cow.dir, "7_Ct_2010_Ps.tif"))
cl[[4]] <- goat_raster <- raster(paste0(cow.dir, "5_Gt_2010_Da.tif"))
cl[[5]] <- goat_aw_raster <- raster(paste0(cow.dir, "6_Gt_2010_Aw.tif"))
cl[[6]] <- goat_ps_raster <- raster(paste0(cow.dir, "7_Gt_2010_Ps.tif"))


#################################################################
#################################################################
# Read in study sites spatial information
#################################################################
#################################################################
# Country: https://data.humdata.org/dataset/south-africa-admin-level-1-boundaries
# 0 = country, 1 = province, 2 = district, 3 = local municipality, 4 = ward
sa <- readOGR(dsn = loc.dir, layer = "zaf_admbnda_adm0_2016SADB_OCHA")
province <- readOGR(dsn = loc.dir, layer = "zaf_admbnda_adm1_2016SADB_OCHA")
province.red <- gSimplify(province, tol = 0.001, topologyPreserve = TRUE)
province.red <- SpatialPolygonsDataFrame(province.red, data = province@data)
province.sf <- as(province.red, "sf")

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
                             proj4string = sa@proj4string, data = peter)
df.sp <- SpatialPoints(coords = cbind(peter$long, peter$lat), 
                       proj4string = sa@proj4string)
df.sf <- st_as_sf(peter, coords = c("long", "lat"), crs = st_crs(sa@proj4string)) 


#################################################################
#################################################################
# Crop and plot
#################################################################
#################################################################
# Set bbox (topleft, topright)
b <- data.frame(lat = c(-26.5, -26.5, -28.5, -28.5), 
                long = c(31.2, 33.2, 31.2, 33.2))
b <- SpatialPoints(coords = cbind(b$long, b$lat), proj4string = df.sp@proj4string)
b2 <- data.frame(lat = c(-26.5, -26.5, -27.5, -27.5), 
                      long = c(32, 33, 32, 33))

croplist <- lapply(cl, crop, y = b)
mun.plot <- st_crop(municipality.sf, b)

# Make a nice ggplot map of SA/Moz, with KZN, Umkhanya-Kude (district) colored,
# ggplot() + 
#     geom_raster(data = as.data.frame(croplist[[1]], xy = TRUE),
#                 aes(x = x, y = y, fill = X5_Ct_2010_Da)) +
#     scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
#     geom_sf(data = mun.plot, fill = NA) + 
#     geom_sf(data = df.sf) +
#     theme_minimal() + theme(legend.title = element_blank()) + 
#     xlab("") + ylab("") + ggtitle(label = "Head of cattle")


# Cattle data
##############################################################
# Based on a survey, but that survey covers the whole study area. Use Da, but 
# based on machine learning and data from 'Africa'
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[1]], xy = TRUE),
                aes(x = x, y = y, fill = X5_Ct_2010_Da)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Head of cattle") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)


# Zoom in
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[2]], xy = TRUE),
                aes(x = x, y = y, fill = X6_Ct_2010_Aw)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Head of cattle (Aw)") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Aw version assumes pixels smaller than the census polygon have equal numbers 
# of animals
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[2]], xy = TRUE),
                aes(x = x, y = y, fill = X6_Ct_2010_Aw)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Head of cattle (Aw)") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Prediction (1) or Data (0)
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[3]], xy = TRUE),
                aes(x = x, y = y, fill = X7_Ct_2010_Ps)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Prediction (1) or Data (0)") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Goat data
##############################################################
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[4]], xy = TRUE),
                aes(x = x, y = y, fill = X5_Gt_2010_Da)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Head of goat") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Aw version assumes pixels smaller than the census polygon have equal numbers 
# of animals
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[5]], xy = TRUE),
                aes(x = x, y = y, fill = X6_Gt_2010_Aw)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Head of cattle (Aw)") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

# Prediction (1) or Data (0)
ggplot() + 
    geom_raster(data = as.data.frame(croplist[[6]], xy = TRUE),
                aes(x = x, y = y, fill = X7_Gt_2010_Ps)) +
    scale_fill_distiller(palette = 18, na.value = "transparent", direction = 1) +
    geom_sf(data = mun.plot, fill = NA) + 
    geom_sf(data = df.sf) +
    theme_minimal() + theme(legend.title = element_blank()) + 
    xlab("") + ylab("") + ggtitle(label = "Head of cattle (Aw)") + 
    ylim(-27.5, -26.7) + xlim(31.8, 33)

#################################################################
#################################################################
# Compare with Peter's data and save... 
#################################################################
#################################################################
# simple = that grid point; bilinear = innterpolated from four sourrounding
df@data$cows <- extract(x = croplist[[1]], y = df.sp, method = 'simple')
df@data$agcows <- extract(x = croplist[[1]], y = df.sp, method = 'bilinear')
df@data$goats <- extract(x = croplist[[4]], y = df.sp, method = 'simple')
df@data$aggoats <- extract(x = croplist[[4]], y = df.sp, method = 'bilinear')
write.csv(df@data, "diptanks_withGLM.csv")
