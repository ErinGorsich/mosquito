# Notes: 
# Do overlay without re-projection; get date for each pixel
# Add MODIS Air Temperature (Temp) - MOD07_L2 Atmospheric Profile product 
# comprises monthly temperature at the closest level to the earth’s surface.
# MODIS Evapotranspiration (ET) from the MOD16 Global Evapotranspiration product
# is calculated monthly as the ratio of Actual to Potential Evapotranspiration (AET/PET).


#################################################################
#################################################################
# Outline: 
#################################################################
# Read in study sites spatial information
# Read in environmental data-sets and clip to KZN and southern Mozambique
# Overlay points and write to file 
#################################################################
#################################################################
rm(list = ls())
setwd("~/GitHub/mosquito/environmental_data")
download = FALSE
loc.dir <- "/Users/u1774615/GitHub/mosquito/environmental_data/country/"
data.dir <- "/Users/u1774615/GitHub/mosquito/environmental_data/"
x <- c("rgdal", "rgeos", "maptools", "sp", "sf", "raster", "MODIS",
       "gdalUtils", "ggplot2", "RColorBrewer") 
lapply(x, library, character.only = TRUE)
# library(terra); library(luna) # for MODIS data download and processing...
# install.packages("remotes") # to install luna 
# remotes::install_github("rspatial/luna")
# cut packages: "ggmap", "dplyr", "plyr", "tmap",

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
    
# An easier way for to get administrative boundaries in the future
#moz <- getData('GADM', country = 'MZ', download = TRUE, path =loc.dir, level=1)
sz <- getData('GADM', country = 'SZ', download = TRUE, path = loc.dir, level=0)

moz <- readRDS(paste0(loc.dir, "gadm36_MOZ_1_sp.rds"))
moz.red<- gSimplify(moz, tol = 0.001, topologyPreserve = TRUE)
moz.red <- SpatialPolygonsDataFrame(moz.red, data = moz@data)
moz.sf <- as(moz.red, "sf")

# Swaziland
sz <- readRDS(paste0(loc.dir, "gadm36_SZ_0_sp.rds"))
sz.red<- gSimplify(sz, tol = 0.001, topologyPreserve = TRUE)
sz.red <- SpatialPolygonsDataFrame(sz.red, data = sz@data)
sz.sf <- as(sz.red, "sf")

# Make a nice ggplot map of SA/Moz, with KZN, Umkhanya-Kude (district) colored,
ggplot() + 
    geom_sf(data = province.sf, fill = "light grey") + 
    geom_sf(data = subset(moz.sf, NAME_1 %in% c("Maputo", "Gaza")), fill = "white") +
    geom_sf(data = sz.sf, fill = "white") +
    geom_sf(data = subset(province.sf, ADM1_ID == "KZN"), fill = "light grey") + 
    #geom_sf(data = subset(municipality.sf, 
    #                      ADM3_EN %in% c("Jozini", "Umhlabuyalingana")), 
    #        fill = "blue") + xlim(17, 33) +
    geom_sf(data = df.sf, size = 0.5, colour = "blue") +
    theme_minimal()

ggplot() +     
    geom_sf(data = subset(municipality.sf, 
                          ADM3_EN %in% c("Jozini", "Umhlabuyalingana")), 
            fill = "light grey") +
    geom_sf(data = df.sf, colour = "blue") + theme_minimal()


#################################################################
#################################################################
# Download MODIS data
#################################################################
#################################################################
# want NDVI, EVI, air temperature, evapotraspiration for 2016-2020 
# This function downloads & stores (can resamples/reprojects, but not recommended)
# hdf files stored in MODIS folder, tifs stored in "job" folder
if (download){
    MODISoptions(gdalPath = data.dir, outDirPath = data.dir, quiet = FALSE,
                 MODISserverOrder = "LPDAAC") # may need to add c("LPDAAC","LAADS") 
    EarthdataLogin() # username = "eringorsich", password = "xNz43pwfNv*dku#")
    start <- "2016.05.01"  # yr-mo-d"
    end <- "2020.04.01"
    ndvi.evi = runGdal("MOD13Q1", collection = "006", tileH = 20, tileV = 11,
                        begin = start, end = end, SDSstring = "111000000010", 
                        job = "NDVI.EVI", outProj="asIn")
    rm(ndvi.evi)

    # temp - https://lpdaac.usgs.gov/products/mod11c2v006/
    air = runGdal("MOD11C2", collection = "006", tileH = 20, tileV = 11,
                  begin = start, end = end, SDSstring = "10000100000000000",
                  job = "air.temp", outProj="asIn")
    rm(air)
    
    # evapotransperation - https://lpdaac.usgs.gov/products/mod16a2gfv006/
    prod <- getProduct("MOD16A2GF")
    et = runGdal("MOD16A2GF", tileH = 20, tileV = 11,
                 begin = start, end = end, job = "et", 
                 outProj="asIn")
    rm(et)
}

# OTHER OPTIONS THAT DIDN'T WORK AS WELL!
# 1. getModis() in the terra package-https://rspatial.org/terra/modis/2-download.html
#     test <- getModis(product = "MOD13Q1", start_date = start, end_date = end, 
#          aoi = bounds, download = TRUE, path = data.dir, 
#          username = "eringorsich", password = "xNz43pwfNv*dku#")
# 2. getHdf in the MODIS package, similar to above but does not reproject
# Use this to see options for a smaller date.



###############################################################
###############################################################
# Read in and process NDVI/EVI data
###############################################################
###############################################################
ndvi <- list.files(path = paste0(data.dir, "NDVI.EVI"), pattern = "NDVI.tif")
list.ndvi <- paste0(data.dir, "NDVI.EVI/", ndvi)
ndvilist <- lapply(list.ndvi, raster)    
#??ASK MAX a better way tifs.stack <- stack(list.files)

evi <- list.files(path = paste0(data.dir, "NDVI.EVI"), pattern = "EVI.tif")
list.evi <- paste0(data.dir, "NDVI.EVI/", evi)
evilist <- lapply(list.evi, raster)  

date <- list.files(path = paste0(data.dir, "NDVI.EVI"), pattern = "year.tif")
list.date <- paste0(data.dir, "NDVI.EVI/", date)
datelist <- lapply(list.date, raster)

# want Peter's data points at this projection...
projection <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
df.transform <- spTransform(df, crs(evilist[[1]]))
df.transform@data <- df@data
df.transform.sf <- as(df.transform, "sf")

# Test transformation/overlay
# province.transform <- spTransform(province.red, crs(evilist[[1]]))
# province.transform.sf <- as(province.transform, "sf")
# ggplot() + 
#     geom_raster(data = as.data.frame(evilist[[1]], xy = TRUE),
#                 aes(x = x, y = y, fill = MOD13Q1.A2016113.250m_16_days_EVI)) +
#     geom_sf(data = province.transform.sf, fill = NA) + 
#     geom_sf(data = df.transform.sf, colour = "blue") + theme_minimal()

get_buffer = function(raster, location, size){
    med <- extract(x = raster, y = location, buffer = size, fun = median)
    var <- extract(x = raster, y = location, buffer = size, 
                   fun = function(x) if (!is.null(x)) var(x, na.rm=TRUE))
    return(c(med, var))
}

get_one_village = function(diptank, rastlist, datelist){
    loc <- subset(df.transform.sf, Diptank == diptank)
    if (length(rastlist) == length(datelist)) {
        temp <- data.frame(matrix(ncol = 12, nrow = length(rastlist)))
        colnames(temp) <- c("Diptank", "date", "approx.date", "ras.0", 
                           "ras.med.1k", "ras.var.1k", "ras.med.2k", "ras.var.2k", 
                           "ras.med.3k", "ras.var.3k", "ras.med.4k", "ras.var.4k")
        for (i in 1:length(rastlist)){
            ras <- rastlist[[i]]
            ras.date <- datelist[[i]]
            if (strsplit(names(ras), "[.]")[[1]][2] == strsplit(names(ras.date), "[.]")[[1]][2]){ 
            } else {
                print("ndvi/evi and date names do not match in order expected")
            }  
            ras.value <- extract(ras, loc) 
            date.value <- extract(ras.date, loc)
            approx.date <- as.character(extractDate(ras, asDate = TRUE)$inputLayerDates)
            med.var.1000 <- get_buffer(ras, loc, 1000)
            med.var.2000 <- get_buffer(ras, loc, 2000)
            med.var.3000 <- get_buffer(ras, loc, 3000)
            med.var.4000 <- get_buffer(ras, loc, 4000)
            temp[i, ] <- c(diptank, date.value, approx.date, ras.value, 
                           med.var.1000, med.var.2000, med.var.3000, med.var.4000)
        }
    } else {
        print("input rasters different lengths")
    }
    return(temp)
}

old <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(old) <- c("Diptank", "date", "approx.date", "ras.0", 
                   "ras.med.1k", "ras.var.1k", "ras.med.2k", "ras.var.2k", 
                   "ras.med.3k", "ras.var.3k", "ras.med.4k", "ras.var.4k")
ndvi <- evi <- old

for (i in 1:length(df.transform.sf$Diptank)){
    diptank <- df.transform.sf$Diptank[i]
    ndvi <- rbind(ndvi, get_one_village(diptank, ndvilist, datelist))
    evi <- rbind(evi, get_one_village(diptank, evilist, datelist))
    if (i == 1){
        head(ndvi, 100)
    }
    print(i)
}
write.csv(ndvi, "ndvidata.csv")
write.csv(evi, "evidata.csv")

rm(ndvi, ndvilist, evi, evilist, list.ndvi, list.evi)

###############################################################
###############################################################
# Read in and process LST data
###############################################################
###############################################################
lst <- list.files(path = paste0(data.dir, "air.temp"), pattern = "Day_CMG.tif") 
list.lst <- paste0(data.dir, "air.temp/", lst)
lstlist <- lapply(list.lst, raster)  

# want Peter's data points at this projection...
df.transform <- spTransform(df, crs(lstlist[[1]]))
df.transform@data <- df@data
df.transform.sf <- as(df.transform, "sf")

# Test transformation/overlay
province.transform <- spTransform(province.red, crs(lstlist[[1]]))
province.transform.sf <- as(province.transform, "sf")
ggplot() + 
    geom_raster(data = as.data.frame(lstlist[[1]], xy = TRUE),
              aes(x = x, y = y, fill = MOD11C2.A2016121.LST_Day_CMG)) +
    geom_sf(data = province.transform.sf, fill = NA) + 
    geom_sf(data = df.transform.sf, colour = "blue") + theme_minimal()

get_one_village_lst = function(diptank, rastlist){
    loc <- subset(df.transform.sf, Diptank == diptank)
    temp <- data.frame(matrix(ncol = 3, nrow = length(rastlist)))
    colnames(temp) <- c("Diptank","approx.date", "ras.0")
        #temp$approx.date <- as.Date(temp$approx.date)
    for (i in 1:length(rastlist)){
        ras <- rastlist[[i]]
        ras.value <- extract(ras, loc) 
        approx.date <- as.character(extractDate(ras, asDate = TRUE)$inputLayerDates)
        temp[i, ] <- c(diptank, approx.date, ras.value)
    }
    return(temp)
}

old <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(old) <- c("Diptank", "approx.date", "ras.0")
lst <- old

for (i in 1:length(df.transform.sf$Diptank)){
    diptank <- df.transform.sf$Diptank[i]
    lst <- rbind(lst, get_one_village_lst(diptank, lstlist))
    print(i)
}
write.csv(lst, "lstdata.csv")
rm(list.lst, lstlist, lst)

###############################################################
###############################################################
# Read in and process ET data
###############################################################
###############################################################
et <- list.files(path = paste0(data.dir, "et"), pattern = "ET_500m.tif") 
list.et <- paste0(data.dir, "et/", et)
etlist <- lapply(list.et, raster)  

# want Peter's data points at this projection...
df.transform <- spTransform(df, crs(etlist[[1]]))
df.transform@data <- df@data
df.transform.sf <- as(df.transform, "sf")

get_one_village_et = function(diptank, rastlist){
    loc <- subset(df.transform.sf, Diptank == diptank)
    temp <- data.frame(matrix(ncol = 11, nrow = length(rastlist)))
    colnames(temp) <- c("Diptank","approx.date", "ras.0", 
                        "ras.med.1k", "ras.var.1k", "ras.med.2k", "ras.var.2k", 
                        "ras.med.3k", "ras.var.3k", "ras.med.4k", "ras.var.4k")
    for (i in 1:length(rastlist)){
        ras <- rastlist[[i]]
        ras.value <- extract(ras, loc) 
        approx.date <- as.character(extractDate(ras, asDate = TRUE)$inputLayerDates)
        med.var.1000 <- get_buffer(ras, loc, 1000)
        med.var.2000 <- get_buffer(ras, loc, 2000)
        med.var.3000 <- get_buffer(ras, loc, 3000)
        med.var.4000 <- get_buffer(ras, loc, 4000)
        temp[i, ] <- c(diptank, approx.date, ras.value, 
                    med.var.1000, med.var.2000, med.var.3000, med.var.4000)
        }
    return(temp)
}

old <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(old) <- c("Diptank", "date", "ras.0", 
                   "ras.med.1k", "ras.var.1k", "ras.med.2k", "ras.var.2k", 
                   "ras.med.3k", "ras.var.3k", "ras.med.4k", "ras.var.4k")
et <- old


for (i in 1:length(df.transform.sf$Diptank)){
    diptank <- df.transform.sf$Diptank[i]
    et <- rbind(et, get_one_village_et(diptank, lstlist))
    print(i)
}
write.csv(et, "etdata.csv")



# CUT - OLD WAY OF PROCESSING... 
# old <- data.frame(matrix(ncol = 16, nrow = 0))
# colnames(old) <- c("Place", "Diptank", "lat", "long", "cattle", "cattle_farmers", 
#                    "date", "ndvi.0", 
#                    "ndvi.med.1k", "ndvi.var.1k", "ndvi.med.2k", "ndvi.var.2k", 
#                    "ndvi.med.3k", "ndvi.var.3k", "ndvi.med.4k", "ndvi.var.4k")
#     
# for (ras in tifslist){
#     # extract value at data point, returns a vector of NDVI values
#     date <- extractDate(ras, asDate = TRUE)$inputLayerDates
#     ndvivals <- extract(ras, df.utm.sf) # works on both sf and sp objects
#     new <- data
#     new$date <- rep(date, length(new[,1]))
#     new$ndvi.0 <- ndvivals
#     
#     # extract median and variance at 1k buffer ring
#     meds <- extract(x = ras, y = df.utm, buffer = 1000, fun = median)
#     var <- extract(x = ras, y = df.utm, buffer = 1000, 
#                     fun = function(x) if (!is.null(x)) var(x, na.rm=TRUE))
#     new$ndvi.med.1k <- meds
#     if (length(var) == length(new[,1])){
#         new$ndvi.var.1k <- var
#     }
#     # extract median and variance at 2k buffer ring
#     meds <- extract(x = ras, y = df.utm, buffer = 2000, fun = median)
#     var <- extract(x = ras, y = df.utm, buffer = 2000, 
#                    fun = function(x) if (!is.null(x)) var(x, na.rm=TRUE))
#     new$ndvi.med.2k <- meds
#     if (length(var) == length(new[,1])){
#         new$ndvi.var.2k <- var    
#     }    
#     # extract median and variance at 3k buffer ring
#     meds <- extract(x = ras, y = df.utm, buffer = 3000, fun = median)
#     var <- extract(x = ras, y = df.utm, buffer = 3000, 
#                    fun = function(x) if (!is.null(x)) var(x, na.rm=TRUE))
#     new$ndvi.med.3k <- meds
#     if (length(var) == length(new[,1])){
#         new$ndvi.var.3k <- var  
#     }
#     # extract median and variance at 4k buffer ring
#     meds <- extract(x = ras, y = df.utm, buffer = 4000, fun = median)
#     var <- extract(x = ras, y = df.utm, buffer = 4000, 
#                    fun = function(x) if (!is.null(x)) var(x, na.rm=TRUE))    
#     new$ndvi.med.4k <- meds
#     if (length(var) == length(new[,1])){
#         new$ndvi.var.4k <- var    
#     }
#     old <- rbind(old, new)
# }
# write.csv(old, "ndvidata.csv")
# rm(old, new, tifslist, files, list.files)
# 
# 








#################################################################
#################################################################
# DELETE ME - OR NOTE SOMEWHERE
#################################################################
#################################################################
# #orgStruc(from = paste(data.dir, "MODIS/MOD13Q1.006", sep = "/"), 
# #         to = paste(data.dir, "MOD13Q1", sep = "/"), 
# #         move = TRUE, structure = "")


# # read in NDVI data.  Valid range = -2000 to 10000, with scale-factor 0.0001
# # For data processing purposes, MODIS VIs are generated in square tile units 
# # that are approximately 1200-by-1200 km (at the equator), and
# # mapped in the Sinusoidal (SIN) grid projection (equal area projection)
# # Naming convention = Product Name.Julian aquisition day (YYYYDDD).location?.
# # version(006).Julian Date of Production (YYYYDDDHHMMSS) 
# r <- rast(file.path(data.dir, "MOD13Q1.A2016145.h31v12.006.2016166145545.hdf"))
# crs(r) <- crs.ndvi
# r.df <- as.data.frame(r[[1]], xy = TRUE)

# # Useful to explore raster file
# # dim(r); nlyr(r); ncell(r) # number of rows, columns, layers # 12 layers # cells
# # res(r) # about 250m
# # head(values(r[[1]]))
# # ndvi2 <- ndvi <- r[[1]]; ndvi; plot(r[[1]])
# 
# 
# # ????gdal_translate(r[[1]], dst_dataset = test.tif) # paste(data.dir, "test.tif", sep = "/"))
# 
# 
# # reproject
# 
# change <- projectRaster(ndvi, crs = crs(municipality.utm))
# ndvi.df <- as.data.frame(ndvi, xy = TRUE) # turn into dataset
# ggplot() + 
#     # geom_sf(data = subset(municipality.ndvi, ADM3_EN %in% c("Jozini", "Umhlabuyalingana"))) +
#     geom_raster(data = r.df, aes(x = x, y = y, fill = X250m.16.days.NDVI)) + 
#     geom_raster(data = r2.df, aes(x = x, y = y, fill = X250m.16.days.NDVI)) +
#     # geom_sf(data = df.sf) + 
#     theme_minimal() + theme(legend.title = element_blank())
# plot(ndvi)
