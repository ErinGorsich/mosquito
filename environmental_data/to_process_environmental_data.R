#################################################################
#################################################################
# Outline: 
#################################################################
# Read in study sites spatial information
# Read in environmental datasets and clip to KZN and southern Moz
# Overlay points 

#################################################################
#################################################################
setwd("~/GitHub/mosquito/environmental_data")
file.dir <- "~/Google Drive/Warwick/mosquito/IGSD 2020/SatelliteInformation/"
x <- c("rgdal", "rgeos", "maptools", "dplyr", "sp", 
       "ggmap", "ggplot2", "plyr", "tmap", "RColorBrewer") 
lapply(x, library, character.only = TRUE)

#################################################################
#################################################################
# Read in study sites spatial information
#################################################################
#################################################################
# Country: https://data.humdata.org/dataset/south-africa-admin-level-1-boundaries
# 0 = country, 1 = province, 2 = district, 3 = local municipality, 4 = ward
sa <- readOGR(dsn = paste(file.dir, "/country/", sep = ""), 
              layer = "zaf_admbnda_adm0_2016SADB_OCHA")
province <- readOGR(dsn = paste(file.dir, "/country/", sep = ""), 
                    layer = "zaf_admbnda_adm1_2016SADB_OCHA")
district <- readOGR(dsn = paste(file.dir, "/country/", sep = ""), 
                    layer = "zaf_admbnda_adm2_2016SADB_OCHA")
municipality <- readOGR(dsn = paste(file.dir, "/country/", sep = ""), 
        layer = "zaf_admbnda_adm3_2016SADB_OCHA")
kzn <- province[province$ADM1_ID == "KZN", ]

# Read in study sites
peter <- read.csv(paste(file.dir, "diptanks.csv", sep = ""))
df <- SpatialPointsDataFrame(coords = cbind(peter$long, peter$lat), 
                             proj4string = sa@proj4string, data = peter)

# Make a nice ggplot map of SA/Moz, with KZN, Umkhanya-Kude (district) coloured,
# light-greay municipalities...
# plot(kzn, axes = TRUE)
# points(df, pch = 19, cex = 0.8)

#################################################################
#################################################################
# Read in environmental datasets and clip to KZN and southern Moz
#################################################################
#################################################################
p1 <- ggplot(plot, aes(long, lat, group = group, fill = nquantile)) + 
    geom_polygon(colour = "black") + # guide = FALSE 
    #theme_clean() + 
    scale_fill_manual(values = pal) + 
    coord_equal() + 
    expand_limits(x = plot$long, y = plot$lat) + 
    labs(fill = "Samples")

