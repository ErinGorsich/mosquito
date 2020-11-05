###############################################
###############################################
# Read in, clean up, reformat Peter's data.
###############################################
###############################################
library(lubridate)
subset <- TRUE

# Load in the raw serological cattle data. 
sero <- read.csv("raw/ndumo_cattle.csv", header=TRUE)

# Convert the date column to a date type.
sero$DATE <- as.Date(sero$date, format="%d%b%Y")

# Animal ID to character
sero$animal_id <- as.character(sero$animal_id)

# subset
subset.villages <- c(5, 6, 7, 9) # try to add 1 & 2 later...
if (subset){
    sero <- sero[sero$place %in% subset.villages, ]
}

# Want data in y, an array with dimensions indexing individuals and weeks (N=623 by T=104).
ids <- unique(as.character(sero$animal_id))
times <- seq(min(sero$DATE), by = 7, length.out = 104)
n <- length(ids)
t <- length(times)

get_one_observation = function(id, start_time){
    min.time <- start_time
    max.time <- start_time + 7
    df <- sero[
        sero$animal_id == id & sero$DATE >= min.time & sero$DATE < max.time, ]
    if (length(df$rvf) == 0){
        #y <- c(0, 0, 1)
        y <- 3
    } else if (length(df$rvf) > 1){
        #y <- rep(paste("error for id, ", id, 
        #    "; more than one observation in week: ", time_chunk[1], sep = ""), 3)
        y <- "error"
    } else if (df$rvf == 0) {
        y <- 1
        #y <- c(1, 0, 0)
    } else if (df$rvf == 1){
        y <- 2
        #y <- c(0, 1, 0)
    } else {
        #y <- rep("check me", 3)
        y <- "check me"
    }
    return(y)
}

# Fill y array
y <- array(NA, dim = c(n, t))
i <- j <- 1
for (id in ids){
    for (t in times){
        y[i, j] <- get_one_observation(id, t)
        j <- j + 1
    }
    i <- i + 1
    j <- 1
}
saveRDS(y, "y.rds")

# id list and village list
lut <- data.frame(id = ids, villages = NA)
lut$villages <- sero$place[match(lut$id, sero$animal_id)]
lut$villages[lut$villages == 5] <- 1
lut$villages[lut$villages == 6] <- 2
lut$villages[lut$villages == 7] <- 3
lut$villages[lut$villages == 9] <- 4
saveRDS(lut, "lut.rds")

