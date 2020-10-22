rm(list = ls())
library(lubridate)

# Load in the raw serological cattle data. 
setwd("~/GitHub/mosquito/data")
sero <- read.csv("raw/ndumo_cattle.csv", header=TRUE)

# Convert the date column to a date type.
sero$DATE <- as.Date(sero$date, format="%d%b%Y")

# Animal ID to character
sero$animal_id <- as.character(sero$animal_id)

# Want data in y, an array with dimensions indexing individuals, weeks, and observed states.  Dimensions N=623  by T=105,  by  3. The observed state, y(i,t), can19be  either  (1,0,0),  (0,1,0),  (0,0,1), representing  test  negative,  test  positive,  or  not20observed respectively.
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
        y <- c(0, 0, 1)
    } else if (length(df$rvf) > 1){
        y <- rep(paste("error for id, ", id, 
            "; more than one observation in week: ", time_chunk[1], sep = ""), 3)
    } else if (df$rvf == 0) {
        y <- c(1, 0, 0)
    } else if (df$rvf == 1){
        y <- c(0, 1, 0)
    } else {
        y <- rep("check me", 3)
    }
    return(y)
}

get_one_animal = function(id){
    # fill a 2d matrix with all ids for animal
    temp <- array(NA, dim = c(t, 3))
    i <- 1
    for(t in times){
        temp[i,] <- get_one_observation(id, t)
        i <- i + 1
    }
    return(temp)
}

# Fill y array
y <- array(NA, dim = c(n, t, 3))
i <- 1
for (id in ids){
    y[i, ,] <- get_one_animal(id)
    i <- i + 1
}
saveRDS(y, "y.rds")

# Fill B
B <- array(NA, dim = c(n, t, 4, 3))
for (i in 1:n){
    for(j in 1:t){
        yval <- y[i, j, ]
        if (yval[3] == 1){
            B[i, j, , ] <- matrix(c(rep(0, 8), rep(1, 4)) , byrow = T)
        } else {
            B[i, j, ,] <- matrix(c(rep(1, 8), rep(0, 4)) , byrow = T)
        }
    }
}
saveRDS(B, "B.rds")
