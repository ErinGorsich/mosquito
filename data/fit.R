###############################################
###############################################
# Run Models
###############################################
###############################################
setwd("~/GitHub/mosquito/data")
rm(list = ls())
library('coda')
library('rjags')
library('R2jags')
clean_data = FALSE

# define models 
###############################################
source('to_define_models.R') # saves basemodel.txt to wd

# define MCMC settings
###############################################
niterations <- 100
nchains <- 3
nadapt <- 100

# define data for model
###############################################
if (clean_data){
    source('to_prep_data_for_JAGS.R') # saves y and lut to file
}
y <- c <- readRDS('y.RDS')  # capture history - ncows by nweeks
ncows <- dim(y)[1]
nweeks <- dim(y)[2]
c[c == 3] <- 0; c[c == 2] <- 1 # was the cow observed
f <- apply(y, 1, function(x) min(which(x!=3)))  # index of first capture
lut <- readRDS('lut.RDS')  # look up table for ids within villages
village <- lut$villages    
nvillages <- length(unique(lut$villages))

# Recommended in JAGS book (but for a hierarchial version) to read in the
# 'known states' as data; dont think this is needed because part of likelihood?
min.four.index <- c()

state <- y
# if test + we know it is recovered
state[state != 2] <- NA
state[state == 2] <- 4
# if test + we know all following are also recovered
for (i in 1:ncows){
    if (sum(na.exclude(state[i, ])) > 3) {  # if there is a 4 at all... 
        first.recovered <- min(which(state[i, ] == 4))
        state[i, first.recovered:nweeks] <- 4
        
        # if test +, we also know state of those immediately preceeding
        if (first.recovered == 1){
        } else if (first.recovered == 2){
            state[i, 1] <- 3
        } else if (first.recovered == 3){
            state[i, 1] <- 2
            state[i, 2] <- 3        
        } else {
            state[i, first.recovered - 1] <- 3
            state[i, first.recovered - 2] <- 2
            state[i, 1:(first.recovered - 3)] <- 1
        }
    }
}


# model is conditional on the first capture, so set first capture  NA --> 
# NOT TRUE HERE BECAUSE WE DON'T KNOW STATE AT ALL FIRST CAPTPURES
# first.observed <- min(which(!is.na(state[i, ])))
# state[i, first.observed] <- NA

#jags.data <- list(y = y, f = f, c = c, nvillages = nvillages, nweeks = nweeks, 
#                  ncows = ncows, village = village)
jags.data <- list(f = f, z = state, nvillages = nvillages, 
                  nweeks = nweeks, ncows = ncows, village = village)

# define initial conditions for base.model
###############################################
# Values in z that are specified in the data are set to NA
init.z <- matrix(0, nrow = ncows, ncol = nweeks)
for (i in 1:ncows){
    init.z[i, which(!is.na(state[i,]))] <- NA
    init.z[i, which(is.na(state[i,]))] <- 1
}
jags.inits <- list(beta0 = 1, beta1 = 0.1, beta3 = 1, z = init.z)
parameters <- c("beta0", "beta1", "beta3")

# Call JAGS from R
###############################################
base <- jags.model(file = "nolags_model.txt", data = jags.data, 
                   inits = jags.inits, n.chains = nchains, n.adapt = nadapt)
base.fit <- coda.samples(model = base, variable.names = parameters, 
                         n.iter = niterations)
# update(base, nburnin) ??? check if want earlier...
dic <- dic.samples(model = base, n.iter = niterations, type = "pD")
print(test, digits = 3)




# CUT STUFF: delete later :) 
# r <- apply(y, 1, function(x) min(which(x == 2))) # index of first recovered
