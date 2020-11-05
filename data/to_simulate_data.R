##########################################################
##########################################################
# Code simulates data first with no error, then with Gaussian process
##########################################################
##########################################################
setwd("~/GitHub/mosquito/data")
library(ggplot2)
library(MASS)
rm(list = ls())

# Who/where/when observed
##########################################################
# Who/where (nids and observation frequency from data)
nids <- c(88, 122, 74, 84)
nweeks <- 104
rvec.init <- runif(4, 0.2, 0.4)*nids # seroprevalence... 
ivec.init <- round((nids - rvec.init)*0.05, 0)
lut <- data.frame(id = seq(1:sum(nids)), 
                  villages = c(rep(1, nids[1]), rep(2, nids[2]), 
                               rep(3, nids[3]), rep(4, nids[4])))

# first capture and capture frequency
y <- readRDS('y.RDS')  # capture history - sum(ninds) cows by nweeks weeks
f <- apply(y, 1, function(x) min(which(x!=3)))  # index of first capture
obs <- apply(y, 1, function(x) which(x != 3)) # list with indicies of observation
c <- y
c[c!=3]<- 1
c[c == 3] <- 0; 
# could make generic here, with same captures every ~2.5 months = 10 weeks

# Functions to set A, for each village and time; needs ninds
##########################################################
setA = function(params, ivec, village.id, lut, test = FALSE){
    # params = beta0, beta1, alpha; 
    # ivec = vector num infected for each village; village.id = focal village
    # Generates a unique A matrix for each village and time...
    nids <- as.data.frame(table(lut$villages))$Freq
    with(params,{
        Z1 <- 0
        for (i in 1:length(ivec)){
            ifelse(i == village.id, 
                temp <- ivec[i]/nids[i],
                temp <- (ivec[i]^alpha)/nids[i])
            Z1 <- temp + Z1
        }
        lambda <- 1 - exp(-exp(beta0 + beta1 * Z1))
        A <- matrix(c(lambda, 1-lambda, 0, 0, 
                      0, 0, 1, 0, 
                      0, 0, 0, 1, 
                      0, 0, 0, 1), ncol = 4, nrow = 4, byrow = TRUE)
    ifelse (test, return(lambda), return(A))
    })
}

B <- c(1, 1, 1, 2)


# Trial values... Note realistic lambda values come from negative betas.
##########################################################
# Non GP version... beta0 + beta1 --> set priors to have mean below 0
beta <- seq(-3, 3, 0.2)
test <- data.frame(beta0 = rep(beta, each = length(beta)), 
                   beta1 = rep(beta, length.out = length(beta)))
test$lambda0 <- NA
for (i in 1:length(test[,1])){
    params <- list(beta0 = test$beta0[i], beta1 = test$beta1[i], alpha = alpha)
    test$lambda0[i] <- setA(params, ivec.init, 1, lut, test = TRUE)
    rm(params)
}
ggplot(data = test, aes(x = beta0, y = beta1, fill = lambda0)) + geom_tile()

# GP version... beta0 + beta1


# Function to simulate state process
##########################################################
sim = function(params, lut, nweeks, ivec.init, c){
    # define empty matrices for state-transition process
    y.sim <- z.sim <- matrix(NA, nrow = length(lut[,1]), ncol = nweeks)
    nids <- as.data.frame(table(lut$villages))$Freq
    
    # Fill t =1 based on initial infections/village; and 95% seroneg S; 5%E; 5% I
    for (i in 1:length(lut[,1])){
        village.id <- lut$villages[i]
        village.prev <- ivec.init[village.id] / nids[village.id]
        z.sim[i, 1] <- which(rmultinom(1, 1, 
            prob = c((1 - village.prev)*0.95, (1 - village.prev)*0.05, 
                     (1 - village.prev)*0.05, village.prev)) == 1)
        y.sim[i, 1] <- B[z.sim[i,1]]*c[i,1]
    }
    # Set A; fill matricies for following time steps
    for (j in 2:nweeks){
        for (i in 1:length(lut[,1])){ # make faster by looping through villages...
            village.id <- lut$villages[i]
            village.ids <- unique(lut$villages)
            states <- matrix(NA, ncol = 4, nrow = length(village.ids))
            ivec.next <- c()
            for(k in 1:length(village.ids)){
                vals <- z.sim[which(lut$villages == village.ids[k]), j-1]
                ivec.next[k] <- length(vals[vals == 3])
            }
            A <- setA(params, ivec.next, village.id, lut)
            z.sim[i, j] <- which(rmultinom(1, 1, prob = A[z.sim[i,j-1], ]) == 1) 
            y.sim[i, j] <- B[z.sim[i,j]]*c[i,j]
        }
    }
    return(list(z.sim, y.sim))
}

# Trial
##########################################################
# regression component
beta0 <- -1 # 0.05 
beta1 <- -3 # 0.2
alpha <- 0.2

params <- list(beta0 = beta0, beta1 = beta1, alpha = alpha)
test <- sim(params, lut, nweeks, ivec.init, c)
z.sim <- test[[1]]
y.sim <- test[[2]]
saveRDS(y.sim, "y.sim.rds")

##########################################################
##########################################################
# With GP - NEEDS FIXED
##########################################################
##########################################################
# Epsilon follows a  Gaussian Process
setGP = function(params){
    # parameter inputs = sigma, mu, rho, tau
    # set globally = nweeks
    t <- seq(1, nweeks, 1) # time from 1:nweeks
    with(params,{
        # Set sigma{ij} = tau^2 * exp(-rho *(t_i - t_j)^2 if i=j; exp is autocor
        # Set sigma{ij} = tau^2 + sigma^2 if i = j
        Sigma <- sigma^2 * diag(nweeks) + tau^2 * exp(-rho * outer(t, t, '-')^2)
        epsilon = mvrnorm(1, rep(0, nweeks), Sigma)
        return(epsilon)
    })
}

# Gaussian Process Version
setA_GP = function(params, ivec, village.id, lut, test = FALSE){
    # Parameters = beta0, beta1, alpha; 
    # ivec = vector num infected for each village; village.id = focal village
    # Generates a unique A matrix for each village and time...
    nids <- as.data.frame(table(lut$villages))$Freq
    with(params,{
        Z1 <- 0
        for (i in 1:length(ivec)){
            ifelse(i == village.id, 
                   temp <- ivec[i]/nids[i],
                   temp <- (ivec[i]^alpha)/nids[i])
            Z1 <- temp + Z1
        }
        epsilon <- simGP(params)
        lambda <- 1 - exp(-exp(beta0 + beta1 * Z1 + epsilon))
        A <- matrix(c(lambda, 1-lambda, 0, 0, 
                      0, 0, 1, 0, 
                      0, 0, 0, 1, 
                      0, 0, 0, 1), ncol = 4, nrow = 4, byrow = TRUE)
        ifelse (test, return(lambda), return(A))
    })
}

# GP component: for epsilon representing error term in regression
sigma <- 0.01 # residual standard deviation
rho <- 1 # decay parameter for the GP auto-correlation function
tau <- 1 # GP standard deviation parameter

