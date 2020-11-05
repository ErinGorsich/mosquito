############################################### 
############################################### 
# Define models - RVF dynamics
############################################### 
###############################################
sink("nolags_model.txt")
cat("model{
    # Priors for regression parameters
    beta0 ~ dt(0, pow(2.5,-2), 1)
    beta1 ~ dt(0, pow(2.5,-2), 1)
    beta3 ~ dt(0, pow(2.5,-2), 1)
    # beta2 ~ dt(0, pow(2.5,-2), 1)
    # beta4 ~ dt(0, pow(2.5,-2), 1)

    # CHECK FOR UNSTANDARDIZED COVARIATES
    # http://www.stat.columbia.edu/~gelman/research/published/priors11.pdf
    # http://www.stat.columbia.edu/~gelman/presentations/weakpriorstalk.pdf
    
    # Define state-transition and observation matrix
    for (j in 1:nvillages){
        for (t in 1:nweeks){
            # State transition: z(i, t) = z(i,t)*A
            A[j, t, 1, 1] <- 1 - p[j, t]
            A[j, t, 1, 2] <- p[j, t]
            A[j, t, 1, 3] <- 0
            A[j, t, 1, 4] <- 0
            A[j, t, 2,  1] <- 0
            A[j, t, 2, 2] <- 0
            A[j, t, 2, 3] <- 1
            A[j, t, 2, 4] <- 0
            A[j, t, 3,  1] <- 0
            A[j, t, 3,  2] <- 0
            A[j, t, 3,  3] <- 0
            A[j, t, 3,  4] <- 1
            A[j, t, 4,  1] <- 0
            A[j, t, 4,  2] <- 0
            A[j, t, 4,  3] <- 0
            A[j, t, 4,  4] <- 1
            }
        }

    # Define X1 and X2
    # X1 with dimensions nvillages*nweeks & elements number infected in village j
    # X2 with same dimensions & elements number infected at all other villages
    for (i in 1:ncows){
        for (t in 1:nweeks){
            indicatorI[village[i], t] <- ifelse(z[i, t] == 3, 1, 0) + 
                indicatorI[village[i], t]
        }
    }
    for (j in 1:nvillages){
        for (t in 1:nweeks){
            X1[j, t] <- indicatorI[j, t]
            X2[j, t] <- indicatorI[1, t] + indicatorI[2, t] + 
                indicatorI[3, t] + indicatorI[4, t] - X1[j, t]
        }
    }

    # Likelihood
    for (i in 1:ncows){
        for (t in (f[i] + 1):nweeks){
            z[i, t] ~ dcat(A[village[i], t-1, z[i, t-1], ])
            p[village[i], t] <- icloglog(beta0 + beta1 * X1[village[i], t-1] + beta3 * X2[village[i], t-1])
        }
    }
    }", fill=TRUE)
sink()


sink("nolags_model.txt")
cat("model{
    # Priors - PLACEHOLDERS CHECK FOR UNSTANDARDIZED COVARIATES & MIXING PARAMS
    # http://www.stat.columbia.edu/~gelman/research/published/priors11.pdf
    # http://www.stat.columbia.edu/~gelman/presentations/weakpriorstalk.pdf
    beta0 ~ dt(0, pow(2.5,-2), 1)
    beta1 ~ dt(0, pow(2.5,-2), 1)
    beta3 ~ dt(0, pow(2.5,-2), 1)
    # beta2 ~ dt(0, pow(2.5,-2), 1)
    # beta4 ~ dt(0, pow(2.5,-2), 1)

    # Define state-transition and observation matrix
    for (j in 1:nvillages){
        for (t in 1:nweeks){
            # State transition: z(i, t) = z(i,t)*A
            A[j, t, 1, 1] <- 1 - p[j, t]
            A[j, t, 1, 2] <- p[j, t]
            A[j, t, 1, 3] <- 0
            A[j, t, 1, 4] <- 0
            A[j, t, 2,  1] <- 0
            A[j, t, 2, 2] <- 0
            A[j, t, 2, 3] <- 1
            A[j, t, 2, 4] <- 0
            A[j, t, 3,  1] <- 0
            A[j, t, 3,  2] <- 0
            A[j, t, 3,  3] <- 0
            A[j, t, 3,  4] <- 1
            A[j, t, 4,  1] <- 0
            A[j, t, 4,  2] <- 0
            A[j, t, 4,  3] <- 0
            A[j, t, 4,  4] <- 1
            }
        }
    #B[1] <- 1
    #B[2] <- 1
    #B[3] <- 1
    #B[4] <- 2

    # Define X1 and X2
    # X1 with dimensions nvillages*nweeks & elements number infected in village j
    # X2 with same dimensions & elements number infected at all other villages
    for (i in 1:ncows){
        for (t in 1:nweeks){
            indicatorI[village[i], t] <- ifelse(z[i, t] == 3, 1, 0) + 
                indicatorI[village[i], t]
        }
    }
    for (j in 1:nvillages){
        for (t in 1:nweeks){
            X1[j, t] <- indicatorI[j, t]
            X2[j, t] <- indicatorI[1, t] + indicatorI[2, t] + 
                indicatorI[3, t] + indicatorI[4, t] - X1[j, t]
        }
    }

    # Likelihood
    for (i in 1:ncows){
        # set first capture
        # z[i, f[i]] <- z[i, f[i]] # set as R if positive, S otherwise
        for (t in (f[i] + 1):nweeks){
            # State process
            z[i, t] ~ dcat(A[village[i], t-1, z[i, t-1], ])
            p[village[i], t] <- icloglog(beta0 + beta1 * X1[village[i], t] + beta3 * X2[village[i], t])
            #y[i, t] <- B[z[i, t]]*c[i, t]
        }
    }
    }", fill=TRUE)
sink()

