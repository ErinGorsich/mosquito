####################################################
####################################################
####################################################
# Conor - Intro mosquito-borne disease model
# 18-June-2020
####################################################
####################################################
####################################################

# Make yourself some notes and outline here, in comments
# Outline
# 1) Data, package, accessory files read in
# 2) Defines basic SIR model
# 3) Runs basic model
# 4) Plots model output after manipulating one parameter value

####################################################
# 1) Data Read in, packages, set Working directory
####################################################
rm(list = ls())

# wd is the folder where you put all your files in to call later
# I've made a googel drive folder with your name... 
# change this working directory to wherever it is stored on your computer
#setwd("~/Google Drive/Conor/SIR intro lesson")
setwd("~/GitHub/mosquito/rvf")

# You can install packages with extra handy features. 
# You always need to load them (library) but you only have to install once.
# The deSolve package integrates systems of differential equations 
install.packages("deSolve")
library(deSolve)

# You can also store functions or extra bits of code in other files
# note how your 'global environment changes'
source('parameters.R', chdir = TRUE)

####################################################
# 2) Defines basic mosquito-borne disease SIR model
####################################################
rhs = function(t, Y, params){
    ##########################################
    # ode, called into ode (deSolve) to analyze
    # inputs: t = timelist, Y = state variables, params = Parameters
    ##########################################
    with(as.list(c(Y, params)), {
        Nv <- Sv + Ev + Iv
        Nh <- Sh + Ih + Rh
        lambda.h <- beta.hv * (Iv / Nv) * (sigma.v * Nv * sigma.h) / 
            (sigma.v * Nv + sigma.h * Nh)
        lambda.v <- beta.vh * (Ih / Nh) * (sigma.v * Nh * sigma.h) /
            (sigma.v * Nv + sigma.h * Nh)
        
        # Host
        dShdt <- birth.h * Ho - death.h * Sh - lambda.h * Sh
        dIhdt <- lambda.h * Sh - death.h * Ih - gamma.h * Ih
        dRhdt <- gamma.h * Ih - death.h * Rh
        
        # Mosquito - Eggs/Larvae
        dSedt = birth.v * Vo * (1 - (phi.v * Iv / Nv)) - gamma.e * Se
        dIedt = birth.v * (phi.v * Iv / Nv) - gamma.e * Ie
        
        # Mosquito - Adults (note Iv equations has extra term from paper)
        dSvdt = gamma.e * Se - death.v * Sv - lambda.v * Sv
        dEvdt = lambda.v * Sv - death.v * Ev - nu.v * Ev
        dIvdt = gamma.e * Ie + nu.v * Ev - death.v*Iv
        
        out <- list(c(Sh = dShdt, Ih = dIhdt, Rh = dRhdt, 
            Se = dSedt, Ie = dIedt, 
            Sv = dSvdt , Ev = dEvdt, Iv = dIvdt))
        return(out)
    })
}

####################################################
# 3) Run basic SIR model
####################################################
# Initial conditions
Sh0 = 950 
Ih0 = 50 
Rh0 = 0 
Se0 = 40*Sh0
Ie0 = 0 
Sv0 = 40*Sh0
Ev0 = 0 
Iv0 = 0
Y0 <- c(Sh = Sh0, Ih = Ih0, Rh = Rh0, Se = Se0, Ie = Ie0, 
    Sv = Sv0, Ev = Ev0, Iv = Iv0)

# Define a list of times we want the solution for = daily for 1 years
times <- seq(0, 365*1, by = 1) 
sol <- list(NA)

# Integrate with ode() function.  See ?ode for why
sol <- as.data.frame(ode(Y0, times, rhs, params.wet))
# FIX ME - NEED TO ADD SEASONS AND CHECK MODEL STRUCTURE

# Ways to look at data frames 
head(sol)
head(sol, 50)
summary(sol)

# Plot output for yourself
plot(x = sol$time, y = sol$S)  

# Plot for sharing (see options with ?par)
plot(x = sol$time, y = sol$Sh, type = "l", xlab = "Time (years)", 
    ylab = "Number of aniamals", las = 1, cex.lab = 1.2)
lines(x = sol$time, y = sol$Ih, col = "dark red")
lines(x = sol$time, y = sol$Rh, col = "blue")
legend("topright", legend = c("Sh", "Ih", "Rh"), lty = 1,
    col = c("black", "dark red", "blue"), bty = "n")

####################################################
# 4) Plots model output after manipulating one parameter value
####################################################
# hint... just plot I... plot value for a few different parameter combinations
# for a challenge, make a for loop to span many possible values of a parameter

#####################################################
#####################################################
# Exercises
# 1) What happens to model dynamics with different initial conditions? 
# 2) Modify the model to add demography (see Keeling text book) - what happens?
#####################################################
#####################################################

