# http://scipp.ucsc.edu/~haber/ph5B/sho09.pdf
# dynamic data analysis book 3.3
############################################
library(deSolve)

############################################
############################################
# forcing functions
############################################
############################################
leach1 = function(t, A, omega, theta){
    x = A*cos(omega * t + theta)
    return(x)
}
leach = function(t, x0, omega, b){
    # want b = v0/omega; where v0 is first derivative evaluated at t = 0?
    x = x0*cos(omega * t) + b*sin(omega * t)
    return(x)
}
jose = function(t, r0, epsilon, k){
    # epsilon is amplitude; k = shape of empty space; r0 sets period
    r = r0 * (1 + epsilon * (sin(pi*t))^k)
    return(r)
}
leach.death = function(t, A, d0, omega, theta){
    x = A*cos(omega * t + theta)
    d = d0 * exp(x)
    return(d)
}

# second order way?
# https://desolve.r-forge.r-project.org/slides/tutorial.pdf
harmonic = function(t, omega, n){ 
    # n is a 2 element vector containing, 
    nprime = n[2]
    n2 = (omega^2) * n[1]
    list(c(n[2], ))
    out <- list(c(nprime, n2))
    return(out)
}
# need to watch precision and integration method here!
leach.secondorder = function(t, omega, n0){
    out <- ode(n = n0, func = harmonic, time = seq(0, 20, 0.001),
               params = 2*pi)
    
}
n0 = c(10, 0) 

# Jose: epsilon is amplitude; k = shape of empty space; r0 sets period
############################################
tvec = seq(0, 3, 0.0001)
par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
plot(x = tvec, y = jose(tvec, r0 = 115/365, epsilon = 0.3, k = 2), type = 'l', 
     ylab = "per capita growth rate", xlab = "years", main = "epsilon=0.3, k=2")
plot(x = tvec, y = jose(tvec, r0 = 115/365, epsilon = 2, k = 2), type = 'l', 
     ylab = "per capita growth rate", xlab = "years", main = "epsilon=2, k=2")
plot(x = tvec, y = jose(tvec, r0 = 115/365, epsilon = 0.3, k = 4), type = 'l', 
     ylab = "per capita growth rate", xlab = "years", main = "epsilon=0.3, k=4")
plot(x = tvec, y = jose(tvec, r0 = 115/365, epsilon = 2, k = 4), type = 'l', 
     ylab = "per capita growth rate", xlab = "years", main = "epsilon=2, k=4")


# Leach 1 - theta shifts right/left; omega sets period; 
# A changes amplitude with peak at A, trough at -A
############################################
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(x = tvec, y = leach1(tvec, A = 2, omega = 2*pi, theta = 0), 
    type = "l", ylab = "per capita population growth rate", xlab = "years")
plot(x = tvec, y = leach1(tvec, A = 2, omega = 2*pi, theta = 0.5), 
    type = "l", ylab = "per capita population growth rate", xlab = "years")
plot(x = tvec, y = leach1(tvec, A = 4, omega = 2*pi, theta = 0), 
    type = "l", ylab = "per capita population growth rate", xlab = "years")
plot(x = tvec, y = leach1(tvec, A = 4, omega = 2*pi, theta = 0.5), 
     type = "l", ylab = "per capita population growth rate", xlab = "years")

# Leach & Leach 1 re-scale to each other. 
############################################
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(x = tvec, y = leach(tvec, x0 = 1, 2*pi, b = 1), type = 'l')
plot(x = tvec, y = leach(tvec, x0 = 2, 2*pi, b = 2), type = 'l')
plot(x = tvec, y = leach(tvec, x0 = 1, 2*pi, b = 4), type = 'l')
plot(x = tvec, y = leach(tvec, x0 = 4, 2*pi, b = 1), type = 'l')
# tvec = seq(0, 365, 0.1) # to use daily timestep divide omega by 365
# plot(x = tvec, y = leach(tvec, x0 = 0.001, omega = 2*pi, b = 0), type = "l")

# x0 = amplitude when b = 0; b = both shifts and scales amplitude. 
par(mfrow = c(2, 2), mar = c(3, 3, 2, 2))
plot(x = tvec, y = leach(tvec, x0 = 0.2, omega = 2*pi, b = 0), 
     type = "l", ylab = "per capita population growth rate", xlab = "years", 
     main = "x0=0.2, b=0")
plot(x = tvec, y = leach(tvec, x0 = 2, omega = 2*pi, b = 0), 
     type = "l", ylab = "per capita population growth rate", xlab = "years", 
     main = "x0=2, b=0")
plot(x = tvec, y = leach(tvec, x0 = 0.2, omega = 2*pi, b = 2), 
     type = "l", ylab = "per capita population growth rate", xlab = "years", 
     main = "x0=0.2, b=2")
plot(x = tvec, y = leach(tvec, x0 = 2, omega = 2*pi, b = 2), 
     type = "l", ylab = "per capita population growth rate", xlab = "years", 
     main = "x0=2, b=2")

# Death term (want around 25-30d max; 15d min = 0.068 - 0.082 years
############################################
par(mfrow = c(2, 2), mar = c(3, 3, 2, 2))
plot(x = tvec, y = 1/leach.death(tvec, A = -0.3, d0 = 12, omega = 2*pi, theta = 0), 
     type = "l", ylab = "vector longevity (years)", xlab = "years", 
     main = "A = -0.3, d0 = 12")
plot(x = tvec, y = 1/leach.death(tvec, A = -0.1, d0 = 12, omega = 2*pi, theta = 0), 
     type = "l", ylab = "vector longevity (years)", xlab = "years", 
     main = "A = -0.1, d0 = 12")
plot(x = tvec, y = 1/leach.death(tvec, A = -0.1, d0 = 14, omega = 2*pi, theta = 0), 
     type = "l", ylab = "vector longevity (years)", xlab = "years", 
     main = "A = -0.3, d0 = 0.9") # yearly timestep
plot(x = tvec, y = 1/leach.death(tvec, A = -0.1, d0 = 13.5, omega = 2*pi, theta = 0), 
     type = "l", ylab = "vector longevity (years)", xlab = "years", 
     main = "A = -0.1, d0 = 13.5")

# FOR DAILY TIMESTEP
plot(x = tvec, y = 1/leach.death(tvec, A = -0.3, d0 = 0.05, omega = 2*pi, theta = 0), 
     type = "l", ylab = "vector longevity (days)", xlab = "years", 
     main = "A = -0.3, d0 = 0.05") # USE THIS ONE for daily timestep !!!!!


############################################
############################################
# Mosquito population dynamics
############################################
############################################
# Just populations, no disease
rhs = function(t, Y, params){
    with(as.list(c(Y, params)), {
        # Mosquito - Adults (note Iv equations has extra term from paper)
        r = leach1(t, A = A, omega = 2*pi, theta = 0)
        dNvdt = r * Nv
        out <- list(c(Nv = dNvdt))
        return(out)
    })
}
# size of outbreak and spaces between are related...
Y0 = c(Nv = 20)
times = seq(0, 10, 0.001)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot(x = times, y = as.data.frame(ode(Y0, times, rhs, list(A = 10)))$Nv, type = 'l', 
     ylab = "Number of mosquitoes", main = "A=10")
plot(x = times, y = as.data.frame(ode(Y0, times, rhs, list(A = 20)))$Nv, type = 'l', 
     ylab = "", main = "A=20")
plot(x = times, y = as.data.frame(ode(Y0, times, rhs, list(A = 30)))$Nv, type = 'l', 
     ylab = "", main = "A=30")

############################################
############################################
# same but trial infection
############################################
############################################
rhs_trial = function(t, Y, params){
    ##########################################
    # ode, called into ode (deSolve) to analyze
    # inputs: t = timelist, Y = named state variables, params = Parameters
    ##########################################
    with(as.list(c(Y, params)), {
        # Mosquito - Adults (note Iv equations has extra term from paper)
        r = leach1(t, A = A, omega = 2*pi, theta = 0)
        death.v = leach.death(t, A = D, d0 = d0, omega = 2*pi, theta = 0)
        # for play - assume a constant force of infection... 
        # with wet season params (Manore); and V:H ratio = 200 (well above min)
        # lambda.v <- 0.38 * (0.5) * (0.33 * 100 * 19) /
        #    (0.33 * Nv + 19 * 50)
        Sv = Nv - Ev - Iv
        dNvdt = r * Nv
        dEvdt = 0.1 * Sv - death.v * Ev - nu.v * Ev
        dIvdt = nu.v * Ev - death.v * Iv
        out <- list(c(Nv = dNvdt, Ev = dEvdt, Iv = dIvdt))
        return(out)
    })
}
plot_subsets = function(sol, time.subset){
    sol$Sv <- sol$Nv - sol$Ev - sol$Iv
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    df <- sol[sol$time %in% time.subset, ]
    plot(x = df$time, y = df$Nv, type = 'l', ylab = "Nv", xlab = "time")
    plot(x = df$time, y = df$Sv, type = 'l', ylab = "Sv", xlab = "time")
    plot(x = df$time, y = df$Ev, type = 'l', ylab = "Ev", xlab = "time")
    plot(x = df$time, y = df$Iv, type = 'l', ylab = "Iv", xlab = "time")
}

# 

Y0 = c(Nv = 20, Ev = 0, Iv = 0)
times = seq(0, 500, 0.001)
params = list(A = 20, D = -0.3, d0 = 0.05, nu.v = 1/14)
sol <- as.data.frame(ode(Y0, times, rhs_trial, params))
plot_subsets(sol, seq(180, 200, 0.001))

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot(x = times, y = sol$Nv, type = 'l', 
     ylab = "Number of mosquitoes")
plot(x = times, y = as.data.frame(ode(Y0, times, rhs, list(A = 20)))$Nv, type = 'l', 
     ylab = "", main = "A=20")
plot(x = times, y = as.data.frame(ode(Y0, times, rhs, list(A = 30)))$Nv, type = 'l', 
     ylab = "", main = "A=30")





rhs.erin = function(t, Y, params){
    ##########################################
    # ode, called into ode (deSolve) to analyze
    # inputs: t = timelist, Y = state variables, params = Parameters
    ##########################################
    with(as.list(c(Y, params)), {
        # Mosquito - Adults (note Iv equations has extra term from paper)
        r = leach()
        dNdt = gamma.e * Se - death.v * Sv - lambda.v * Sv
        out <- list(c(N = dNdt))
        return(out)
    })
}

