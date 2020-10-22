####################################################
####################################################
####################################################
# RVF mosquito-borne disease model
####################################################
####################################################
####################################################
library(deSolve)
library(ggplot2)
setwd("~/GitHub/mosquito/rvf")
rm(list = ls())

# Load parameters
source('parameters_one_population.R', chdir = TRUE)
source('parameters_two_populations.R', chdir = TRUE)
source('parameters_three_populations.R', chdir = TRUE)

# Load odes
source('to_define_odes.R', chdir = TRUE)

# convenience functions for plots (functions = plot_hosts(), plot_vectors())
source('to_make_plots.R', chdir = TRUE)

####################################################
# Trial 1 - Make sure if no disease initially, none emerges; host populations
####################################################
# One mosquito
Y0 <- c(Sh = 500, Eh = 0, Ih = 0, Rh = 0, 
    Nv = 20 , Ev1 = 0, Ev2 = 0, Ev3 = 0, Iv = 0)
times <- seq(0, 5, 0.002)
sol <- as.data.frame(ode(Y0, times, rhs.one, params.one))
plot_hosts(sol, times)
plot_vectors(sol, times)
# plot_vectors(sol, seq(1, 100, 0.01)) # stable from beginning without disease

# Two mosquitoes
Y0 <- c(Sh = 500, Eh = 0, Ih = 0, Rh = 0, 
       N1v = 20 , E1v1 = 0, E1v2 = 0, E1v3 = 0, I1v = 0, 
       N2v = 20 , E2v1 = 0, E2v2 = 0, E2v3 = 0, I2v = 0)
times <- seq(0, 5, 0.002)
sol <- as.data.frame(ode(Y0, times, rhs.two, params.two))
plot_hosts(sol, times)
plot_vectors(sol, times)

# Three mosquitoes
Y0 <- c(Sh = 500, Eh = 0, Ih = 0, Rh = 0, 
       N1v = 20 , E1v1 = 0, E1v2 = 0, E1v3 = 0, I1v = 0, 
       N2v = 50 , E2v1 = 0, E2v2 = 0, E2v3 = 0, I2v = 0, 
       N3v = 20 , E3v1 = 0, E3v2 = 0, E3v3 = 0, I3v = 0)
times <- seq(0, 5, 0.002)
sol <- as.data.frame(ode(Y0, times, rhs.three, params.three))
plot_hosts(sol, times)
plot_vectors(sol, times)


####################################################
# Trial 2 - Add disease
####################################################
# One mosquito
Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
        Nv = 20 , Ev1 = 0, Ev2 = 0, Ev3 = 0, Iv = 0)
times <- seq(0, 5, 0.0002)
sol.dz <- as.data.frame(ode(Y0, times, rhs.one, params.one, method = "ode45"))
plot_hosts(sol.dz, times)
plot_vectors(sol.dz, times)

# Two mosquitoes
Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
        N1v = 20 , E1v1 = 0, E1v2 = 0, E1v3 = 0, I1v = 0, 
        N2v = 20 , E2v1 = 0, E2v2 = 0, E2v3 = 0, I2v = 0)
times <- seq(0, 5, 0.002)
sol <- as.data.frame(ode(Y0, times, rhs.two, params.two))
plot_hosts(sol, times)
plot_vectors(sol, times)

# Three mosquitoes
Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
        N1v = 20 , E1v1 = 0, E1v2 = 0, E1v3 = 0, I1v = 0, 
        N2v = 50 , E2v1 = 0, E2v2 = 0, E2v3 = 0, I2v = 0, 
        N3v = 20 , E3v1 = 0, E3v2 = 0, E3v3 = 0, I3v = 0)
times <- seq(0, 5, 0.002)
sol <- as.data.frame(ode(Y0, times, rhs.three, params.three))
plot_hosts(sol, times)
plot_vectors(sol, times)

####################################################
# Trial 3 - Increase rho  until RVF persists at E+I > 1
####################################################
bite.rate <- seq(0.5, 10, 0.5)
newdf = data.frame(bite.rate = c(), Ih = c(), time = c())
for (b in bite.rate){
    params.temp <- params
    params.temp['bite'][[1]] <- b
    Y0 = c(Sh = 490, Eh = 5, Ih = 5, Rh = 0, 
           Nv = 100 , Ev = 0, Iv = 0)
    times = seq(0, 400, 0.001)
    sol <- as.data.frame(ode(Y0, times, rhs, params.temp))
    tempdf <- data.frame(bite.rate = rep(b, length(sol[,1])), 
                         Ih = sol$Ih, 
                         time = sol$time)
    newdf <- rbind(newdf, tempdf)
    print(b)
}
plot(x = newdf$time[newdf$bite.rate == 0.5], y = newdf$Ih[newdf$bite.rate == 0.5], 
     type = 'l', ylab = "Ih", xlab = "time", ylim = c(0, 20))
lines(x = newdf$time[newdf$bite.rate == 1], y = newdf$Ih[newdf$bite.rate == 1], 
      type = 'l', col = "light blue")
lines(x = newdf$time[newdf$bite.rate == 2], y = newdf$Ih[newdf$bite.rate == 2], 
      type = 'l', col = "blue")
lines(x = newdf$time[newdf$bite.rate == 3], y = newdf$Ih[newdf$bite.rate == 3], 
      type = 'l', col = "dark blue")

ggplot(data = subset(newdf, bite.rate %in% c(0.5, 1, 1.5, 2, 2.5, 3)), 
       aes(x = time, y = Ih, colour = as.factor(bite.rate))) + geom_line()

ggplot(data = subset(newdf, bite.rate %in% c(0.5, 1, 1.5, 2, 2.5, 3)), 
       aes(x = time, y = Sh, colour = as.factor(bite.rate))) + geom_line()

# plot at bite rate = 2 for a long time
times = seq(0, 1000, 0.001)
params.temp <- params
params.temp['bite'][[1]] <- 2
sol <- as.data.frame(ode(Y0, times, rhs, params.temp))
plot(y = sol$Ih, x = sol$time, type = "l")
