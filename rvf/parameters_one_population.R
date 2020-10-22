#########################################################
#########################################################
# A file to store the parameters and their definitions
# FOR ONE POPULATION OF MOSQUITOES
#########################################################
#########################################################
# Host per capita birth and death rates (15 years)
birth.h <- (1 / 15)
death.h <- birth.h
Ho = 500

# additional host mortality due to RVF
alpha.h <- 0

# recovery rate in hosts = 1 / infectious period
gamma.h <- 365 / 4 

# latency rate in hosts = 1 / intrinsic incubation period
u.h <- 365 /3

# Vector population parameters
A = 10
D = - 0.1 # if yearly time step
d0 = 13.5 # if yearly time step
# D = - 0.3 # if daily time step
# d0 = 0.05 # if daily time step


# Rate mosquitoes move from incubating to infectious = 1 / EIP 
v.v <- 365 * 3 / 14

# vertical transmission rate = proportion of eggs/larvae from infected Aedes???
phi.v <-  0.01

# Probability of transmission
rho.hv <- 0.12
rho.vh <- 0.38

# blood feeding rate (f in equations)
bite = 365 * 4  # a in equations

# may need to change name labels later
params.one <- params <- list(
    # host parameters
    death.h = death.h, birth.h = birth.h, gamma.h = gamma.h, 
    u.h = u.h, alpha.h = alpha.h, Ho = Ho,
    # vector parameters
    A = A, D = D, d0 = d0, v.v = v.v, phi.v = phi.v,
    # transmission
    rho.hv = rho.hv, rho.vh = rho.vh, bite = bite)

