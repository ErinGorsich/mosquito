#########################################################
#########################################################
# A file to store the parameters and their definitions
# FOR TWO POPULATIONs OF MOSQUITOES
#########################################################
#########################################################
# Host per capita birth and death rates (15 years --> to days)
birth.h <- 1 / 15
death.h <- birth.h
Ho = 500

# additional host mortality due to RVF
alpha.h <- 0

# recovery rate in hosts = 1 / infectious period
gamma.h <- 365 / 4

# latency rate in hosts = 1 / intrinsic incubation period
u.h <- 365 / 3

# Vector population parameters for death rates
D = - 0.1 # if yearly time step
d0 = 13.5 # if yearly time step
# D = - 0.3 # if daily time step
# d0 = 0.05 # if daily time step

# Vector population parameters for population dynamics (yearly timestep)
A1 = 10
A2 = 10
A3 = 20

# Rate mosquitoes move from incubating to infectious = 1 / EIP 
v.v <- 365 * 3 / 14

# vertical transmission rate = proportion of eggs/larvae from infected Aedes???
phi.v <-  0.01

# Transmission rates for population 1
rho1.hv <- 0.12
rho1.vh <- 0.38

# Transmission rates for population 2
rho2.hv <- 2 * 0.12
rho2.vh <- 2* 0.38

# Transmission rates for population 3
rho3.hv <- 0.5 * 0.12
rho3.vh <- 0.5 * 0.38

# blood feeding rate (f in equations)
bite = 365 * 4  # a in equations

# may need to change name labels later
params.three <- params <- list(
    # host parameters
    death.h = death.h, birth.h = birth.h, alpha.h = alpha.h, gamma.h = gamma.h, 
    u.h = u.h, Ho = Ho,
    # vector parameters
    A1 = A1, A2 = A2, A3 = A3, D = D, d0 = d0, v.v = v.v, phi.v = phi.v,
    # transmission
    rho1.hv = rho1.hv, rho1.vh = rho1.vh, 
    rho2.hv = rho2.hv, rho2.vh = rho2.vh, 
    rho3.hv = rho3.hv, rho3.vh = rho3.vh, bite = bite)

