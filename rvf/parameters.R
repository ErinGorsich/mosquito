# A file to store the parameters and their definitions
# NOTE - need parameters in days not years
# put them in a list at the end

# Host per capita birth and death rates (15 years --> to days)
birth.h <- (1 / 15) / 365
death.h <- birth.h

# additional host mortality due to RVF
delta.h <- 0.01

# recovery rate = 1 / infectious period
gamma.h <- 1 / 4

# Vector per capita birth and death rates
birth.v.wet <- 1 / 20
birth.v.dry <- 1 / 14
death.v.wet <- birth.v.wet
death.v.dry <- birth.v.dry

# Mosquito hatch rate = 1 / duration of egg & larval stage
gamma.e.wet <- 1 / 10
gamma.e.dry <- 1 / 1000

# Rate mosquitoes move from incubating to infectious = 1 / EIP 
nu.v <- 1/14

# vertical transmission rate = porportion of eggs/larvae from infected  Aedes mosquitoes.
# No one knows this one :) 
phi.v <-  0.01

# Transmission rates
beta.hv <- 0.12
beta.vh <- 0.38

# Mixing rates
sigma.h <-  19
sigma.v.wet <- 0.33
sigma.v.dry <- 0.25
Ho <- 1000
Vo <- 40*Ho # female mosquitoes

# may need to change name labels later
params.wet <- list(
    # host parameters
    death.h = death.h, birth.h = birth.h, delta.h = delta.h, gamma.h = gamma.h, 
    # vector parameters
    death.v = death.v.wet, birth.v = birth.v.wet, gamma.e = gamma.e.wet, 
    nu.v = nu.v, phi.v = phi.v,
    # mixing
    beta.hv = beta.hv, beta.vh = beta.vh, sigma.h = sigma.h, 
    sigma.v = sigma.v.wet, Ho = Ho, Vo = Vo)

params.dry <- list(
    # host parameters
    death.h = death.h, birth.h = birth.h, delta.h = delta.h, gamma.h = gamma.h, 
    # vector parameters
    death.v = death.v.dry, birth.v = birth.v.dry, gamma.e = gamma.e.dry, 
    nu.v = nu.v, phi.v = phi.v,
    # mixing
    beta.hv = beta.hv, beta.vh = beta.vh, sigma.h = sigma.h, 
    sigma.v = sigma.v.dry,  Ho = Ho, Vo = Vo)
