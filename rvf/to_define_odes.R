####################################################
####################################################
# script defines RVF odes
####################################################
####################################################

####################################################
# 2) Defines basic mosquito-borne disease SIR model
# using bite rate in Keeling & Rohani (each mosquito bites at a constant rate)
####################################################
leach.growth = function(t, A, omega, theta){
    x = A * cos(omega * t + theta)
    return(x)
}
leach.death = function(t, D, d0, omega, theta){
    x = D * cos(omega * t + theta)
    d = d0 * exp(x)
    return(d)
}

rhs.one = function(t, Y, params){
    ##########################################
    # ode, called into ode (deSolve) to analyze
    # inputs: t = timelist, Y = state variables, params = Parameters
    ##########################################
    with(as.list(c(Y, params)), {
        # set mosquito forcing terms
        r = leach.growth(t, A = A, omega = 2*pi, theta = 0)
        death.v = leach.death(t, D = D, d0 = d0, omega = 2 * pi, theta = 0)
        temp <- Nv - Ev1 - Ev2 - Ev3 - Iv
        Sv <- ifelse(temp >= 0, temp, 0)
        Nh <- Sh + Eh + Ih + Rh
        lambda.h <- rho.hv * bite *  Iv / Nh
        lambda.v <- rho.vh * bite * Ih / Nh
        
        # Host
        dShdt <- birth.h * Ho - death.h * Sh - lambda.h * Sh
        dEhdt <- lambda.h * Sh - death.h * Eh - u.h * Eh
        dIhdt <- u.h * Eh - death.h * Ih - gamma.h * Ih - alpha.h * Ih
        dRhdt <- gamma.h * Ih - death.h * Rh
        
        # Mosquito - Adults
        dNvdt = r * Nv
        dEv1dt = lambda.v * Sv - death.v * Ev1 - v.v * Ev1
        dEv2dt =  v.v * Ev1 - death.v * Ev2 - v.v * Ev2
        dEv3dt =  v.v * Ev2 - death.v * Ev3 - v.v * Ev3
        dIvdt = v.v * Ev3 - death.v * Iv
        
        out <- list(c(Sh = dShdt, Eh = dEhdt, Ih = dIhdt, Rh = dRhdt, 
                      Nv = dNvdt , Ev1 = dEv1dt, Ev2 = dEv2dt, Ev3 = dEv3dt, 
                      Iv = dIvdt))
        return(out)
    })
}


rhs.two = function(t, Y, params){
    ##########################################
    # ode, called into ode (deSolve) to analyze
    # inputs: t = timelist, Y = state variables, params = Parameters
    ##########################################
    with(as.list(c(Y, params)), {
        # set mosquito forcing terms
        r1 = leach.growth(t, A = A1, omega = 2*pi, theta = 0)
        r2 = leach.growth(t, A = A2, omega = 2*pi, theta = 0)
        death.v = leach.death(t, D = D, d0 = d0, omega = 2 * pi, theta = 0)
        
        temp1 <- N1v - E1v1 - E1v2 - E1v3 - I1v
        temp2 <- N2v - E2v1 - E2v2 - E2v3 - I2v
        S1v <- ifelse(temp1 >= 0, temp1, 0)
        S2v <- ifelse(temp2 >= 0, temp2, 0)

        Nh <- Sh + Eh + Ih + Rh
        
        lambda.h <- (rho1.hv * I1v + rho2.hv * I2v) * bite / Nh
        lambda1.v <- rho1.vh * bite * Ih / Nh
        lambda2.v <- rho2.vh * bite * Ih / Nh
                
        # Host
        dShdt <- birth.h * Ho - death.h * Sh - lambda.h * Sh
        dEhdt <- lambda.h * Sh - death.h * Eh - u.h * Eh
        dIhdt <- u.h * Eh - death.h * Ih - gamma.h * Ih - alpha.h * Ih
        dRhdt <- gamma.h * Ih - death.h * Rh
        
        # Mosquito - Adults
        dN1vdt = r1 * N1v
        dE1v1dt = lambda1.v * S1v - death.v * E1v1 - v.v * E1v1
        dE1v2dt =  v.v * E1v1 - death.v * E1v2 - v.v * E1v2
        dE1v3dt =  v.v * E1v2 - death.v * E1v3 - v.v * E1v3
        dI1vdt = v.v * E1v3 - death.v * I1v
        
        # Mosquito - Adults 2
        dN2vdt = r2 * N2v
        dE2v1dt = lambda2.v * S2v - death.v * E2v1 - v.v * E2v1
        dE2v2dt =  v.v * E2v1 - death.v * E2v2 - v.v * E2v2
        dE2v3dt =  v.v * E2v2 - death.v * E2v3 - v.v * E2v3
        dI2vdt = v.v * E2v3 - death.v * I2v
        
        out <- list(c(Sh = dShdt, Eh = dEhdt, Ih = dIhdt, Rh = dRhdt, 
                      N1v = dN1vdt, E1v1 = dE1v1dt, E1v2 = dE1v2dt, 
                      E1v3 = dE1v3dt, I1v = dI1vdt, 
                      N2v = dN2vdt, E2v1 = dE2v1dt, E2v2 = dE2v2dt, 
                      E2v3 = dE2v3dt, I1v = dI2vdt))
        return(out)
    })
}


rhs.three = function(t, Y, params){
    ##########################################
    # ode, called into ode (deSolve) to analyze
    # inputs: t = timelist, Y = state variables, params = Parameters
    ##########################################
    with(as.list(c(Y, params)), {
        # set mosquito forcing terms
        r1 = leach.growth(t, A = A1, omega = 2*pi, theta = 0)
        r2 = leach.growth(t, A = A2, omega = 2*pi, theta = 0)
        r3 = leach.growth(t, A = A3, omega = 2*pi, theta = 0)
        death.v = leach.death(t, D = D, d0 = d0, omega = 2 * pi, theta = 0)
        
        temp1 <- N1v - E1v1 - E1v2 - E1v3 - I1v
        temp2 <- N2v - E2v1 - E2v2 - E2v3 - I2v
        temp3 <- N3v - E3v1 - E3v2 - E3v3 - I3v
        S1v <- ifelse(temp1 >= 0, temp1, 0)
        S2v <- ifelse(temp2 >= 0, temp2, 0)
        S3v <- ifelse(temp3 >= 0, temp3, 0)
        
        Nh <- Sh + Eh + Ih + Rh

        lambda.h <- (rho1.hv * I1v + rho2.hv * I2v + rho3.hv * I3v) * bite / Nh
        lambda1.v <- rho1.vh * bite * Ih / Nh
        lambda2.v <- rho2.vh * bite * Ih / Nh
        lambda3.v <- rho3.vh * bite * Ih / Nh
                
        # Host
        dShdt <- birth.h * Ho - death.h * Sh - lambda.h * Sh
        dEhdt <- lambda.h * Sh - death.h * Eh - u.h * Eh
        dIhdt <- u.h * Eh - death.h * Ih - gamma.h * Ih - alpha.h * Ih
        dRhdt <- gamma.h * Ih - death.h * Rh
        
        # Mosquito - Adults
        dN1vdt = r1 * N1v
        dE1v1dt = lambda1.v * S1v - death.v * E1v1 - v.v * E1v1
        dE1v2dt =  v.v * E1v1 - death.v * E1v2 - v.v * E1v2
        dE1v3dt =  v.v * E1v2 - death.v * E1v3 - v.v * E1v3
        dI1vdt = v.v * E1v3 - death.v * I1v
        
        # Mosquito - Adults 2
        dN2vdt = r2 * N2v
        dE2v1dt = lambda2.v * S2v - death.v * E2v1 - v.v * E2v1
        dE2v2dt =  v.v * E2v1 - death.v * E2v2 - v.v * E2v2
        dE2v3dt =  v.v * E2v2 - death.v * E2v3 - v.v * E2v3
        dI2vdt = v.v * E2v3 - death.v * I2v
        
        # Mosquito - Adults 3
        dN3vdt = r3 * N3v
        dE3v1dt = lambda3.v * S3v - death.v * E3v1 - v.v * E3v1
        dE3v2dt =  v.v * E3v1 - death.v * E3v2 - v.v * E3v2
        dE3v3dt =  v.v * E3v2 - death.v * E3v3 - v.v * E3v3
        dI3vdt = v.v * E3v3 - death.v * I3v
        
        out <- list(c(Sh = dShdt, Eh = dEhdt, Ih = dIhdt, Rh = dRhdt, 
                      N1v = dN1vdt, E1v1 = dE1v1dt, E1v2 = dE1v2dt, 
                      E1v3 = dE1v3dt, I1v = dI1vdt, 
                      N2v = dN2vdt, E2v1 = dE2v1dt, E2v2 = dE2v2dt, 
                      E2v3 = dE2v3dt, I1v = dI2vdt, 
                      N3v = dN3vdt, E3v1 = dE3v1dt, E3v2 = dE3v2dt, 
                      E3v3 = dE3v3dt, I3v = dI3vdt))
        return(out)
    })
}
