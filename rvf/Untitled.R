####################################################
####################################################
# For Conor: RVF mosquito-borne disease model
####################################################
####################################################
library(deSolve)
library(ggplot2)
setwd("~/GitHub/mosquito/rvf") # change this working directory to yours

rm(list = ls())

# Load odes
source('to_define_odes.R', chdir = TRUE)

# Function to run model
run_one = function(number.mosquitoes, parameter.file, times = seq(0, 5, 0.0002), 
                   output.file = TRUE, output.name = "rvf"){
    number.mosquitoes <- as.numeric(number.mosquitoes)
    if (number.mosquitoes == 1) {
        print("running one-mosquito model")
        Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
                Nv = 20 , Ev1 = 0, Ev2 = 0, Ev3 = 0, Iv = 0)
        if (length(parameter.file) == 14){
            sol <- as.data.frame(ode(Y0, times, rhs.one, parameter.file, 
                                        method = "ode45"))
        } else{
            print("check parameter file, one-mosquito model needs 14 parameters")
        }
    } else if (number.mosquitoes == 2) {
        print("running two-mosquito model")
        Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
                N1v = 20 , E1v1 = 0, E1v2 = 0, E1v3 = 0, I1v = 0, 
                N2v = 20 , E2v1 = 0, E2v2 = 0, E2v3 = 0, I2v = 0)
        sol <- as.data.frame(ode(Y0, times, rhs.two, params.two))
        
    } else if (number.mosquitoes == 3) {
        print("running three-mosquito model")
        
    } else{
        print("error - number of mosquitoes not 1, 2, or 3")
    }
}


# Example of how it works...
source(file.choose())


