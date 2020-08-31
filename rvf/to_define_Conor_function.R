# Function to run model
run_one = function(number.mosquitoes, parameter.file = file.choose(), 
                   times = seq(0, 5, 0.0002), 
                   output.file = TRUE, output.name = "rvf"){
    number.mosquitoes <- as.numeric(number.mosquitoes)
    source(parameter.file)
    if (number.mosquitoes == 1) {
        print("running one-mosquito model")
        Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
                Nv = 20 , Ev1 = 0, Ev2 = 0, Ev3 = 0, Iv = 0)
        if (length(params) == 14){
            sol <- as.data.frame(ode(Y0, times, rhs.one, params, 
                                     method = "ode45"))
        } else{
            print("error: check parameter file, one-mosquito model needs 14 parameters")
        }
    } else if (number.mosquitoes == 2) {
        print("running two-mosquito model")
        Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
                N1v = 20 , E1v1 = 0, E1v2 = 0, E1v3 = 0, I1v = 0, 
                N2v = 20 , E2v1 = 0, E2v2 = 0, E2v3 = 0, I2v = 0)
        if (length(params) == 17){
            sol <- as.data.frame(ode(Y0, times, rhs.two, params))
        } else{
            print("error: check parameter file, two-mosquito model needs 17 parameters")
        }
    } else if (number.mosquitoes == 3) {
        print("running three-mosquito model")
        Y0 <- c(Sh = 500, Eh = 5, Ih = 5, Rh = 0, 
                N1v = 20 , E1v1 = 0, E1v2 = 0, E1v3 = 0, I1v = 0, 
                N2v = 50 , E2v1 = 0, E2v2 = 0, E2v3 = 0, I2v = 0, 
                N3v = 20 , E3v1 = 0, E3v2 = 0, E3v3 = 0, I3v = 0)
        if (length(params) == 20){
            sol <- as.data.frame(ode(Y0, times, rhs.three, params))
        } else{
            print("error: check parameter file, three-mosquito model needs 20 parameters")
        }
    } else{
        print("error - number of mosquitoes not 1, 2, or 3")
    }
    if(output.file){
        write.csv(sol, paste(output.name, ".csv", sep = ""))
    }
    return(sol)
}