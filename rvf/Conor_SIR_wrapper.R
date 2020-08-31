####################################################
####################################################
# For Conor: RVF mosquito-borne disease model
####################################################
####################################################
library(deSolve)
library(ggplot2)
setwd("~/GitHub/mosquito/rvf") # change this working directory to yours
rm(list = ls())

# Load odes & function to run model
source('to_define_odes.R', chdir = TRUE)
source('to_define_Conor_function.R', chdir = TRUE)
source('to_make_plots.R', chdir = TRUE)

# Notes: run_one is a function with the following arguments
# 1. number.mosquitoes = 1, 2, or 3. Defines which model you want to run, 
# the with 1, 2, or 3 species (can see these three models in to_define_odes.R)
# 2. parameter.file = a location to source parameter files.  The default will 
# bring up a window so you can choose this manually. This means you do not have
# to specify the file location when running the model. I recommend you name these
# intelligently. Each file must create a list called params.
# 3. times = a time vector specifying the units you would like solutions for. 
# The default is to run the model for 5 years, so I recommend you ignore this one.
# 4. output.file = TRUE/FALSE depending on if you want an output file. 
# The default is TRUE; set to FALSE if you don't want to save a file, ignore otherwise.
# 5. output.name = what do you want to name the file?

# Example of how it works...
sol <- run_one(number.mosquitoes = 1, output.name = "test")
plot_hosts(sol)
plot_vectors(sol)
