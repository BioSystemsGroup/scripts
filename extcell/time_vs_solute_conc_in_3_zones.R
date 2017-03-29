# Name: Yanli Xu
# Date: Jan 11th, 2017
# Note: This script is used for comparing time vs solute_concentration in three zones
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("time_vs_solute_conc_in_3_zones.R"), the script will start working 
# Input file: XXXX-XX-XX-XXXX-XXXX_profile_mean_per_node.csv
# plot all the time vs profile_solute ("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N" )
# This R script plots every solute on one page. Every concentration of a solute has a plot. Every plot has the lines superimposed on the surface of the light color dots.

# Example to use this script
# > source("time_vs_solute_conc_in_3_zones.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data/bolus_in_2016_11/161108_161111
# Enter 9 concentration values separated by comma: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
# You have  9  plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input the cycleLimit of these experiments: 5000

mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_solute_conc <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "profile_mean_per_node.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("profile_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  # Ask the user how to display the plot
  mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: ")) 
  
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  exp <- eval(parse(text = exp_names[length(files)]))
  solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
  
  circumference_type_A_SS = 30
  circumference_type_B_SS = 5
  length_type_A_SS = 8
  length_type_B_SS = 33.33
  percent_type_A_SS = 0.9
  percent_type_B_SS = 0.1
  HEP_dencity = 0.9
  Num_Zone_0_SS = 45
  Num_Zone_1_SS = 20
  Num_Zone_2_SS = 3
  
  conc_type_A_SS = (circumference_type_A_SS^2)/3.14*length_type_A_SS/(circumference_type_A_SS*length_type_A_SS*HEP_dencity)
  conc_type_B_SS = (circumference_type_B_SS^2)/3.14*length_type_B_SS/(circumference_type_A_SS*length_type_A_SS*HEP_dencity)
  
  conc_z0_factor = conc_type_A_SS * Num_Zone_0_SS * percent_type_A_SS + conc_type_B_SS * Num_Zone_0_SS * percent_type_B_SS
  conc_z1_factor = conc_type_A_SS * Num_Zone_1_SS * percent_type_A_SS + conc_type_B_SS * Num_Zone_1_SS * percent_type_B_SS
  conc_z2_factor = conc_type_A_SS * Num_Zone_2_SS * percent_type_A_SS + conc_type_B_SS * Num_Zone_2_SS * percent_type_B_SS
  
  for(solute_num in 2:length(solute_names))
  {
    par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
    # get the max value in every experiment 
    max_for_every_exp <- c()
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      max_for_every_exp[i] = max(max(mav(exp[, solute_num])*conc_z0_factor, na.rm = T), max(mav(exp[, solute_num])*conc_z1_factor, na.rm = T), max(mav(exp[, solute_num])*conc_z2_factor, na.rm = T))
    }
    y_max = max(max_for_every_exp)
    y_min = 0
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }
    
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      title = paste("Time_vs", solute_names[solute_num], "conc", conc[i], sep = "_")
      plot(mav(exp[, solute_num]) * conc_z0_factor ~ exp$Time, type = "l", main = title, xlab = "Time", ylab = solute_names[solute_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
      lines(mav(exp[, solute_num]) * conc_z1_factor ~ exp$Time, col = "green")
      lines(mav(exp[, solute_num]) * conc_z2_factor ~ exp$Time, col = "red")
      if(i == 1)
      {
        legend(run_time*0.6, y_max, c("Z0","Z1", "Z2"), col = c("black", "green", "red"), lty=c(1,1), lwd=c(2.5,2.5))
      }
      
    }
  }
  
}
time_vs_solute_conc()