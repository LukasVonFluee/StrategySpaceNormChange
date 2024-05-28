###################################################################################
# Title: Analysis of model for paper: 
# "Redefine the threshold in threshold models to study heterogeneity in learning and its effects on social norm change"
# Authors: Lukas von Flüe
###################################################################################

intervention1line <- 50

# intervention2line <- 200

###################################################################################
# The first part of this script produces several plots per parameter combination
# The second part produces combined plots
###################################################################################

# Part 1: Separate Plots

###################################################################################
# Load data with file names corresponding to parameter combinations
###################################################################################

base_dir <- getwd()

# List all subdirectories
subdirectories <- list.dirs(path = base_dir, recursive = FALSE)

# Loop through each subdirectory and load and process the .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Initialize a list to store the results for the current subdirectory
  subdirectory_results <- list()
  
  # Load each .RData file
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Extract just the parameter combination name from the full path
    param_name <- basename(dirname(rdata_file))
    
    # Store result in subdirectory_results list
    subdirectory_results[[param_name]] <- result
  }
  
  ###################################################################################
  
  # Plotting parameters:
  
  windowWidth <- 10
  windowHeight <- 11
  
  cexEquil <- 2
  lwdEquil <- 2
  
  cexNum <- 1.5
  cexWord <- 2
  
  ###################################################################################
  ###################################################################################
  
  # Plots
  
  ###################################################################################
  ###################################################################################
  
  for (param_name in names(subdirectory_results)) {
    current_output <- subdirectory_results[[param_name]]
    
    
    ###################################################################################
    # Plotting frequencies of coordination and miscoordination
    ###################################################################################
    
    # Calculate fractions
    
    current_output$summary_results$fract_coord_alt <- current_output$summary_results$freq_coord_alt/(current_output$N/2)
    current_output$summary_results$fract_coord_alt_low_ci <- current_output$summary_results$low_ci_freq_coord_alt/(current_output$N/2)
    current_output$summary_results$fract_coord_alt_high_ci <- current_output$summary_results$high_ci_freq_coord_alt/(current_output$N/2)
    
    current_output$summary_results$fract_miscoordination <- current_output$summary_results$miscoordination/(current_output$N/2)
    current_output$summary_results$fract_miscoordination_low_ci <- current_output$summary_results$low_ci_miscoordination/(current_output$N/2)
    current_output$summary_results$fract_miscoordination_high_ci <- current_output$summary_results$high_ci_miscoordination/(current_output$N/2)
    
    # Create the filename for this parameter combination
    filename <- file.path(subdir, "freq_coordination.pdf")
    
    pdf(width = windowWidth, height = windowHeight, file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create the initial line plot with coordination on Alt
    plot(1:current_output$t_max, current_output$summary_results$fract_coord_alt, type = "l",  lwd = 3,
         xlab = "Period", ylab = "Fraction",
         # main = "Fraction of (mis)coordination",
         col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_coord_alt_low_ci, col = "darkgreen", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_coord_alt_high_ci, col = "darkgreen", lty = 2)
    
    # # # Add coordination on SQ
    # lines(1:current_output$t_max, current_output$summary_results$freq_coord_sq, col = "blue", lty = 1, lwd = 3)
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_sq, col = "blue", lty = 2)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_sq, col = "blue", lty = 2)
    
    # # Add miscoordination
    lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination, col = "red", lty = 1, lwd = 3)
    
    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination_low_ci, col = "red", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination_high_ci, col = "red", lty = 2)
    
    # # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt", "Miscoordination", "95% CI Miscoord."), 
    #        col = c("blue","blue", "darkgreen", "darkgreen", "red", "red"), lty = c(1, 2, 1, 2, 1, 2))
    #  
    # # Close the graphics device and save the plot as a PDF file
    dev.off()
    
    ###################################################################################
    # Plotting choice fractions
    ###################################################################################
    current_output$summary_results$fract_alt <- current_output$summary_results$freq_alt / current_output$N
    current_output$summary_results$fract_sq <- current_output$summary_results$freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_sq <- current_output$summary_results$low_ci_freq_sq / current_output$N
    current_output$summary_results$fract_high_ci_freq_sq <- current_output$summary_results$high_ci_freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_alt <- current_output$summary_results$low_ci_freq_alt / current_output$N
    current_output$summary_results$fract_high_ci_freq_alt <- current_output$summary_results$high_ci_freq_alt / current_output$N
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "fract_choices.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create the initial line plot with Alt choices
    plot(1:current_output$t_max, current_output$summary_results$fract_alt, type = "l", lwd = 3,
         xlab = "Period", ylab = "Fraction",
         # main = "Fraction of Alt choices",
         col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
    
    # # Add SQ choices
    # lines(1:current_output$t_max, current_output$summary_results$fract_sq, col = "blue", lty = 1, lwd = 3)
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_sq, col = "blue", lty = 2)
    # lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_sq, col = "blue", lty = 2)
    
    
    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI", "Alt", "95% CI"), 
    #        col = c("blue", "blue", "darkgreen", "darkgreen"), lty = c(1, 2, 1, 2))
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting average payoffs
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "average_payoff.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create plot with average payoffs
    plot(1:current_output$t_max, current_output$summary_results$avg_payoff, type = "l", lwd = 3,
         xlab = "Period", ylab = "Average payoff",
         main = "Average payoffs",
         col = "blue", ylim = c(0, (current_output$d_mean+current_output$sigma)),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_avg_payoff, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_avg_payoff, col = "blue", lty = 2)
    
    # # If we only show overall avg payoffs:
    # legend("topright", legend = c("Average payoff", "95% CI"), col = c("blue", "blue"), lty = c(1, 2))
    # 
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting Gini Coefficients
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "gini_coefficient.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create plot with gini coefficients
    plot(1:current_output$t_max, current_output$summary_results$gini_coefficient, type = "l", lwd = 3,
         xlab = "Period", ylab = "Gini Coefficient",
         # main = "Gini Coefficient",
         col = "blue", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
    
    # # If we only show overall avg payoffs:
    # legend("topright", legend = c("Gini Coefficient", "95% CI"), col = c("blue", "blue"), lty = c(1, 2))
    # 
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
  }
}


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# Part 2: Combined plots

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# List all subdirectories
subdirectories <- list.dirs(path = base_dir, recursive = FALSE)

# Initialize a list to store the loaded .RData files
loaded_data_list <- list()

# Loop through each subdirectory and load the corresponding .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Extract just the parameter combination name from the subdirectory
  param_name <- basename(subdir)
  
  # Load each .RData file
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Store the loaded data in the list with the subdirectory name as the list element name
    loaded_data_list[[param_name]] <- result
  }
}

###################################################################################

# Gather all treatments in separate lists
conservative_list <- list()
herd_list <- list()
success_list <- list()
conditional_list <- list()
majoritarian_list <- list()
no_intervention_list <- list()

for (param_name in names(loaded_data_list)) {
  if (grepl("target0", param_name)) {
    no_intervention_list[[param_name]] <- loaded_data_list[[param_name]]
  } else if (grepl("target1", param_name)) {
    conservative_list[[param_name]] <- loaded_data_list[[param_name]]
  } else if (grepl("target2", param_name)) {
    herd_list[[param_name]] <- loaded_data_list[[param_name]]
  } else if (grepl("target3", param_name)) {
    success_list[[param_name]] <- loaded_data_list[[param_name]]
  } else if (grepl("target4", param_name)) {
    conditional_list[[param_name]] <- loaded_data_list[[param_name]]
  } else if (grepl("target5", param_name)) {
    majoritarian_list[[param_name]] <- loaded_data_list[[param_name]]
  }
}

###################################################################################
###################################################################################

if (!dir.exists("combined_plots")) {
  dir.create("combined_plots")
}
setwd("combined_plots")


for (i in 1:length(conservative_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 2
  cexWord <- 2.25
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("alpha", conservative_list[[i]]$alpha, 
                   "_phi", conservative_list[[i]]$phi, 
                   "_N", conservative_list[[i]]$N,
                   "_n", conservative_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # No intervention
  
  no_intervention_list[[i]]$summary_results$fract_alt <- no_intervention_list[[i]]$summary_results$freq_alt / no_intervention_list[[i]]$N
  no_intervention_list[[i]]$summary_results$fract_sq <- no_intervention_list[[i]]$summary_results$freq_sq / no_intervention_list[[i]]$N
  no_intervention_list[[i]]$summary_results$fract_low_ci_freq_sq <- no_intervention_list[[i]]$summary_results$low_ci_freq_sq / no_intervention_list[[i]]$N
  no_intervention_list[[i]]$summary_results$fract_high_ci_freq_sq <- no_intervention_list[[i]]$summary_results$high_ci_freq_sq / no_intervention_list[[i]]$N
  no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt <- no_intervention_list[[i]]$summary_results$low_ci_freq_alt / no_intervention_list[[i]]$N
  no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt <- no_intervention_list[[i]]$summary_results$high_ci_freq_alt / no_intervention_list[[i]]$N
  
  # Conservative
  
  conservative_list[[i]]$summary_results$fract_alt <- conservative_list[[i]]$summary_results$freq_alt / conservative_list[[i]]$N
  conservative_list[[i]]$summary_results$fract_sq <- conservative_list[[i]]$summary_results$freq_sq / conservative_list[[i]]$N
  conservative_list[[i]]$summary_results$fract_low_ci_freq_sq <- conservative_list[[i]]$summary_results$low_ci_freq_sq / conservative_list[[i]]$N
  conservative_list[[i]]$summary_results$fract_high_ci_freq_sq <- conservative_list[[i]]$summary_results$high_ci_freq_sq / conservative_list[[i]]$N
  conservative_list[[i]]$summary_results$fract_low_ci_freq_alt <- conservative_list[[i]]$summary_results$low_ci_freq_alt / conservative_list[[i]]$N
  conservative_list[[i]]$summary_results$fract_high_ci_freq_alt <- conservative_list[[i]]$summary_results$high_ci_freq_alt / conservative_list[[i]]$N
  
  # Herd followers:
  
  herd_list[[i]]$summary_results$fract_alt <- herd_list[[i]]$summary_results$freq_alt / herd_list[[i]]$N
  herd_list[[i]]$summary_results$fract_sq <- herd_list[[i]]$summary_results$freq_sq / herd_list[[i]]$N
  herd_list[[i]]$summary_results$fract_low_ci_freq_sq <- herd_list[[i]]$summary_results$low_ci_freq_sq / herd_list[[i]]$N
  herd_list[[i]]$summary_results$fract_high_ci_freq_sq <- herd_list[[i]]$summary_results$high_ci_freq_sq / herd_list[[i]]$N
  herd_list[[i]]$summary_results$fract_low_ci_freq_alt <- herd_list[[i]]$summary_results$low_ci_freq_alt / herd_list[[i]]$N
  herd_list[[i]]$summary_results$fract_high_ci_freq_alt <- herd_list[[i]]$summary_results$high_ci_freq_alt / herd_list[[i]]$N
  
  # Success
  
  success_list[[i]]$summary_results$fract_alt <- success_list[[i]]$summary_results$freq_alt / success_list[[i]]$N
  success_list[[i]]$summary_results$fract_sq <- success_list[[i]]$summary_results$freq_sq / success_list[[i]]$N
  success_list[[i]]$summary_results$fract_low_ci_freq_sq <- success_list[[i]]$summary_results$low_ci_freq_sq / success_list[[i]]$N
  success_list[[i]]$summary_results$fract_high_ci_freq_sq <- success_list[[i]]$summary_results$high_ci_freq_sq / success_list[[i]]$N
  success_list[[i]]$summary_results$fract_low_ci_freq_alt <- success_list[[i]]$summary_results$low_ci_freq_alt / success_list[[i]]$N
  success_list[[i]]$summary_results$fract_high_ci_freq_alt <- success_list[[i]]$summary_results$high_ci_freq_alt / success_list[[i]]$N
  
  # Conditional
  
  conditional_list[[i]]$summary_results$fract_alt <- conditional_list[[i]]$summary_results$freq_alt / conditional_list[[i]]$N
  conditional_list[[i]]$summary_results$fract_sq <- conditional_list[[i]]$summary_results$freq_sq / conditional_list[[i]]$N
  conditional_list[[i]]$summary_results$fract_low_ci_freq_sq <- conditional_list[[i]]$summary_results$low_ci_freq_sq / conditional_list[[i]]$N
  conditional_list[[i]]$summary_results$fract_high_ci_freq_sq <- conditional_list[[i]]$summary_results$high_ci_freq_sq / conditional_list[[i]]$N
  conditional_list[[i]]$summary_results$fract_low_ci_freq_alt <- conditional_list[[i]]$summary_results$low_ci_freq_alt / conditional_list[[i]]$N
  conditional_list[[i]]$summary_results$fract_high_ci_freq_alt <- conditional_list[[i]]$summary_results$high_ci_freq_alt / conditional_list[[i]]$N
  
  # Majoritarian
  
  majoritarian_list[[i]]$summary_results$fract_alt <- majoritarian_list[[i]]$summary_results$freq_alt / majoritarian_list[[i]]$N
  majoritarian_list[[i]]$summary_results$fract_sq <- majoritarian_list[[i]]$summary_results$freq_sq / majoritarian_list[[i]]$N
  majoritarian_list[[i]]$summary_results$fract_low_ci_freq_sq <- majoritarian_list[[i]]$summary_results$low_ci_freq_sq / majoritarian_list[[i]]$N
  majoritarian_list[[i]]$summary_results$fract_high_ci_freq_sq <- majoritarian_list[[i]]$summary_results$high_ci_freq_sq / majoritarian_list[[i]]$N
  majoritarian_list[[i]]$summary_results$fract_low_ci_freq_alt <- majoritarian_list[[i]]$summary_results$low_ci_freq_alt / majoritarian_list[[i]]$N
  majoritarian_list[[i]]$summary_results$fract_high_ci_freq_alt <- majoritarian_list[[i]]$summary_results$high_ci_freq_alt / majoritarian_list[[i]]$N
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices conservative
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  
  ###################################################################################
  
  # Add confidence intervals
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
  
  # Conservatives
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_alt, col = "darkgreen", lty = 1, lwd = 3)
  
  # Add confidence intervals
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
  
  # Herd followers
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_alt, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "red", lty = 2)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "red", lty = 2)
  
  # Success oriented
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  
  # Conditional
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_alt, col = "orange", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "orange", lty = 2)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "orange", lty = 2)
  
  # Majoritarian
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_alt, col = "violet", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "violet", lty = 2)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "violet", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  # No intervention
  
  no_intervention_list[[i]]$summary_results$fract_miscoord <- no_intervention_list[[i]]$summary_results$miscoordination / (no_intervention_list[[i]]$N/2)
  no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci <- no_intervention_list[[i]]$summary_results$low_ci_miscoordination / (no_intervention_list[[i]]$N/2)
  no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci <- no_intervention_list[[i]]$summary_results$high_ci_miscoordination / (no_intervention_list[[i]]$N/2)
  
  # Conservative
  
  conservative_list[[i]]$summary_results$fract_miscoord <- conservative_list[[i]]$summary_results$miscoordination / (conservative_list[[i]]$N/2)
  conservative_list[[i]]$summary_results$fract_miscoord_low_ci <- conservative_list[[i]]$summary_results$low_ci_miscoordination / (conservative_list[[i]]$N/2)
  conservative_list[[i]]$summary_results$fract_miscoord_high_ci <- conservative_list[[i]]$summary_results$high_ci_miscoordination / (conservative_list[[i]]$N/2)
  
  # Herd followers:
  
  herd_list[[i]]$summary_results$fract_miscoord <- herd_list[[i]]$summary_results$miscoordination / (herd_list[[i]]$N/2)
  herd_list[[i]]$summary_results$fract_miscoord_low_ci <- herd_list[[i]]$summary_results$low_ci_miscoordination / (herd_list[[i]]$N/2)
  herd_list[[i]]$summary_results$fract_miscoord_high_ci <- herd_list[[i]]$summary_results$high_ci_miscoordination / (herd_list[[i]]$N/2)
  
  # Success
  
  success_list[[i]]$summary_results$fract_miscoord <- success_list[[i]]$summary_results$miscoordination / (success_list[[i]]$N/2)
  success_list[[i]]$summary_results$fract_miscoord_low_ci <- success_list[[i]]$summary_results$low_ci_miscoordination / (success_list[[i]]$N/2)
  success_list[[i]]$summary_results$fract_miscoord_high_ci <- success_list[[i]]$summary_results$high_ci_miscoordination / (success_list[[i]]$N/2)
  
  # Conditional
  
  conditional_list[[i]]$summary_results$fract_miscoord <- conditional_list[[i]]$summary_results$miscoordination / (conditional_list[[i]]$N/2)
  conditional_list[[i]]$summary_results$fract_miscoord_low_ci <- conditional_list[[i]]$summary_results$low_ci_miscoordination / (conditional_list[[i]]$N/2)
  conditional_list[[i]]$summary_results$fract_miscoord_high_ci <- conditional_list[[i]]$summary_results$high_ci_miscoordination / (conditional_list[[i]]$N/2)
  
  # Majoritarian
  
  majoritarian_list[[i]]$summary_results$fract_miscoord <- majoritarian_list[[i]]$summary_results$miscoordination / (majoritarian_list[[i]]$N/2)
  majoritarian_list[[i]]$summary_results$fract_miscoord_low_ci <- majoritarian_list[[i]]$summary_results$low_ci_miscoordination / (majoritarian_list[[i]]$N/2)
  majoritarian_list[[i]]$summary_results$fract_miscoord_high_ci <- majoritarian_list[[i]]$summary_results$high_ci_miscoordination / (majoritarian_list[[i]]$N/2)
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord, type = "l",
       xlab = "Period", ylab = "Fraction of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci, col = "black", lty = 2, lwd = 0.75)
  
  # Miscoord conservatives
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord, col = "darkgreen", lty = 1, lwd = 3)
  
  # Add confidence intervals (conservative)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_low_ci, col = "darkgreen", lty = 2, lwd = 0.75)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_high_ci, col = "darkgreen", lty = 2, lwd = 0.75)
  
  # Miscoord herd followers
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (herd)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_low_ci, col = "red", lty = 2, lwd = 0.75)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_high_ci, col = "red", lty = 2, lwd = 0.75)
  
  # Miscoord success oriented
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # Miscoord conditional
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord, col = "orange", lty = 1, lwd = 3)
  
  # Add confidence intervals (conditional)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_low_ci, col = "orange", lty = 2, lwd = 0.75)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_high_ci, col = "orange", lty = 2, lwd = 0.75)
  
  # Miscoord majoritarian
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord, col = "violet", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_low_ci, col = "violet", lty = 2, lwd = 0.75)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_high_ci, col = "violet", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  legend("topleft", legend = c("Amenable", "Resistant", "Individualists", "Conservatives", "Success"),
         col = c("darkgreen", "red", "blue", "orange", "violet"),
         lty = c(1, 1, 1, 1, 1))
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "black", ylim = c(0, (current_output$d_mean+current_output$sigma)),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  
  # Average payoffs for conservatives
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$avg_payoff, col = "darkgreen", lty = 1, lwd = 3)
  
  # Add confidence intervals (conservative)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_avg_payoff, col = "darkgreen", lty = 2, lwd = 0.75)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_avg_payoff, col = "darkgreen", lty = 2, lwd = 0.75)
  
  # Average payoffs for herd followers
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$avg_payoff, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (herd)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  
  # Average payoffs for success oriented
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  
  # Average payoffs for conditional followers
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$avg_payoff, col = "orange", lty = 1, lwd = 3)
  
  # Add confidence intervals (conditional)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  
  # Average payoffs for majoritarian followers
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$avg_payoff, col = "violet", lty = 1, lwd = 3)
  
  # Add confidence intervals (majoritarian)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  
  ###
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_gini_coefficient, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_gini_coefficient, col = "black", lty = 2)
  
  # Gini coefficients for conservatives
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$gini_coefficient, col = "darkgreen", lty = 1, lwd = 3)
  
  # Add confidence intervals (conservative)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_gini_coefficient, col = "darkgreen", lty = 2)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_gini_coefficient, col = "darkgreen", lty = 2)
  
  # Gini coefficients for herd followers
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$gini_coefficient, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (herd)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_gini_coefficient, col = "red", lty = 2)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_gini_coefficient, col = "red", lty = 2)
  
  # Gini coefficients for success oriented
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  
  # Gini coefficients for conditional followers
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$gini_coefficient, col = "orange", lty = 1, lwd = 3)
  
  # Add confidence intervals (conditional)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_gini_coefficient, col = "orange", lty = 2)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_gini_coefficient, col = "orange", lty = 2)
  
  # Gini coefficients for majoritarian
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$gini_coefficient, col = "violet", lty = 1, lwd = 3)
  
  # Add confidence intervals (majoritarian)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_gini_coefficient, col = "violet", lty = 2)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_gini_coefficient, col = "violet", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(conservative_list[[i]]$beta))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  # text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  # text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
}




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Combined plots: No intervention vs Target 1
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

for (i in 1:length(conservative_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 2
  cexWord <- 2.25
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("target1", 
                   "beta", conservative_list[[i]]$beta, 
                   "_phi", conservative_list[[i]]$phi, 
                   "_N", conservative_list[[i]]$N,
                   "_n", conservative_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # No intervention
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices conservative
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
  
  # Conservatives
  
  # Add dashed lines for coordination on Alt (Conservatives)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  
  # # Herd followers
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_alt, col = "red", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "red", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "red", lty = 2)
  # 
  # # Success oriented
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Conditional
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_alt, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "orange", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "orange", lty = 2)
  # 
  # # Majoritarian
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_alt, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "violet", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord, type = "l",
       xlab = "Period", ylab = "Fraction of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci, col = "black", lty = 2, lwd = 0.75)
  
  # Miscoord conservatives
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (conservative)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # # Miscoord herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord, col = "red", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_low_ci, col = "red", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_high_ci, col = "red", lty = 2, lwd = 0.75)
  # 
  # # Miscoord success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Miscoord conditional
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_low_ci, col = "orange", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_high_ci, col = "orange", lty = 2, lwd = 0.75)
  # 
  # # Miscoord majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_low_ci, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_high_ci, col = "violet", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("SQ Amenable", "95% CI", "Alt Amenable", "95% CI", "SQ Resistant", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "pink", "pink", "red", "red"),
  #        lty = c(1, 2, 1, 2, 1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "black", ylim = c(0, (current_output$d_mean+current_output$sigma)),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  
  # Average payoffs for conservatives
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (conservative)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  
  # # Average payoffs for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$avg_payoff, col = "red", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$avg_payoff, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for majoritarian followers
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$avg_payoff, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  
  ###
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_gini_coefficient, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_gini_coefficient, col = "black", lty = 2)
  
  # Gini coefficients for conservatives
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (conservative)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  
  # # Gini coefficients for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$gini_coefficient, col = "red", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_gini_coefficient, col = "red", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_gini_coefficient, col = "red", lty = 2)
  # 
  # # Gini coefficients for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$gini_coefficient, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_gini_coefficient, col = "orange", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_gini_coefficient, col = "orange", lty = 2)
  # 
  # # Gini coefficients for majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$gini_coefficient, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_gini_coefficient, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_gini_coefficient, col = "violet", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(conservative_list[[i]]$beta))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
}




################################################################################
# Combined plots: No intervention vs Target 2
################################################################################

for (i in 1:length(conservative_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 2
  cexWord <- 2.25
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("target2", 
                   "beta", conservative_list[[i]]$beta, 
                   "_phi", conservative_list[[i]]$phi, 
                   "_N", conservative_list[[i]]$N,
                   "_n", conservative_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # No intervention
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices conservative
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
  
  # Conservatives
  
  # # Add dashed lines for coordination on Alt (Conservatives)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  
  # # Herd followers
  #
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Success oriented
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Conditional
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_alt, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "orange", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "orange", lty = 2)
  # 
  # # Majoritarian
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_alt, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "violet", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord, type = "l",
       xlab = "Period", ylab = "Fraction of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci, col = "black", lty = 2, lwd = 0.75)
  
  # # Miscoord conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # Miscoord herd followers
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (herd)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Miscoord success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Miscoord conditional
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_low_ci, col = "orange", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_high_ci, col = "orange", lty = 2, lwd = 0.75)
  # 
  # # Miscoord majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_low_ci, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_high_ci, col = "violet", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("SQ Amenable", "95% CI", "Alt Amenable", "95% CI", "SQ Resistant", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "pink", "pink", "red", "red"),
  #        lty = c(1, 2, 1, 2, 1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "black", ylim = c(0, (current_output$d_mean+current_output$sigma)),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  
  # # Average payoffs for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  
  # Average payoffs for herd followers
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (herd)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$avg_payoff, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for majoritarian followers
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$avg_payoff, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  
  ###
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_gini_coefficient, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_gini_coefficient, col = "black", lty = 2)
  
  # # Gini coefficients for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  
  # Gini coefficients for herd followers
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (herd)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$gini_coefficient, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_gini_coefficient, col = "orange", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_gini_coefficient, col = "orange", lty = 2)
  # 
  # # Gini coefficients for majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$gini_coefficient, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_gini_coefficient, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_gini_coefficient, col = "violet", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(conservative_list[[i]]$beta))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
}


################################################################################
# Combined plots: No intervention vs Target 3
################################################################################

for (i in 1:length(conservative_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 2
  cexWord <- 2.25
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("target3", 
                   "beta", conservative_list[[i]]$beta, 
                   "_phi", conservative_list[[i]]$phi, 
                   "_N", conservative_list[[i]]$N,
                   "_n", conservative_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # No intervention
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices conservative
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
  
  # Conservatives
  
  # # Add dashed lines for coordination on Alt (Conservatives)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  
  # # Herd followers
  #
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Success oriented
  # 
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Conditional
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_alt, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "orange", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "orange", lty = 2)
  # 
  # # Majoritarian
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_alt, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "violet", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord, type = "l",
       xlab = "Period", ylab = "Fraction of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci, col = "black", lty = 2, lwd = 0.75)
  
  # # Miscoord conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # # Miscoord herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # Miscoord success oriented
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Miscoord conditional
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_low_ci, col = "orange", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_high_ci, col = "orange", lty = 2, lwd = 0.75)
  # 
  # # Miscoord majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_low_ci, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_high_ci, col = "violet", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("SQ Amenable", "95% CI", "Alt Amenable", "95% CI", "SQ Resistant", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "pink", "pink", "red", "red"),
  #        lty = c(1, 2, 1, 2, 1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "black", ylim = c(0, (current_output$d_mean+current_output$sigma)),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  
  # # Average payoffs for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  
  # # Average payoffs for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # Average payoffs for success oriented
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$avg_payoff, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_avg_payoff, col = "orange", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for majoritarian followers
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$avg_payoff, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  
  ###
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_gini_coefficient, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_gini_coefficient, col = "black", lty = 2)
  
  # # Gini coefficients for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  
  # # Gini coefficients for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # Gini coefficients for success oriented
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$gini_coefficient, col = "orange", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_gini_coefficient, col = "orange", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_gini_coefficient, col = "orange", lty = 2)
  # 
  # # Gini coefficients for majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$gini_coefficient, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_gini_coefficient, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_gini_coefficient, col = "violet", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(conservative_list[[i]]$beta))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
}


################################################################################
# Combined plots: No intervention vs Target 4
################################################################################

for (i in 1:length(conservative_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 2
  cexWord <- 2.25
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("target4", 
                   "beta", conservative_list[[i]]$beta, 
                   "_phi", conservative_list[[i]]$phi, 
                   "_N", conservative_list[[i]]$N,
                   "_n", conservative_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # No intervention
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices conservative
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
  
  # Conservatives
  
  # # Add dashed lines for coordination on Alt (Conservatives)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  
  # # Herd followers
  #
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Success oriented
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Conditional
  # 
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Majoritarian
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_alt, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "violet", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord, type = "l",
       xlab = "Period", ylab = "Fraction of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci, col = "black", lty = 2, lwd = 0.75)
  
  # # Miscoord conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # # Miscoord herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Miscoord success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # Miscoord conditional
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (conditional)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Miscoord majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_low_ci, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_high_ci, col = "violet", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("SQ Amenable", "95% CI", "Alt Amenable", "95% CI", "SQ Resistant", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "pink", "pink", "red", "red"),
  #        lty = c(1, 2, 1, 2, 1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "black", ylim = c(0, (current_output$d_mean+current_output$sigma)),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  
  # # Average payoffs for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  
  # # Average payoffs for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # Average payoffs for conditional followers
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (conditional)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for majoritarian followers
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$avg_payoff, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_avg_payoff, col = "violet", lty = 2, lwd = 0.75)
  
  ###
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_gini_coefficient, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_gini_coefficient, col = "black", lty = 2)
  
  # # Gini coefficients for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  
  # # Gini coefficients for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # Gini coefficients for conditional followers
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (conditional)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for majoritarian
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$gini_coefficient, col = "violet", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (majoritarian)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_gini_coefficient, col = "violet", lty = 2)
  # lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_gini_coefficient, col = "violet", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(conservative_list[[i]]$beta))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
}




################################################################################
# Combined plots: No intervention vs Target 5
################################################################################

for (i in 1:length(conservative_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 2
  cexWord <- 2.25
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("target5", 
                   "beta", conservative_list[[i]]$beta, 
                   "_phi", conservative_list[[i]]$phi, 
                   "_N", conservative_list[[i]]$N,
                   "_n", conservative_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # No intervention
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices conservative
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
  
  # Conservatives
  
  # # Add dashed lines for coordination on Alt (Conservatives)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  
  # # Herd followers
  #
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Success oriented
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Conditional
  # 
  # # Add dashed lines for coordination on Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals for Alt (Resistant)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  # 
  # # Majoritarian
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_alt, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord, type = "l",
       xlab = "Period", ylab = "Fraction of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci, col = "black", lty = 2, lwd = 0.75)
  
  # # Miscoord conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # # Miscoord herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Miscoord success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # # Miscoord conditional
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  # 
  # Miscoord majoritarian
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (success)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_low_ci, col = "blue", lty = 2, lwd = 0.75)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$fract_miscoord_high_ci, col = "blue", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("SQ Amenable", "95% CI", "Alt Amenable", "95% CI", "SQ Resistant", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "pink", "pink", "red", "red"),
  #        lty = c(1, 2, 1, 2, 1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "black", ylim = c(0, (current_output$d_mean+current_output$sigma)),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
  
  # # Average payoffs for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  
  # # Average payoffs for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # # Average payoffs for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  # 
  # Average payoffs for majoritarian followers
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$avg_payoff, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (majoritarian)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_avg_payoff, col = "blue", lty = 2, lwd = 0.75)
  
  ###
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "black", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  ###################################################################################
  # Intervention lines
  ###################################################################################
  abline(v = intervention1line, col = "red", lty = 2)
  # abline(v = intervention2line, col = "red", lty = 2)
  ###################################################################################
  
  # Add confidence intervals (no intervention)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_gini_coefficient, col = "black", lty = 2)
  lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_gini_coefficient, col = "black", lty = 2)
  
  # # Gini coefficients for conservatives
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conservative)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:conservative_list[[i]]$t_max, conservative_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  
  # # Gini coefficients for herd followers
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (herd)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:herd_list[[i]]$t_max, herd_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for success oriented
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (success)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:success_list[[i]]$t_max, success_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # # Gini coefficients for conditional followers
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  # 
  # # Add confidence intervals (conditional)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  # lines(1:conditional_list[[i]]$t_max, conditional_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  # 
  # Gini coefficients for majoritarian
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$gini_coefficient, col = "blue", lty = 1, lwd = 3)
  
  # Add confidence intervals (majoritarian)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
  lines(1:majoritarian_list[[i]]$t_max, majoritarian_list[[i]]$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(conservative_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(conservative_list[[i]]$beta))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
}


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# Separate plots Alt fraction: No intervention vs target 1
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

# Loop through each subdirectory and load and process the .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Initialize a list to store the results for the current subdirectory
  subdirectory_results <- list()
  
  # Load each .RData file
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Extract just the parameter combination name from the full path
    param_name <- basename(dirname(rdata_file))
    
    # Store result in subdirectory_results list
    subdirectory_results[[param_name]] <- result
  }
  
  ###################################################################################
  
  # Plotting parameters:
  
  windowWidth <- 12
  windowHeight <- 11
  
  cexEquil <- 2
  lwdEquil <- 2
  
  cexNum <- 1.5
  cexWord <- 2
  
  ###################################################################################
  ###################################################################################
  
  # Plots
  
  ###################################################################################
  ###################################################################################
  
  for (param_name in names(subdirectory_results)) {
    current_output <- subdirectory_results[[param_name]]
    
    # ###################################################################################
    # # Plotting frequencies of coordination and miscoordination
    # ###################################################################################
    # 
    # # Calculate fractions
    # 
    # current_output$summary_results$fract_coord_alt <- current_output$summary_results$freq_coord_alt/(current_output$N/2)
    # current_output$summary_results$fract_coord_alt_low_ci <- current_output$summary_results$low_ci_freq_coord_alt/(current_output$N/2)
    # current_output$summary_results$fract_coord_alt_high_ci <- current_output$summary_results$high_ci_freq_coord_alt/(current_output$N/2)
    # 
    # current_output$summary_results$fract_miscoordination <- current_output$summary_results$miscoordination/(current_output$N/2)
    # current_output$summary_results$fract_miscoordination_low_ci <- current_output$summary_results$low_ci_miscoordination/(current_output$N/2)
    # current_output$summary_results$fract_miscoordination_high_ci <- current_output$summary_results$high_ci_miscoordination/(current_output$N/2)
    # 
    # # Create the filename for this parameter combination
    # filename <- file.path(subdir, "freq_coordination.pdf")
    # 
    # pdf(width = windowWidth, height = windowHeight, file = filename)
    # par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    # 
    # # Create the initial line plot with coordination on Alt
    # plot(1:current_output$t_max, current_output$summary_results$fract_coord_alt, type = "l",  lwd = 3,
    #      xlab = "Period", ylab = "Fraction",
    #      # main = "Fraction of (mis)coordination",
    #      col = "darkgreen", ylim = c(0, 1),
    #      cex.axis = cexNum,cex.lab = cexWord)
    # 
    # # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$fract_coord_alt_low_ci, col = "darkgreen", lty = 2)
    # lines(1:current_output$t_max, current_output$summary_results$fract_coord_alt_high_ci, col = "darkgreen", lty = 2)
    # 
    # # # # Add coordination on SQ
    # # lines(1:current_output$t_max, current_output$summary_results$freq_coord_sq, col = "blue", lty = 1, lwd = 3)
    # # 
    # # # Add confidence intervals
    # # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_sq, col = "blue", lty = 2)
    # # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_sq, col = "blue", lty = 2)
    # 
    # # # Add miscoordination
    # lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination, col = "red", lty = 1, lwd = 3)
    # 
    # # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination_low_ci, col = "red", lty = 2)
    # lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination_high_ci, col = "red", lty = 2)
    # 
    # # # # Add a legend
    # # legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt", "Miscoordination", "95% CI Miscoord."), 
    # #        col = c("blue","blue", "darkgreen", "darkgreen", "red", "red"), lty = c(1, 2, 1, 2, 1, 2))
    # #  
    # # # Close the graphics device and save the plot as a PDF file
    # dev.off()
    
    ###################################################################################
    # Plotting choice fractions
    ###################################################################################
    current_output$summary_results$fract_alt <- current_output$summary_results$freq_alt / current_output$N
    current_output$summary_results$fract_sq <- current_output$summary_results$freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_sq <- current_output$summary_results$low_ci_freq_sq / current_output$N
    current_output$summary_results$fract_high_ci_freq_sq <- current_output$summary_results$high_ci_freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_alt <- current_output$summary_results$low_ci_freq_alt / current_output$N
    current_output$summary_results$fract_high_ci_freq_alt <- current_output$summary_results$high_ci_freq_alt / current_output$N
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "fract_choices_no_vs_int.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create the initial line plot with Alt choices
    plot(1:current_output$t_max, current_output$summary_results$fract_alt, type = "l", lwd = 3,
         xlab = "Period", ylab = "Fraction",
         # main = "Fraction of Alt choices",
         col = "blue", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord)
    
    ###################################################################################
    # Intervention lines
    ###################################################################################
    abline(v = intervention1line, col = "red", lty = 2)
    # abline(v = intervention2line, col = "red", lty = 2)
    ###################################################################################
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_alt, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_alt, col = "blue", lty = 2)
    
    # Alt choices no intervention
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, col = "black", lty = 1, lwd = 3)
    
    # Add confidence intervals
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
    
    
    
    # # Add SQ choices
    # lines(1:current_output$t_max, current_output$summary_results$fract_sq, col = "blue", lty = 1, lwd = 3)
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_sq, col = "blue", lty = 2)
    # lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_sq, col = "blue", lty = 2)
    
    
    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI", "Alt", "95% CI"), 
    #        col = c("blue", "blue", "darkgreen", "darkgreen"), lty = c(1, 2, 1, 2))
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
  }
  
}




setwd('..')
