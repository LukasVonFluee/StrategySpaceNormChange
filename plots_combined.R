###################################################################################
# Analysis of model for paper: 
# "Enhance the threshold in threshold models to study heterogeneity in learning and its effects on social norm change"
# Authors: Lukas von Flüe
###################################################################################

# This script produces combined plots with all treatments

# This defines the time period at which intervention is implemented and will be
# used further below to create red dashed vertical line at this time period
intervention1line <- 50 

###################################################################################
base_dir <- getwd()

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

# Function to create plots
create_plots <- function(loaded_data_list, h_value, phi_value, G_value) {
  # Gather all treatments in separate lists
  individualist_list <- list()
  conformist_list <- list()
  success_list <- list()
  other_list <- list()
  random_list <- list()
  no_intervention_list <- list()
  
  for (param_name in names(loaded_data_list)) {
    result <- loaded_data_list[[param_name]]
    
    # Check if the h, phi, and G values match
    if (result$h != h_value || result$phi != phi_value || result$G != G_value) next
    
    # Calculate fractions because in simulation we recorded frequencies
    result$summary_results$fract_alt <- result$summary_results$freq_alt / result$N
    result$summary_results$fract_sq <- result$summary_results$freq_sq / result$N
    result$summary_results$fract_low_ci_freq_sq <- result$summary_results$low_ci_freq_sq / result$N
    result$summary_results$fract_high_ci_freq_sq <- result$summary_results$high_ci_freq_sq / result$N
    result$summary_results$fract_low_ci_freq_alt <- result$summary_results$low_ci_freq_alt / result$N
    result$summary_results$fract_high_ci_freq_alt <- result$summary_results$high_ci_freq_alt / result$N
    result$summary_results$fract_miscoord <- result$summary_results$miscoordination / (result$N/2)
    result$summary_results$fract_miscoord_low_ci <- result$summary_results$low_ci_miscoordination / (result$N/2)
    result$summary_results$fract_miscoord_high_ci <- result$summary_results$high_ci_miscoordination / (result$N/2)
    
    if (grepl("target0", param_name)) {
      no_intervention_list[[param_name]] <- result
    } else if (grepl("target1", param_name)) {
      individualist_list[[param_name]] <- result
    } else if (grepl("target2", param_name)) {
      conformist_list[[param_name]] <- result
    } else if (grepl("target3", param_name)) {
      success_list[[param_name]] <- result
    } else if (grepl("target4", param_name)) {
      other_list[[param_name]] <- result
    } else if (grepl("target5", param_name)) {
      random_list[[param_name]] <- result
    }
  }
  
  dir_name <- paste("combined_plots_h", h_value, "_phi", phi_value, "_G", G_value, sep = "")
  
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
  setwd(dir_name)
  
  for (i in 1:length(individualist_list)) {
    
    # Plotting parameters:
    
    windowWidth <- 20
    windowHeight <- 15
    
    lineWidth <- 3
    
    cexNum <- 2
    cexWord <- 2.25
    
    # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
    label <- bquote(italic(alpha) == .(individualist_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    ###################################################################################
    
    # Plot 1: Fraction of Alt choices (upper left)
    
    ###################################################################################
    
    pdf(file = paste("alpha", individualist_list[[i]]$alpha, 
                     "_phi", individualist_list[[i]]$phi, 
                     "_N", individualist_list[[i]]$N,
                     "_n", individualist_list[[i]]$n,
                     "_h", individualist_list[[i]]$h,
                     "_G", individualist_list[[i]]$G,
                     ".pdf", sep = "") , width = windowWidth, height = windowHeight)
    
    par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
    
    # Plot par:
    par(fig = c(0,0.45,0.5,1))
    
    # Alt choices individualist
    plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
         xlab = "Period", ylab = "Fraction of Alt choices",
         col = "black", ylim = c(-0.05, 1.05),
         cex.axis = cexNum,cex.lab = cexWord)
    mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
    
    # Intervention lines
    abline(v = intervention1line, col = "red", lty = 2)
    
    # Adding horizontal dashed lines at y = 0 and y = 1
    abline(h = 0, col = "black", lty = 2, lwd = 1) # Dashed line at y = 0
    abline(h = 1, col = "black", lty = 2, lwd = 1) # Dashed line at y = 1
    
    # Add confidence intervals
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "black", lty = 2)
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "black", lty = 2)
    
    # Add lines for different strategies
    strategies <- list(individualist_list, conformist_list, success_list, other_list, random_list)
    colors <- c("darkgreen", "red", "blue", "orange", "violet")
    for (j in 1:length(strategies)) {
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$fract_alt, col = colors[j], lty = 1, lwd = 3)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$fract_low_ci_freq_alt, col = colors[j], lty = 2)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$fract_high_ci_freq_alt, col = colors[j], lty = 2)
    }
    
    legend_y_top <- 0.92  # Set the top of the legend slightly below y = 1, adjust this value as needed
    
    # Add a legend with coordinates
    legend("topleft", legend = c("All groups", "Individualists", "Conformists", "Success-based", "Conservatives", "Random"),
           col = c("black", colors), lty = 1, lwd = 3,
           y.intersp = 1.2,  # Adjust spacing between lines
           xjust = 1, yjust = 1,  # Right-justify the legend box at the specified coordinate
           xpd = TRUE,  # Allow positioning outside the plot area
           cex = 1.25,
           inset = c(0, 1 - legend_y_top)) 
    
    ###################################################################################
    
    # Plot 2: Miscoordination (Upper right)
    
    ###################################################################################
    
    par(fig = c(0.45,0.9,0.5,1),new = T)
    
    # Create the initial line plot with frequencies of miscoordination for amenable targets
    plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord, type = "l",
         xlab = "Period", ylab = "Fraction of miscoordination",
         col = "black", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
    mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
    
    # Intervention lines
    abline(v = intervention1line, col = "red", lty = 2)
    
    # Adding horizontal dashed lines at y = 0 and y = 1
    abline(h = 0, col = "black", lty = 2, lwd = 1) # Dashed line at y = 0
    abline(h = 1, col = "black", lty = 2, lwd = 1) # Dashed line at y = 1
    
    # Add confidence intervals
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_low_ci, col = "black", lty = 2, lwd = 0.75)
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$fract_miscoord_high_ci, col = "black", lty = 2, lwd = 0.75)
    
    # Add lines for different strategies
    for (j in 1:length(strategies)) {
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$fract_miscoord, col = colors[j], lty = 1, lwd = 3)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$fract_miscoord_low_ci, col = colors[j], lty = 2, lwd = 0.75)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$fract_miscoord_high_ci, col = colors[j], lty = 2, lwd = 0.75)
    }
    
    ###################################################################################
    
    # Plot 3: Average payoffs (Lower left)
    
    ###################################################################################
    
    par(fig = c(0,0.45,0,0.5),new = T)
    
    # Create the initial line plot with Average payoffs for amenable targets
    plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$avg_payoff, type = "l",
         xlab = "Period", ylab = "Average utility",
         col = "black", ylim = c(0, (individualist_list[[i]]$d_mean + individualist_list[[i]]$sigma)),
         cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
    mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
    
    # Intervention lines
    abline(v = intervention1line, col = "red", lty = 2)
    
    # Adding horizontal dashed lines at y = 0 and y = 1
    abline(h = 0, col = "black", lty = 2, lwd = 1) # Dashed line at y = 0
    abline(h = 2, col = "black", lty = 2, lwd = 1) # Dashed line at y = 1
    
    # Add confidence intervals
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_avg_payoff, col = "black", lty = 2, lwd = 0.75)
    
    # Add lines for different strategies
    for (j in 1:length(strategies)) {
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$avg_payoff, col = colors[j], lty = 1, lwd = 3)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$low_ci_avg_payoff, col = colors[j], lty = 2, lwd = 0.75)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$high_ci_avg_payoff, col = colors[j], lty = 2, lwd = 0.75)
    }
    
    ###################################################################################
    
    # Plot 4: Gini coefficient (Lower right)
    
    ###################################################################################
    
    par(fig = c(0.45,0.9,0,0.5),new = T)
    
    # Create the initial line plot with Gini coefficients for amenable targets
    plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$gini_coefficient, type = "l",
         xlab = "Period", ylab = "Gini coefficient",
         col = "black", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
    mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
    
    # Intervention lines
    abline(v = intervention1line, col = "red", lty = 2)
    
    # Adding horizontal dashed lines at y = 0 and y = 1
    abline(h = 0, col = "black", lty = 2, lwd = 1) # Dashed line at y = 0
    abline(h = 1, col = "black", lty = 2, lwd = 1) # Dashed line at y = 1
    
    # Add confidence intervals
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_gini_coefficient, col = "black", lty = 2)
    lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_gini_coefficient, col = "black", lty = 2)
    
    # Add lines for different strategies
    for (j in 1:length(strategies)) {
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$gini_coefficient, col = colors[j], lty = 1, lwd = 3)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$low_ci_gini_coefficient, col = colors[j], lty = 2)
      lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$high_ci_gini_coefficient, col = colors[j], lty = 2)
    }
    
    # Adding alpha and beta values on the side
    alpha_val <- bquote(italic(alpha) == .(individualist_list[[i]]$alpha))
    beta_val <- bquote(italic(beta) == 2)
    
    par(fig = c(0.9,1,0,1),new = T)
    par(mar=c(0,0,0,0))
    plot(0:1,0:1,xlab='',ylab='',type = 'n', axes=F)
    text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
    text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
    
    dev.off()
    
    ###################################################################################
    
    # Group-specific plots if G > 1
    
    if (G_value > 1) {
      # Plot for Group 1
      pdf(file = paste("alpha", individualist_list[[i]]$alpha, 
                       "_phi", individualist_list[[i]]$phi, 
                       "_N", individualist_list[[i]]$N,
                       "_n", individualist_list[[i]]$n,
                       "_h", individualist_list[[i]]$h,
                       "_G", individualist_list[[i]]$G,
                       "_group1.pdf", sep = "") , width = windowWidth, height = windowHeight)
      
      par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
      
      # Alt choices in Group 1
      plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$freq_alt_group1 / (no_intervention_list[[i]]$N/G_value), type = "l", lwd = lineWidth,
           xlab = "Period", ylab = "Fraction of Alt choices in Group 1",
           col = "black", ylim = c(0, 1),
           cex.axis = cexNum,cex.lab = cexWord)
      mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
      
      # Intervention lines
      abline(v = intervention1line, col = "red", lty = 2)
      
      # Adding horizontal dashed lines at y = 0 and y = 1
      abline(h = 0, col = "black", lty = 2, lwd = 1) # Dashed line at y = 0
      abline(h = 1, col = "black", lty = 2, lwd = 1) # Dashed line at y = 1
      
      # Add confidence intervals
      lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_freq_alt_group1 / (no_intervention_list[[i]]$N/G_value), col = "black", lty = 2)
      lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_freq_alt_group1 / (no_intervention_list[[i]]$N/G_value), col = "black", lty = 2)
      
      # Add lines for different strategies
      for (j in 1:length(strategies)) {
        lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$freq_alt_group1 / (strategies[[j]][[i]]$N/G_value), col = colors[j], lty = 1, lwd = 3)
        lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$low_ci_freq_alt_group1 / (strategies[[j]][[i]]$N/G_value), col = colors[j], lty = 2)
        lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$high_ci_freq_alt_group1 / (strategies[[j]][[i]]$N/G_value), col = colors[j], lty = 2)
      }
      
      legend_y_top <- 0.962  # Set the top of the legend slightly below y = 1, adjust this value as needed
      
      # Add a legend with coordinates
      legend("bottomright", legend = c("All groups", "Individualists", "Conformists", "Success-based", "Conservatives", "Random"),
             col = c("black", colors), lty = 1, lwd = 3,
             y.intersp = 1.2,  # Adjust spacing between lines
             xjust = 1, yjust = 1,  # Right-justify the legend box at the specified coordinate
             xpd = TRUE,  # Allow positioning outside the plot area
             cex = 1.75,
             inset = c(0, 1 - legend_y_top)) 
      
      dev.off()
      
      # Plot for Group 2
      pdf(file = paste("alpha", individualist_list[[i]]$alpha, 
                       "_phi", individualist_list[[i]]$phi, 
                       "_N", individualist_list[[i]]$N,
                       "_n", individualist_list[[i]]$n,
                       "_h", individualist_list[[i]]$h,
                       "_G", individualist_list[[i]]$G,
                       "_group2.pdf", sep = "") , width = windowWidth, height = windowHeight)
      
      par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
      
      # Alt choices in Group 2
      plot(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$freq_alt_group2 / (no_intervention_list[[i]]$N/G_value), type = "l", lwd = lineWidth,
           xlab = "Period", ylab = "Fraction of Alt choices in Group 2",
           col = "black", ylim = c(0, 1),
           cex.axis = cexNum,cex.lab = cexWord)
      mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
      
      # Intervention lines
      abline(v = intervention1line, col = "red", lty = 2)
      
      # Adding horizontal dashed lines at y = 0 and y = 1
      abline(h = 0, col = "black", lty = 2, lwd = 1) # Dashed line at y = 0
      abline(h = 1, col = "black", lty = 2, lwd = 1) # Dashed line at y = 1
      
      # Add confidence intervals
      lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$low_ci_freq_alt_group2 / (no_intervention_list[[i]]$N/G_value), col = "black", lty = 2)
      lines(1:no_intervention_list[[i]]$t_max, no_intervention_list[[i]]$summary_results$high_ci_freq_alt_group2 / (no_intervention_list[[i]]$N/G_value), col = "black", lty = 2)
      
      # Add lines for different strategies
      for (j in 1:length(strategies)) {
        lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$freq_alt_group2 / (strategies[[j]][[i]]$N/G_value), col = colors[j], lty = 1, lwd = 3)
        lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$low_ci_freq_alt_group2 / (strategies[[j]][[i]]$N/G_value), col = colors[j], lty = 2)
        lines(1:strategies[[j]][[i]]$t_max, strategies[[j]][[i]]$summary_results$high_ci_freq_alt_group2 / (strategies[[j]][[i]]$N/G_value), col = colors[j], lty = 2)
      }
      
      legend_y_top <- 0.962  # Set the top of the legend slightly below y = 1, adjust this value as needed
      
      # Add a legend with coordinates
      legend("topleft", legend = c("All groups", "Individualists", "Conformists", "Success-based", "Conservatives", "Random"),
             col = c("black", colors), lty = 1, lwd = 3,
             y.intersp = 1.2,  # Adjust spacing between lines
             xjust = 1, yjust = 1,  # Right-justify the legend box at the specified coordinate
             xpd = TRUE,  # Allow positioning outside the plot area
             cex = 1.75,
             inset = c(0, 1 - legend_y_top)) 
      
      dev.off()
    }
  }
  
  setwd('..')
}

# Loop over each combination of h, phi, and G and create plots
h_values <- unique(sapply(loaded_data_list, function(x) x$h))
phi_values <- unique(sapply(loaded_data_list, function(x) x$phi))
G_values <- unique(sapply(loaded_data_list, function(x) x$G))

for (h in h_values) {
  for (phi in phi_values) {
    for (G in G_values) {
      create_plots(loaded_data_list, h, phi, G)
    }
  }
}
