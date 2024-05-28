###################################################################################
# Title: Agent-based simulation model for paper: 
# "Redefine the threshold in threshold models to study heterogeneity in learning and its effects on social norm change"
# Authors: Lukas von Flüe
###################################################################################

################################################################################
rm(list=ls())
################################################################################

# Coordination game payoff matrix with x_values:

#       SQ        Alt
##########################
# SQ    a+x_i     b+x_i
# Alt   a         d
##########################

# It is a strict coordination game because for each agent $i$, $a + x_{i} > a$, and $b + x_{i} < d$, where $b<d$.
# The x_i values are distributed over the open interval (0,(d-b)) according to a beta distribution with shape parameters alpha and beta.

# Risk dominance of SQ if:
# (x_i > (d − b)/2).

################################################################################

# Test parameters

################################################################################

################################################################################

if (!require("parallel")) {
  install.packages("parallel", dependencies = TRUE)
  library("parallel")
}

# Define number of cores for parallel processing
num_cores <- detectCores() - 1

# 
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library("ggplot2")
}

################################################################################
# Gini function
################################################################################
# References: Gini (1912), Schmidt & Wichardt (2019):
# The argument of the function, x, are the agents' payoffs of a given period.

my_gini <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  sorted_x <- sort(x)
  i <- 1:n
  numerator <- sum(2 * i * sorted_x)
  denominator <- n * sum_x
  gini <- numerator / denominator - (n + 1) / n
  return(gini)
}

################################################################################

# Function to generate all combinations of bits (which correspond to the information types) in binary counting order
generateInfoCombinations <- function(n_info_types) {
  intToBinaryString <- function(int, n_info_types) {
    binaryString <- sprintf(paste0("%0", n_info_types, "s"), as.character(as.binary(int)))
    binaryString <- gsub(" ", "0", binaryString)
    return(binaryString)
  }
  as.binary <- function(x) {
    paste(rev(as.integer(intToBits(x))[1:n_info_types]), collapse = "")
  }
  info_combinations <- sapply(0:(2^n_info_types - 1), function(x) intToBinaryString(x, n_info_types))
  info_combinations_matrix <- matrix(as.integer(unlist(strsplit(info_combinations, ""))), ncol = n_info_types, byrow = TRUE)
  colnames(info_combinations_matrix) <- paste0("Info", 1:n_info_types)
  return(info_combinations_matrix)
}

################################################################################
# Function to generate strategies
################################################################################
# The parameter "N" is the population size
# The parameter "strat_dist" controls which distribution will be initialised for a given simulation
# The parameter "n_info_types" controls the length of the strategy array. If n_info_types=3, array length is = 8, because we model a binary choice (2^3).
# The parameters "alpha" and "beta" are shape parameters of a beta distribution. We only use this if we work with choice probabilities defined over the open interval (0,1).

generateStrategies <- function(N, strat_dist, alpha, beta, n_info_types) {
  strategies <- vector("list", length = N)
  sl_type <- rep(NA, N)  # Initialize social learning type vector
  total_combinations <- 2^n_info_types
  
  for (i in 1:N) {
    if (strat_dist == 0) {
      strategies[[i]] <- rep(0, total_combinations)
    } else if (strat_dist == 1) {
      strategies[[i]] <- rep(1, total_combinations)
    } else if (strat_dist == 2) {
      strategies[[i]] <- round(runif(total_combinations), 2)
    } else if (strat_dist == 3) {
      strategies[[i]] <- round(rbeta(total_combinations, alpha, beta), 2)
    } else if (strat_dist == 4) {
      strategy <- numeric(total_combinations)
      strategy[1] <- rbeta(1, alpha, beta)
      strategy[2:total_combinations] <- round(runif(total_combinations - 1), 2)
      strategies[[i]] <- strategy
    } else if (strat_dist == 5) {
      strategy <- numeric(total_combinations)
      strategy[1] <- 0
      strategy[2:total_combinations] <- round(runif(total_combinations - 1), 2)
      strategies[[i]] <- strategy
    } else if (strat_dist == 6) { # Create 1/3 individualists, 1/3 conformists, 1/3 success-based
      type <- sample(1:3, 1)
      sl_type[i] <- type
      strategy <- numeric(total_combinations)
      strategy[1] <- 0
      
      if (type == 1) { # Individualists
        low_indices <- c(2, 3, 4)
        high_indices <- c(5, 6, 7, 8)
      } else if (type == 2) { # Conformists
        low_indices <- c(2, 5, 6)
        high_indices <- c(3, 4, 7, 8)
      } else if (type == 3) { # Success-Oriented
        low_indices <- c(3, 5, 7)
        high_indices <- c(2, 4, 6, 8)
      }
      
      strategy[low_indices] <- runif(length(low_indices), 0, 0.25)
      strategy[high_indices] <- runif(length(high_indices), 0.75, 1)
      strategies[[i]] <- strategy
    } else if (strat_dist == 7) { # Create all conformists
      type <- 2
      sl_type[i] <- type
      strategy <- numeric(total_combinations)
      strategy[1] <- 0
      
      if (type == 1) { # Individualists
        low_indices <- c(2, 3, 4)
        high_indices <- c(5, 6, 7, 8)
      } else if (type == 2) { # Conformists
        low_indices <- c(2, 5, 6)
        high_indices <- c(3, 4, 7, 8)
      } else if (type == 3) { # Success-Oriented
        low_indices <- c(3, 5, 7)
        high_indices <- c(2, 4, 6, 8)
      }
      
      strategy[low_indices] <- runif(length(low_indices), 0, 0.25)
      strategy[high_indices] <- runif(length(high_indices), 0.75, 1)
      strategies[[i]] <- strategy
    } else if (strat_dist == 8) { # Deterministic strategies all conformists
      type <- 2
      sl_type[i] <- type
      strategy <- numeric(total_combinations)
      strategy[1] <- 0
      
      if (type == 1) { # Individualists
        low_indices <- c(2, 3, 4)
        high_indices <- c(5, 6, 7, 8)
      } else if (type == 2) { # Conformists
        low_indices <- c(2, 5, 6)
        high_indices <- c(3, 4, 7, 8)
      } else if (type == 3) { # Success-Oriented
        low_indices <- c(3, 5, 7)
        high_indices <- c(2, 4, 6, 8)
      }
      
      strategy[low_indices] <- 0
      strategy[high_indices] <- 1
      strategies[[i]] <- strategy
    } else if (strat_dist == 9) { # Deterministic strategies 1/3 individualists, 1/3 conformists, 1/3 success oriented
      type <- sample(1:3, 1)
      sl_type[i] <- type
      strategy <- numeric(total_combinations)
      
      if (type == 1) { # Individualists
        low_indices <- c(1, 2, 3, 4)
        high_indices <- c(5, 6, 7, 8)
      } else if (type == 2) { # Conformists
        low_indices <- c(1, 2, 5, 6)
        high_indices <- c(3, 4, 7, 8)
      } else if (type == 3) { # Success-Oriented
        low_indices <- c(1, 3, 5, 7)
        high_indices <- c(2, 4, 6, 8)
      }
      
      strategy[low_indices] <- 0
      strategy[high_indices] <- 1
      strategies[[i]] <- strategy
    } else if (strat_dist == 10) { # Create 1/2 conformists, 1/2 success-based
      type <- sample(2:3, 1)
      sl_type[i] <- type
      strategy <- numeric(total_combinations)
      
      if (type == 1) { # Individualists
        low_indices <- c(1, 2, 3, 4)
        high_indices <- c(5, 6, 7, 8)
      } else if (type == 2) { # Conformists
        low_indices <- c(1, 2, 5, 6)
        high_indices <- c(3, 4, 7, 8)
      } else if (type == 3) { # Success-Oriented
        low_indices <- c(1, 3, 5, 7)
        high_indices <- c(2, 4, 6, 8)
      }
      
      strategy[low_indices] <- 0
      strategy[high_indices] <- 1
      strategies[[i]] <- strategy
    } else if (strat_dist == 11) { # Create 1/5 conservatives, 1/5 individualists, 1/5 conformists, 1/5 success-based, 1/5 random
      type <- sample(1:5, 1)
      sl_type[i] <- type
      strategy <- numeric(total_combinations)
      
      if (type == 1) { # Individualists
        low_indices <- c(1, 2, 3, 4)
        high_indices <- c(5, 6, 7, 8)
      } else if (type == 2) { # Conformists
        low_indices <- c(1, 2, 5, 6)
        high_indices <- c(3, 4, 7, 8)
      } else if (type == 3) { # Success-Oriented
        low_indices <- c(1, 3, 5, 7)
        high_indices <- c(2, 4, 6, 8)
      } else if (type == 4) { # conservatives
        low_indices <- c(1, 2, 3, 4, 5, 6, 7, 8)
        high_indices <- c()
      } else if (type == 5) { # random
        low_indices <- sample(1:8,4)
        high_indices <- setdiff(seq_along(1:8), low_indices)
      }
      
      strategy[low_indices] <- 0
      strategy[high_indices] <- 1
      strategies[[i]] <- strategy
      
    } else if (strat_dist == 12) { # Seed population with random strategies
      
      strategy <- numeric(total_combinations)
      strategy <- rep(0,8)
      high_indices <- sample(1:8,4)
      strategy[high_indices] <- 1
      strategies[[i]] <- strategy
      
    } else if (strat_dist == 13) { # Deterministic strategies 1/4 conservatives, 1/4 individualists, 1/4 conformists, 1/4 success oriented
      type <- sample(1:4, 1)
      sl_type[i] <- type
      strategy <- numeric(total_combinations)
      
      if (type == 1) { # Individualists
        low_indices <- c(1, 2, 3, 4)
        high_indices <- c(5, 6, 7, 8)
      } else if (type == 2) { # Conformists
        low_indices <- c(1, 2, 5, 6)
        high_indices <- c(3, 4, 7, 8)
      } else if (type == 3) { # Success-Oriented
        low_indices <- c(1, 3, 5, 7)
        high_indices <- c(2, 4, 6, 8)
      } else if (type == 4) { # Conservatives
        low_indices <- c(1, 2, 3, 4, 5, 6, 7, 8)
        high_indices <- c()
      }
      
      strategy[low_indices] <- 0
      strategy[high_indices] <- 1
      strategies[[i]] <- strategy
    }
  }
  
  return(list(strategies = strategies, sl_type = sl_type))
}

# sum(agents$sl_type==1)

################################################################################
# Function to classify agents certain learning types
################################################################################

# After initialising agents and generating strategies, we can analyse the distribution
# of certain learning types.
# The selection of types below is arbitrary and types may be removed/substituted/added.

classify_agents <- function(strategy) {
  if (all(strategy == c(0, 0, 0, 0, 1, 1, 1, 1))) {
    return("Individualist")
  } else if (all(strategy == c(0, 0, 1, 1, 0, 0, 1, 1))) {
    return("Conformist")
  } else if (all(strategy == c(0, 1, 0, 1, 0, 1, 0, 1))) {
    return("Success-Oriented")
  } else if (all(strategy == rep(0, 8))) {
    return("Conservative")
  } else if (all(strategy == rep(1, 8))) {
    return("Activist")
  } else {
    return("Other")
  }
}


################################################################################
# Sample function
################################################################################

# Sample agents to apply intervention to

# Sample a fraction "phi" of the agents
sample_fraction <- function(agents, phi, h) {
  
  if(h==0){ # target random
    
    agents$respond[1:(N*phi)] <- 1
    
  } else if(h==1){ # target most amenable
    
    # We order agents in increasing order according to their x_i values, and
    # target N*phi agents with lowest x_i values:
    agents <- agents[order(agents$x_i), ]
    # Response to intervention is probabilistic and proportional to threshold.
    # Note, an agent's threshold value is given by; agent$x_i/(d-b)
    # We make response a decreasing function of threshold values and define
    # the probability to switch as; 1-(agent$x_i/(d-b))
    # prob_draw <- runif(N*phi)
    # agents$respond[1:(N*phi)] <- ifelse(prob_draw<=(1-(agents$x_i[1:(N*phi)]/(d_mean-b_mean))), 1, 0)
    # We start with simpler model where reponse is certain
    agents$respond[1:(N*phi)] <- 1
    
  } else if(h==2){ # target most resistant
    
    # If we target most resistant, we order in decreasing order:
    agents <- agents[order(agents$x_i, decreasing = TRUE), ]
    prob_draw <- runif(N*phi)
    # agents$respond[1:(N*phi)] <- ifelse(prob_draw<=(1-(agents$x_i[1:(N*phi)]/(d_mean-b_mean))), 1, 0)
    # We start with simpler model where reponse is certain
    agents$respond[1:(N*phi)] <- 1
    
  }
  
  return(agents)
  
}


################################################################################
# Intervention function
################################################################################

# Intervention function
apply_intervention <- function(agents, strategy_indices) {
  agents$strategy[agents$respond==1] <- lapply(agents$strategy[agents$respond==1], function(strategy) {
    strategy[strategy_indices] <- 1
    non_targeted_indices <- setdiff(seq_along(strategy), strategy_indices)
    strategy[non_targeted_indices] <- 0
    return(strategy)
  })
  
  agents$x_i[agents$respond==1] <- 0
  
  return(agents)
}

################################################################################
# Mutation function
################################################################################

# This function is currently not used and will only be used if model is changed into 
# a gene-culture co-evolution model.

mutate_strategies <- function(agents, mu) {
  for (i in 1:nrow(agents)) {
    mutations <- runif(length(agents$strategy[[i]])) < mu
    if (any(mutations)) {
      agents$strategy[[i]][mutations] <- round(runif(sum(mutations)), 2)
    }
  }
  return(agents)
}

# # Calculate the probability that at least one choice probability of a given agent changes in a given period
# mutation_rate <- 0.001
# prob_no_mutation <- (1 - mutation_rate)^8
# prob_at_least_one_mutation <- 1 - prob_no_mutation
# 
# prob_at_least_one_mutation

################################################################################
################################################################################
################################################################################
# Test parameter values
################################################################################
################################################################################
################################################################################

t_max <- 100
N <- 100 # size of total population
n <- 10 # n defines the sample size of observations agents take to collect social info
G <- 1 # number of groups

alpha <- 2.25 # shape parameter for beta distribution
beta <- 2 # shape parameter for beta distribution

n_info_types <- 3 # number of types of info agents can use

# Testing stuff

# strategies <- generateStrategies(N, strat_dist, alpha, beta, n_info_types)
# 
# agents <- data.frame(
#   id = 1:N,
#   group = rep(NA, N),
#   respond = rep(NA, N),
#   q = rep(NA, N),
#   exp_sq = rep(NA, N),
#   exp_alt = rep(NA, N), 
#   IB = rep(NA, N),
#   CB = rep(NA, N),
#   SB = rep(NA, N),
#   strategy = I(strategies),
#   choice = rep(NA, N),
#   payoff = rep(NA, N)
# )
# 
# # Load the necessary library for visualization
# library(ggplot2)
# 
# # Function to visualize the density of N random values from a beta distribution
# visualize_density <- function(values, index) {
#   # Convert the values to a data frame for ggplot
#   df <- data.frame(Value = values)
#   
#   # Create the density plot
#   p <- ggplot(df, aes(x = Value)) +
#     geom_density(fill = "blue", alpha = 0.5) +
#     labs(title = paste("Density Plot for Strategy Index", index),
#          x = "Value",
#          y = "Density") +
#     xlim(0, 1) +  # Set x-axis limits to be between 0 and 1
#     theme_minimal()
#   
#   # Print the plot
#   print(p)
# }
# 
# # Example usage: Visualize density for the first strategy index
# # Assuming `agents` data frame is already defined and each agent's strategy is a list
# 
# index <- 1
# values <- sapply(agents$strategy, function(strategy) strategy[index])
# visualize_density(values, index)
# 
# for(i in 1:8){
#   
#   index <- i
#   values <- sapply(agents$strategy, function(strategy) strategy[index])
#   visualize_density(values, index)
#   
# }


# network_random defines whether agents are assigned to G groups randomly or according to homophily
# if network_random=1 agents are randomly assigned to G groups
# if network_random=0 we assign agents to G groups based on either their x_i values or similarity scores (depending on whether we use idiosyncratic x_i values or not)
# this is only relevant if we implement x_i values, which I currently don't in this model
network_random <- 1

################################################################################
# Coordination game payoffs:
################################################################################

a_mean <- 0.75
b_mean <- 1
c_mean <- 1
d_mean <- 2
sigma <- 0

################################################################################
# Intervention
################################################################################

target <- 4 # type of intervention

intervention <- t_max/2 # time period at which intervention is implemented

phi <- 0.2 # size of intervention (fraction of agents targeted)

h <- 0 # If h==0 target amenable, if h==1 target resistant, based on x_i values

################################################################################
# Initialise initial strategy distribution
################################################################################

# strat_dist <- 0 would generate 8-long array of 0s for each agent, representing a strategy that implies choosing 0 (=SQ) for each possible information combination
# strat_dist <- 1 would generate 8-long array of 1s for each agent, representing a strategy that implies choosing 1 (=Alt) for each possible information combination
# strat_dist <- 2 # This generates strategies that are defined over the open interval (0,1), randomly drawn from a uniform distribution, separately for each agent
# strat_dist <- 3 # choice probabilities are drawn from beta distribution
# strat_dist <- 4 # Choice probability for (0,0,0) is drawn from beta distribution, and rest of the probabilities are drawn from uniform distribution.
# strat_dist <- 5 # Choice probability for (0,0,0) is set = 0, and rest of the probabilities are drawn from uniform distribution.
# strat_dist <- 6 # Choice probability for (0,0,0) is set = 0, and rest of the probabilities are drawn such that there are 1/3 individualists, 1/3 conformists, and 1/3 success-based learners.
strat_dist <- 12
################################################################################
# Initialise choice distribution
################################################################################

# To initialise system we assume a fraction of agents choosing SQ before t=1.
initial_alt <- 0

################################################################################
# Mutation of strategies
################################################################################

# Mutation rate to change strategies (I currently don't do that)
mu <- 0.01

################################################################################
# Strategies: Information-choice mappings
################################################################################

# Agents can observe different types of private and social information:

# Individual bias (IB):
# - Expected payoff for choosing 0/1 -> which is higher?

# Social learning (SL):
# - Majority choice -> What did the majority choose? (CB = conformist bias)
# - Choice made by most/more successful agent (in whole population or only in sample or only in ingroup, etc.) (SB = success bias)

# These three different types of information yield the following 8 mappings from combinations of information to choice if choice is binary:
# 0 for CB means, majority has chosen SQ in t-1, and 1 means majority has chosen Alt
# 0 for SB means, most successful agent has chosen SQ in t-1, and 1 means most successful agent has chosen Alt

# First index:    IB
# Second index:   CB
# Third index:    SB

# (0, 0, 0) -> 0
# (0, 0, 0) -> 1
# (0, 0, 1) -> 0
# (0, 0, 1) -> 1
# (0, 1, 0) -> 0
# (0, 1, 0) -> 1
# (0, 1, 1) -> 0
# (0, 1, 1) -> 1
# (1, 0, 0) -> 0
# (1, 0, 0) -> 1
# (1, 0, 1) -> 0
# (1, 0, 1) -> 1
# (1, 1, 0) -> 0
# (1, 1, 0) -> 1
# (1, 1, 1) -> 0
# (1, 1, 1) -> 1

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Simulation function
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

sim <- function(N=100, G=1, n=10, t_max=100, target=1, network_random=1, strat_dist=3,
                n_info_types=3, initial_alt=0, alpha=2, beta=6, 
                phi=0.5, intervention=50, h=0, mu=0.001, 
                a_mean=2, b_mean=1, c_mean=1, d_mean=3, sigma=0.5) {  
  
  info_combinations <- generateInfoCombinations(n_info_types)
  
  result <- generateStrategies(N, strat_dist, alpha, beta, n_info_types)
  # sl_type <- result$sl_type
  strategies <- result$strategies
  
  agents <- data.frame(
    id = 1:N,
    group = rep(NA, N),
    x_i = rep(NA, N),
    respond = rep(0, N),
    q = rep(NA, N),
    exp_sq = rep(NA, N),
    exp_alt = rep(NA, N), 
    IB = rep(NA, N),
    CB = rep(NA, N),
    SB = rep(NA, N),
    strategy = I(strategies),
    # sl_type = sl_type,
    choice = rep(NA, N),
    payoff = rep(NA, N)
  )
  
  # # # Checking the distribution of types of agents:
  # # 
  # # Classify each agent based on their strategy
  # agents$sl_type <- sapply(agents$strategy, classify_agents)
  # 
  # # Calculate the fractions of each type
  # type_counts <- table(agents$sl_type)
  # type_fractions <- type_counts / N
  # 
  # # Print the fractions
  # print(type_fractions)
  
  # Accessing an individual strategy for the first agent, for example
  # print(agents$strategy[[1]])  # Prints the entire strategy vector for the first agent
  # print(agents$strategy[[1]][1])  # Prints the first strategy element of the first agent
  
  # If strat_dist == 4, check whether first index is indeed lower on average across agents than other strategy indeces:
  
  # # Convert the list of strategies to a matrix for easier manipulation
  # strategy_matrix <- do.call(rbind, agents$strategy)
  # 
  # # Calculate the mean for each of the eight indices
  # mean_strategies <- colMeans(strategy_matrix)
  # 
  # # Print the results
  # for (i in 1:8) {
  #   cat(sprintf("Mean probability for strategy index %d: %.4f\n", i, mean_strategies[i]))
  # }
  
  # head(agents)
  
  # # Create based on beta distribution with shape values specified as parameters in function
  x_i <- rbeta(N, shape1 = alpha, shape2 = beta) * ((d_mean - b_mean)-0)
  # assign the x_i values
  agents$x_i <- x_i
  
  # # Activists and conservatives have extreme x_i values
  # agents$x_i[which(agents$sl_type==4)] <- 1
  # agents$x_i[which(agents$sl_type==5)] <- 0
  
  # # # Density plot of x_i values:
  # ggplot(data = data.frame(x = agents$x_i), aes(x)) +
  #   geom_density(trim = TRUE) +  # Use trim = TRUE to limit density within the range of data
  #   labs(title = "Density Plot of x_i", x = "x_i", y = "Density") +
  #   theme_minimal() +
  #   theme(plot.title = element_text(hjust = 0.5))  # Center the title
  
  if (network_random == 0) {
    if (G >= 1) {
      agents$group <- rep(1:G, each = N/G)
    }
  } else {
    if (G == 1) {
      agents$group <- 1
    }
  }
  
  summary_results <- data.frame(
    freq_coord_sq = rep(0, t_max),
    freq_coord_alt = rep(0, t_max),
    miscoordination = rep(0, t_max),
    freq_sq = rep(0, t_max),
    freq_alt = rep(0, t_max),
    avg_payoff_sq = rep(0, t_max),
    avg_payoff_alt = rep(0, t_max),
    avg_payoff = rep(0, t_max),
    gini_coefficient = rep(0, t_max)
  )
  
  strategy_distribution <- as.data.frame(matrix(NA, t_max))
  strategy_distribution$avg_choice_probs <- vector("list", t_max)
  
  ################################################################################
  ################################################################################
  ################################################################################
  # Start for loop
  ################################################################################
  ################################################################################
  ################################################################################
  
  for (t in 1:t_max) {
    
    if (t == 1) { # In the first period, we assume SQ equilibrium. Hence, each agent assigns "0" to every information type.
      for (g in 1:G) {
        group_indices <- which(agents$group == g)
        agents$IB[group_indices] <- 0
        agents$CB[group_indices] <- ifelse(initial_alt >= 0.5, 1, 0)
        agents$SB[group_indices] <- 0
      }
      
    } else if (t == intervention) {
      
      # agents <- sample_fraction(agents,phi,h)
      
      # mean(agents$x_i[agents$respond==1])
      
      # Apply the intervention to the sampled agents
      apply_intervention_to_sampled_agents <- function(agents, strategy_indices) {
        
        sampled_agents <- agents[which(agents$respond==1), ]
        
        # Apply intervention to the sampled agents
        updated_sampled_agents <- apply_intervention(sampled_agents, strategy_indices)
        
        agents$strategy[agents$respond==1] <- updated_sampled_agents$strategy
        
        return(agents)
      }
      
      ################################################################################
      # Apply the intervention
      ################################################################################
      if (target == 0) { # No intervention
        
      } else if (target == 1) { # Introduce activists
        
        agents <- sample_fraction(agents,phi,h)
        
        strategy_indices <- c(1, 2, 3, 4, 5, 6, 7, 8)
        
        agents <- apply_intervention(agents, strategy_indices)
        
      } else if (target == 2) { # Introduce individualists
        
        agents <- sample_fraction(agents,phi,h)
        
        strategy_indices <- c(5, 6, 7, 8)   # Individual bias indices in the strategy array
        
        agents <- apply_intervention(agents, strategy_indices)
        
      } else if (target == 3) { # Introduce conformists
        
        agents <- sample_fraction(agents,phi,h)
        
        strategy_indices <- c(3, 4, 7, 8)
        
        agents <- apply_intervention(agents, strategy_indices)
        
      } else if (target == 4) { # Introduce success oriented
        
        agents <- sample_fraction(agents,phi,h)
        
        strategy_indices <- c(2, 4, 6, 8)
        
        agents <- apply_intervention(agents, strategy_indices)
        
      } else if (target == 5) { # Introduce random learners
        
        agents <- sample_fraction(agents,phi,h)
        
        strategy_indices <- sample(1:8,4)
        
        agents <- apply_intervention(agents, strategy_indices)
        
      }

    }
    
    ###############################################################################
    # Sampling observations
    ###############################################################################
    
    if (t > 1) {
      for (g in 1:G) {
        group_indices <- which(agents$group == g)
        for (agent in group_indices) {
          ###############################################################################  
          # Sampling observations
          ###############################################################################
          sample_indices <- sample(group_indices, n, replace = length(group_indices) <= n)
          sampled_choices <- agents$choice[sample_indices]
          ###############################################################################  
          # IB (individual bias)
          ############################################################################### 
          agents$q[agent] <- mean(sampled_choices)
          agents$exp_sq[agent] <- (1 - agents$q[agent]) * ((a_mean + agents$x_i[agent])) + (agents$q[agent] * (b_mean + agents$x_i[agent]))
          agents$exp_alt[agent] <- (1 - agents$q[agent]) * a_mean + agents$q[agent] * d_mean
          agents$IB[agent] <- ifelse(agents$exp_alt[agent] >= agents$exp_sq[agent], 1, 0)
          ###############################################################################  
          # CB (conformity bias)
          ###############################################################################  
          agents$CB[agent] <- ifelse(agents$q[agent] >= 0.5, 1, 0)
          ###############################################################################  
          # SB (success bias)
          ###############################################################################  
          sampled_payoffs <- agents$payoff[sample_indices]
          max_payoff <- max(sampled_payoffs)
          most_successful_indices <- sample_indices[sampled_payoffs == max_payoff]
          most_successful_idx <- ifelse(length(most_successful_indices) > 1, sample(most_successful_indices, 1), most_successful_indices)
          agents$SB[agent] <- agents$choice[most_successful_idx]
        }
      }
    } 
    
    # previous_agents <- agents
    
    ###############################################################################
    # Deriving choices based on observations and strategies
    ###############################################################################
    
    make_choice <- function(i, agents, mu) {
      index <- sum(agents[i, c("IB", "CB", "SB")] * 2^(rev(seq_along(agents[i, c("IB", "CB", "SB")])) - 1)) + 1
      prob <- agents$strategy[[i]][index]
      
      # Generate random values outside the loop
      r1 <- runif(1)
      r2 <- runif(1)
      
      if (r1 <= prob) {
        if (r2 >= mu) {
          return(1)
        } else {
          return(0)
        }
      } else {
        if (r2 >= mu) {
          return(0)
        } else {
          return(1)
        }
      }
    }
    
    # Apply the make_choice function to all agents
    agents$choice <- sapply(1:N, function(i) make_choice(i, agents, mu))
    
    ###############################################################################
    # Coordination game
    ###############################################################################
    
    cumulative_freq_coord_sq <- 0
    cumulative_freq_coord_alt <- 0
    cumulative_miscoordination <- 0
    
    for (g in 1:G) {
      group_agents_indices <- which(agents$group == g)
      shuffled_indices <- sample(group_agents_indices)
      player_1_indices <- shuffled_indices[1:(length(shuffled_indices)/2)]
      player_2_indices <- shuffled_indices[(length(shuffled_indices)/2 + 1):length(shuffled_indices)]
      
      choice_player_1 <- agents$choice[player_1_indices]
      choice_player_2 <- agents$choice[player_2_indices]
      
      both_sq <- choice_player_1 == 0 & choice_player_2 == 0
      both_alt <- choice_player_1 == 1 & choice_player_2 == 1
      
      freq_coord_sq <- sum(both_sq)
      freq_coord_alt <- sum(both_alt)
      miscoordination <- ((length(group_agents_indices)) / 2) - freq_coord_sq - freq_coord_alt
      
      cumulative_freq_coord_sq <- cumulative_freq_coord_sq + freq_coord_sq
      cumulative_freq_coord_alt <- cumulative_freq_coord_alt + freq_coord_alt
      cumulative_miscoordination <- cumulative_miscoordination + miscoordination
      
      # Assign payoffs for coordination on SQ
      agents$payoff[player_1_indices][both_sq] <- a_mean + agents$x_i[player_1_indices][both_sq]
      agents$payoff[player_2_indices][both_sq] <- a_mean + agents$x_i[player_2_indices][both_sq]
      
      # Assign payoffs for coordination on Alt
      agents$payoff[player_1_indices][both_alt] <- d_mean
      agents$payoff[player_2_indices][both_alt] <- d_mean
      
      # For miscoordinated pairs, handle the mismatched choices
      mis_sq_alt <- choice_player_1 == 0 & choice_player_2 == 1
      mis_alt_sq <- choice_player_1 == 1 & choice_player_2 == 0
      
      agents$payoff[player_1_indices][mis_sq_alt] <- b_mean + agents$x_i[player_1_indices][mis_sq_alt]
      agents$payoff[player_2_indices][mis_sq_alt] <- a_mean
      
      agents$payoff[player_1_indices][mis_alt_sq] <- a_mean
      agents$payoff[player_2_indices][mis_alt_sq] <- b_mean + agents$x_i[player_1_indices][mis_alt_sq]
    }
    
    avg_probs_at_t <- colMeans(do.call(rbind, agents$strategy))
    strategy_distribution$avg_choice_probs[[t]] <- avg_probs_at_t
    
    summary_results$freq_coord_sq[t] <- cumulative_freq_coord_sq
    summary_results$freq_coord_alt[t] <- cumulative_freq_coord_alt
    summary_results$miscoordination[t] <- cumulative_miscoordination
    
    summary_results$freq_sq[t] <- length(agents[which(agents$choice == 0), "choice"])
    summary_results$freq_alt[t] <- length(agents[which(agents$choice == 1), "choice"])
    
    num_agents_sq <- length(agents[which(agents$choice == 0), "choice"])
    summary_results$avg_payoff_sq[t] <- ifelse(num_agents_sq > 0, sum(agents[which(agents$choice == 0), "payoff"]) / num_agents_sq, NA)
    
    num_agents_alt <- length(agents[which(agents$choice == 1), "choice"])
    summary_results$avg_payoff_alt[t] <- ifelse(num_agents_alt > 0, sum(agents[which(agents$choice == 1), "payoff"]) / num_agents_alt, NA)
    
    summary_results$avg_payoff[t] <- sum(agents[, "payoff"]) / N
    summary_results$gini_coefficient[t] <- my_gini(agents[, "payoff"])
    
  }
  
  return(list(summary_results = summary_results, strategy_distribution = strategy_distribution,
              N = N, G = G, t_max = t_max, n = n, mu = mu, alpha = alpha, beta = beta, network_random = network_random, strat_dist = strat_dist,
              target = target, phi = phi, a_mean = a_mean, b_mean = b_mean, c_mean = c_mean, d_mean = d_mean, sigma = sigma, intervention = intervention))
}

# Parameter values
param_combinations <- expand.grid(
  N = c(100),
  G = c(1),
  n = c(10),
  t_max = c(100),
  strat_dist = c(8),
  network_random = c(1),
  n_info_types = c(3),
  initial_alt = c(0),
  alpha = c(8),
  beta = c(2),
  target = c(0,1,2,3,4,5),
  phi = c(0.2),
  a_mean = c(0.75),
  b_mean = c(1),
  c_mean = c(0),
  d_mean = c(2),
  sigma = c(0),
  intervention = c(50),
  h = c(2),
  mu = c(0.001)
)

n_sim <- 1000
# num_cores <- detectCores() - 1


results_list <- list()  # to store results for each parameter combination

# Measure the time it takes to run the simulations
start_time <- Sys.time()


# Go through all the different parameter combinations
results_list <- mclapply(1:nrow(param_combinations), function(i) {
  
  current_params <- param_combinations[i, ]
  
  t_max <- current_params$t_max
  
  avg_summary_results <- data.frame(freq_coord_sq=rep(0,t_max), 
                                    low_ci_freq_coord_sq=rep(0,t_max),
                                    high_ci_freq_coord_sq=rep(0,t_max),
                                    freq_coord_alt=rep(0,t_max),
                                    low_ci_freq_coord_alt=rep(0,t_max),
                                    high_ci_freq_coord_alt=rep(0,t_max),
                                    miscoordination=rep(0,t_max),
                                    low_ci_miscoordination=rep(0,t_max),
                                    high_ci_miscoordination=rep(0,t_max),
                                    freq_sq=rep(0,t_max),
                                    low_ci_freq_sq=rep(0,t_max),
                                    high_ci_freq_sq=rep(0,t_max),
                                    freq_alt=rep(0,t_max),
                                    low_ci_freq_alt=rep(0,t_max),
                                    high_ci_freq_alt=rep(0,t_max),
                                    avg_payoff_sq=rep(0,t_max),
                                    avg_payoff_alt=rep(0,t_max),
                                    avg_payoff=rep(0,t_max),
                                    low_ci_avg_payoff=rep(0,t_max),
                                    high_ci_avg_payoff=rep(0,t_max),
                                    gini_coefficient=rep(0,t_max),
                                    low_ci_gini_coefficient=rep(0,t_max),
                                    high_ci_gini_coefficient=rep(0,t_max))
  
  freq_coord_sq_n_sim <- matrix(0,t_max,n_sim)
  freq_coord_alt_n_sim <- matrix(0,t_max,n_sim)
  miscoordination_n_sim <- matrix(0,t_max,n_sim)
  freq_sq_n_sim <- matrix(0,t_max,n_sim)
  freq_alt_n_sim <- matrix(0,t_max,n_sim)
  avg_payoff_n_sim <- matrix(0,t_max,n_sim)
  gini_coefficient_n_sim <- matrix(0,t_max,n_sim)
  
  # Initialize list to aggregate strategy distributions across all simulations
  strategy_distribution_all_sims <- vector("list", current_params$t_max)
  for (t in 1:current_params$t_max) {
    strategy_distribution_all_sims[[t]] <- list()
  }
  
  
  # Run n_sim simulations:
  for (j in 1:n_sim) {
    
    results <- sim(N=current_params$N, 
                   G=current_params$G, 
                   n=current_params$n, 
                   t_max=current_params$t_max,
                   strat_dist=current_params$strat_dist,
                   network_random=current_params$network_random, 
                   n_info_types=current_params$n_info_types, 
                   initial_alt=current_params$initial_alt,
                   alpha=current_params$alpha,
                   beta=current_params$beta,
                   target=current_params$target, 
                   phi=current_params$phi, 
                   a_mean=current_params$a_mean, 
                   b_mean=current_params$b_mean,
                   c_mean=current_params$c_mean,
                   d_mean=current_params$d_mean,
                   sigma=current_params$sigma,
                   intervention=current_params$intervention,
                   h=current_params$h,
                   mu=current_params$mu
    )
    
    N <- current_params$N
    G <- current_params$G 
    n <- current_params$n 
    t_max <- current_params$t_max
    strat_dist <- current_params$strat_dist
    network_random <- current_params$network_random
    n_info_types <- current_params$n_info_types 
    initial_alt <- current_params$initial_alt
    alpha <- current_params$alpha
    beta <- current_params$beta
    target <- current_params$target 
    phi <- current_params$phi 
    a_mean <- current_params$a_mean
    b_mean <- current_params$b_mean
    c_mean <- current_params$c_mean
    d_mean <- current_params$d_mean
    sigma <- current_params$sigma
    intervention <- current_params$intervention
    h <- current_params$h
    mu <- current_params$mu
    
    # Record following measures for all n_sim simulation runs
    freq_sq_n_sim[,j] <- results$summary_results$freq_sq
    freq_alt_n_sim[,j] <- results$summary_results$freq_alt
    freq_coord_sq_n_sim[,j] <- results$summary_results$freq_coord_sq
    freq_coord_alt_n_sim[,j] <- results$summary_results$freq_coord_alt
    miscoordination_n_sim[,j] <- results$summary_results$miscoordination
    avg_payoff_n_sim[,j] <- results$summary_results$avg_payoff
    gini_coefficient_n_sim[,j] <- results$summary_results$gini_coefficient
    
    # Store strategy distributions for this simulation
    for (t in 1:current_params$t_max) {
      strategy_distribution_all_sims[[t]][[j]] <- results$strategy_distribution$avg_choice_probs[[t]]
    }
    
  }
  
  # After running n_sim simulations for a given parameter combination, we take
  # averages of all the measures:
  
  # Aggregate strategy distributions across simulations
  avg_choice_probs <- vector("list", current_params$t_max)
  for (t in 1:current_params$t_max) {
    matrix_data <- do.call(rbind, strategy_distribution_all_sims[[t]])
    avg_choice_probs[[t]] <- colMeans(matrix_data, na.rm = TRUE)  # handle NA values
  }
  
  # Other measures:
  avg_summary_results$freq_coord_sq <- rowMeans(freq_coord_sq_n_sim)
  avg_summary_results$freq_coord_alt <- rowMeans(freq_coord_alt_n_sim)
  avg_summary_results$miscoordination <- rowMeans(miscoordination_n_sim)
  avg_summary_results$freq_sq <- rowMeans(freq_sq_n_sim)
  avg_summary_results$freq_alt <- rowMeans(freq_alt_n_sim)
  avg_summary_results$avg_payoff <- rowMeans(avg_payoff_n_sim)
  avg_summary_results$gini_coefficient <- rowMeans(gini_coefficient_n_sim)
  
  # Now that we have the mean values, averaged over the n_sim simulation runs, 
  # we can calculate the bootstrapped confidence intervals:
  
  all_measures <- list()
  
  all_measures[[1]] <- freq_sq_n_sim
  all_measures[[2]] <- freq_alt_n_sim
  all_measures[[3]] <- freq_coord_sq_n_sim
  all_measures[[4]] <- freq_coord_alt_n_sim
  all_measures[[5]] <- miscoordination_n_sim
  all_measures[[6]] <- avg_payoff_n_sim
  all_measures[[7]] <- gini_coefficient_n_sim
  
  all_ci_values <- array(0, dim = c(t_max, 2, length(all_measures))) 
  
  # rename columns
  dimnames(all_ci_values) <- list(
    period = 1:t_max,
    ci_values = c("lower_bound", "upper_bound"),
    measures = c("freq_sq_n_sim", "freq_alt_n_sim", "freq_coord_sq_n_sim", "freq_coord_alt_n_sim", "miscoordination_n_sim", "avg_payoff_n_sim", "gini_coefficient_n_sim")
  )
  
  for (k in 1:length(all_measures)) {
    
    # Number of bootstrap replicates
    R <- 999
    
    # Create an empty matrix to store the confidence intervals
    conf_intervals <- matrix(NA, nrow = t_max, ncol = 2)
    
    for (period in 1:t_max) {
      # Go through the "all_measures" list and for each measure, extract the
      # data for current period
      data_to_bootstrap <- all_measures[[k]][period, ]
      
      # Initialize an empty vector to store bootstrap means
      bootstrap_means <- numeric(R)
      
      # Perform bootstrapping R times
      for (l in 1:R) {
        # Resample the data with replacement
        resampled_data <- sample(data_to_bootstrap, replace = TRUE)
        
        # Calculate the mean for the resampled data
        bootstrap_means[l] <- mean(resampled_data)
      }
      
      mean_and_boot <- rep(0,1+R)
      
      mean_and_boot[1:R] <- bootstrap_means
      mean_and_boot[R+1] <- rowMeans(all_measures[[k]])[period]
      
      ordered_means <- sort(mean_and_boot)
      
      index_low <- floor(0.025 * length(ordered_means))
      lower_bound <- ordered_means[index_low]
      
      index_high <- ceiling(0.975 * length(ordered_means))
      upper_bound <- ordered_means[index_high]
      
      # Store the confidence interval in the "all_ci_values" matrix
      all_ci_values[period,1,k] <- lower_bound
      all_ci_values[period,2,k] <- upper_bound
      
    }
    
    # print(all_ci_values)
    
  }  
  
  # Record confidence intervals in avg_summary_results data frame
  avg_summary_results$low_ci_freq_sq <- all_ci_values[,"lower_bound","freq_sq_n_sim"]
  avg_summary_results$high_ci_freq_sq <- all_ci_values[,"upper_bound","freq_sq_n_sim"]
  avg_summary_results$low_ci_freq_alt <- all_ci_values[,"lower_bound","freq_alt_n_sim"]
  avg_summary_results$high_ci_freq_alt <- all_ci_values[,"upper_bound","freq_alt_n_sim"]
  avg_summary_results$low_ci_freq_coord_sq <- all_ci_values[,"lower_bound","freq_coord_sq_n_sim"]
  avg_summary_results$high_ci_freq_coord_sq <- all_ci_values[,"upper_bound","freq_coord_sq_n_sim"]
  avg_summary_results$low_ci_freq_coord_alt <- all_ci_values[,"lower_bound","freq_coord_alt_n_sim"]
  avg_summary_results$high_ci_freq_coord_alt <- all_ci_values[,"upper_bound","freq_coord_alt_n_sim"]
  avg_summary_results$low_ci_miscoordination <- all_ci_values[,"lower_bound","miscoordination_n_sim"]
  avg_summary_results$high_ci_miscoordination <- all_ci_values[,"upper_bound","miscoordination_n_sim"]
  avg_summary_results$low_ci_avg_payoff <- all_ci_values[,"lower_bound","avg_payoff_n_sim"]
  avg_summary_results$high_ci_avg_payoff <- all_ci_values[,"upper_bound","avg_payoff_n_sim"]
  avg_summary_results$low_ci_gini_coefficient <- all_ci_values[,"lower_bound","gini_coefficient_n_sim"] 
  avg_summary_results$high_ci_gini_coefficient <- all_ci_values[,"upper_bound","gini_coefficient_n_sim"] 
  
  # Store averaged results and parameter values of given parameter combination in results_list:
  
  return(list(summary_results = avg_summary_results, strategy_distribution = strategy_distribution_all_sims,
              avg_choice_probs = avg_choice_probs, n_sim=n_sim,
              n_info_types=n_info_types, initial_alt=initial_alt, 
              N = N, G=G, t_max = t_max, n = n, mu=mu, alpha = alpha, beta = beta, network_random=network_random, strat_dist=strat_dist,
              target = target, phi = phi, a_mean = a_mean, b_mean = b_mean, c_mean = c_mean, d_mean = d_mean, sigma = sigma, h = h, intervention=intervention))
}, mc.cores = num_cores)

################################################################################
end_time <- Sys.time()

# Calculate the elapsed time
elapsed_time <- end_time - start_time
print(paste("Total time taken:", elapsed_time))

################################################################################
################################################################################
# Create names for files according to parameter combinations:
name_combination <- function(row) {
  paste0("N", row["N"],
         "G", row["G"],
         "n", row["n"],
         "t_max", row["t_max"],
         "strat_dist", row["strat_dist"],
         "network_random", row["network_random"],
         "n_info_types", row["n_info_types"],
         "initial_alt", row["initial_alt"],
         "alpha_", row["alpha"],
         "beta_", row["beta"],
         "_target_", row["target"], 
         "_phi_", row["phi"], 
         "_a_mean_", row["a_mean"],
         "_b_mean_", row["b_mean"],
         "_c_mean_", row["c_mean"],
         "_d_mean_", row["d_mean"],
         "_sigma_", row["sigma"],
         "intervention_", row["intervention"],
         "_h_", row["h"],
         "_mu_", row["mu"])
}

names(results_list) <- apply(param_combinations, 1, name_combination)

# Replace '.' and '_' with '' (remove them)
cleaned_names <- lapply(names(results_list), function(name) {
  return(gsub("[._]", "", name))
})

cleaned_names <- unlist(cleaned_names)

# get current working directory
results_dir <- getwd() 

# Iterate through the results_list and save each result separately in 
# subdirectory corresponding to parameter combinations:
for(i in 1:length(results_list)) {
  # Name for the subdirectory (and the file) based on the names of results_list
  folder_and_file_name <- cleaned_names[i]
  
  # Check if directory exists and create if it doesn't
  if (!dir.exists(folder_and_file_name)) {
    dir.create(folder_and_file_name)
  }
  
  # File name with .RData extension
  file_name <- paste0(folder_and_file_name, ".RData")
  
  # Full path to where the file will be saved (inside the subdirectory)
  file_path <- file.path(results_dir, folder_and_file_name, file_name)
  
  # Convert list item to an environment
  e <- list2env(list(result = results_list[[i]]))
  
  # save the environment
  save(list = "result", envir = e, file = file_path)
}

################################################################################
################################################################################

# session information
xfun::session_info()

# cite R
citation()


