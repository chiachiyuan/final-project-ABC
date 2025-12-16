## Simulation
# simulate one realization of an outbreak in a single household
# Input: Household size n and immune parameters qc & qh
# Output: The total number of people who eventually got sick (total_infected)

# Defining the Model Parameters
# n (Household Size): How many people live in this house
# "q" stands for the probability of avoiding (escaping) infection
# qh (Household Escape Probability): The probability a susceptible person avoids infection from one infected household member.
# qc (Community Escape Probability): The probability a susceptible person avoids infection from the community.
# max_gen: The maximum number of "generations" (waves of infection) allowed.

# Initialization
# S (Susceptible): We start assuming everyone in the house is healthy but vulnerable (n people)
# I (Infected): Initially, zero people are infected inside the house
# total_infected: A counter to track the final outbreak size.

simulate_household <- function(n, qh, qc, max_gen = 10) {
  S <- n
  I <- 0
  total_infected <- 0
  
  # Generation 0: Infection enters from the community
  # Logic: The infection must start somewhere.
  # This first step simulates household members catching the virus from the "community" (outside world).
  # Since qc = Probability of escaping infection from the community
  # 1−qc = Probability of getting infected from the community
  
  # 1: We are running this experiment 1 time for this household.
  # S: The number of trials (people in the house).
  # 1 - qc: The probability of "success" (getting infected) for each person.
  # initial_cases: The number of people who brought the virus home
  initial_cases <- rbinom(1, S, 1 - qc)
  I <- initial_cases
  S <- S - initial_cases # We move the newly infected people from S (Healthy) to I (Infected).
  total_infected <- total_infected + initial_cases
  
  # Generation 1+ (Household Transmission Loop)
  # The simulation continues as long as: there are infectious people (I > 0) & there are still healthy people to infect (S > 0)
  # Assume the simulation continues in discrete generations
  # assume people infected in generation t are infectious only for that generation
  # In generation t+1, they are recovered, and the newly infected people become the spreaders
  gen <- 1
  while (gen <= max_gen && I > 0 && S > 0) {
    # Generation 1+: Infection spreads inside the house based on how many people are already sick
    # Probability of escaping infection from household contacts: qh^I
    # Logic: You are a healthy person. There are I sick people in the house. 
    # Infection attempts are independent. To stay safe, you must escape Person 1 & Person 2 & ...
    # p_escape_house = qh * qh * qh.....
    p_escape_house <- qh^I
    # We assume community infection is the initial seed (Generation 0)
    # Subsequent generations are driven by household transmission
    # p_escape_total: The total chance a susceptible person survives this wave without getting sick
    # p_infect: The probability they fail to escape (get sick)
    p_escape_total <- p_escape_house 
    p_infect <- 1 - p_escape_total
    # Simulating New Infections
    # S: remaining healthy people
    new_inf <- rbinom(1, S, p_infect) 
    I <- new_inf # The newly sick become the infectors for the next round
    S <- S - new_inf # Remove them from the healthy pool
    total_infected <- total_infected + new_inf # Add to total count
    gen <- gen + 1 # Advance time
  }
  return(total_infected)
}

# We use this function in ABC Algorithm to see if a specific pair of (qc,qh) produces "Total Infected" counts that match the real history. 
# If the simulation matches the data, the parameters are accepted as plausible.


#-------------------------------------------------------------------------------
## ABC Algorithm
# Prior Distribution
# Assume we know nothing else about qc and qh, so every value between 0 and 1 is equally likely (Uniform)
# Uniform(0,1) for both parameters
# The first number represents qc: Community escape probability
# The second number represents qh: Household escape probability
prior_distribution <- function() {
  return(runif(2, 0, 1))
}

# generate one valid sample (one accepted pair of qc,qh) from the posterior distribution
generate_abc_sample <- function(observed_data_vector,
                                summary_statistic_fn,
                                prior_distribution_fn,
                                data_generating_fn,
                                epsilon) {
  # keep guessing parameters and simulating data until it finds a result that is "good enough"
  # makes a random guess for the parameters based on the Prior Distribution (it picks two random numbers between 0 and 1)
  # Goal: To generate one valid particle (a pair of parameters qc,qh) that fits the data
  while(TRUE) {
    # Sample Parameters
    theta <- prior_distribution_fn() # Returns c(qc, qh)
    qc_val <- theta[1] # The specific numerical value for qc
    qh_val <- theta[2] # The specific numerical value for qh
    # Simulate Data: use the guessed parameters to create a fake epidemic history
    y <- data_generating_fn(qc_val, qh_val)
    # Calculate stats: take the simulated data y and formats it to match the format of the real data 
    stat_sim <- summary_statistic_fn(y)
    stat_obs <- observed_data_vector
    # Compute Distance: the Euclidean Distance between the real data and the fake data
    # (Simulated−Real)^2: Square the differences to make negatives positive
    # Add them all up, and take the square root
    dist <- sqrt(sum((stat_sim - stat_obs)^2))
    # High Distance: The simulation looks nothing like reality. The guess was bad.
    # Low Distance: The simulation looks very similar to reality. The guess was good.
    # If the distance is smaller than the allowed error (epsilon), we accepct the parameters and return them
    # If the distance is larger, goes back to while(TRUE), and tries again with a new random guess
    if(dist < epsilon) {
      # Return valid parameter set
      return(theta) 
    }
  }
}

#-------------------------------------------------------------------------------
## Run Analysis
set.seed(42) #ensures reproducibility
# target_matrix: The real data we are trying to match
# n_samples: The target size of the posterior distribution
# epsilon: The error tolerance
abc_analysis <- function(target_matrix, n_samples=1000, epsilon=20) {
  # Capture Population Structure: replicate the exact same community structure to make a fair comparison
  local_hh_sizes <- get_household_sizes(target_matrix)
  # Simulate for every household in the list
  gen_func <- function(qc, qh) {
    sapply(local_hh_sizes, simulate_household, qh=qh, qc=qc)
  }
  obs_stats <- as.vector(target_matrix)
  # forces the simulated data into the exact same shape
  # Define Summary Statistic
  sum_stat_func <- function(sim_data) {
    tab <- table(
      factor(sim_data, levels = 0:(nrow(target_matrix)-1)),
      factor(local_hh_sizes, levels = 1:ncol(target_matrix))
    )
    return(as.vector(tab))
  }
  
#------------------------------------------------------------------------------- 
  # Run ABC
  # find one valid pair of (qc, qh) that fits the data
  # repeats this process n times
  # generates the posterior distribution
  posterior_results <- replicate(n = n_samples, 
                                 generate_abc_sample(
                                   observed_data_vector = obs_stats,
                                   summary_statistic_fn = sum_stat_func,
                                   prior_distribution_fn = prior_distribution,
                                   data_generating_fn = gen_func,
                                   epsilon = epsilon
                                 ))
  # Transpose to data frame.
  df <- data.frame(t(posterior_results))
  colnames(df) <- c("qc", "qh")
  return(df)
}

#-------------------------------------------------------------------------------
## Execution
# Figure 3a: Tecumseh
# Data is larger, so Distance will be larger.
# Tecumseh 1977-78
hh_sizes_global <- get_household_sizes(counts_1977) #counts exactly how many households of size 1, 2, 3...
posterior_1977 <- abc_analysis(counts_1977, n_samples=1000, epsilon=20)

# Tecumseh 1980-81
hh_sizes_global <- get_household_sizes(counts_1980)
posterior_1980 <- abc_analysis(counts_1980, n_samples=1000, epsilon=20)

# Figure 3c: Seattle
# Data is smaller, so Distance will be smaller.
# Seattle 1975-76
hh_sizes_global <- get_household_sizes(counts_1975)
posterior_1975 <- abc_analysis(counts_1975, n_samples=1000, epsilon=10)

# Seattle 1978-79
hh_sizes_global <- get_household_sizes(counts_1978)
posterior_1978 <- abc_analysis(counts_1978, n_samples=1000, epsilon=10)

#-------------------------------------------------------------------------------
## Plotting
# X-Axis (qh):
# Left (Low qh): High transmission in the household (hard to escape)
# Right (High qh): Low transmission in the household (easy to escape)
# Y-Axis (qc)
# Bottom (Low qc): High transmission from the community
# Top (High qc): Low transmission from the community

par(mfrow=c(1,2))
# Plot 1: Tecumseh
plot(posterior_1977$qh, posterior_1977$qc, 
     xlim=c(0,1), ylim=c(0,1),
     xlab="Household escape prob (qh)", ylab="Community escape prob (qc)",
     main="Tecumseh (Fig 3a)",
     pch=16, col=rgb(0,0,1,0.5)) # Blue
points(posterior_1980$qh, posterior_1980$qc, 
       pch=16, col=rgb(1,0,0,0.5)) # Red
legend("bottomleft", legend=c("1977-78", "1980-81"), 
       col=c("blue", "red"), pch=16, bty="n")

# Plot 2: Seattle
plot(posterior_1975$qh, posterior_1975$qc, 
     xlim=c(0,1), ylim=c(0,1), # Force the axes to go from 0 to 1
     xlab="Household escape prob (qh)", ylab="Community escape prob (qc)",
     main="Seattle (Fig 3c)",
     pch=16, col=rgb(0,0,1,0.5)) # Blue
points(posterior_1978$qh, posterior_1978$qc, 
       pch=16, col=rgb(1,0,0,0.5)) # Red
legend("bottomleft", legend=c("1975-76", "1978-79"), 
       col=c("blue", "red"), pch=16, bty="n")

