#creating data sets for figures 3a and 3c

#epidemic 1977-78
counts_1977 <- matrix(
  c(
    66, 87, 25, 22, 4,    # 0 infected
    13, 14, 15, 9, 4,     # 1 infected
    4,  4,  9, 1, 0,      # 2 infected
    4,  3,  1, 3, 0,      # 3 infected
    1,  1,  1, 0, 0,      # 4 infected
    0,  1,  0, 0, 0       # 5 infected
  ),
  nrow = 6, byrow = TRUE
)
rownames(counts_1977) <- 0:5  # infected
colnames(counts_1977) <- 1:5  # household size

#epidemic 1980-81
counts_1980 <- matrix(
  c(
    44, 62, 47, 38, 9,     # 0 infected
    10, 13, 8,  11, 5,     # 1 infected
    9,  2,  7,  3,  0,     # 2 infected
    3,  5,  1,  0,  0,     # 3 infected
    1,  0,  0,  0,  0,     # 4 infected
    0,  0,  0,  1,  0      # 5 infected
  ),
  nrow = 6, byrow = TRUE
)
rownames(counts_1980) <- 0:5
colnames(counts_1980) <- 1:5

convert_to_df <- function(count_matrix, epidemic_name) {
  df <- data.frame(epidemic = character(),
                   household_size = numeric(),
                   infected = numeric(),
                   stringsAsFactors = FALSE)
  
  for (inf in 0:5) {
    for (size in 1:5) {
      n_households <- count_matrix[as.character(inf), as.character(size)]
      if (n_households > 0) {
        temp <- data.frame(
          epidemic = epidemic_name,
          household_size = rep(size, n_households),
          infected = rep(inf, n_households)
        )
        df <- rbind(df, temp)
      }
    }
  }
  return(df)
}

#convert both epidemics
observed_1977 <- convert_to_df(counts_1977, "1977-78")
observed_1980 <- convert_to_df(counts_1980, "1980-81")

#combine into one data set
observed_data <- rbind(observed_1977, observed_1980)


head(observed_data)
table(observed_data$epidemic, observed_data$household_size)
table(observed_data$epidemic, observed_data$infected)

convert_to_df <- function(count_matrix, epidemic_name) {
  df <- data.frame(epidemic = character(),
                   household_size = numeric(),
                   infected = numeric(),
                   stringsAsFactors = FALSE)
  
  for (inf in 0:5) {
    for (size in 1:5) {
      n_households <- count_matrix[as.character(inf), as.character(size)]
      if (n_households > 0) {
        temp <- data.frame(
          epidemic = epidemic_name,
          household_size = rep(size, n_households),
          infected = rep(inf, n_households)
        )
        df <- rbind(df, temp)
      }
    }
  }
  return(df)
}

#convert both epidemics
observed_1977 <- convert_to_df(counts_1977, "1977-78")
observed_1980 <- convert_to_df(counts_1980, "1980-81")

#combine into one data set
observed_data <- rbind(observed_1977, observed_1980)

#checking
head(observed_data)
table(observed_data$epidemic, observed_data$household_size)
table(observed_data$epidemic, observed_data$infected)

write.csv(observed_data, "fig3a.csv", row.names = FALSE)
