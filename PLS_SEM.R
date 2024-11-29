library(plspm)
library(ggplot2)

# Load data from the Excel file
df <- PLS_SEM

# Define the constructs
HumanInfluence <- c(0, 0, 0, 0, 0, 0, 0)
Ncycle <- c(0, 0, 0, 0, 0, 0, 0)
ROSproducing <- c(0, 0, 0, 0, 0, 0, 0)
ROSconsuming <- c(0, 0, 0, 0, 0, 0, 0)
SoilProperties <- c(0, 0, 0, 0, 0, 0, 0)
SoilProcesses <- c(0, 0, 0, 0, 0, 0, 0)
MeanNOyflux <- c(1, 1, 1, 1, 1, 1, 0)

# Define the path matrix
path_matrix = rbind(HumanInfluence, Ncycle, ROSproducing, ROSconsuming, SoilProperties, SoilProcesses, MeanNOyflux)

# Define the blocks (adjust the indices based on the column order)
blocks = list(
  c(1),  # HumanInfluence
  c(2:10),  # Ncycle
  c(11:13),  # ROSproducing
  c(14:15),  # ROSconsuming
  c(16:23),  # SoilProperties
  c(24:27),  # SoilProcesses
  c(28)  # MeanNOyflux
)

# Define the modes
some_modes = c("A", "A", "A", "A", "A", "A", "B")

# Define the scaling
example_scaling = list(
  c("NUM"), 
  c("NUM", "NUM", "NUM", "NUM", "NUM", "NUM", "NUM", "NUM", "NUM"), 
  c("NUM", "NUM", "NUM"), 
  c("NUM", "NUM"), 
  c("NUM", "NUM", "NUM", "NUM", "NUM", "NUM", "NUM", "NUM"), 
  c("NUM", "NUM", "NUM", "NUM"), 
  c("NUM")
)

# Run the PLS-PM analysis
neof_pls = plspm(df, path_matrix, blocks, modes = some_modes, scaling = example_scaling)

# View the results
summary(neof_pls)

plot(neof_pls)

boot_results <- plspm(df, path_matrix, blocks, boot.val = TRUE, br = 1000)

# Summary of bootstrap results
summary(boot_results$boot)
boot_paths <- boot_results$boot$paths

conf_intervals_list <- list()

# Calculate the 95% confidence intervals for each path coefficient
for (i in 1:ncol(boot_paths)) {
  conf_intervals_list[[colnames(boot_paths)[i]]] <- quantile(boot_paths[, i], probs = c(0.025, 0.975))
}

# Convert the list to a data frame for better readability
conf_intervals_df <- do.call(rbind, conf_intervals_list)
colnames(conf_intervals_df) <- c("2.5%", "97.5%")

# Print the confidence intervals for each path
print(conf_intervals_df)


# Extract the original path coefficients
original_paths <- neof_pls$path_coefs
path_names <- rownames(neof_pls$path_coefs)

# Extract the bootstrapped path coefficients
boot_paths <- boot_results$boot$paths

# Check the structure of the boot_paths to ensure it matches expectations
str(boot_paths)

# Initialize a vector to store p-values for each path
p_values <- numeric(length(original_paths))

# Calculate p-values for each path coefficient
for (i in 1:length(original_paths)) {
  # Get the bootstrapped coefficients for the current path
  boot_coefs <- boot_paths[, i]
  
  # Get the original coefficient for the current path
  orig_coef <- original_paths[i, 1]
  
  # Calculate the proportion of bootstrapped coefficients that are more extreme than the original coefficient
  p_values[i] <- mean(abs(boot_coefs) >= abs(orig_coef))
}

# Create a data frame with the results
results_df <- data.frame(
  Path = path_names,
  Original_Coefficient = as.vector(original_paths),
  P_Value = p_values
)

# Print the results
print(results_df)




boot_results <- plspm(df, path_matrix, blocks, boot.val = TRUE, br = 500)

# Extract bootstrapped path coefficients
boot_paths <- boot_results$boot$paths

# Extract the original path coefficients
original_paths <- as.vector(neof_pls$path_coefs)
path_names <- rownames(neof_pls$path_coefs)

# Initialize vector to store p-values
p_values <- numeric(length(original_paths))

# Calculate p-values for each path coefficient
for (i in 1:length(original_paths)) {
  # Get the bootstrapped coefficients for the current path
  boot_coefs <- boot_paths[, i]  # Ensure [, i] correctly accesses the i-th column
  
  # Get the original coefficient for the current path
  orig_coef <- original_paths[i]
  
  # Calculate the proportion of bootstrapped coefficients that are more extreme than the original coefficient
  if (orig_coef >= 0) {
    p_values[i] <- mean(boot_coefs >= orig_coef)
  } else {
    p_values[i] <- mean(boot_coefs <= orig_coef)
  }
}

# Adjust p-values for two-tailed test
p_values <- p_values * 2

# Create a data frame with results
results_df <- data.frame(
  Path = path_names,
  Original_Coefficient = original_paths,
  P_Value = p_values
)

# Print results
print(results_df)

