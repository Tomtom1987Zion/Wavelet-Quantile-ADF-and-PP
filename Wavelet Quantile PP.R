##=======Wavelet Quantile Phillips-Perron (Wavelet QPP)  ========================##

##=======Authors : Tomiwa Sunday ADEBAYO and Victoria Olushola OLANREWAJU====##

##=======Title: Journey toward affordable and modern energy: Role of income inequality and technological innovation====## 

##=======DOI: 10.1002/ep.14555====##

rm(list=ls(all=TRUE))

# Load necessary libraries
library(readxl)
library(urca)
library(parallel)
library(waveslim)  # For wavelet analysis
library(ggplot2)
library(ggthemes)  # For additional themes and color palettes
options(warn=-1)
options(mc.cores=detectCores())

setwd("C:/Users/Admin/Desktop/Collaboration/UNITED KINGDOM PAPERS/Wavelet Quantile ADF and PP")

# Load the data from the Excel file
dataset <- read_excel("DATA.xlsx")


# Attach the dataset for easier variable access
attach(dataset)

# Define the sequence of quantiles to be tested
tau <- seq(from = 0.05, to = 0.95, by = 0.05)

# Define a function to determine the maximum possible decomposition level
max_decomp_level <- function(series_length, max_J = 5) {
  J <- floor(log2(series_length))
  if (J > max_J) {
    J <- max_J
  }
  return(max(1, J))  # Ensure J is at least 1
}

# Define a function to perform wavelet decomposition with an appropriate level
wavelet_decompose <- function(series) {
  J <- max_decomp_level(length(series))
  return(as.data.frame(mra(series, wf = "la8", J = J, method = "modwt", boundary = "periodic")))
}

# Define a function to perform quantile PP test
quantile_pp_test <- function(y, tau) {
  results <- numeric(length(tau))
  
  for (i in 1:length(tau)) {
    ytau <- quantile(y, probs = tau[i])
    psiy <- rep(tau[i], length(y)) - as.numeric(y - ytau < 0)
    
    result <- ur.pp(psiy, type="Z-tau", model="constant")
    results[i] <- result@teststat[1]
  }
  
  return(results)
}

# Define a function to perform Wavelet Quantile PP test with different levels
wavelet_quantile_pp_tests <- function(variable_name) {
  # Perform wavelet decomposition
  wavelet_data <- wavelet_decompose(dataset[[variable_name]])
  
  # Use different levels for short, medium, and long run
  short_run <- wavelet_data[, 1] + wavelet_data[, 2]  # Short-term components
  medium_run <- wavelet_data[, 3]  # Medium-term component
  long_run <- wavelet_data[, 4] + wavelet_data[, 5]  # Long-term components
  
  # Perform quantile PP tests for each level
  pp_short <- quantile_pp_test(short_run, tau)
  pp_medium <- quantile_pp_test(medium_run, tau)
  pp_long <- quantile_pp_test(long_run, tau)
  
  # Combine results into data frames with Run labels
  df_pp <- data.frame(
    Quantile = tau,
    Test_Statistic = c(pp_short, pp_medium, pp_long),
    Variable = variable_name,
    Run = rep(c("Short Term", "Medium Term", "Long Term"), each = length(tau))
  )
  
  return(df_pp)
}

# Apply the Wavelet Quantile PP tests to each variable (LEG, LFD, LIQ, LREC, LTI)
results_LEG <- wavelet_quantile_pp_tests("LEG")
results_LFD <- wavelet_quantile_pp_tests("LFD")
results_LIQ <- wavelet_quantile_pp_tests("LIQ")
results_LREC <- wavelet_quantile_pp_tests("LREC")
results_LTI <- wavelet_quantile_pp_tests("LTI")

# Combine all results into one data frame
results_all <- rbind(results_LEG, results_LFD, results_LIQ, results_LREC, results_LTI)

# Ensure that the results are properly numeric
results_all$Quantile <- as.numeric(results_all$Quantile)
results_all$Test_Statistic <- as.numeric(results_all$Test_Statistic)

# Plot the results using ggplot2 with enhanced visual styling
p <- ggplot(results_all, aes(x = Quantile, y = Test_Statistic, color = Run, shape = Run, group = interaction(Variable, Run))) +
  geom_line(size = 1.5, alpha = 0.8) +  # Slight transparency to reduce overlap
  geom_point(size = 3.5, stroke = 1.5) +
  geom_hline(yintercept = -3.49, linetype = "dotted", color = "red", size = 1) +  # 1% critical value
  geom_hline(yintercept = -2.89, linetype = "dashed", color = "blue", size = 1) +  # 5% critical value
  geom_hline(yintercept = -2.58, linetype = "solid", color = "green", size = 1) +  # 10% critical value
  labs(title = "Wavelet Quantile PP Test Results",
       subtitle = "Analysis of Stationarity Across Quantiles",
       x = "Quantiles",
       y = "Test Statistics",
       color = "Time Horizon",
       shape = "Time Horizon") +
  facet_wrap(~ Variable, scales = "free_y") +  # Separate plots for each variable
  theme_minimal(base_size = 14) +  # Use a minimal theme with larger base font size for readability
  scale_color_manual(values = c("Short Term" = "#E69F00", "Medium Term" = "#56B4E9", "Long Term" = "#009E73")) +  # Use distinct colors for time horizons
  scale_shape_manual(values = c(16, 17, 18)) +  # Different shapes for short, medium, and long term
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Clear white background
    panel.background = element_rect(fill = "white", color = NA),  # Clear white panel background
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold", size = 14, color = "#333333"),
    axis.title.y = element_text(face = "bold", size = 14, color = "#333333"),
    axis.text = element_text(size = 12, color = "#333333"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#222222"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#666666"),
    legend.position = "top",  # Move legend to the top
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Display the plot
print(p)





