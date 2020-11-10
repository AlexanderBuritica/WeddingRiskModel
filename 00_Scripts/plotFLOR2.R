library(tidyverse)
library(glue)

# get recommendation function
source("00_Scripts/simulateFLOR.R")


# SETUP ----

# set theme for plots
theme_set(theme_minimal(base_family = "Avenir"))

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")


# PLOT FUNCTIONS 
# plot guest count distribution with 95% confidence interval


plot_guest_count <- function(df_final) {
  
  # generate 2.5 and 97.5 percentiles from simulation data
  ci_int_guest <- quantile(df_final$A_1Z, p = c(0.025, .975))
  
  # create histogram plot of results
  p <- df_final %>%
    ggplot(aes(A_1Z)) +
    geom_histogram(binwidth = 1, 
                   fill = cbPalette[6],
                   color = "white") +
    geom_hline(yintercept = 0, size = .5, colour="#333333") +
    geom_vline(xintercept = ci_int_guest[1], linetype = "dashed") +
    geom_vline(xintercept = ci_int_guest[2], linetype = "dashed") +
    labs(title = "Total Guest A",
         subtitle = glue("95% Confidence Estimate: {ci_int_guest[1]} -",
                         "{ci_int_guest[2]} guests"),
         x = "Total Guests A",
         y = "Simulation Trials",
         caption = "Source: Simulation Results")
  
  return(p) 
}
# plot cost distribution with 95% confidence interval
plot_cost <- function(df_final, Rent_Min_Obl = 0.25) {
  
  ci_int_cost <- quantile(df_final$Rent_Min_Obl, p = c(0.025, .975))
  # create histogram of results
  p <- df_final %>%
    ggplot(aes(Rent_Min_Obl)) +
    # histogram bin size should be equal to per guest variable cost
    geom_histogram(binwidth = Rent_Min_Obl,
                   fill = cbPalette[8],
                   color = "white") +
    geom_hline(yintercept = 0, size = .5, colour="#333333") +
    labs(title = "Total Guest Rentabilidad Minima Obligatoria",
         subtitle = glue("95% Confidence Estimate: ${ci_int_cost[1]} -",
                         "${ci_int_cost[2]}"),
         x = "Rentabilidad Minima Obligatoria",
         y = "Simulation Trials",
         caption = "Source: Simulation Results") +
    geom_vline(xintercept = ci_int_cost[1], linetype = "dashed") +
    geom_vline(xintercept = ci_int_cost[2], linetype = "dashed")
  
  return(p)
}