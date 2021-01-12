library(tidyverse)
library(glue)

# get recommendation function
source("00_Scripts/simulateFLOR3.R")


# SETUP ----

# set theme for plots
theme_set(theme_minimal(base_family = "Avenir"))

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")


# PLOT FUNCTIONS 
# plot guest count distribution with 95% confidence interval



# plot cost distribution with 95% confidence interval
plot_cost <- function(df_final) {
  
  ci_int_cost <- quantile(df_final$Rent_Min_Obl, p = c(0.025, .975))
  # create histogram of results
  p <- df_final %>%
    ggplot(aes(Rent_Min_Obl)) +
    # histogram bin size should be equal to per guest variable cost
    geom_histogram(binwidth = 0.25,
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
    geom_vline(xintercept = ci_int_cost[2], linetype = "dashed")+ 
    facet_wrap(.~Escenario)
  
  return(p)
}