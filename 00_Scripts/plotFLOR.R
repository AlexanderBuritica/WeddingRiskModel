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
         caption = "Source: Simulation Results")+ 
    facet_wrap(Escenario~.)
  
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
    geom_vline(xintercept = ci_int_cost[2], linetype = "dashed")+ 
    facet_wrap(.~Escenario)
  
  return(p)
}


# plot the risk profile with probability of going over budget
plot_risk <- function(df_final, Rent_Min_Obl = 0.25) {
  
  ci_int_risk <- quantile(df_final$Rent_Min_Obl, p = c(0.025, .975))
  
  # risk profile
  p_over_budget <- df_final %>%
    summarise(p_val = mean(Rent_Min_Obl < 3.03))
  
  # plot risk profile with 95% confidence interval
  p <- df_final %>%
    # color histrogram bins based on positive/negative risk
    ggplot(aes(Rent_Min_Obl, fill = PorDebajRetiro)) +
    # histogram bin size should be equal to per guest variable cost
    geom_histogram(binwidth = Rent_Min_Obl,
                   color = "white") +
    geom_hline(yintercept = 0, size = .5, colour = "#333333") +
    scale_fill_manual(values = c(cbPalette[4], cbPalette[5], cbPalette[7])) +
    labs(title = "Total Budget Risk",
         subtitle = glue("95% Confidence Estimate: ${ci_int_risk[1]} - ${ci_int_risk[2]} ",
                         "({p_over_budget * 100}% risk)"),
         x = "Net Risk",
         y = "Simulation Trials",
         caption = "Source: Simulation Results") +
    guides(fill = FALSE) +
    geom_vline(xintercept = ci_int_risk[1], linetype = "dashed") +
    geom_vline(xintercept = ci_int_risk[2], linetype = "dashed")+ 
    facet_wrap(.~Escenario)
  
  # if there are no even budget outcomes
  if (length(unique(df_final$PorDebajRetiro)) <3.03) {
    p <- p +
      scale_fill_manual(values = c(cbPalette[4], cbPalette[7]))
  }
  
  return(p)
}



# plot the distribution of recommendation outcomes
plot_recommendation <- function(df_final, rent_min_esp_retiro_progr) {
  
  # get recommendation for subtitle
  ovr_recommendation <- recommend(df_final, rent_min_esp_retiro_progr)
  
  # build main plot
  p <- df_final %>%
    ggplot(aes(PorDebajRetiro, fill = PorDebajRetiro)) +
    # convert counts to percentages
    geom_bar(aes(y = (..count..) / sum(..count..))) + 
    scale_y_continuous(labels = function(x) glue("{round(x , 2)}%")) +
    labs(
      title = "Recommendation Outcomes",
      subtitle = glue("Overall Recommendation: {ovr_recommendation}"),
      x = "",
      y = "Simulation Trials",
      caption = "Source: Simulation Results"
    ) +
    guides(fill = FALSE) +
    geom_hline(yintercept = 0, size = .5, colour = "#333333") +
    # add dashed line for risk tolernace
    geom_hline(yintercept = rent_min_esp_retiro_progr, linetype = "dashed")+ 
    facet_wrap(.~Escenario)
  
  # if risk tolerance is exceeded, color the Invite Less bar 
  if (mean(df_final$Rent_Min_Obl < 3.03) >= rent_min_esp_retiro_progr) {
    p <- p + 
      scale_fill_manual(values = c(cbPalette[1], cbPalette[7]))
  } else {
    # else color the Invite All bar
    p <- p + 
      scale_fill_manual(values = c(cbPalette[4], cbPalette[1]))
  }
  
  return(p)
}








