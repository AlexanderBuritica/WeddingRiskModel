
ab<-df_final %>%
  filter( Escenario=="1.Conservador")%>%
  filter(A_1Z<= 6.063)%>%
  mutate(maximo=max(A_1Z))%>%
  filter(maximo==A_1Z)%>%
  select(Rent_Min_Obl)


%>%
  filter(A_1Z<= 6.063)
  
A_1Z==maximo &
  %>%
  
  
  
  
  
  
  
  
  
  
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
      facet_wrap(.~Escenario)
    
    return(p) 
  }


  
  
  
  
  
  
  
