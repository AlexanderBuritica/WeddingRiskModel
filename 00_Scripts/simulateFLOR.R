library(tidyverse)
source("00_Scripts/calculate.R")

# SIMULATION FUNCTIONS ----




# function to return a sample of guests for a single trial
sample_guests <- function(n, p) {
  # sum of random sample of size n with prob p of attending
  count <- sum(rbinom(n, 1, p))
}



simulate_weddings_1 <- function(k, n, p,
                                B_1, B2_1,
                                disminucion,A_1) {
  
  A_1Z  <- replicate(
    # for k trials...
    k, runif(1, min = n-5, max = n),
    # sample n guests of probability p_1 to p_2
    sample_guests(A_1, p = runif(1, min = p[1], max = p[2]))
  )
  df_1 <- data.frame(A_1Z) %>%
    as.tibble() %>%
    mutate(trial = row_number(),
           Rent_Min_Obl= 0,
           Class_Rent="",
           Escenario=""
    )
  
  for ( i  in 1:nrow(df_1)){
    "seleccione el individuo i"
    bar_1_i <- df_1[i,]
    id_1 <- bar_1_i$A_1Z
    #print( id)
    total_cost_i =calculate_RENT_min_OBL_1Z(B_1, B2_1,
                                            disminucion,id_1)
    
    CLASS_cost_i =class_RENT_min_OBL_1Z(B_1, B2_1,
                                        disminucion,id_1)
    
    
    #print( total_cost_i)
    df_1[i,3] <- total_cost_i
    df_1[i,4] <- CLASS_cost_i
    df_1[i,5] <- "1.Conservador"
    
    
  }
  return(df_1)
}

simulate_weddings_2 <- function(k, n, p,
                                B_2, B2_2,
                                disminucion,A_1) {
  
  A_1Z  <- replicate(
    # for k trials...
    k, runif(1, min = n-5, max = n),
    # sample n guests of probability p_1 to p_2
    sample_guests(A_1, p = runif(1, min = p[1], max = p[2]))
  )
  
  
  df_2 <- data.frame(A_1Z) %>%
    as.tibble() %>%
    mutate(trial = row_number(),
           Rent_Min_Obl= 0,
           Class_Rent="",
           Escenario=""
    )
  
  for ( i  in 1:nrow(df_2)){
    "seleccione el individuo i"
    bar_2_i <- df_2[i,]
    id_2 <- bar_2_i$A_1Z
    #print( id)
    total_cost_2_i =calculate_RENT_min_OBL_2Z(B_2, B2_2,
                                              disminucion+0.05,id_2)
    
    CLASS_cost_2_i =class_RENT_min_OBL_2Z(B_2, B2_2,
                                          disminucion+0.05,id_2)
    
    #print( total_cost_i)
    df_2[i,3] <- total_cost_2_i
    df_2[i,4] <- CLASS_cost_2_i
    df_2[i,5] <- "2.Moderado"
    
  }
  return(df_2)
}


simulate_weddings_3 <- function(k, n, p,
                                B_3, B2_3,
                                disminucion,A_1) {
  
  A_1Z  <- replicate(
    # for k trials...
    k, runif(1, min = n-5, max = n),
    # sample n guests of probability p_1 to p_2
    sample_guests(A_1, p = runif(1, min = p[1], max = p[2]))
  )
  
  df_3 <- data.frame(A_1Z) %>%
    as.tibble() %>%
    mutate(trial = row_number(),
           Rent_Min_Obl= 0,
           Class_Rent="",
           Escenario=""
    )
  
  for ( i  in 1:nrow(df_3)){
    "seleccione el individuo i"
    bar_3_i <- df_3[i,]
    id_3 <- bar_3_i$A_1Z
    #print( id)
    total_cost_3_i =calculate_RENT_min_OBL_3Z(B_3, B2_3,
                                              disminucion+0.1,id_3)
    
    CLASS_cost_3_i =class_RENT_min_OBL_3Z(B_3, B2_3,
                                          disminucion+0.1,id_3)
    
    #print( total_cost_i)
    df_3[i,3] <- total_cost_3_i
    df_3[i,4] <- CLASS_cost_3_i
    df_3[i,5] <- "3.Mayor Riesgo"
    
  }
  return(df_3)
}













