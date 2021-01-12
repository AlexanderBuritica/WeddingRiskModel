library(tidyverse)
source("00_Scripts/calculateFLOR_V2.R")

# SIMULATION FUNCTIONS ----
alex <- function(df_final) {
  # percentage of simulations that go over budget
  df_final<-df_final%>%
    group_by( Escenario)%>%
    select(Rent_Min_Obl)%>%
    summarise(Rent_Min_Obl=mean(Rent_Min_Obl))
  
  recommendation<-  df_final[1,2]
  recommendation<-as.numeric(recommendation)
  return(recommendation)
}
# function alex1
alex2 <- function(df_final) {
  # percentage of simulations that go over budget
  df_final<-df_final%>%
    group_by( Escenario)%>%
    select(Rent_Min_Obl)%>%
    summarise(Rent_Min_Obl=mean(Rent_Min_Obl))
  
  recommendation2<-  df_final[2,2]
  recommendation2<-as.numeric(recommendation2)
  return(recommendation2)
}
# function alex3
alex3 <- function(df_final) {
  # percentage of simulations that go over budget
  df_final<-df_final%>%
    group_by( Escenario)%>%
    select(Rent_Min_Obl)%>%
    summarise(Rent_Min_Obl=mean(Rent_Min_Obl))
  
  recommendation3<-  df_final[3,2]
  recommendation3<-as.numeric(recommendation3)
  return(recommendation3)
}
# function to return a sample of guests for a single trial 4
sample_guests <- function(n, p) {
  # sum of random sample of size n with prob p of attending
  count <- sum(rbinom(n, 1, p))
}
# function to return a sample of guests for a single trial
simulate_weddings_2x <- function(k, n, p,
                                 B_1, B2_1,
                                 disminucion,SKANDIA,COLFONDOS,PORVENIR,PROTECCION) {
  
  SKANDIA_Z  <- replicate(
    # for k trials...
    k, runif(1, min = n-5, max = n),
    # sample n guests of probability p_1 to p_2
    sample_guests(SKANDIA, p = runif(1, min = p[1], max = p[2]))
  )
  COLFONDOS_Z  <- replicate(
    # for k trials...
    k, runif(1, min = n-5, max = n),
    # sample n guests of probability p_1 to p_2
    sample_guests(COLFONDOS, p = runif(1, min = p[1], max = p[2]))
  )
  PORVENIR_Z  <- replicate(
    # for k trials...
    k, runif(1, min = n-5, max = n),
    # sample n guests of probability p_1 to p_2
    sample_guests(PORVENIR, p = runif(1, min = p[1], max = p[2]))
  )
  PROTECCION_Z  <- replicate(
    # for k trials...
    k, runif(1, min = n-5, max = n),
    # sample n guests of probability p_1 to p_2
    sample_guests(PROTECCION, p = runif(1, min = p[1], max = p[2]))
  )
  df_1 <- data.frame(SKANDIA_Z,COLFONDOS_Z,PORVENIR_Z,PROTECCION_Z) %>%
    as.tibble() %>%
    mutate(trial = row_number(),
           Rent_Min_Obl= 0,
           Class_Rent="",
           Escenario=""
    )
  df_2 <- data.frame(SKANDIA_Z,COLFONDOS_Z,PORVENIR_Z,PROTECCION_Z) %>%
    as.tibble() %>%
    mutate(trial = row_number(),
           Rent_Min_Obl= 0,
           Class_Rent="",
           Escenario=""
           
    )
  df_3 <- data.frame(SKANDIA_Z,COLFONDOS_Z,PORVENIR_Z,PROTECCION_Z) %>%
    as.tibble() %>%
    mutate(trial = row_number(),
           Rent_Min_Obl= 0,
           Class_Rent="",
           Escenario=""
    )
  for ( i  in 1:nrow(df_1)){
    "seleccione el individuo i"
    bar_1_i <- df_1[i,]
    id_1 <- bar_1_i$SKANDIA_Z
    id_2 <- bar_1_i$COLFONDOS_Z
    id_3 <- bar_1_i$PORVENIR_Z
    id_4 <- bar_1_i$PROTECCION_Z
    #print( id)
    total_cost_i =calculate_RENT_min_OBL_1Z(B_1, B2_1,
                                            disminucion,id_1,id_2,id_3,id_4)
    CLASS_cost_i =class_RENT_min_OBL_1Z(B_1, B2_1,
                                        disminucion,id_1,id_2,id_3,id_4)
    #print( total_cost_i)
    df_1[i,6] <- total_cost_i
    df_1[i,7] <- CLASS_cost_i
    df_1[i,8] <- "1.Conservador"
    "seleccione el individuo i"
    bar_2_i <- df_2[i,]
    id_2_1 <- bar_2_i$SKANDIA_Z
    id_2_2 <- bar_2_i$COLFONDOS_Z
    id_2_3 <- bar_2_i$PORVENIR_Z
    id_2_4 <- bar_2_i$PROTECCION_Z
    #print( id)
    total_cost_2_i =calculate_RENT_min_OBL_2Z(B_1, B2_1,
                                              disminucion+0.05,id_2_1,id_2_2,id_2_3,id_2_4)
    CLASS_cost_2_i =class_RENT_min_OBL_2Z(B_1, B2_1,
                                          disminucion+0.05,id_2_1,id_2_2,id_2_3,id_2_4)
    #print( total_cost_i)
    df_2[i,6] <- total_cost_2_i
    df_2[i,7] <- CLASS_cost_2_i
    df_2[i,8] <- "2.Moderado"
    "seleccione el individuo i"
    bar_3_i <- df_3[i,]
    id_3_1 <- bar_3_i$SKANDIA_Z
    id_3_2 <- bar_3_i$COLFONDOS_Z
    id_3_3 <- bar_3_i$PORVENIR_Z
    id_3_4 <- bar_3_i$PROTECCION_Z
    #print( id)
    total_cost_3_i =calculate_RENT_min_OBL_3Z(B_1, B2_1,
                                              disminucion+0.1,id_3_1,id_3_2,id_3_3,id_3_4)
    CLASS_cost_3_i =class_RENT_min_OBL_3Z(B_1, B2_1,
                                          disminucion+0.1,id_3_1,id_3_2,id_3_3,id_3_4)
    #print( total_cost_i)
    df_3[i,6] <- total_cost_3_i
    df_3[i,7] <- CLASS_cost_3_i
    df_3[i,8] <- "3.Mayor Riesgo"
  }
  df_final<-rbind(df_1,df_2)
  df_final<-rbind(df_final,df_3)
  return(df_final)
}