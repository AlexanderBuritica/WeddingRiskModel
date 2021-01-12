cat("\f")
rm(list = ls())

# customize output options 
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE,
                      fig.width = 6, fig.asp = 0.618, out.width = "70%", 
                      fig.align = "center")

# libraries used in report
library(knitr)
library(kableExtra)
library(tidyverse)
library(glue)

# set theme for plots
theme_set(theme_minimal(base_family = "Avenir"))

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")










calculate_RENT_min_OBL_1Z <- function( B_1, B2_1,
                                       disminucion,SKANDIA,COLFONDOS,PORVENIR,PROTECCION) {
  
  fondo <- c("SKANDIA","COLFONDOS","PORVENIR", "PROTECCION")
  factor_Pond_1 <- c(0.098,0.242,0.330,0.330)
  renta_1<-c(SKANDIA,COLFONDOS,PORVENIR, PROTECCION)
  df_fondos_1 <- data.frame(fondo, factor_Pond_1,renta_1)
  df_fondos_1 <- df_fondos_1 %>%
    mutate(prom_pond_1=factor_Pond_1*renta_1)
  # number of people invited
  A_1 <- df_fondos_1 %>%
    summarise(prom_pond_1=sum(prom_pond_1))
  # number of people invited
  # calculate added guest cost
  rent_min_oblig1_1Z <- ((A_1)*(1-disminucion)+((B_1)*disminucion))
  rent_min_oblig2_1Z <- ((A_1-(A_1*0.3))*(1-disminucion))  +((B2_1)*disminucion)
  rent_min_oblig3_1Z <- rent_min_oblig1_1Z-2
  # add to fixed cost
  rent_min_obl_tot_1Z <- min(rent_min_oblig1_1Z,rent_min_oblig2_1Z,rent_min_oblig3_1Z)
  return(rent_min_obl_tot_1Z)
}
class_RENT_min_OBL_1Z <- function(B_1, B2_1,
                                  disminucion,SKANDIA,COLFONDOS,PORVENIR,PROTECCION) {
  fondo <- c("SKANDIA","COLFONDOS","PORVENIR", "PROTECCION")
  factor_Pond_1 <- c(0.098,0.242,0.330,0.330)
  renta_1<-c(SKANDIA,COLFONDOS,PORVENIR, PROTECCION)
  df_fondos_1 <- data.frame(fondo, factor_Pond_1,renta_1)
  df_fondos_1 <- df_fondos_1 %>%
    mutate(prom_pond_1=factor_Pond_1*renta_1)
  # number of people invited
  A_1 <- df_fondos_1 %>%
    summarise(prom_pond_1=sum(prom_pond_1))
    # calculate added guest cost
  rent_min_oblig1_1Z <- ((A_1)*(1-disminucion)+((B_1)*disminucion))
  rent_min_oblig2_1Z <- ((A_1-(A_1*0.3))*(1-disminucion))  +((B2_1)*disminucion)
  rent_min_oblig3_1Z <- rent_min_oblig1_1Z-2
  # add to fixed cost
  rent_min_obl_tot_1Z <- min(rent_min_oblig1_1Z,rent_min_oblig2_1Z,rent_min_oblig3_1Z)
  # add to fixed cost
  class_rent_1Z<- ifelse(rent_min_obl_tot_1Z == rent_min_oblig1_1Z, "(A*70%)+(B*30%)",
                         ifelse(rent_min_obl_tot_1Z == rent_min_oblig2_1Z, "((A*70%)+(B*30%)DISMINUIDO EN UN 30%)",  "(A*70%)+(B*30%) MENOS 2%"))
  return(class_rent_1Z)
}
calculate_RENT_min_OBL_2Z <- function(B_1, B2_1,
                                      disminucion,SKANDIA,COLFONDOS,PORVENIR,PROTECCION ) {
  fondo <- c("SKANDIA","COLFONDOS","PORVENIR", "PROTECCION")
  factor_Pond_2 <- c(0.103,0.330,0.237,0.330)
  renta_2<-c(SKANDIA,COLFONDOS,PORVENIR, PROTECCION)
  df_fondos_2 <- data.frame(fondo, factor_Pond_2,renta_2)
  df_fondos_2 <- df_fondos_2 %>%
    mutate(prom_pond_2=factor_Pond_2*renta_2)
  # number of people invited
  A_1 <- df_fondos_2 %>%
    summarise(prom_pond_2=sum(prom_pond_2))
  # calculate added guest cost
  rent_min_oblig1_2Z <- ((A_1)*(1-0.2)+((B_1)*0.2))
  rent_min_oblig2_2Z <- ((A_1-(A_1*0.35))*(1-0.2))  +((B2_1)*0.2)
  rent_min_oblig3_2Z <- rent_min_oblig1_2Z-3
  # add to fixed cost
  rent_min_obl_tot_2Z <- min(rent_min_oblig1_2Z,rent_min_oblig2_2Z,rent_min_oblig3_2Z)
  return(rent_min_obl_tot_2Z)
}
class_RENT_min_OBL_2Z <- function(B_1, B2_1,
                                  disminucion,SKANDIA,COLFONDOS,PORVENIR,PROTECCION ) {
  fondo <- c("SKANDIA","COLFONDOS","PORVENIR", "PROTECCION")
  factor_Pond_2 <- c(0.103,0.330,0.237,0.330)
  renta_2<-c(SKANDIA,COLFONDOS,PORVENIR, PROTECCION)
  df_fondos_2 <- data.frame(fondo, factor_Pond_2,renta_2)
  df_fondos_2 <- df_fondos_2 %>%
    mutate(prom_pond_2=factor_Pond_2*renta_2)
  # number of people invited
  A_1 <- df_fondos_2 %>%
    summarise(prom_pond_2=sum(prom_pond_2))
  # calculate added guest cost
  rent_min_oblig1_2Z <- ((A_1)*(1-0.2)+((B_1)*0.2))
  rent_min_oblig2_2Z <- ((A_1-(A_1*0.35))*(1-0.2))  +((B2_1)*0.2)
  rent_min_oblig3_2Z <- rent_min_oblig1_2Z-3
  # add to fixed cost
  rent_min_obl_tot_2Z <- min(rent_min_oblig1_2Z,rent_min_oblig2_2Z,rent_min_oblig3_2Z)
  class_rent_2Z<- ifelse(rent_min_obl_tot_2Z == rent_min_oblig1_2Z, "(A*80%)+(B*20%)",
                         ifelse(rent_min_obl_tot_2Z == rent_min_oblig2_2Z, "((A*80%)+(B*20%)DISMINUIDO EN UN 35%)",  "(A*80%)+(B*20%) MENOS 3%"))
  return(class_rent_2Z)
}
calculate_RENT_min_OBL_3Z <- function(B_1, B2_1,
                                      disminucion,SKANDIA,COLFONDOS,PORVENIR,PROTECCION ) {
  fondo <- c("SKANDIA","COLFONDOS","PORVENIR", "PROTECCION")
  factor_Pond_3 <- c(0.169,0.171,0.330,0.330)
  renta_3<-c(SKANDIA,COLFONDOS,PORVENIR, PROTECCION)
  df_fondos_3 <- data.frame(fondo, factor_Pond_3,renta_3)
  df_fondos_3 <- df_fondos_3 %>%
    mutate(prom_pond_3=factor_Pond_3*renta_3)
  # number of people invited
  A_1 <- df_fondos_3 %>%
    summarise(prom_pond_3=sum(prom_pond_3))
  # calculate added guest cost
  rent_min_oblig1_3Z <- ((A_1)*(1-0.1)+((B_1)*0.1))
  rent_min_oblig2_3Z <- ((A_1-(A_1*0.4))*(1-0.1))  +((B2_1)*0.1)
  rent_min_oblig3_3Z <- rent_min_oblig1_3Z-4
  # add to fixed cost
  rent_min_obl_tot_3Z <- min(rent_min_oblig1_3Z,rent_min_oblig2_3Z,rent_min_oblig3_3Z)
  return(rent_min_obl_tot_3Z)
}
class_RENT_min_OBL_3Z <- function(B_1, B2_1,
                                  disminucion,SKANDIA,COLFONDOS,PORVENIR,PROTECCION) {
  fondo <- c("SKANDIA","COLFONDOS","PORVENIR", "PROTECCION")
  factor_Pond_3 <- c(0.169,0.171,0.330,0.330)
  renta_3<-c(SKANDIA,COLFONDOS,PORVENIR, PROTECCION)
  df_fondos_3 <- data.frame(fondo, factor_Pond_3,renta_3)
  df_fondos_3 <- df_fondos_3 %>%
    mutate(prom_pond_3=factor_Pond_3*renta_3)
  # number of people invited
  A_1 <- df_fondos_3 %>%
    summarise(prom_pond_3=sum(prom_pond_3))
  # calculate added guest cost
  rent_min_oblig1_3Z <- ((A_1)*(1-0.1)+((B_1)*0.1))
  rent_min_oblig2_3Z <- ((A_1-(A_1*0.4))*(1-0.1))  +((B2_1)*0.1)
  rent_min_oblig3_3Z <- rent_min_oblig1_3Z-4
  # add to fixed cost
  rent_min_obl_tot_3Z <- min(rent_min_oblig1_3Z,rent_min_oblig2_3Z,rent_min_oblig3_3Z)
  class_rent_3Z<- ifelse(rent_min_obl_tot_3Z == rent_min_oblig1_3Z, "(A*90%)+(B*10%)",
                         ifelse(rent_min_obl_tot_3Z == rent_min_oblig2_3Z, "((A*90%)+(B*10%)DISMINUIDO EN UN 40%)",  "(A*90%)+(B*10%) MENOS 4%"))
  return(class_rent_3Z)
}



sample_guests <- function(n, p) {
  # sum of random sample of size n with prob p of attending
  count <- sum(rbinom(n, 1, p))
}
# function to return a sample of guests for a single trial




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
guest_sim_tbl1 <- simulate_weddings_2x(k = 100, 
                                       n = 10, 
                                       p = c(0.3, 0.85),
                                       SKANDIA=6.37	,
                                       COLFONDOS=6.11	,
                                       PORVENIR=6.11	,
                                       PROTECCION=6.11	,
                                       B_1 = 7.2, 
                                       B2_1 = 4.64, 
                                       disminucion = 0.3)

