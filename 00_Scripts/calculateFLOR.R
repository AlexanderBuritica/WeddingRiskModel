# CALCULATION FUNCTIONS ----

# function to calculate total cost
calculate_RENT_min_OBL_1Z <- function( B_1, B2_1,
                                       disminucion,A_1) {
  # calculate added guest cost
  rent_min_oblig1_1Z <- ((A_1)*(1-disminucion)+((B_1)*disminucion))
  rent_min_oblig2_1Z <- ((A_1-(A_1*0.3))*(1-disminucion))  +((B2_1)*disminucion)
  rent_min_oblig3_1Z <- rent_min_oblig1_1Z-2
  
  # add to fixed cost
  rent_min_obl_tot_1Z <- min(rent_min_oblig1_1Z,rent_min_oblig2_1Z,rent_min_oblig3_1Z)
  return(rent_min_obl_tot_1Z)
}

class_RENT_min_OBL_1Z <- function(B_1, B2_1,
                                  disminucion,A_1) {
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
                                      disminucion,A_1 ) {
  # calculate added guest cost
  rent_min_oblig1_2Z <- ((A_1)*(1-0.2)+((B_1)*0.2))
  rent_min_oblig2_2Z <- ((A_1-(A_1*0.35))*(1-0.2))  +((B2_1)*0.2)
  rent_min_oblig3_2Z <- rent_min_oblig1_2Z-3
  
  # add to fixed cost
  rent_min_obl_tot_2Z <- min(rent_min_oblig1_2Z,rent_min_oblig2_2Z,rent_min_oblig3_2Z)
  return(rent_min_obl_tot_2Z)
}
class_RENT_min_OBL_2Z <- function(B_1, B2_1,
                                 disminucion,A_1) {
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
                                      disminucion,A_1) {
  # calculate added guest cost
  rent_min_oblig1_3Z <- ((A_1)*(1-0.1)+((B_1)*0.1))
  rent_min_oblig2_3Z <- ((A_1-(A_1*0.4))*(1-0.1))  +((B2_1)*0.1)
  rent_min_oblig3_3Z <- rent_min_oblig1_3Z-4
  
  # add to fixed cost
  rent_min_obl_tot_3Z <- min(rent_min_oblig1_3Z,rent_min_oblig2_3Z,rent_min_oblig3_3Z)
  return(rent_min_obl_tot_3Z)
}

class_RENT_min_OBL_3Z <- function(B_1, B2_1,
                                  disminucion,A_1) {
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


