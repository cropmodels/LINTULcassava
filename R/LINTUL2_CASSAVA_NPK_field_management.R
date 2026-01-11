#----------------------------------------------------------------------------------------------------#
# Fertilizer settings                                  
#
# Author:       AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         18-12-2019
#
#----------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA_FIELD_MANAGEMENT                                               #
# Purpose: Listing the field related input parameters for Lintul2                   #
#---------------------------------------------------------------------#

LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK <- function(SiteInfo, MODEL_PARAM, fertilizerNPK) { 

  phys_soil = c(
    ROOTDM = SiteInfo[1,"Maximum_rooting_depth.m"] ,    # m            :     maximum rooting depth
    WCAD   = SiteInfo[1,"Water_content_air_dry.m3.H2O..m.3.soil."],   # m3 m-3      :     soil water content at air dryness 
    WCWP   = SiteInfo[1,"Water_content_wilting_point.m3.H2O..m.3.soil."],   # m3 m-3      :     soil water content at wilting point
    WCFC   = SiteInfo[1,"Water_content_field_capacity.m3.H2O..m.3.soil."],    # m3 m-3      :     soil water content at field capacity 
    WCWET  = SiteInfo[1,"Water_content_wet.m3.H2O..m.3.soil."],   # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
    WCST   = SiteInfo[1,"Water_content_saturated.m3.H2O..m.3.soil."],    # m3 m-3      :     soil water content at full saturation 
    DRATE  = SiteInfo[1,"Drainage_rate.mm.d.1"]      # mm d-1      :     max drainage rate
  )

  mgmt <- list(
    DOYPL  = SiteInfo[1,"Day_of_planting.DOY"],    # Day nr of planting and basal NPK application
    DOYHAR = SiteInfo[1,"Day_of_harvest.Days_since_1_january_of_planting_year"]    # Day nr of harvest, counted from 1 Jan in year of planting
  )
 
 soil_param <- list()
  #Add soil supply, extra uptake from soil for omission treatments 
  #needs to be in g/m2!!
  if (sum(fertilizerNPK[,"K"]) == 0 & sum(fertilizerNPK[,"P"]) == 0) {
    soil_param[["NMINI"]] = SiteInfo[1,"N_soil.kg.ha"] * 0.1
  }else{
    soil_param[["NMINI"]] = SiteInfo[1,"N_soil.kg.ha"] * 0.1 + SiteInfo[1,"N_PK.kg.ha"] * 0.1
  }
  if (sum(fertilizerNPK[,"N"]) == 0 & sum(fertilizerNPK[,"K"]) == 0) {
    soil_param[["PMINI"]] = SiteInfo[1,"P_soil.kg.ha"] * 0.1
  }else{
    soil_param[["PMINI"]] = SiteInfo[1,"P_soil.kg.ha"] * 0.1 + SiteInfo[1,"P_NK.kg.ha"] * 0.1
  }
  if (sum(fertilizerNPK[,"N"]) == 0 & sum(fertilizerNPK[,"P"]) == 0) {
    soil_param[["KMINI"]] = SiteInfo[1,"K_soil.kg.ha"] * 0.1
  }else{
    soil_param[["KMINI"]] = SiteInfo[1,"K_soil.kg.ha"] * 0.1 + SiteInfo[1,"K_NP.kg.ha"] * 0.1
  }
  
  soil_param[["WCI"]]    =  soil_param[["WCFC"]]    # m3 m-3      :     initial soil water content set at field capacity
  #Uptake of control treatment should be taken up in period to harvest
  #Assumed that all becomes gradually available with fixed rate per day
  #Available nutrients can be taken up in 90% of the season
  season_length <- mgmt[["DOYHAR"]] - mgmt[["DOYPL"]]
  soil_param[["RTNMINS"]] =  (1/0.9) * soil_param[["NMINI"]] / season_length # gN m-2 d-1
  soil_param[["RTPMINS"]] =  (1/0.9) * soil_param[["PMINI"]] / season_length # gP m-2 d-1
  soil_param[["RTKMINS"]] =  (1/0.9) * soil_param[["KMINI"]] / season_length # gK m-2 d-1
  
  # Fertilizer N, P and K applications, 
  # Note: that the days when fertilizers should be surrounded with the values
  # of the day before. Because an interpolation function is used. 

  FERNTAB <- matrix(c(  0,   0,
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"], fertilizerNPK[1,"N"] * MODEL_PARAM[["N_RECOV"]],                       
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_first_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_first_topdressing.DOY"], fertilizerNPK[2,"N"] * MODEL_PARAM[["N_RECOV"]],                       
                        SiteInfo[1,"Day_of_first_topdressing.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_second_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_second_topdressing.DOY"], fertilizerNPK[3,"N"] * MODEL_PARAM[["N_RECOV"]],                       
                        SiteInfo[1,"Day_of_second_topdressing.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_third_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_third_topdressing.DOY"], fertilizerNPK[4,"N"] * MODEL_PARAM[["N_RECOV"]],                       
                        SiteInfo[1,"Day_of_third_topdressing.DOY"] + 1, 0,
                        2000, 0), ncol = 2, byrow=TRUE)  # kg N/ha as function of day number
  
  FERPTAB <- matrix(c(  0,   0,
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"], fertilizerNPK[1,"P"] * MODEL_PARAM[["P_RECOV"]],                       
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_first_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_first_topdressing.DOY"], fertilizerNPK[2,"P"] * MODEL_PARAM[["P_RECOV"]],                       
                        SiteInfo[1,"Day_of_first_topdressing.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_second_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_second_topdressing.DOY"], fertilizerNPK[3,"P"] * MODEL_PARAM[["P_RECOV"]],                       
                        SiteInfo[1,"Day_of_second_topdressing.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_third_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_third_topdressing.DOY"], fertilizerNPK[4,"P"] * MODEL_PARAM[["P_RECOV"]],                       
                        SiteInfo[1,"Day_of_third_topdressing.DOY"] + 1, 0,
                        2000, 0), ncol = 2, byrow=TRUE)  # kg P/ha as function of day number
  
  FERKTAB <- matrix(c(  0,   0,
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"], fertilizerNPK[1,"K"] * MODEL_PARAM[["K_RECOV"]],                       
                        SiteInfo[1,"Day_of_basal_fertilizer_application.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_first_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_first_topdressing.DOY"], fertilizerNPK[2,"K"] * MODEL_PARAM[["K_RECOV"]],                       
                        SiteInfo[1,"Day_of_first_topdressing.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_second_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_second_topdressing.DOY"], fertilizerNPK[3,"K"] * MODEL_PARAM[["K_RECOV"]],                       
                        SiteInfo[1,"Day_of_second_topdressing.DOY"] + 1, 0, 
                        SiteInfo[1,"Day_of_third_topdressing.DOY"] - 1, 0, 
                        SiteInfo[1,"Day_of_third_topdressing.DOY"], fertilizerNPK[4,"K"] * MODEL_PARAM[["K_RECOV"]],                       
                        SiteInfo[1,"Day_of_third_topdressing.DOY"] + 1, 0,
                        2000, 0), ncol = 2, byrow=TRUE)  # kg K/ha as function of day number
  
  fertilizer=list(FERNTAB = FERNTAB, FERPTAB = FERPTAB, FERKTAB = FERKTAB)
  
  list(soil_phys=phys_soil, soil_chem=soil_param, mgmt=c(mgmt, fertilizer))
   
  #NOTE: the order of concatenation matters!!!! FIELD PARAM needs to go first.
  #return( c(FIELD_PARAM, MODEL_PARAM))
}


