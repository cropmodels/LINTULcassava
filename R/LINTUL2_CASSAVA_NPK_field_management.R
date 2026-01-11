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
 
 soil_param <- list()
  #Add soil supply, extra uptake from soil for omission treatments 
  #needs to be in g/m2!!
  soil_param[["NMINI"]] = SiteInfo[1,"N_soil.kg.ha"] * 0.1
  if (any(fertilizerNPK[,"K"] > 0) || any(fertilizerNPK[,"P"] > 0)) {
	soil_param[["NMINI"]] = soil_param[["NMINI"]] + SiteInfo[1,"N_PK.kg.ha"] * 0.1
  }
  soil_param[["PMINI"]] = SiteInfo[1,"P_soil.kg.ha"] * 0.1
  if (any(fertilizerNPK[,"N"] > 0) || any(fertilizerNPK[,"K"] > 0)) {
    soil_param[["PMINI"]] = soil_param[["PMINI"]] + SiteInfo[1,"P_NK.kg.ha"] * 0.1
  }
  soil_param[["KMINI"]] = SiteInfo[1,"K_soil.kg.ha"] * 0.1
  if (any(fertilizerNPK[,"N"] > 0) || any(fertilizerNPK[,"P"] > 0)) {
    soil_param[["KMINI"]] = soil_param[["KMINI"]] + SiteInfo[1,"K_NP.kg.ha"] * 0.1
  }
  
  

	fdays <- c("Day_of_basal_fertilizer_application.DOY", "Day_of_first_topdressing.DOY", "Day_of_second_topdressing.DOY", "Day_of_third_topdressing.DOY")
	fert <- as.matrix(cbind(DAYS=t(SiteInfo[1,fdays]), fertilizerNPK[, c("N", "P", "K")]))
	colnames(fert)[1] <- "DAYS"
	
  mgmt <- list(
    DOYPL  = SiteInfo[1,"Day_of_planting.DOY"],    # Day nr of planting and basal NPK application
    DOYHAR = SiteInfo[1,"Day_of_harvest.Days_since_1_january_of_planting_year"], # Day nr of harvest, counted from 1 Jan in year of planting
	FERTAB = fert 
  )
  list(soil_phys=phys_soil, soil_chem=soil_param, mgmt=mgmt)
   
  #NOTE: the order of concatenation matters!!!! FIELD PARAM needs to go first.
  #return( c(FIELD_PARAM, MODEL_PARAM))
}


