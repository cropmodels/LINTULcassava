#-------------------------------------------------------------------------------------------------#
# FUNCTION gla
#
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         29-01-2019
#
# This file contains a component of the LINTUL-CASSAVA model. The purpose of this function is to
# compute daily increase of the LAI. 
# 
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
#--------------------------------------------------------------------------------------------------#
gla <-function(DTEFF,TSUMCROP,LAII,RGRL,DELT,SLA,LAI,GLV,TSUMLA_MIN,TRANRF,WC,WCWP,RWCUTTING,FLV,LAIEXPOEND,DORMANCY) {
  
  # Growth during maturation stage
  GLAI <-SLA * GLV * (1-DORMANCY)  # m2 m-2 d-1
  
  # Growth during juvenile stage
  if(TSUMCROP < TSUMLA_MIN && LAI < LAIEXPOEND) {
    GLAI <-((LAI * (exp(RGRL * DTEFF * DELT) - 1) / DELT) +abs(RWCUTTING)*FLV*SLA) * TRANRF  # m2 m-2 d-1
  }
  
  # Growth at day of seedling emergence
  if(TSUMCROP > 0 && LAI == 0 && WC > WCWP) {
    GLAI <- LAII / DELT  # m2 m-2 d-1
  }
  
  # Growth before seedling emergence
  if(TSUMCROP == 0) {
    GLAI <- 0     # m2 m-2 d-1
  }
  
  return(GLAI)
}