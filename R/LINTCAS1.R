#------------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL_CASSAVA_RUN  
#
# Author:       Rob van den Beuken
# Modified:     AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
#
# The LINTUL-CASSAVA_RUN function is running the full LINTUL2-CASSAVA script with all its components. 
# This function adds year details
#
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#------------------------------------------------------------------------------------------------------#

#LINTUL CASSAVA for WATER LIMITED PRODUCTION:
LINTCAS1 <- function(weather, crop, soil, management, control){

# should use dates, not DAYS
  weather$DAYS <- weather$DOY[1] + (1:nrow(weather))-1
	
  pars <- c(crop, soil, management, IRRIGF=control$IRRIGF, DELT=control$timestep)

  ini_states <- LC_iniSTATES(pars)
  steps <- seq(control$startDOY, management$DOYHAR, by = control$timestep)
  state_wlim <- deSolve::euler(ini_states, steps, LC_model, pars, WDATA = weather)
  
  i <- which(weather$DAYS == control$startDOY)
  i <- i:(i+nrow(state_wlim)-1)
 
  data.frame(year_planting=weather$YEAR[1], year=weather$YEAR[i], DOY=weather$DOY[i], state_wlim)
}

