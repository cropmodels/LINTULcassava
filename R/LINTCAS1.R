#------------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL_CASSAVA_RUN  
#
# Author:       Rob van den Beuken
# Modified:     AGT Schut, RJ Hijmans
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
#
# The LINTUL-CASSAVA_RUN function is running the full LINTUL2-CASSAVA script with all its components. 
# This function adds year details
#
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#------------------------------------------------------------------------------------------------------#

LINTCAS1 <- function(weather, crop, soil, management, control){

  names(weather) <- toupper(names(weather))
  weather$DAYS <- as.integer(format(weather$DATE[1], "%j")) + (1:nrow(weather))-1
  management$DOYPL <- as.integer(format(management$PLDATE, "%j"))
  management$DOYHAR <- management$DOYPL + as.integer(management$HVDATE - management$PLDATE)
  pars <- c(crop, soil, management, IRRIGF=control$IRRIGF, DELT=control$timestep)

  startDOY <- as.integer(format(control$startDATE, "%j"))
  ini_states <- LC_iniSTATES(pars)
  steps <- seq(startDOY, management$DOYHAR, by=control$timestep)
  state_wlim <- deSolve::euler(ini_states, steps, LC_model, pars, WDATA=weather)
  
  i <- which(weather$DATE == control$startDATE)
  i <- i:(i+nrow(state_wlim)-1)
 
#  data.frame(year_planting=weather$YEAR[1], year=weather$YEAR[i], DOY=weather$DOY[i], state_wlim)
  data.frame(date=weather$DATE[i], state_wlim)
}

