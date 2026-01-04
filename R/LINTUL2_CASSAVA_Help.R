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
LC_run <- function(weather, crop, soil, management, control){

  wdata <- derive_wth_vars(weather)
  # should use dates, not DOYS
  wdata$DOYS <- wdata$DOY[1] + (1:nrow(wdata))-1
	
  pars <- c(crop, soil, IRRIGF=management$IRRIGF, DELT=control$timestep)

  state_wlim <- deSolve::ode(LINTUL2_CASSAVA_iniSTATES(pars), 
                    seq(management$starttime, management$endtime, by = control$timestep), 
                    LINTUL2_CASSAVA_v2.0, pars, WDATA = wdata, method = "euler")
  
  i <- which(wdata$DOYS == management$starttime)
  i <- i:(i+nrow(state_wlim)-1)
 
  data.frame(year_planting=weather$YEAR[1], year=weather$YEAR[i], DOY=weather$DOY[i], state_wlim)
}

