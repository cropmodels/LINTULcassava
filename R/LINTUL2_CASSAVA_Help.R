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
LC_run <- function(wdata, crop, soil, management){

  wdata <- derive_wth_vars(wdata)
  # should use dates, not DOYS
  wdata$DOYS <- wdata$DOY[1] + (1:nrow(wdata))-1
	
  pars <- c(crop, soil, IRRIGF=management$IRRIGF)

  DELT <- as.numeric(pars[which(names(pars)=='DELT')])
  state_wlim <- deSolve::ode(LINTUL2_CASSAVA_iniSTATES(pars), 
                    seq(management$starttime, management$endtime, by = DELT), 
                    LINTUL2_CASSAVA_v2.0, pars,  WDATA = wdata,
                    method = "euler")
  
  state_wlim = data.frame(state_wlim)

  year <- management$year
  year_info = data.frame(year_planting = rep(year, nrow(state_wlim)),
                         year = rep(as.numeric(year), nrow(state_wlim)),
                         DOY = state_wlim[,'time'])
  
  if (as.numeric(year)%%4 == 0 & management$endtime > 366){
    ii<-which(year_info[,'DOY'] > 366)
    year_info[ii,'year'] <- year_info[ii,'year'] + 1
    year_info[ii,'DOY'] <- year_info[ii,'DOY'] - 366
  } else if (management$endtime > 365){
    ii<-which(year_info[,'DOY'] > 365)
    year_info[ii,'DOY'] <- year_info[ii,'DOY'] - 365
    year_info[ii,'year'] <- year_info[ii,'year'] + 1
  }

  return(Modelresults = cbind(year_info, state_wlim))
}

