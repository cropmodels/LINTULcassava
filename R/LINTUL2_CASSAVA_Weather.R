#-------------------------------------------------------------------------------------------------#
# FUNCTION get_weather
#
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         29-01-2019
#
# This file contains a component of the LINTUL-CASSAVA model. The purpose of this function is to
# list the weather data required for LINTUL2-Cassava. 
# 
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
#--------------------------------------------------------------------------------------------------#
get_weather <- function(directory="./Weather/",country="Edo",station="1",year=substr('2016', 2,4), endtime = 365){
  weather1   <- matrix(data=as.numeric(unlist(scan(file=paste(directory,country,station,".",year,sep=""),
                                                  what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
  weather1 <- weather1[-c(1),]
  
  # If the growth of the crop covers two years, weather data of two years is needed. 
  if ( (as.numeric(year) %% 4 == 0 && endtime > 366) || (endtime > 365) ){
    year2 = paste0(as.numeric(year) + 1)
    year2 = paste0('00', year2)
    year2 = substr(year2, nchar(year2)-2,nchar(year2))
    weather2 <- matrix(data=as.numeric(unlist(scan(file=paste(directory,country,station,".",year2,sep=""),
                                                   what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
    #Cut off site information (lat, lon etc) if given
    #weather2 <- weather2[-c(1),]
    if (as.numeric(year) %% 4 == 0){
      weather2[,3] <- weather2[,3] + 366
    } else {
      weather2[,3] <- weather2[,3] + 365
    }
    weather <- rbind(weather1, weather2)
  } else {
      weather = weather1
  }

  RDD   = as.vector(weather[,4])    # kJ m-2 d-1:     daily global radiation     
  TMMN  = as.vector(weather[,5])    # deg. C   :     daily minimum temperature  
  TMMX  = as.vector(weather[,6])    # deg. C   :     daily maximum temperature  
  
  SatVP_TMMN    = 0.611 * exp(17.4 * weather[,5] / (weather[,5] + 239))     # kPa         :     Saturation vapour pressure
  SatVP_TMMX    = 0.611 * exp(17.4 * weather[,6] / (weather[,6] + 239))     # kPa         :     Saturation vapour pressure
  VPD_MN = pmax(0, SatVP_TMMN - weather[,7])
  VPD_MX = pmax(0, SatVP_TMMX - weather[,7])

  
  WDATA <- data.frame(
    YEAR 	  = as.vector(weather[,2]),  # year           
    DOYS 	  = as.vector(weather[,3]),  # day number since 1 Jan in the year of planting         
    VP 	  = as.vector(weather[,7]),    # kPa        :     vapour pressure            
    WN 	  = as.vector(weather[,8]),    # m s-1      :     wind speed                 
    RAIN  = as.vector(weather[,9]),    # mm         :     precipitation              
    DTR    = RDD / 1e+03,              # MJ m-2 d-1 :     incoming radiation (converted from kJ to MJ)
    DAVTMP = 0.5 * (TMMN + TMMX),       # Deg. C     :     daily average temperature
    VPD_MN  = as.vector(VPD_MN),    # deg. C   :     daily minimum temperature  
    VPD_MX  = as.vector(VPD_MX)    # deg. C   :     daily maximum temperature  
  )
}
