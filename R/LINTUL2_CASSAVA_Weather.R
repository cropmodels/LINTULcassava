
derive_wth_vars <- function(w) {
  # kPa: Saturation vapour pressure at tmin and tmax
  SatVP_TMMN = 0.611 * exp(17.4 * w$TMIN / (w$TMIN + 239)) 
  SatVP_TMMX = 0.611 * exp(17.4 * w$TMAX / (w$TMAX + 239)) 
  # vapour pressure deficits
  VPD_MN = pmax(0, SatVP_TMMN - w$VAPR)
  VPD_MX = pmax(0, SatVP_TMMX - w$VAPR)
  
  data.frame(
    NAME = w$NAME,
    STNID = w$STNID,
    YEAR = w$YEAR,  # year           
    DOYS = w$DOY,  # day number since 1 Jan in the year of planting         
    VP 	 = w$VAPR,    # kPa        : vapour pressure            
    WN 	 = w$WIND,    # m s-1      : wind speed                 
    RAIN = w$PREC,    # mm         : precipitation              
    DTR  = w$SRAD / 1000, # MJ m-2 d-1 :   incoming radiation (converted from kJ to MJ)
    DAVTMP = 0.5 * (w$TMIN + w$TMAX),   # Deg. C     :     daily average temperature
    VPD_MN  = VPD_MN,    # xPascal : VPD at daily minimum temperature  
    VPD_MX  = VPD_MX    #  xPascal: VPD at daily maximum temperature  
  )
}

read_CABO_wth <- function(fname) {
	w <- matrix(data=as.numeric(unlist( scan(fname, what=as.list(rep("",9)),
                      comment.char='*', fill=TRUE, quiet=TRUE))), ncol=9)
	meta <- NULL
	# if the first row has location data, cols 6:9 should be NA
	if (is.na(w[1,8])) {
		meta <- c(w[2,1], w[1, 1:5])
		names(meta) <- c("STNID", "longitude", "latitude", "elevation", "AngA", "AngB")
		w <- w[-1,]
	}  
	colnames(w) <- c("STNID", "YEAR", "DOY", "SRAD", "TMIN", "TMAX", "VAPR", "WIND", "PREC")
	d <- data.frame(w)
	attr(d, "location") <- meta
	d
}

get_one_wth <- function(path, country, station, year) {
	yr <- substr(year, 2, 4)
	fname <- file.path(path, paste0(country, station, ".", yr))
	read_CABO_wth(fname)
}


get_weather <- function(directory="./Weather/",country="Edo",station="1",year, endtime = 365){
  
	stopifnot(nchar(year) == 4)	
	w <- get_one_wth(directory, country, station, year)
	# If the growth of the crop covers two years, weather data of two years is needed. 
	if (endtime > nrow(w)) {
		w2 <- get_one_wth(directory, country, station, year+1)
		w <- rbind(w, w2)
		w[, "DOY"] <- 1:nrow(w)
	}
	w
}

#get_wth1 <- function(...) {
#  w <- get_weather(...)
#  LINTULcassava:::derive_wth_vars(w)
#}
#wd <- get_wth1(directory=system.file("weather", package="LINTULcassava"), country="nig", station='1', year=year, endtime = 506)


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




get_weather_old <- function(directory="./Weather/",country="Edo",station="1",year, endtime = 365){

    
  if (nchar(year) == 4) year=substr(year, 2,4)
  # avoid missing trailing folder slash problem
  fname= file.path(directory, paste0(country,station,".",year))
  weather1   <- matrix(data=as.numeric(unlist(scan(fname,
                                                   what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
  weather1 <- weather1[-c(1),]
  
  # If the growth of the crop covers two years, weather data of two years is needed. 
  if ( (as.numeric(year) %% 4 == 0 && endtime > 366) || (endtime > 365) ){
    year2 = paste0(as.numeric(year) + 1)
    year2 = paste0('00', year2)
    year2 = substr(year2, nchar(year2)-2,nchar(year2))
    fname2=file.path(directory, paste0(country,station,".",year2))
    weather2 <- matrix(data=as.numeric(unlist(scan(fname2,
                                                   what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
    #Cut off site information (lat, lon etc) if given
#RH: then you should check for that?
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

