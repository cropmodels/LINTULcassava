

LINTCAS <- function(weather, crop, soil, management, control, NPK=FALSE, level=3) {
	if (level == 3) {
		LINTCAS3(weather, crop, soil, management, control, NPK=NPK)
	} else if (level == 2) {
		if (isTRUE(NPK) || isTRUE(control$nutrient_limited)) {
			LINTCAS2_NPK(weather, crop, soil, management, control)
		} else {
			LINTCAS2(weather, crop, soil, management, control)		
		}
	} else {
		if (isTRUE(NPK) || isTRUE(control$nutrient_limited)) {
			LINTCAS1_NPK(weather, crop, soil, management, control)			
		} else {
			LINTCAS1(weather, crop, soil, management, control)	
		}
	}
}

LINTCAS3 <- function(weather, crop, soil, management, control, NPK) {
## R interface to C++ implementation 
## Robert Hijmans, January 2026
	names(weather) <- tolower(names(weather))
	control$NPKmodel <- isTRUE(NPK) || isTRUE(control$nutrient_limited)
	d <- .LC(crop, weather, soil, management, control)
	m <- data.frame(matrix(d[[1]], ncol=length(d[[2]]), byrow=TRUE))
	names(m) <- d[[2]]
	date <- as.Date(control$startDATE) - 1  + m[, "step"]
	data.frame(date=date, m)
}


LINTCAS1 <- function(weather, crop, soil, management, control){
## original model, calls LC_model
## Author: Rob van den Beuken
## modfied by RH
  names(weather) <- toupper(names(weather))
  weather$DAYS <- as.integer(format(weather$DATE[1], "%j")) + (1:nrow(weather))-1
  management$DOYPL <- as.integer(format(management$PLDATE, "%j"))
  management$DOYHAR <- management$DOYPL + as.integer(management$HVDATE - management$PLDATE)


  pars <- c(crop, soil, management, IRRIGF=!control$water_limited, DELT=control$timestep)

  startDOY <- as.integer(format(control$startDATE, "%j"))
  ini_states <- LC_iniSTATES(pars)
  steps <- seq(startDOY, management$DOYHAR, by=control$timestep)
  state_wlim <- deSolve::euler(ini_states, steps, LC_model, pars, WDATA=weather)
  
  i <- which(weather$DATE == control$startDATE)
  i <- i:(i+nrow(state_wlim)-1)
 
#  data.frame(year_planting=weather$YEAR[1], year=weather$YEAR[i], DOY=weather$DOY[i], state_wlim)
  data.frame(date=weather$DATE[i], state_wlim)
}

LINTCAS1_NPK <- function(wdata, crop, soil, management, control){
# Author: AGT Schut
# modfied by RH

	management$DOYPL <- as.integer(format(management$PLDATE, "%j"))
	management$DOYHAR <- management$DOYPL + as.integer(management$HVDATE - management$PLDATE)

	management$FERTAB[,1] <- management$FERTAB[,1] + management$DOYPL
	management$FERNTAB <- management$FERTAB[,1:2]
	management$FERPTAB <- management$FERTAB[,c(1,3)]
	management$FERKTAB <- management$FERTAB[,c(1,4)]
	surround_days <- function(d, recovery) {
		if (is.null(d)) return(d)
		d[,2] <- d[,2] * recovery
		d <- d[rep(1:nrow(d), each=3), ]
		d[,1] <- d[,1] + c(-1:1)
		d[-seq(2, nrow(d), by=3), 2] = 0
		rbind(cbind(0,0), d, cbind(2000,0))
	}
	for (v in c("FERNTAB", "FERPTAB", "FERKTAB")) {
		management[[v]] <- surround_days(management[[v]], crop[[paste0(substr(v, 4, 4), "_RECOV")]])
	}

  ## initial soil water content set at field capacity
	#soil$WCI = soil[["WCFC"]]    # m3 m-3 

	names(wdata) <- toupper(names(wdata))
	wdata$DOYS <- 1:nrow(wdata)

	#Uptake of control treatment should be taken up in period to harvest. Assumed that all becomes gradually available with fixed rate per day. Available nutrients can be taken up in 90% of the season 
	## RH. What about very short or long seasons? Perhaps better use a decay function in the model?
	season_length <- management[["DOYHAR"]] - management[["DOYPL"]]
	soil[["RTNMINS"]] =  (1/0.9) * soil[["NMINI"]] / season_length # gN m-2 d-1
	soil[["RTPMINS"]] =  (1/0.9) * soil[["PMINI"]] / season_length # gP m-2 d-1
	soil[["RTKMINS"]] =  (1/0.9) * soil[["KMINI"]] / season_length # gK m-2 d-1

	pars <- c(management, soil, crop, control, 
			NUTRIENT_LIMITED=control$nutrient_limited, IRRIGF=!control$water_limited, DELT=control$timestep)
	startDOY <- as.integer(format(control$startDATE, "%j"))
	steps <- seq(startDOY, management[["DOYHAR"]], by = control$timestep)
	ini <- LC_NPK_iniSTATES(pars)
	state_res <- deSolve::euler(ini, steps, LC_model_NPK, pars, WDATA = wdata)

	state_res <- data.frame(state_res)    
	info <- data.frame(date=wdata$DATE[state_res$time], step=1:nrow(state_res))
	state_res$time <- NULL
	cbind(info, state_res)
}

