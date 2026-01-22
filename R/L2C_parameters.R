# Authors: AGT Schut, RJ Hijmans


LC_crop <- function(x, npk=FALSE) {
	stopifnot(x %in% c("Adiele", "Ezui"))
	if (x == "Adiele") {
		p <- L2C_PARAMETERS_ADIELE()
	} else { #if (x == "Ezui") {
		p <- L2C_PARAMETERS_EZUI() 
	}
	p$IRRIGF <- NULL #control parameter 
	p$ROOTDM <- NULL  # soil parameter
	p$DELT <- NULL # control paramter
	p$WSOREDISTFRAC <- NULL # not a crop parameter 
	
	if (npk) {
		c(p, L2C_NPK_PARAMETERS())
	} else {
		p
	}
}

Adiele <- function(site, year, NPK=FALSE) { 

	params = c(
		"ROOTDM"= "Maximum_rooting_depth.m", # m : maximum rooting depth
		"DOYPL" = "Day_of_planting.DOY",  # Day nr of planting and basal NPK application
		"DOYHAR" = "Day_of_harvest.Days_since_1_january_of_planting_year", # Day nr of harvest, counted from 1 Jan in year of planting		
		"WCAD" = "Water_content_air_dry.m3.H2O..m.3.soil.", # m3 m-3 : soil water content at air dryness 
		"WCWP" = "Water_content_wilting_point.m3.H2O..m.3.soil.", # m3 m-3 : soil water content at wilting point
		"WCFC" = "Water_content_field_capacity.m3.H2O..m.3.soil.", # m3 m-3 : soil water content at field capacity 
		"WCWET" = "Water_content_wet.m3.H2O..m.3.soil.", # m3 m-3 : critical soil water content for transpiration reduction due to waterlogging
		"WCST" = "Water_content_saturated.m3.H2O..m.3.soil.", # m3 m-3 : soil water content at full saturation 
		"DRATE" = "Drainage_rate.mm.d.1" # mm d-1 : max drainage rate
	)

	ss <- readRDS(system.file(package="LINTULcassava", "ex/fields.rds"))
	if (site == "Benue") {
		site <- "Benue2"
	} else if (site %in% c("CRS", "Cross River")) {
		if (year == 2016) {
			site <- "Cross River1"
		} else {
			site <- "Cross River2"
		}
	} 
	s <- ss[(ss$Location==site) & (ss$Year_of_planting==year), ]
	if (nrow(s) == 0) {
		stop("no parameters data available for this site/year")
	}
	i <- match(params, names(s))
	x <- as.list(s[i])
	names(x) <- names(params)

	PLDATE <- as.Date(paste0(year, "-01-01")) + x$DOYPL - 1
	HVDATE <- PLDATE + (x$DOYHAR-x$DOYPL)
	#mng <- list(DOYPL=x$DOYPL, DOYHAR=x$DOYHAR, PLDATE=PLDATE, HVDATE=HVDATE)
	mng <- list(PLDATE=PLDATE, HVDATE=HVDATE)
	cntr <- list(timestep=1, startDATE=PLDATE-100)
	x$DOYPL <- x$DOYHAR <- NULL
	
	if (site=="Edo") {
		wth <- Adiele_weather("Benin", year)
	} else if (site == "Cross River1") {
		wth <- Adiele_weather("Ogoja CRS", year)
	} else if (site == "Cross River2") {
		wth <- Adiele_weather("Ikom CRS", year)
	} else if (site == "Benue2") {
		wth <- Adiele_weather("Benue", year)
	} else {
		stop("something went wrong")
	}
		
	p <- list(weather=wth, soil=x, management=mng, control=cntr)
	
	if (NPK) {
		sites <- readRDS(system.file(package="LINTULcassava", "ex/fieldsNPK.rds"))
		sinfo <- sites[sites$Location == site & sites$Year_of_planting == year, ]
		NPK <- cbind(N = c(  0, 100, 100, 100),#kg/ha N
					 P = c(100,   0,   0,   0),#kg/ha P
					 K = c(  0, 100, 100, 100)) #kg/ha K
		mg <- FIELD_MANAGEMENT_NPK(sinfo, crop, NPK)
		p$soil <- c(p$soil, mg$soil_chem)
		p$management$FERTAB <- mg$mgmt$FERTAB
		p$control$NPK_model=TRUE
	}
	p
}


Adiele_weather <- function(site, year) { 
	wth <- readRDS(system.file(package="LINTULcassava", "ex/weather.rds"))
	Y <- as.integer(format(wth$date, "%Y"))
	w <- wth[(wth$name==site) & (Y >= year), ]
	if (nrow(w) == 0) {
		stop("no weather data available for this site/year")
	}
	w
}



FIELD_MANAGEMENT_NPK <- function(SiteInfo, MODEL_PARAM, fertilizerNPK) { 

	phys_soil = c(
		ROOTDM = SiteInfo[1,"Maximum_rooting_depth.m"] ,    # m            :     maximum rooting depth
		WCAD   = SiteInfo[1,"Water_content_air_dry.m3.H2O..m.3.soil."],   # m3 m-3      :     soil water content at air dryness 
		WCWP   = SiteInfo[1,"Water_content_wilting_point.m3.H2O..m.3.soil."],   # m3 m-3      :     soil water content at wilting point
		WCFC   = SiteInfo[1,"Water_content_field_capacity.m3.H2O..m.3.soil."],    # m3 m-3      :     soil water content at field capacity 
		WCWET  = SiteInfo[1,"Water_content_wet.m3.H2O..m.3.soil."],   # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
		WCST   = SiteInfo[1,"Water_content_saturated.m3.H2O..m.3.soil."],    # m3 m-3      :     soil water content at full saturation 
		DRATE  = SiteInfo[1,"Drainage_rate.mm.d.1"]      # mm d-1      :     max drainage rate
	)
 
	chem_soil <- list()
	#Add soil supply, extra uptake from soil for omission treatments 
	#needs to be in g/m2!!
	chem_soil[["NMINI"]] = SiteInfo[1,"N_soil.kg.ha"] * 0.1
	if (any(fertilizerNPK[,"K"] > 0) || any(fertilizerNPK[,"P"] > 0)) {
		chem_soil[["NMINI"]] = chem_soil[["NMINI"]] + SiteInfo[1,"N_PK.kg.ha"] * 0.1
	}
	chem_soil[["PMINI"]] = SiteInfo[1,"P_soil.kg.ha"] * 0.1
	if (any(fertilizerNPK[,"N"] > 0) || any(fertilizerNPK[,"K"] > 0)) {
		chem_soil[["PMINI"]] = chem_soil[["PMINI"]] + SiteInfo[1,"P_NK.kg.ha"] * 0.1
	}
	chem_soil[["KMINI"]] = SiteInfo[1,"K_soil.kg.ha"] * 0.1
	if (any(fertilizerNPK[,"N"] > 0) || any(fertilizerNPK[,"P"] > 0)) {
		chem_soil[["KMINI"]] = chem_soil[["KMINI"]] + SiteInfo[1,"K_NP.kg.ha"] * 0.1
	}
  
	fdays <- c("Day_of_basal_fertilizer_application.DOY", "Day_of_first_topdressing.DOY", "Day_of_second_topdressing.DOY", "Day_of_third_topdressing.DOY")
	DAYS <- unname(unlist(SiteInfo[1,fdays]))
	#dates <- as.Date(paste0(SiteInfo[1, "Year_of_planting"], "-01-01")) + DAYS - 1
	DAP <- DAYS - SiteInfo[1, "Day_of_planting.DOY"]
	fert <- cbind(DAP=DAP, fertilizerNPK[, c("N", "P", "K")])
	
	mgmt <- list(
		DOYPL  = SiteInfo[1,"Day_of_planting.DOY"],    # Day nr of planting and basal NPK application
		DOYHAR = SiteInfo[1,"Day_of_harvest.Days_since_1_january_of_planting_year"], # Day nr of harvest, counted from 1 Jan in year of planting
		FERTAB = fert 
	)
	list(soil_phys=phys_soil, soil_chem=chem_soil, mgmt=mgmt)
   
  #NOTE: the order of concatenation matters!!!! FIELD PARAM needs to go first.
  #return( c(FIELD_PARAM, MODEL_PARAM))
}


