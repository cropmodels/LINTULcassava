

LC_crop <- function(x) {
	stopifnot(x %in% c("Adiele", "Ezui"))
	if (x == "Adiele") {
		p <- LINTUL2_CASSAVA_PARAMETERS_ADIELE()
	} else { #if (x == "Ezui") {
		p <- LINTUL2_CASSAVA_PARAMETERS_EZUI() 
	}
	#p[["IRRIGF"]] <- as.integer(as.logical(irri))
	p$IRRIGF <- NULL #management paramter 
	p$ROOTDM <- NULL  # soil parameter
	p$DELT <- NULL # control paramter
	p
}

Adiele <- function(site, year) { 

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

	lcp <- system.file(package="LINTULcassava")
	ss <- readRDS(file.path(lcp, "ex/fields.rds"))
	if (site == "Benue") {
		site <- "Benue2"
	} else if (site == "Cross River") {
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

	mng <- list(DOYPL=x$DOYPL, DOYHAR=x$DOYHAR)
	cntr <- list(timestep=1, starttime=x$DOYPL-100)
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
		
	list(weather=wth, soil=x, management=mng, control=cntr)
}


Adiele_weather <- function(site, year) { 
	lcp <- system.file(package="LINTULcassava")
	wth <- readRDS(file.path(lcp, "ex/weather.rds"))
	w <- wth[(wth$NAME==site) & (wth$YEAR >= year), ]
	if (nrow(w) == 0) {
		stop("no weather data available for this site/year")
	}
	w
}

