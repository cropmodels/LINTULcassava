# this is the same model but implementing it similar to how I would do it in C++
# to test it prior to translation

iniRates <- function() {
	as.list(c(
		ROOTD=0,# m d-1
		WA=0,   # mm d-1  
		TSUM=0, TSUMCROP=0, # Deg. C d-1
		TSUMCROPLEAFAGE=0, DORMTSUM=0,# Deg. C d-1
		PUSHDORMRECTSUM=0, PUSHREDISTENDTSUM=0, # Deg. C d-1
		DORMTIME=0, # d d-1
		WCUTTING=0, # g DM m-2 d-1
		TRAIN=0,# mm d-1
		PAR=0,# MJ m-2 d-1
		LAI=0,# m2 m-2 d-1
		WLVD=0, WLV=0, WST=0, WSO=0, WRT=0, WLVG=0, # g DM m-2 d-1
		TRAN=0, EVAP=0, PTRAN=0, PEVAP=0, RUNOFF=0, NINTC=0, DRAIN=0, # mm d-1
        REDISTLVG=0, REDISTSO=0,# g DM m-2 d-1
        PUSHREDISTSUM=0,# Deg. C d-1
        WSOFASTRANSLSO=0) # g DM m-2 d-1
	)
}


get_states <- function(S, R) {
	S$ROOTD <- S$ROOTD + R$ROOTD
	S$WA <- S$WA + R$WA
	S$TSUM <- S$TSUM + R$TSUM
	S$TSUMCROP <- S$TSUMCROP + R$TSUMCROP
	S$TSUMCROPLEAFAGE <- S$TSUMCROPLEAFAGE + R$TSUMCROPLEAFAGE
	S$DORMTSUM <- S$DORMTSUM + R$DORMTSUM
	S$PUSHDORMRECTSUM <- S$PUSHDORMRECTSUM + R$PUSHDORMRECTSUM
	S$PUSHREDISTENDTSUM <- S$PUSHREDISTENDTSUM + R$PUSHREDISTENDTSUM
	S$DORMTIME <- S$DORMTIME + R$DORMTIME
	S$WCUTTING <- S$WCUTTING + R$WCUTTING
	S$TRAIN <- S$TRAIN + R$TRAIN
	S$PAR <- S$PAR + R$PAR
	S$LAI <- S$LAI + R$LAI
	S$WLVD <- S$WLVD + R$WLVD
	S$WLV <- S$WLV + R$WLV
	S$WST <- S$WST + R$WST
	S$WSO <- S$WSO + R$WSO
	S$WRT <- S$WRT + R$WRT
	S$WLVG <- S$WLVG + R$WLVG
	S$TRAN <- S$TRAN + R$TRAN
	S$EVAP <- S$EVAP + R$EVAP
	S$PTRAN <- S$PTRAN + R$PTRAN
	S$PEVAP <- S$PEVAP + R$PEVAP
	S$RUNOFF <- S$RUNOFF + R$RUNOFF
	S$NINTC <- S$NINTC + R$NINTC
	S$DRAIN <- S$DRAIN + R$DRAIN
	S$REDISTLVG <- S$REDISTLVG + R$REDISTLVG
	S$REDISTSO <- S$REDISTSO + R$REDISTSO
	S$PUSHREDISTSUM <- S$PUSHREDISTSUM + R$PUSHREDISTSUM
	S$WSOFASTRANSLSO <- S$WSOFASTRANSLSO + R$WSOFASTRANSLSO
	S
}	   
	   
get_rates <- function(today, W, S, R, crop, soil, management, DELT=1) {

    
    if (S$TSUM >= crop$FINTSUM) {
		# If the plant is not growing anymore all plant related rates are set to 0.
		return(R)
    } else {      
		
		#Daily weather data. 
		SatVP_TMMN = 0.611 * exp(17.4 * W$TMIN / (W$TMIN + 239)) 
		SatVP_TMMX = 0.611 * exp(17.4 * W$TMAX / (W$TMAX + 239)) 
  # vapour pressure deficits
		W$VPD_MN = max(0, SatVP_TMMN - W$VAPR)
		W$VPD_MX = max(0, SatVP_TMMX - W$VAPR)
		W$SRAD = W$SRAD / 1000
		W$TAVG = 0.5 * (W$TMIN + W$TMAX)   # Deg. C     :     daily average temperature
		
		R$TRAIN <- W$PREC                   # mm d-1           : rain rate, mm d-1
		DTEFF  <- max(0, W$TAVG - crop$TBASE) # Deg. C           : effective daily temperature
		R$PAR  <- crop$FPAR * W$SRAD             # PAR MJ m-2 d-1   : PAR radiation


      # Temperature sum after planting
		R$TSUM <- ifelse((today - management$PLDATE) >= 0, DTEFF, 0) # Deg. C 
      
      # Determine water content of rooted soil
      # RH: Use of WC  is confusing as it also a state variable 
	  WC <- 0.001 * S$WA/S$ROOTD       # (-) 
      
      #-----------------------------------------EMERGENCE-----------------------------------------------#
      # emergence occurs (1) when the temperature sum exceeds the temperature sum needed for emergence. And (2)
      # when enough water is available in the soil. 
      
      EMERG <- ((S$TSUMCROP > 0) || ((WC-soil$WCWP) >= 0 && (S$TSUM-crop$OPTEMERGTSUM) >= 0))
      
      # Emergence of the crop is used to calculate the temperature sum of the crop.
      R$TSUMCROP <- DTEFF * EMERG      # Deg. C
      
      #------------------------------------FIBROUS ROOT GROWTH------------------------------------------#
      # If the soil water content drops to, or below, wilting point fibrous root growth stops.
      # Root growth continues till the maximum rooting depth is reached.
      # The rooting depth (m) is calculated from a maximum rate of change in rooting depth, 
      # the emergence of the crop and the constraints mentioned above.
      
      if ((S$ROOTD-soil$ROOTDM) < 0 && (WC-soil$WCWP) >= 0) {
        R$ROOTD <- crop$RRDMAX * EMERG               # mm d-1
      } else { 
        R$ROOTD = 0
      }
      
      #----------------------------------------WATER BALANCE---------------------------------------------#
      # Explored water of new soil water layers by the roots, explored soil is assumed to have a FC soil moisture
      # content).
      EXPLOR <- 1000 * R$ROOTD * soil$WCFC                # mm d-1
      
      # Interception of the canopy, depends on the amount of rainfall and the LAI. 
      R$NINTC <- min(R$TRAIN, (crop$FRACRNINTC * S$LAI))     # mm d-1
      
      # Potential evaporation and transpiration are calculated using the Penman equation.
      PENM <- penman(W$TAVG, W$VAPR, W$SRAD, S$LAI, W$WIND, R$NINTC)
      R$PTRAN <- PENM$PTRAN                          # mm d-1
      R$PEVAP <- PENM$PEVAP                          # mm d-1
      # Soil moisture content at severe drought and the critical soil moisture content are calculated to see if drought stress occurs in the crop. The critical soil moisture content depends on the transpiration coefficient which is a measure of how drought resistant the crop is. 
      WCSD <- soil$WCWP * crop$TWCSD
      WCCR <- soil$WCWP + max(WCSD-soil$WCWP, (R$PTRAN/(R$PTRAN+crop$TRANCO) * (soil$WCFC-soil$WCWP)))

      # The actual evaporation and transpiration is based on the soil moisture contents and the potential evaporation and transpiration rates.
      EVA <- evaptr(R$PEVAP, R$PTRAN, S$ROOTD, S$WA, soil$WCAD,
				soil$WCWP,crop$TWCSD,soil$WCFC, soil$WCWET, soil$WCST, crop$TRANCO, DELT)
      R$TRAN <- EVA$TRAN                             # mm d-1
      R$EVAP <- EVA$EVAP                             # mm d-1
      
      # The transpiration reduction factor is defined as the ratio between actual and potential transpiration
      TRANRF <- ifelse(R$PTRAN <= 0, 1, R$TRAN/R$PTRAN) # (-)
      
      # Drainage and Runoff is calculated using the drunir function.
      DRUNIR  <- drunir(R$TRAIN, R$NINTC, R$EVAP, R$TRAN, management$IRRIGF, soil$DRATE,
	                                 DELT, S$WA, S$ROOTD, soil$WCFC, soil$WCST)
      R$DRAIN  <- DRUNIR$DRAIN                      # mm d-1
      R$RUNOFF <- DRUNIR$RUNOFF                     # mm d-1
      
      # Rate of change of soil water amount
      R$WA <- (R$TRAIN + EXPLOR + DRUNIR$IRRIG) - (R$NINTC + R$RUNOFF + R$TRAN + R$EVAP + R$DRAIN)  # mm d-1

# again?
      WC  <- 0.001 * S$WA/S$ROOTD                      # (-)

	if (!EMERG) return(R)

      
      #------------------------DORMANCY AND RECOVERY-------------------------------------------#
      # The crop enters the dormancy phase as the soil water content is lower than the soil water content at 
      # severe drought and as the LAI is lower than the minimal LAI. 
      dormancy <- ((WC-WCSD) <= 0) && ((S$LAI - crop$LAI_MIN) <= 0)
      
      # The crop goes out of dormancy if the water content is higher than a certain recovery water content and as the water content is larger than the wilting point soil moisture content. 
      pushdor <- ((WC - crop$RECOV * WCCR) >= 0) && ((WC - soil$WCWP) >= 0)
      
      # The redistributed fraction of storage root DM to the leaves.  
      WSOREDISTFRAC <- ifelse(S$WSO == 0, 1, S$REDISTSO/S$WSO)
 
      
 # Three push functions are used to determine the redistribution and recovery from dormancy, a final function DORMANCY is used to indicate if the crop is still in dormancy:
      # (1) PUSHREDISTEND: The activation of the PUSHREDISTEND function ends the redistribution phase. Redistribution stops when the redistributed fraction reached the maximum redistributed fraction or when the minimum amount of new leaves is produced after dormancy or when the Tsum during the recovery exceeds the maximum redistribution temperature sum. 
      # (2) PUSHREDIST: The activation of the PUSHREDIST function ends the dormancy phase including the delay temperature sum needed for the redistribution of DM. 
      # (3) PUSHDORMREC: Indicates if the the crop is still in dormancy. Dormancy can only when the temperature sum of the crop exceeds the temperature sum of the branching. 
	  
		PUSHREDISTEND <- ((((WSOREDISTFRAC - crop$WSOREDISTFRACMAX) >= 0) || 
					((S$REDISTLVG - crop$WLVGNEWN)>= 0) || 
					((S$PUSHREDISTSUM - crop$TSUMREDISTMAX) >= 0)) && 
					(S$PUSHREDISTSUM > 0))

      PUSHREDIST  <- ifelse((S$PUSHDORMRECTSUM - crop$DELREDIST) >= 0, !PUSHREDISTEND, FALSE)  # (-)
	  
      PUSHDORMREC <- pushdor && (S$DORMTSUM > 0) && (!PUSHREDIST) && ((S$TSUMCROP - crop$TSUMSBR) >= 0) # (-)
      
      DORMANCY <- (dormancy || PUSHDORMREC) && (!PUSHREDIST) && ((S$TSUMCROP - crop$TSUMSBR) >= 0)     # (-)
      
      # The temperature sums related to the dormancy and recovery periods.
      R$DORMTSUM = DTEFF * DORMANCY - (S$DORMTSUM/DELT) * PUSHREDIST # Deg. C
      R$PUSHDORMRECTSUM = DTEFF * PUSHDORMREC - (S$PUSHDORMRECTSUM/DELT) * (!(PUSHDORMREC || PUSHREDIST))  # Deg. C
	  
      R$PUSHREDISTSUM = DTEFF * PUSHREDIST - (S$PUSHREDISTSUM/DELT) * PUSHREDISTEND  # Deg. C
      R$PUSHREDISTENDTSUM = DTEFF * PUSHREDIST - (S$PUSHREDISTENDTSUM/DELT) * (!PUSHREDISTEND) # Deg. C   
      
      # No. of days in dormancy
      R$DORMTIME = DORMANCY  # d
      
      # Dry matter redistribution after dormancy. The rate of redistribution of the storage roots dry matter to leaf dry matter. A certain fraction is lost for the conversion of storage organs dry matter to leaf dry matter.
      R$REDISTSO = crop$RRREDISTSO * S$WSO * PUSHREDIST - (S$REDISTSO/DELT) * (S$DORMTSUM > 0)  # g DM m-2 d-1
      R$REDISTLVG = crop$SO2LV * R$REDISTSO * (!DORMANCY)  # g DM m-2 d-1
      
	  #RH not used
	  #RREDISTMAINTLOSS = (1 - crop$SO2LV) * R$REDISTSO    # g DM m-2 d-1
  
      
      #----------LIGHT INTERCEPTION AND GROWTH-----------------------------------------#
      # Light interception and total crop growth rate.
      PARINT <- R$PAR * (1 - exp(-crop$K_EXT * S$LAI))                             # MJ m-2 d-1
      LUE <- crop$LUE_OPT * stats::approx(crop$TTB[,1], crop$TTB[,2], W$TAVG)$y   # g DM m-2 d-1
      
      GTOTAL <- LUE * PARINT * TRANRF * (!DORMANCY)  # g DM m-2 d-1
      
      #----------LEAF SENESCENCE------------------------------------------------------#
      
      #-------- AGE
      # The calculation of the physiological leaf age.  
      R$TSUMCROPLEAFAGE <- DTEFF * EMERG - (S$TSUMCROPLEAFAGE/DELT) * PUSHREDIST     # Deg. C
      
      # Relative death rate due to aging depending on leaf age and the daily average temperature. 
      RDRDV = ifelse(S$TSUMCROPLEAFAGE - crop$TSUMLLIFE >= 0, stats::approx(crop$RDRT[,1], crop$RDRT[,2], W$TAVG)$y, 0) # d-1
      #--------
      
      #-------- SHEDDING
      # Relative death rate due to self shading, depending on a critical leaf area index at which leaf shedding is
      # induced. Leaf shedding is limited to a maximum leaf shedding per day. 
      RDRSH <- crop$RDRSHM * (S$LAI-crop$LAICR) / crop$LAICR          # d-1
      RDRSH <- min(max(0, RDRSH), crop$RDRSHM)  
      #--------
      
      #-------- DROUGHT
      # ENSHED triggers enhanced leaf senescence due to severe drought or excessive soil water. It assumes that drought or excessive water does not affect young leaves. It only affects leaves that have a reached a given fraction of the leaf age. 

      ENHSHED <- (((WC-WCSD) < 0) || ((WC-soil$WCWET) >= 0)) && 
				((S$TSUMCROPLEAFAGE - crop$FRACTLLFENHSH * crop$TSUMLLIFE) >= 0)    # (-)
      
      # Relative death rate due to severe drought
      RDRSD <- crop$RDRB * ENHSHED    # d-1
      #--------
      
      # Effective relative death rate and the resulting decrease in LAI.
      RDR <- ifelse((S$TSUMCROPLEAFAGE - crop$TSUMLLIFE) >= 0, max(RDRDV, RDRSH, RDRSD), 0) 	# d-1
      #      DLAI  <- LAI * RDR * (1 - FASTRANSLSO) * (1 - DORMANCY)    # m2 m-2 d-1
      DLAI  <- S$LAI * RDR * (!DORMANCY)    # m2 m-2 d-1
      
      # Fraction of the maximum specific leaf area index depending on the temperature sum of the crop. And its specific leaf area index. 
      FRACSLACROPAGE <- stats::approx(crop$FRACSLATB[,1], crop$FRACSLATB[,2], S$TSUMCROP)$y  # (-)
      SLA <- crop$SLA_MAX * FRACSLACROPAGE    # m2 g-1 DM
      
      # The rate of storage root DM production with DM supplied by the leaves before abscission. 
      R$WSOFASTRANSLSO <- S$WLVG * RDR * crop$FASTRANSLSO * (!DORMANCY)        # g storage root DM m-2 d-1
      
      # Decrease in leaf weight due to leaf senesence. 
      #      DLV <- (WLVG * RDR - RWSOFASTRANSLSO) * (1 - DORMANCY)             # g leaves DM m-2 d-1
      DLV <- S$WLVG * RDR * (!DORMANCY)                                 # g leaves DM m-2 d-1
      R$WLVD <- (DLV - R$WSOFASTRANSLSO)                                   # g leaves DM m-2 d-1
      
      
      #------------------------------------PARTITIONING---------------------------------------------------#
      # Allocation of assimilates to the different organs. The fractions are modified for water availability.
      FRTMOD <- max(1, 1/(TRANRF+0.5))			                             # (-)
      # Fibrous roots
      FRT    <- stats::approx(crop$FRTTB[,1], crop$FRTTB[,2], S$TSUMCROP)$y * FRTMOD # (-)
      FSHMOD <- (1 - FRT) / (1 - FRT / FRTMOD)                           # (-)
      # Leaves
      FLV    <- stats::approx(crop$FLVTB[,1], crop$FLVTB[,2], S$TSUMCROP)$y * FSHMOD # (-)
      # Stems
      FST    <- stats::approx(crop$FSTTB[,1], crop$FSTTB[,2], S$TSUMCROP)$y * FSHMOD # (-)
      # Storage roots
      FSO    <- stats::approx(crop$FSOTB[,1], crop$FSOTB[,2], S$TSUMCROP)$y * FSHMOD # (-)
      
      #When plants emerge from dormancy, leaf growth may go far too quickly. 
      #Adjust partitioning if LAI too large
      FLV_ADJ  <- FLV * max(0, min(1, (S$LAI-crop$LAICR) / crop$LAICR))
      FLV <- FLV - FLV_ADJ
      FSO <- FSO + 0.66 * FLV_ADJ #Not used assimilated go for 2/3 to storage roots
      FST <- FST + 0.34 * FLV_ADJ #Not used assimilated go for 1/3 to stem
           
      # Minimal stem cutting weight. 
      WCUTTINGMIN <- crop$WCUTTINGMINPRO * crop$WCUTTINGIP
      
      # Stem cutting partioning at emergence. 
      if (EMERG && (S$WST == 0)) {
        R$WCUTTING <- S$WCUTTING *(-crop$FST_CUTT - crop$FRT_CUTT - crop$FLV_CUTT - crop$FSO_CUTT) 
        R$WRT <- crop$WCUTTINGIP * crop$FRT_CUTT      # g fibrous root DM m-2 d-1
        R$WST <- crop$WCUTTINGIP * crop$FST_CUTT      # g stem DM m-2 d-1
        R$WLVG <- crop$WCUTTINGIP * crop$FLV_CUTT     # g leaves DM m-2 d-1   
        R$WSO  <- crop$WCUTTINGIP * crop$FSO_CUTT     # g storage root DM m-2 d-1
      } else if (S$TSUM > crop$OPTEMERGTSUM) { 
        R$WCUTTING <- -crop$RDRWCUTTING * S$WCUTTING * ((S$WCUTTING-WCUTTINGMIN) >= 0) * TRANRF * EMERG * (!DORMANCY)  # g stem cutting DM m-2 d-1
        R$WRT   <- (abs(GTOTAL)+abs(R$WCUTTING)) * FRT	# g fibrous root DM m-2 d-1
        R$WST   <- (abs(GTOTAL)+abs(R$WCUTTING)) * FST	# g stem DM m-2 d-1
        R$WLVG  <- (abs(GTOTAL)+abs(R$WCUTTING)) * FLV - DLV + R$REDISTLVG * PUSHREDIST # g leaves DM m-2 d-1 
        R$WSO   <- (abs(GTOTAL)+abs(R$WCUTTING)) * FSO + R$WSOFASTRANSLSO - R$REDISTSO	 # g storage root DM m-2 d-1		  
      } else {
        R$WCUTTING <- 0   # g stem cutting DM m-2 d-1
        R$WRT <- 0        # g fibrous root DM m-2 d-1
        R$WST <- 0        # g stem DM m-2 d-1
        R$WLVG <- 0       # g leaves DM m-2 d-1 
        R$WSO  <- 0       # g storage root DM m-2 d-1
      }
      # Growth of the leaf weight
      R$WLV = R$WLVG + R$WLVD                  # g leaves DM m-2 d-1
      # Total biomass increase 
      #RH: this is total biomass (state) not the increase (rate) (apparently not used)
	  #WGTOTAL = WLV+WST+WCUTTING+WSO+WRT  # g DM m-2 d-1
      
      #--------------------------------------------LEAF GROWTH---------------------------------------------------#
      
      # Green leaf weight 
      GLV <- FLV * (GTOTAL + abs(R$WCUTTING)) + R$REDISTLVG * PUSHREDIST  # g green leaves DM m-2 d-1
      
      # Growth of the leaf are index
      GLAI <- gla(DTEFF, S$TSUMCROP, crop$LAII, crop$RGRL, DELT, SLA, S$LAI, GLV, 
				crop$TSUMLA_MIN, TRANRF, WC, soil$WCWP, R$WCUTTING, FLV,
                crop$LAIEXPOEND, DORMANCY)     # m2 m-2 d-1
      
      # Change in LAI due to new growth of leaves
      R$LAI <- GLAI - DLAI    # m2 m-2 d-1

      #Combined 30 rates
      #RATES <- c(RROOTD=RROOTD, RWA=RWA, RTSUM=RTSUM, RTSUMCROP=RTSUMCROP, RTSUMCROPLEAFAGE=RTSUMCROPLEAFAGE, RDORMTSUM=RDORMTSUM, RPUSHDORMRECTSUM=RPUSHDORMRECTSUM, RPUSHREDISTENDTSUM=RPUSHREDISTENDTSUM,           RDORMTIME=RDORMTIME, RWCUTTING=RWCUTTING, RTRAIN=RTRAIN, RPAR=RPAR, RLAI=RLAI,                 RWLVD=RWLVD, RWLV=RWLV, RWST=RWST, RWSO=RWSO,RWRT=RWRT, RWLVG=RWLVG, RTRAN=RTRAN, REVAP=REVAP, RPTRAN=RPTRAN, RPEVAP=RPEVAP, RRUNOFF=RRUNOFF, RNINTC=RNINTC, RDRAIN=RDRAIN, RREDISTLVG=RREDISTLVG, RREDISTSO=RREDISTSO,RPUSHREDISTSUM=RPUSHREDISTSUM, RWSOFASTRANSLSO=RWSOFASTRANSLSO) 

    }
	R
}


LINTCAS2 <- function(weather, crop, soil, management, control){

	DELT <- control$timestep
	pars <- c(crop, soil, management$IRRIGF, DELT=DELT)
	iR <- iniRates() 	
	wth <- weather[weather$DATE >= control$startDATE, ]
	
	season <- seq(control$startDATE, management$HVDATE, by = DELT)
	out <- vector(length=length(season), mode="list")
	S <- as.list(LC_iniSTATES(pars))
	for (i in 1:length(season)) {
		R <- get_rates(season[i], wth[i, ], S, iR, crop, soil, management, DELT)
		states <- unlist(S)
		AUX <- c(WSOTHA = S[["WSO"]] * 0.01, TRANRF = ifelse(R$PTRAN <= 0, 1, R$TRAN/R$PTRAN), 
				HI = states[["WSO"]] / sum(states[c("WSO", "WLV", "WST", "WRT")]))
		# order 
		R <- R[names(S)]
		rates <- unlist(R)
		names(rates) <- paste0("R", names(rates))
		out[[i]] <- c(states, AUX, rates)
		#S <- S + rates
		S <- get_states(S, R)
    }
	out <- do.call(rbind, out)

# for compatability with the original output
	steps <- seq(control$startDOY, management$DOYHAR, by = DELT)
	j <- 1:length(steps)
	DAYS <- (weather$DOY[1] + (1:nrow(weather))-1)[steps]

	out <- data.frame(year_planting=wth$YEAR[1], year=wth$YEAR[j], DOY=weather$DOY[steps], time=DAYS, out)
	out
}

#i = 405
#today <- steps[i]; W=wth[i, ]; S=as.list(S)
#new[405, c(42, 44, 45, 50, 52, 54, 56, 64, 65, 66)]
#old[405, c(42, 44, 45, 50, 52, 54, 56, 64, 65, 66)]
