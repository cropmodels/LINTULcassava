#-------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA
#
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Date:         28-01-2019
# Adapted:      A.G.T. Schut
# Email:        tom.schut@wur.nl
#
# This file contains the code behind the LINTUL-CASSAVA model. 
#
# LINTUL1: Light INTerception and UtiLization simulator. A simple general crop growth, model which 
#          simulates dry matter produciton as a result of light interception and utilization with a 
#          constant light use efficiency.
# LINTUL2: Is an extended version of LINTUL1. LINTUL 2 includes a simple water balance for studying
#          the effects of drought.
# 
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
#--------------------------------------------------------------------------------------------------#
#source('./Components/LINTUL2_Cassava_Penman.R')
#source('./Components/LINTUL2_Cassava_Actevapo.R')
#source('./Components/LINTUL2_Cassava_DrainRunIrri.R')
#source('./Components/LINTUL2_Cassava_GLAI.R')
#source('./Components/LINTUL2_Cassava_Weather.R')


#As used for JOY's work
LC_model <- function(Time, State, Pars, WDATA){
  with(as.list(c(State, Pars)), {
    #Daily weather data. Use the weather data from the last day 
    #if smaller time-steps are taken
    WDATA <- subset(WDATA, DOYS == floor(Time))

    # Determine weather conditions
    RTRAIN <- WDATA$RAIN                   # mm d-1           : rain rate, mm d-1
    DTEFF  <- max(0, WDATA$DAVTMP - TBASE) # Deg. C           : effective daily temperature
    RPAR   <- FPAR * WDATA$DTR             # PAR MJ m-2 d-1   : PAR radiation
    
    # Determine rates when crop is still growing
    if(TSUM < FINTSUM){
      
      # Temperature sum after planting
      RTSUM <- DTEFF*ifelse((Time-DOYPL) >= 0, 1, 0) # Deg. C 
      
      # Determine water content of rooted soil
      WC  <- 0.001 * WA/ROOTD         # (-) 
      
      #-----------------------------------------EMERGENCE-----------------------------------------------#
      # emergence occurs (1) when the temperature sum exceeds the temperature sum needed for emergence. And (2)
      # when enough water is available in the soil. 
      
      if((WC-WCWP) >= 0 && (TSUM-OPTEMERGTSUM) >= 0) { 	
        emerg1 <- 1} else { emerg1 <- 0 }
      
      # once the crop is established is does not disappear again
      if(TSUMCROP > 0) {									
        emerg2 <- 1 } else { emerg2 <- 0}
      
      EMERG  <- max(emerg1,emerg2) # (-)
      
      # Emergence of the crop is used to calculate the temperature sum of the crop.
      RTSUMCROP <- DTEFF*EMERG      # Deg. C
      
      #------------------------------------FIBROUS ROOT GROWTH------------------------------------------#
      # If the soil water content drops to, or below, wilting point fibrous root growth stops.
      # Root growth continues till the maximum rooting depth is reached.
      # The rooting depth (m) is calculated from a maximum rate of change in rooting depth, 
      # the emergence of the crop and the constraints mentioned above.
      
      if((ROOTD-ROOTDM) < 0 && (WC-WCWP) >= 0) {
        RROOTD <- RRDMAX * EMERG               # mm d-1
      }else{ 
        RROOTD = 0
      }
      
      
      #----------------------------------------WATER BALANCE---------------------------------------------#
      # Explored water of new soil water layers by the roots, explored soil is assumed to have a FC soil moisture
      # content).
      EXPLOR <- 1000 * RROOTD * WCFC                # mm d-1
      
      # Interception of the canopy, depends on the amount of rainfall and the LAI. 
      RNINTC <- min(RTRAIN, (FRACRNINTC * LAI))     # mm d-1
      
      # Potential evaporation and transpiration are calculated using the Penman equation.
      PENM   <- penman(WDATA$DAVTMP,WDATA$VP,WDATA$DTR,LAI,WDATA$WN,RNINTC)
      RPTRAN <- PENM$PTRAN                          # mm d-1
      RPEVAP <- PENM$PEVAP                          # mm d-1
      # Soil moisture content at severe drought and the critical soil moisture content are calculated to see if
      # drought stress occurs in the crop. The critical soil moisture content depends on the transpiration coefficient
      # which is a measure of how drought resistant the crop is. 
      WCSD <- WCWP * TWCSD
      WCCR <- WCWP + pmax(WCSD-WCWP, (RPTRAN/(RPTRAN+TRANCO) * (WCFC-WCWP)))

      # The actual evaporation and transpiration is based on the soil moisture contents and the potential evaporation 
      # and transpiration rates.
      EVA   <- evaptr(RPEVAP,RPTRAN,ROOTD,WA,WCAD,WCWP,TWCSD,WCFC,WCWET,WCST,TRANCO,DELT)
      RTRAN <- EVA$TRAN                             # mm d-1
      REVAP <- EVA$EVAP                             # mm d-1
      
      # The transpiration reduction factor is defined as the ratio between actual and potential transpiration
      TRANRF <- ifelse(RPTRAN <= 0, 1, RTRAN/RPTRAN) # (-)
      
      # Drainage and Runoff is calculated using the drunir function.
      DRUNIR  <- drunir(RTRAIN,RNINTC,REVAP,RTRAN,IRRIGF,DRATE,DELT,WA,ROOTD,WCFC,WCST)
      RDRAIN  <- DRUNIR$DRAIN                      # mm d-1
      RRUNOFF <- DRUNIR$RUNOFF                     # mm d-1
      
      # Rate of change of soil water amount
      RWA <- (RTRAIN + EXPLOR + DRUNIR$IRRIG) - (RNINTC + RRUNOFF + RTRAN + REVAP + RDRAIN)  # mm d-1
      WC  <- 0.001 * WA/ROOTD                      # (-)
      
      #-------------------------------------------DORMANCY AND RECOVERY-------------------------------------------#
      # The crop enters the dormancy phase as the soil water content is lower than the soil water content at 
      # severe drought and as the LAI is lower than the minimal LAI. 
      if ((WC-WCSD) <= 0 && (LAI - LAI_MIN) <= 0){
        dormancy = 1
      } else {
        dormancy = 0
      }
      
      # The crop goes out of dormancy if the water content is higher than a certain recovery water content and as the
      # water content is larger than the wilting point soil moisture content. 
      if ((WC - RECOV * WCCR) >= 0 && (WC - WCWP) >= 0){
        pushdor = 1
      } else {
        pushdor = 0
      }
      
      # The redistributed fraction of storage root DM to the leaves.  
      if (WSO == 0) {
        WSOREDISTFRAC = 1
      } else {
        WSOREDISTFRAC = REDISTSO/WSO
      }
      
      # Three push functions are used to determine the redistribution and recovery from dormancy, a final function DORMANCY
      # is used to indicate if the crop is still in dormancy:
      # (1) PUSHREDISTEND: The activation of the PUSHREDISTEND function ends the redistribution phase. Redistribution stops
      # when the redistributed fraction reached the maximum redistributed fraction or when the minimum amount of new leaves
      # is produced after dormancy or when the Tsum during the recovery exceeds the maximum redistribution temperature sum. 
      # (2) PUSHREDIST: The activation of the PUSHREDIST function ends the dormancy phase including the delay temperature 
      # sum needed for the redistribution of DM. 
      # (3) PUSHDORMREC: Indicates if the the crop is still in dormancy. Dormancy can only when the temperature sum of the 
      # crop exceeds the temperature sum of the branching. 
      PUSHREDISTEND <- pmax(ifelse((WSOREDISTFRAC-WSOREDISTFRACMAX) >= 0, 1, 0), 
                            ifelse((REDISTLVG - WLVGNEWN)>= 0, 1, 0),
                            ifelse((PUSHREDISTSUM - TSUMREDISTMAX) >= 0, 1, 0)) * ifelse(-PUSHREDISTSUM >= 0, 0, 1)     # (-)
      
      PUSHREDIST  <- ifelse((PUSHDORMRECTSUM - DELREDIST) >= 0, 1, 0)* (1 - PUSHREDISTEND)                              # (-)
      PUSHDORMREC <- pushdor*ifelse(-DORMTSUM >= 0, 0, 1) * (1 - PUSHREDIST) * ifelse((TSUMCROP - TSUMSBR) >= 0, 1, 0) # (-)
      
      DORMANCY <- pmax(dormancy, PUSHDORMREC) * (1 - PUSHREDIST) * ifelse((TSUMCROP - TSUMSBR) >= 0, 1, 0)     # (-)
      
      # The temperature sums related to the dormancy and recovery periods.
      RDORMTSUM = DTEFF *DORMANCY - (DORMTSUM/DELT) * PUSHREDIST                                             # Deg. C
      RPUSHDORMRECTSUM = DTEFF * PUSHDORMREC - (PUSHDORMRECTSUM/DELT) * (1 - PUSHDORMREC) * (1 - PUSHREDIST) # Deg. C
      RPUSHREDISTSUM = DTEFF * PUSHREDIST - (PUSHREDISTSUM/DELT) * PUSHREDISTEND                             # Deg. C
      RPUSHREDISTENDTSUM = DTEFF * PUSHREDIST - (PUSHREDISTENDTSUM/DELT) * (1 - PUSHREDISTEND)               # Deg. C   
      
      # No. of days in dormancy
      RDORMTIME = DORMANCY  # d
      
      # Dry matter redistribution after dormancy. The rate of redistribution of the storage roots dry matter to 
      # leaf dry matter. A certain fraction is lost for the conversion of storage organs dry matter to leaf dry
      # matter.
      RREDISTSO = RRREDISTSO * WSO * PUSHREDIST - (REDISTSO/DELT) *ifelse(-DORMTSUM >= 0, 0, 1)  # g DM m-2 d-1
      RREDISTLVG = SO2LV * RREDISTSO * (1- DORMANCY)                                             # g DM m-2 d-1
      RREDISTMAINTLOSS = (1 - SO2LV) * RREDISTSO                                                 # g DM m-2 d-1
      
      
      #------------------------------------LIGHT INTERCEPTION AND GROWTH-----------------------------------------#
      # Light interception and total crop growth rate.
      PARINT <- RPAR * (1 - exp(-K_EXT * LAI))                             # MJ m-2 d-1
      LUE    <- LUE_OPT * approx(TTB[,1], TTB[,2], WDATA$DAVTMP)$y   # g DM m-2 d-1
      
      GTOTAL <- LUE * PARINT * TRANRF * (1 - DORMANCY)  # g DM m-2 d-1
      
      #-------------------------------------LEAF SENESCENCE------------------------------------------------------#
      
      #-------- AGE
      # The calculation of the physiological leaf age.  
      RTSUMCROPLEAFAGE <- DTEFF * EMERG - (TSUMCROPLEAFAGE/DELT) * PUSHREDIST     # Deg. C
      
      # Relative death rate due to aging depending on leaf age and the daily average temperature. 
      RDRDV = ifelse(TSUMCROPLEAFAGE - TSUMLLIFE >= 0, approx(RDRT[,1], RDRT[,2], WDATA$DAVTMP)$y, 0) # d-1
      #--------
      
      #-------- SHEDDING
      # Relative death rate due to self shading, depending on a critical leaf area index at which leaf shedding is
      # induced. Leaf shedding is limited to a maximum leaf shedding per day. 
      RDRSH <- RDRSHM * (LAI-LAICR) / LAICR          # d-1
      
      if(RDRSH < 0) {
        RDRSH <- 0                    # d-1
      } else if(RDRSH >=RDRSHM) {
        RDRSH <- RDRSHM               # d-1
      }
      #--------
      
      #-------- DROUGHT
      # ENSHED triggers enhanced leaf senescence due to severe drought or excessive soil water. It assumes that drought or 
      # excessive water does not affect young leaves. It only affects leaves that have a reached a given fraction of the leaf
      # age. 
      ENHSHED <- max(ifelse((WC-WCSD) >= 0, 0, 1), ifelse((WC-WCWET) >= 0, 1, 0))*
        ifelse((TSUMCROPLEAFAGE-FRACTLLFENHSH*TSUMLLIFE) >= 0, 1, 0)    # (-)
      
      # Relative death rate due to severe drought
      RDRSD <- RDRB *ENHSHED    # d-1
      #--------
      
      # Effective relative death rate and the resulting decrease in LAI.
      RDR   <- max(RDRDV, RDRSH, RDRSD) * ifelse((TSUMCROPLEAFAGE - TSUMLLIFE) >= 0, 1, 0) 	# d-1
      #      DLAI  <- LAI * RDR * (1 - FASTRANSLSO) * (1 - DORMANCY)  			                        # m2 m-2 d-1
      DLAI  <- LAI * RDR * (1 - DORMANCY)  			                        # m2 m-2 d-1
      
      # Fraction of the maximum specific leaf area index depending on the temperature sum of the crop. And its specific leaf
      # area index. 
      FRACSLACROPAGE <- approx(FRACSLATB[,1], FRACSLATB[,2], TSUMCROP)$y  # (-)
      SLA <- SLA_MAX *FRACSLACROPAGE                                      # m2 g-1 DM
      
      # The rate of storage root DM production with DM supplied by the leaves before abscission. 
      RWSOFASTRANSLSO <- WLVG * RDR * FASTRANSLSO * (1 - DORMANCY)        # g storage root DM m-2 d-1
      
      # Decrease in leaf weight due to leaf senesence. 
      #      DLV <- (WLVG * RDR - RWSOFASTRANSLSO) * (1 - DORMANCY)             # g leaves DM m-2 d-1
      DLV <- WLVG * RDR * (1 - DORMANCY)                                 # g leaves DM m-2 d-1
      RWLVD <- (DLV - RWSOFASTRANSLSO)                                   # g leaves DM m-2 d-1
      
      
      #--------------------------------------------PARTITIONING---------------------------------------------------#
      # Allocation of assimilates to the different organs. The fractions are modified for water availability.
      FRTMOD <- max(1, 1/(TRANRF+0.5))			                             # (-)
      # Fibrous roots
      FRT    <- approx(FRTTB[,1],FRTTB[,2],TSUMCROP)$y * FRTMOD		       # (-)
      FSHMOD <- (1 - FRT) / (1 - FRT / FRTMOD)                           # (-)
      # Leaves
      FLV    <- approx(FLVTB[,1],FLVTB[,2],TSUMCROP)$y * FSHMOD	         # (-)
      # Stems
      FST    <- approx(FSTTB[,1],FSTTB[,2],TSUMCROP)$y * FSHMOD	         # (-)
      # Storage roots
      FSO    <- approx(FSOTB[,1],FSOTB[,2],TSUMCROP)$y * FSHMOD	         # (-)
      
      #When plants emerge from dormancy, leaf growth may go far too quickly. 
      #Adjust partitioning if LAI too large
      FLV_ADJ  <- FLV * max(0,min(1,(LAI-LAICR) / LAICR))
      FLV <- FLV - FLV_ADJ
      FSO <- FSO + 0.66 * FLV_ADJ #Not used assimilated go for 2/3 to storage roots
      FST <- FST + 0.34 * FLV_ADJ #Not used assimilated go for 1/3 to stem
      
      
      
      # Minimal stem cutting weight. 
      WCUTTINGMIN <- WCUTTINGMINPRO * WCUTTINGIP
      
      # Stem cutting partioning at emergence. 
      if (EMERG == 1 && WST == 0){
        RWCUTTING <- WCUTTING *(-FST_CUTT - FRT_CUTT - FLV_CUTT - FSO_CUTT) 
        RWRT <- WCUTTINGIP * FRT_CUTT                                        # g fibrous root DM m-2 d-1
        RWST <- WCUTTINGIP * FST_CUTT                                        # g stem DM m-2 d-1
        RWLVG <- WCUTTINGIP * FLV_CUTT                                       # g leaves DM m-2 d-1   
        RWSO  <- WCUTTINGIP * FSO_CUTT                                       # g storage root DM m-2 d-1
      } else if (TSUM > OPTEMERGTSUM) { 
        RWCUTTING <- -RDRWCUTTING * WCUTTING * ifelse((WCUTTING-WCUTTINGMIN) >= 0, 1, 0) * TRANRF * EMERG * (1 - DORMANCY)  # g stem cutting DM m-2 d-1
        RWRT   <- (abs(GTOTAL)+abs(RWCUTTING)) * FRT	                                     # g fibrous root DM m-2 d-1
        RWST   <- (abs(GTOTAL)+abs(RWCUTTING)) * FST	                                     # g stem DM m-2 d-1
        RWLVG  <- (abs(GTOTAL)+abs(RWCUTTING)) * FLV - DLV + RREDISTLVG * PUSHREDIST 	     # g leaves DM m-2 d-1 
        RWSO   <- (abs(GTOTAL)+abs(RWCUTTING)) * FSO + RWSOFASTRANSLSO - RREDISTSO	       # g storage root DM m-2 d-1		  
      } else{
        RWCUTTING <- 0   # g stem cutting DM m-2 d-1
        RWRT <- 0        # g fibrous root DM m-2 d-1
        RWST <- 0        # g stem DM m-2 d-1
        RWLVG <- 0       # g leaves DM m-2 d-1 
        RWSO  <- 0       # g storage root DM m-2 d-1
      }
      # Growth of the leaf weight
      RWLV = RWLVG+RWLVD                  # g leaves DM m-2 d-1
      ## Total biomass increase 
	  ##WGTOTAL = WLV+WST+WCUTTING+WSO+WRT  # g DM m-2 d-1
      
      #--------------------------------------------LEAF GROWTH---------------------------------------------------#
      
      # Green leaf weight 
      GLV <- FLV * (GTOTAL + abs(RWCUTTING)) + RREDISTLVG * PUSHREDIST  # g green leaves DM m-2 d-1
      
      # Growth of the leaf are index
      GLAI <- gla(DTEFF,TSUMCROP,LAII,RGRL,DELT,SLA,LAI,GLV,TSUMLA_MIN,TRANRF,WC,WCWP,RWCUTTING,FLV,
                  LAIEXPOEND,DORMANCY)     # m2 m-2 d-1
      
      # Change in LAI due to new growth of leaves
      RLAI <- GLAI - DLAI    # m2 m-2 d-1

      #Combined 30 rates
      RATES <- c(RROOTD=RROOTD,RWA=RWA,
                 RTSUM=RTSUM, RTSUMCROP=RTSUMCROP, 
                 RTSUMCROPLEAFAGE=RTSUMCROPLEAFAGE,RDORMTSUM=RDORMTSUM,
                 RPUSHDORMRECTSUM=RPUSHDORMRECTSUM,RPUSHREDISTENDTSUM=RPUSHREDISTENDTSUM, 
                 RDORMTIME=RDORMTIME, RWCUTTING=RWCUTTING, 
                 RTRAIN=RTRAIN,RPAR=RPAR,RLAI=RLAI,
                 RWLVD=RWLVD, RWLV=RWLV,RWST=RWST,RWSO=RWSO,RWRT=RWRT, 
                 RWLVG=RWLVG,RTRAN=RTRAN,REVAP=REVAP,RPTRAN=RPTRAN,RPEVAP=RPEVAP, RRUNOFF=RRUNOFF, RNINTC=RNINTC, RDRAIN=RDRAIN, 
                 RREDISTLVG=RREDISTLVG,RREDISTSO=RREDISTSO,RPUSHREDISTSUM=RPUSHREDISTSUM,RWSOFASTRANSLSO=RWSOFASTRANSLSO) 
      #Auxiliaries
      #Converts g m-2 to t ha-1
      AUX <- c(WSOTHA = WSO * 0.01, TRANRF = TRANRF, HI = WSO / (WSO + WLV + WST + WRT))
      
      
    }else{
      # If the plant is not growing anymore all plant related rates are set to 0.
      RATES <- c(RROOTD=0,# m d-1
                 RWA=0,   # mm d-1  
                 RTSUM=0, RTSUMCROP=0, # Deg. C d-1
                 RTSUMCROPLEAFAGE=0,RDORMTSUM=0,# Deg. C d-1
                 RPUSHDORMRECTSUM=0,RPUSHREDISTENDTSUM=0, # Deg. C d-1
                 RDORMTIME=0, # d d-1
                 RWCUTTING=0, # g DM m-2 d-1
                 RTRAIN=0,# mm d-1
                 RPAR=0,# MJ m-2 d-1
                 RLAI=0,# m2 m-2 d-1
                 RWLVD=0, RWLV=0,RWST=0,RWSO=0,RWRT=0, RWLVG=0, # g DM m-2 d-1
                 RTRAN=0,REVAP=0,RPTRAN=0,RPEVAP=0, RRUNOFF=0, RNINTC=0, RDRAIN=0, # mm d-1
                 RREDISTLVG=0,RREDISTSO=0,# g DM m-2 d-1
                 RPUSHREDISTSUM=0,# Deg. C d-1
                 RWSOFASTRANSLSO=0) # g DM m-2 d-1
                 
      #Auxiliaries
      AUX <- c(WSOTHA = 0, TRANRF = NULL, HI = NULL)
    }
    
    
    return(list(RATES,c(AUX, RATES)))
  })
}

