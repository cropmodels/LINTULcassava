#-------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA_NPK
#
# Author:       Rob van den Beuken, J. Adiele and A.G.T. Schut
# Copyright:    Copyright 2020, PPS
# Email:        tom.schut@wur.nl
# Date modified: 06-02-2020
#
# This file contains the code behind the LINTUL-CASSAVA_NPK model. 
#
# LINTUL1: Light INTerception and UtiLization simulator. A simple general crop growth, model which 
#          simulates dry matter produciton as a result of light interception and utilization with a 
#          constant light use efficiency.
# LINTUL2: Is an extended version of LINTUL1. LINTUL 2 includes a simple water balance for studying
#          the effects of drought.
# LINTUL2-Cassava: Is a modified version of LINTUL 2 for cassava.
# LINTUL2-Cassava_NPK: Is an extended version of LINTUL2-Cassava including nutrient, which is based
#          on LINTUL5. 
# Build on version as used in Adiele et al. 2020 Eur. J. Agronomy")
# Plant Production Systems, WU, 2020
#
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
# Developer LINTUL5: Wolf, J. (2012). LINTUL5: Simple generic model for simulation of crop growth 
# under potential water limited and nitrogen, phosphorus and potassium limited conditions. Wageningen
# University, Group Plant Production Systems. 
#
#--------------------------------------------------------------------------------------------------#
#ENSURE that LINTUL_CASSAVA is also present!!
#Load basic functions
#source('./Components/LINTUL2_Cassava_Penman.R')
#source('./Components/LINTUL2_Cassava_Actevapo.R')
#source('./Components/LINTUL2_Cassava_DrainRunIrri.R')
#source('./Components/LINTUL2_Cassava_Weather.R')
#Load NPK functions
#source('./Components/LINTUL2_Cassava_NPK_GLAI.R')
#source('./Components/LINTUL2_Cassava_NPK_NPKI.R')
#source('./Components/LINTUL2_Cassava_NPK_NutrientDynamics.R')

LINTUL2_CASSAVA_NPK <- function(Time, State, Pars, WDATA){
  with(as.list(c(State, Pars)), {
    WDATA <- subset(WDATA, DOYS == floor(Time))
    # Determine weather conditions
	SatVP_TMMN = 0.611 * exp(17.4 * WDATA$TMIN / (WDATA$TMIN + 239)) 
	SatVP_TMMX = 0.611 * exp(17.4 * WDATA$TMAX / (WDATA$TMAX + 239)) 
  # vapour pressure deficits
	WDATA$VPD_MN = pmax(0, SatVP_TMMN - WDATA$VAPR)
	WDATA$VPD_MX = pmax(0, SatVP_TMMX - WDATA$VAPR)
	WDATA$SRAD = WDATA$SRAD / 1000
	WDATA$TAVG = 0.5 * (WDATA$TMIN + WDATA$TMAX)   # Deg. C     :     daily average temperature

    # Determine weather conditions
    RTRAIN <- WDATA$PREC                   # mm d-1           : rain rate, mm d-1
    DTEFF  <- max(0, WDATA$TAVG - TBASE) # Deg. C           : effective daily temperature
    RPAR   <- FPAR * WDATA$SRAD             # PAR MJ m-2 d-1   : PAR radiation
   
    # Determine rates when crop is still growing
    if(Time >= DOYPL && TSUM < FINTSUM){
      
      # if(Time == DOYPL){
      #   print("LINTUL-CASSAVA-NPK")
      #   print("Build on version as used in Adiele et al. 2020 Eur. J. Agronomy")
      #   print("Plant Production Systems, WU, 2020")
      # }
      
      # Temperature sum after planting. Should integrate to dC
      RTSUM <- (DTEFF / DELT) *ifelse((Time-DOYPL) >= 0, 1, 0) # Deg. C d-1
      
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
      PENM   <- penman(WDATA$TAVG,WDATA$VAPR, WDATA$SRAD,LAI, WDATA$WIND,RNINTC)
      RPTRAN <- PENM$PTRAN                          # mm d-1
      RPEVAP <- PENM$PEVAP                          # mm d-1
      
      # Soil moisture content at severe drought and the critical soil moisture content are calculated to see if
      # drought stress occurs in the crop. The critical soil moisture content depends on the transpiration coefficient
      # which is a measure of how drought resistant the crop is. 
      WCSD <- WCWP * TWCSD
      WCCR <- WCWP + pmax(WCSD-WCWP, (RPTRAN /(RPTRAN + TRANCO) * (WCFC-WCWP)))
      
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
      
      #--------------------------------------------NUTRIENT LIMITATION-------------------------------------------#
      # The nutrient limitation is based on the nutrient concentrations in the organs of the crop. A nutrition index
      # is calculated to quantify nutrient limitation. 
      
      # Minimum and maximum nutrient concentrations in the leaves
      NMINLV <- approx(NMINMAXLV[,1], NMINMAXLV[,2], TSUMCROP)$y   # g N g-1 DM
      PMINLV <- approx(PMINMAXLV[,1], PMINMAXLV[,2], TSUMCROP)$y   # g P g-1 DM
      KMINLV <- approx(KMINMAXLV[,1], KMINMAXLV[,2], TSUMCROP)$y   # g K g-1 DM
      NMAXLV <- approx(NMINMAXLV[,1], NMINMAXLV[,3], TSUMCROP)$y   # g N g-1 DM
      PMAXLV <- approx(PMINMAXLV[,1], PMINMAXLV[,3], TSUMCROP)$y   # g P g-1 DM
      KMAXLV <- approx(KMINMAXLV[,1], KMINMAXLV[,3], TSUMCROP)$y   # g K g-1 DM
      # Minimum and maximum concentrations in the stems
      NMINST <- approx(NMINMAXST[,1], NMINMAXST[,2], TSUMCROP)$y   # g N g-1 DM
      PMINST <- approx(PMINMAXST[,1], PMINMAXST[,2], TSUMCROP)$y   # g P g-1 DM
      KMINST <- approx(KMINMAXST[,1], KMINMAXST[,2], TSUMCROP)$y   # g K g-1 DM
      NMAXST <- approx(NMINMAXST[,1], NMINMAXST[,3], TSUMCROP)$y   # g N g-1 DM
      PMAXST <- approx(PMINMAXST[,1], PMINMAXST[,3], TSUMCROP)$y   # g P g-1 DM
      KMAXST <- approx(KMINMAXST[,1], KMINMAXST[,3], TSUMCROP)$y   # g K g-1 DM
      # Minimum and maximum nutrient concentrations in the storage organs
      NMINSO <- approx(NMINMAXSO[,1], NMINMAXSO[,2], TSUMCROP)$y   # g N g-1 DM
      PMINSO <- approx(PMINMAXSO[,1], PMINMAXSO[,2], TSUMCROP)$y   # g P g-1 DM
      KMINSO <- approx(KMINMAXSO[,1], KMINMAXSO[,2], TSUMCROP)$y   # g K g-1 DM
      NMAXSO <- approx(NMINMAXSO[,1], NMINMAXSO[,3], TSUMCROP)$y   # g N g-1 DM
      PMAXSO <- approx(PMINMAXSO[,1], PMINMAXSO[,3], TSUMCROP)$y   # g P g-1 DM
      KMAXSO <- approx(KMINMAXSO[,1], KMINMAXSO[,3], TSUMCROP)$y   # g K g-1 DM
      # Minimum and maximum nutrient concentrations in the roots
      NMINRT <- approx(NMINMAXRT[,1], NMINMAXRT[,2], TSUMCROP)$y   # g N g-1 DM
      PMINRT <- approx(PMINMAXRT[,1], PMINMAXRT[,2], TSUMCROP)$y   # g P g-1 DM
      KMINRT <- approx(KMINMAXRT[,1], KMINMAXRT[,2], TSUMCROP)$y   # g K g-1 DM
      NMAXRT <- approx(NMINMAXRT[,1], NMINMAXRT[,3], TSUMCROP)$y   # g N g-1 DM
      PMAXRT <- approx(PMINMAXRT[,1], PMINMAXRT[,3], TSUMCROP)$y   # g P g-1 DM
      KMAXRT <- approx(KMINMAXRT[,1], KMINMAXRT[,3], TSUMCROP)$y   # g K g-1 DM
      
      NPKICAL <- npkical(WLVG, WST, WSO,
                         NMINLV, PMINLV, KMINLV, 
                         NMINST, PMINST, KMINST,
                         NMINSO, PMINSO, KMINSO,
                         NMAXLV, PMAXLV, KMAXLV, 
                         NMAXST, PMAXST, KMAXST,
                         NMAXSO, PMAXSO, KMAXSO,
                         FR_MAX, K_NPK_NI, K_MAX,
                         ANLVG, ANST, ANSO, 
                         APLVG, APST, APSO, 
                         AKLVG, AKST, AKSO)

      # Nutrient limitation reduction factor when nutrient limition is switched on
      if(NUTRIENT_LIMITED){
        #Simple based on daily values
        NPKI <- pmax(0, pmin(1, NPKICAL$NPKI)) # (-)
        #Shortly after emergence nutrient stress does not occur
        NPKI <- ifelse(TSUMCROP < TSUM_NPKI, 1, NPKI)
      }else{
        NPKI =1
      }

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
      LUE    <- LUE_OPT * approx(TTB[,1], TTB[,2], WDATA$TAVG)$y   # g DM m-2 d-1
      
      # When water stress is more severe or nutrient is stress is more severe
      if (TRANRF <= NPKI){
        GTOTAL <- LUE * PARINT * TRANRF * (1 - DORMANCY)  # g DM m-2 d-1
        
      } else{
        GTOTAL <- LUE * PARINT * NPKI * (1 - DORMANCY)  # g DM m-2 d-1 
      }
      
      #-------------------------------------LEAF SENESCENCE------------------------------------------------------#
      
      #-------- AGE
      # The calculation of the physiological leaf age.  
      RTSUMCROPLEAFAGE <- DTEFF * EMERG - (TSUMCROPLEAFAGE/DELT) * PUSHREDIST     # Deg. C
      
      # Relative death rate due to aging depending on leaf age and the daily average temperature. 
      RDRDV = ifelse(TSUMCROPLEAFAGE - TSUMLLIFE >= 0, approx(RDRT[,1], RDRT[,2], WDATA$TAVG)$y, 0) # d-1
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
      
      #-------- NUTRIENT LIMITATION
      # Leaf death due to nutrient limitation is added on top op the relative death rate due to age, shade 
      # and drought. 
      RDRNS <- RDRNS * (1-NPKI)    # d-1
      #--------
      
      # Effective relative death rate and the resulting decrease in LAI. Leaf death can only occur, when
      # the leaves are old enough. 
      RDR   <- (max(RDRDV, RDRSH, RDRSD) + RDRNS) * ifelse((TSUMCROPLEAFAGE - TSUMLLIFE) >= 0, 1, 0) 	# d-1
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
      # Nutrient limitation is also assumed to affect partitioning to the roots. 
      FRTMOD <- max(1, 1/(TRANRF * NPKI + 0.5))			                             # (-)
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
      
      #Default rates for weight, N,P,K amount changes in plant organs and the stem cutting 
      RWCUTTING <- 0   # g stem cutting DM m-2 d-1
      RWRT <- 0        # g fibrous root DM m-2 d-1
      RWST <- 0        # g stem DM m-2 d-1
      RWLVG <- 0       # g leaves DM m-2 d-1 
      RWSO  <- 0       # g storage root DM m-2 d-1
      RWCUTTING=0      # g cutting DM m-2 d-1
      RNCUTTING=0      # g cutting N m-2 d-1
      RPCUTTING=0      # g cutting P m-2 d-1
      RKCUTTING=0      # g cutting K m-2 d-1

      # Stem cutting partioning at emergence. 
      if (EMERG == 1 && WST == 0){
        RWCUTTING <- WCUTTING *(-FST_CUTT - FRT_CUTT - FLV_CUTT - FSO_CUTT) 
        RWRT <- WCUTTINGIP * FRT_CUTT                                        # g fibrous root DM m-2 d-1
        RWST <- WCUTTINGIP * FST_CUTT                                        # g stem DM m-2 d-1
        RWLVG <- WCUTTINGIP * FLV_CUTT                                       # g leaves DM m-2 d-1   
        RWSO  <- WCUTTINGIP * FSO_CUTT                                       # g storage root DM m-2 d-1
        
        #The amount of N, P, K transfered depends on max. concentrations in LV, ST, RT and SO 
        RNCUTTING <- -(RWLVG * NMAXLV + RWST * NMAXST + RWSO * NMAXSO + RWRT * NMAXRT)
        RPCUTTING <- -(RWLVG * PMAXLV + RWST * PMAXST + RWSO * PMAXSO + RWRT * PMAXRT)
        RKCUTTING <- -(RWLVG * KMAXLV + RWST * KMAXST + RWSO * KMAXSO + RWRT * KMAXRT)
                
      } else if (TSUM > OPTEMERGTSUM) { 
        #Movement of DM and NPK to other plant parts, depending on partitioning
        RWCUTTING <- -RDRWCUTTING * WCUTTING * ifelse((WCUTTING-WCUTTINGMIN) >= 0, 1, 0) * TRANRF * EMERG * (1 - DORMANCY)  # g stem cutting DM m-2 d-1
        RWRT   <- (abs(GTOTAL)+abs(RWCUTTING)) * FRT	                                     # g fibrous root DM m-2 d-1
        RWST   <- (abs(GTOTAL)+abs(RWCUTTING)) * FST	                                     # g stem DM m-2 d-1
        RWLVG  <- (abs(GTOTAL)+abs(RWCUTTING)) * FLV - DLV + RREDISTLVG * PUSHREDIST 	     # g leaves DM m-2 d-1 
        RWSO   <- (abs(GTOTAL)+abs(RWCUTTING)) * FSO + RWSOFASTRANSLSO - RREDISTSO	       # g storage root DM m-2 d-1	
        
        #The amount of N, P, K transfered depends on max. concentrations in LV, ST, RT and SO 
        RNCUTTING <- (RWCUTTING/WCUTTING)* NCUTTING #g N m-2 d-1, proportional to DM
        RPCUTTING <- (RWCUTTING/WCUTTING)* PCUTTING #g P m-2 d-1, proportional to DM
        RKCUTTING <- (RWCUTTING/WCUTTING)* KCUTTING #g K m-2 d-1, proportional to DM
      }
        
      # Growth of the leaf weight
      RWLV = RWLVG+RWLVD                  # g leaves DM m-2 d-1
      # Total biomass increase 
      WGTOTAL = WLV+WST+WCUTTING+WSO+WRT  # g DM m-2 d-1
      
      #-------------------------------------------NUTRIENT DYNAMICS------------------------------------------#
      # Nutrient amounts in the crop, and the nutrient amount available for crop uptake are calculated here 
      # using the nutrientdyn function. 
      NUTRIENTDYN <- nutrientdyn(Time, Pars, State,
                                 NMINLV, PMINLV, KMINLV, 
                                 NMINST, PMINST, KMINST,
                                 NMINSO, PMINSO, KMINSO, 
                                 NMINRT, PMINRT, KMINRT,
                                 NMAXLV, PMAXLV, KMAXLV, 
                                 NMAXST, PMAXST, KMAXST,
                                 NMAXSO, PMAXSO, KMAXSO, 
                                 NMAXRT, PMAXRT, KMAXRT,
                                 EMERG, TRANRF, NPKICAL$NNI, NPKICAL$PNI, NPKICAL$KNI,
                                 FLV, FST, FRT, FSO, 
                                 RWCUTTING, RNCUTTING, RPCUTTING, RKCUTTING,
                                 RREDISTLVG, PUSHREDIST, 
                                 RWLVD, RREDISTSO)

 
      # Rate of change of the actual nitrogen amounts in the different crop organs. 
      RANLVG <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RANLVG)  # g N m-2 d-1
      RANLVD <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RANLVD)  # g N m-2 d-1
      RANST <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RANST)  # g N m-2 d-1
      RANRT <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RANRT)  # g N m-2 d-1
      RANSO <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RANSO)  # g N m-2 d-1
      
      # Rate of change of the actual phosporus amounts in the different crop organs.
      RAPLVG <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAPLVG)  # g P m-2 d-1
      RAPLVD <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAPLVD)  # g P m-2 d-1
      RAPST <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAPST)  # g P m-2 d-1
      RAPRT <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAPRT)  # g P m-2 d-1
      RAPSO <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAPSO)  # g P m-2 d-1
      
      # Rate of change of the actual potassium amounts in the different crop organs.
      RAKLVG <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAKLVG)  # g K m-2 d-1
      RAKLVD <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAKLVD)  # g K m-2 d-1
      RAKST <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAKST)  # g K m-2 d-1
      RAKRT <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAKRT)  # g K m-2 d-1 
      RAKSO <- ifelse(EMERG == 1 && WST == 0, 0, NUTRIENTDYN$RAKSO)  # g K m-2 d-1
      
      # Rate of change of the total mineral N, P and K available for crop uptake. 
      RNMINT <- NUTRIENTDYN$RNMINT  # g N m-2 d-1
      RPMINT <- NUTRIENTDYN$RPMINT  # g P m-2 d-1
      RKMINT <- NUTRIENTDYN$RKMINT  # g K m-2 d-1
      
      # Rate of the nutrient amount which becomes available due to soil mineralization.  
      RNMINS <- NUTRIENTDYN$RNMINS  # g N m-2 d-1
      RPMINS <- NUTRIENTDYN$RPMINS  # g P m-2 d-1
      RKMINS <- NUTRIENTDYN$RKMINS  # g K m-2 d-1
      # Rate of the nutrient amount which becomes available due to fertilization.  
      RNMINF <- NUTRIENTDYN$RNMINF  # g N m-2 d-1
      RPMINF <- NUTRIENTDYN$RPMINF  # g P m-2 d-1
      RKMINF <- NUTRIENTDYN$RKMINF  # g K m-2 d-1
      
      
      #--------------------------------------------LEAF GROWTH---------------------------------------------------#
      
      # Green leaf weight 
      GLV <- FLV * (GTOTAL + abs(RWCUTTING)) + RREDISTLVG * PUSHREDIST  # g green leaves DM m-2 d-1
      
      # Growth of the leaf are index
      GLAI <- gla(DTEFF,TSUMCROP,LAII,RGRL,DELT,SLA,LAI,GLV,TSUMLA_MIN,TRANRF,WC,WCWP,RWCUTTING,FLV,
                  LAIEXPOEND,DORMANCY, NLAI, NPKI)     # m2 m-2 d-1
      
      # Change in LAI due to new growth of leaves
      RLAI <- GLAI - DLAI    # m2 m-2 d-1
      
      #Combined rates
      RATES <- c(RROOTD=RROOTD,RWA=RWA,
                 RTSUM=RTSUM, RTSUMCROP=RTSUMCROP, 
                 RTSUMCROPLEAFAGE=RTSUMCROPLEAFAGE,RDORMTSUM=RDORMTSUM,
                 RPUSHDORMRECTSUM=RPUSHDORMRECTSUM,RPUSHREDISTENDTSUM=RPUSHREDISTENDTSUM, 
                 RDORMTIME=RDORMTIME, RWCUTTING=RWCUTTING, 
                 RTRAIN=RTRAIN,RPAR=RPAR,RLAI=RLAI,
                 RWLVD=RWLVD, RWLV=RWLV,RWST=RWST,RWSO=RWSO,RWRT=RWRT, 
                 RWLVG=RWLVG,RTRAN=RTRAN,REVAP=REVAP,RPTRAN=RPTRAN,RPEVAP=RPEVAP, RRUNOFF=RRUNOFF, RNINTC=RNINTC, RDRAIN=RDRAIN, 
                 RREDISTLVG=RREDISTLVG,RREDISTSO=RREDISTSO,RPUSHREDISTSUM=RPUSHREDISTSUM,RWSOFASTRANSLSO=RWSOFASTRANSLSO, #30 rates
                 #27 rates for NPK
                 RNCUTTING=RNCUTTING, RPCUTTING=RPCUTTING, RKCUTTING=RKCUTTING, 
                 RANLVG=RANLVG, RANLVD=RANLVD, RANST=RANST, RANRT=RANRT, RANSO=RANSO, 
                 RAPLVG=RAPLVG, RAPLVD=RAPLVD, RAPST=RAPST, RAPRT=RAPRT, RAPSO=RAPSO, 
                 RAKLVG=RAKLVG, RAKLVD=RAKLVD, RAKST=RAKST, RAKRT=RAKRT, RAKSO=RAKSO, 
                 RNMINT=RNMINT, RPMINT=RPMINT, RKMINT=RKMINT, 
                 RNMINS=RNMINS, RPMINS=RPMINS, RKMINS=RKMINS,
                 RNMINF=RNMINF, RPMINF=RPMINF, RKMINF=RKMINF)
      #Auxiliaries
      AUX <- c(WSOTHA = WSO * 0.01, TRANRF = TRANRF, 
               HI = WSO / (WSO + WLV + WST + WRT), 
               NPKI = NPKI, 
               NNI = NPKICAL$NNI, PNI = NPKICAL$PNI, KNI = NPKICAL$KNI,
              #Add concentrations in organs in % 
              NcLV = 100 * ANLVG / WLVG, NcST = 100 * ANST / WST, NcSO = 100 * ANSO / WSO, NcRT = 100 * ANRT / WRT,
              PcLV = 100 * APLVG/WLVG,   PcST = 100 * APST / WST, PcSO = 100 * APSO / WSO, PcRT = 100 * APRT / WRT,
              KcLV = 100 * AKLVG/WLVG,   KcST = 100 * AKST / WST, KcSO = 100 * AKSO / WSO, KcRT = 100 * AKRT / WRT,
              #Total living plant concentrations in % or g/100g
              Nc= 100 * (ANLVG+ANST+ANSO+ANRT) / (WLVG + WST + WSO + WRT),
              Pc= 100 * (APLVG+APST+APSO+APRT) / (WLVG + WST + WSO + WRT),
              Kc= 100 * (AKLVG+AKST+AKSO+AKRT) / (WLVG + WST + WSO + WRT))
      
    }else{
      # If the plant is not growing anymore all plant related rates are set to 0.
      #30 rates
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
                 RWSOFASTRANSLSO=0, # g DM m-2 d-1
                 #27 rates for NPK
                 RNCUTTING=0, RPCUTTING=0, RKCUTTING=0, 
                 RANLVG=0, RANLVD=0, RANST=0, RANRT=0, RANSO=0, # g N m-2 d-1
                 RAPLVG=0, RAPLVD=0, RAPST=0, RAPRT=0, RAPSO=0, # g P m-2 d-1
                 RAKLVG=0, RAKLVD=0, RAKST=0, RAKRT=0, RAKSO=0, # g K m-2 d-1
                 RNMINT=0, RPMINT=0, RKMINT=0, # g N,P,K m-2 d-1
                 RNMINS=0, RPMINS=0, RKMINS=0, # g N,P,K m-2 d-1
                 RNMINF=0, RPMINF=0, RKMINF=0) # g N,P,K m-2 d-1

      #Auxiliaries
      AUX <- c(WSOTHA = 0, TRANRF = NA, HI  = NA, 
               NPKI = NA, NNI = NA,PNI = NA, KNI = NA,
               NcLV = NA, NcST = NA, NcSO = NA, NcRT = NA,
               PcLV = NA, PcST = NA, PcSO = NA, PcRT = NA,
               KcLV = NA, KcST = NA, KcSO = NA, KcRT = NA,
               Nc = NA, Pc = NA, Kc = NA)
    }


    return(list(RATES,c(AUX, RATES)))
           
  })
}

