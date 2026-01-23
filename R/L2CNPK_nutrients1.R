#-------------------------------------------------------------------------------------------------#
# FUNCTION npkical
#
# Author:       Rob van den Beuken, adapted by AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         06-02-2020
#
# compute the nutrient limitations due to nitrogen, phosphorus and potassium.  
#
#--------------------------------------------------------------------------------------------------#

#source('./Components/Mirrored_Monod_function.r')


Mirrored_Monod <- function(x, K, Kmax = 4){
	if (K <= Kmax){
		ifelse(x == 0, 0, (K+1) * x/(x+K))
	} else {
		K = max(0, 2 * Kmax - K)
		x = 1-x
		1 - ifelse(x == 0, 0, (K+1) * x/(x+K))
	}
}

npkical <- function(WLVG, WST, WSO, NMINLV, PMINLV, KMINLV,
                    NMINST, PMINST, KMINST, NMINSO, PMINSO, KMINSO, 
                 NMAXLV, PMAXLV, KMAXLV, NMAXST, PMAXST, KMAXST,
                 NMAXSO, PMAXSO, KMAXSO, FR_MAX, K_NPK_NI, K_MAX,
                 ANLVG, ANST, ANSO, APLVG, APST, APSO, AKLVG, AKST, AKSO){
  
  #---------------- Nutrient concentrations
  # Minimum nutrient content in the living biomass
  NMIN <- WLVG * NMINLV + WST * NMINST + WSO * NMINSO    # g N m-2
  PMIN <- WLVG * PMINLV + WST * PMINST + WSO * PMINSO    # g P m-2
  KMIN <- WLVG * KMINLV + WST * KMINST + WSO * KMINSO    # g K m-2
  
  # Maximum nutrient content in the living biomass
  NMAX <- NMAXLV * WLVG + NMAXST * WST + NMAXSO * WSO   # g N m-2 
  PMAX <- PMAXLV * WLVG + PMAXST * WST + PMAXSO * WSO   # g P m-2 
  KMAX <- KMAXLV * WLVG + KMAXST * WST + KMAXSO * WSO   # g K m-2 
  
  
  # Optimal nutrient content in the living biomass
  NOPT <- NMIN + FR_MAX * (NMAX - NMIN)   # g N m-2 
  POPT <- PMIN + FR_MAX * (PMAX - PMIN)   # g P m-2 
  KOPT <- KMIN + FR_MAX * (KMAX - KMIN)   # g K m-2 
  #----------------
  
  #---------------- Actual nutrient concentrations

  # Actual nutrient amounts in the living biomass 
  NACT <- ANLVG + ANST + ANSO  # g N m-2
  PACT <- APLVG + APST + APSO   # g P m-2
  KACT <- AKLVG + AKST + AKSO   # g K m-2 
  
  #-------------- Nutrition Indices
  NNI <- ifelse( (NOPT - NMIN) == 0, 0, (NACT - NMIN) / (NOPT - NMIN) )  # (-)
  PNI <- ifelse( (POPT - PMIN) == 0, 0, (PACT - PMIN) / (POPT - PMIN) )  # (-)
  KNI <- ifelse( (KOPT - KMIN) == 0, 0, (KACT - KMIN) / (KOPT - KMIN) )  # (-)
  
  NNI <- min(1, max(0,NNI))
  PNI <- min(1, max(0,PNI))
  KNI <- min(1, max(0,KNI))
  

  #Combined effect. 
  #Multiplication allows to have extra growth reduction if multiple nutrients are deficient
  #The "Monod" acts as scalar to reduce effect of minor deficiencies that do not affect growth rates but are compensated by dilution. A mirrored Monod function to determine effect of N, P and K stress on NPKI 
  NPKI = Mirrored_Monod(x = NNI*PNI*KNI, K = K_NPK_NI, Kmax = K_MAX)
  
  return(data.frame(NNI = NNI, PNI = PNI, KNI = KNI, NPKI = NPKI))
}


#-------------------------------------------------------------------------------------------------#
# FUNCTION nutrientdyn
#
# Author:       Rob van den Beuken, adapted by AGT Schut 18 DEC 2019
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         18-12-2019
#
# compute the rates of the crop nutrient amounts and the soil nutrient amounts available for crop 
# uptake.   
#
#--------------------------------------------------------------------------------------------------#
nutrientdyn <- function(Time, Pars, States,
                        NMINLV, PMINLV, KMINLV, 
                        NMINST, PMINST, KMINST,
                        NMINSO, PMINSO, KMINSO, 
                        NMINRT, PMINRT, KMINRT,
                        NMAXLV, PMAXLV, KMAXLV, 
                        NMAXST, PMAXST, KMAXST,
                        NMAXSO, PMAXSO, KMAXSO,
                        NMAXRT, PMAXRT, KMAXRT,
                        EMERG, TRANRF, NNI, PNI, KNI,
                        FLV, FST, FRT, FSO, 
                        RWCUTTING,RNCUTTING,RPCUTTING,RKCUTTING,
                        RREDISTLVG, PUSHREDIST, 
                        RWLVD, RREDISTSO){
  
  with(as.list(c(States, Pars)),{
    #---------------- Fertilizer application
    # Ferilizer N/P/K application (kg N/P/K ha-1 d-1)
    RFERTN <- stats::approx(FERNTAB[,1], FERNTAB[,2], Time)$y    # kg N ha-1 d-1
    RFERTN <- (RFERTN * 1000) / 10000                     # g N m-2 d-1 
    
    RFERTP <- stats::approx(FERPTAB[,1], FERPTAB[,2], Time)$y    # kg P ha-1 d-1
    RFERTP <- (RFERTP * 1000) / 10000                     # g P m-2 d-1
    
    RFERTK <- stats::approx(FERKTAB[,1], FERKTAB[,2], Time)$y    # kg N ha-1 d-1
    RFERTK <- (RFERTK * 1000) / 10000                     # g N m-2 d-1
    #---------------- 
 

    #---------------- Translocatable nutrient amounts
    # The amount of translocatable nutrients to the storage organs is the actual nutrient amount minus the 
    # optimal amount in the plant leaves, stems and roots. For the roots it is the minimum between the 
    # amount of nutrients available and as a fraction of the amount of translocatable nutrients from the
    # stem and leaves. The total translocatable nutrients is the sum of this.
    ATNLV <- pmax(0, ANLVG - WLVG * (NMINLV + FR_MAX * (NMAXLV - NMINLV))) # g N m-2
    ATNST <- pmax(0,  ANST - WST  * (NMINST + FR_MAX * (NMAXST - NMINST))) # g N m-2
    ATNSO <- pmax(0,  ANSO - WSO  * (NMINSO + FR_MAX * (NMAXSO - NMINSO))) # g N m-2
    ATNRT <- pmax(0,  ANRT - WRT  * (NMINRT + FR_MAX * (NMAXRT - NMINRT))) # g N m-2
    
    ATN   <- ATNLV + ATNST + ATNSO + ATNRT                 # g N m-2   

    
    ATPLV <- pmax(0, APLVG - WLVG * (PMINLV + FR_MAX * (PMAXLV - PMINLV))) # g P m-2
    ATPST <- pmax(0,  APST - WST  * (PMINST + FR_MAX * (PMAXST - PMINST))) # g P m-2
    ATPSO <- pmax(0,  APSO - WSO  * (PMINSO + FR_MAX * (PMAXSO - PMINSO))) # g P m-2
    ATPRT <- pmax(0,  APRT - WRT  * (PMINRT + FR_MAX * (PMAXRT - PMINRT))) # g P m-2
    ATP   <- ATPLV + ATPST + ATPSO + ATPRT                 # g P m-2  
    
    ATKLV <- pmax(0, AKLVG - WLVG * (KMINLV + FR_MAX * (KMAXLV - KMINLV))) # g K m-2
    ATKST <- pmax(0,  AKST - WST  * (KMINST + FR_MAX * (KMAXST - KMINST))) # g K m-2
    ATKSO <- pmax(0,  AKSO - WSO  * (KMINSO + FR_MAX * (KMAXSO - KMINSO))) # g K m-2
    ATKRT <- pmax(0,  AKRT - WRT  * (KMINRT + FR_MAX * (KMAXRT - KMINRT))) # g K m-2  
    ATK   <- ATKLV + ATKST + ATKSO + ATKRT                 # g K m-2  
    #--------------

    
  
    #---------------- Nutrient demand
    # The nutrient demand is calculated as the difference between the amount of translocatable nutrients and
    # the maximum nutrient content. The total nutrient demand is the sum of the demands of the different organs.
  
    NDEML  <- pmax(NMAXLV * WLVG - ANLVG, 0)        # g N m-2
    NDEMS  <- pmax(NMAXST *  WST - ANST, 0)          # g N m-2
    NDEMR  <- pmax(NMAXRT *  WRT - ANRT, 0)          # g N m-2
    NDEMSO <- pmax(NMAXSO *  WSO - ANSO, 0)          # g N m-2
    NDEMTO <- pmax(0, (NDEML + NDEMS + NDEMSO + NDEMR))      # g N m-2
    
    PDEML  <- pmax(PMAXLV * WLVG - APLVG, 0)        # g P m-2
    PDEMS  <- pmax(PMAXST *  WST - APST, 0)          # g P m-2
    PDEMR  <- pmax(PMAXRT *  WRT - APRT, 0)          # g P m-2
    PDEMSO <- pmax(PMAXSO *  WSO - APSO, 0)          # g P m-2
    PDEMTO <- pmax(0, (PDEML + PDEMS + PDEMSO + PDEMR))      # g P m-2
    
    KDEML  <- pmax(KMAXLV * WLVG - AKLVG, 0)        # g K m-2
    KDEMS  <- pmax(KMAXST *  WST - AKST, 0)          # g K m-2
    KDEMR  <- pmax(KMAXRT *  WRT - AKRT, 0)          # g K m-2
    KDEMSO <- pmax(KMAXSO *  WSO - AKSO, 0)          # g K m-2
    KDEMTO <- pmax(0, (KDEML + KDEMS + KDEMSO + KDEMR))      # g K m-2
    #---------------
    
    #--------------- Net nutrient translocation in the crop
    #Internal relocation of nutrients depends on relative content
    #When there is no demand, there should also be no redistribution
    
    # Computation of the translocation of nutrients from the different organs
    # Daily redistribution to balance nutrient contents between all organs
    # Based on relative demand of organs
    RNTLV <- ifelse(TCNPKT * NDEMTO == 0, 0, ((NDEML / NDEMTO)  *  ATN - ATNLV)/TCNPKT)    # g N m-2 d-1
    RNTST <- ifelse(TCNPKT * NDEMTO == 0, 0, ((NDEMS / NDEMTO)  *  ATN - ATNST)/TCNPKT)    # g N m-2 d-1
    RNTSO <- ifelse(TCNPKT * NDEMTO == 0, 0, ((NDEMSO/ NDEMTO)  *  ATN - ATNSO)/TCNPKT)    # g N m-2 d-1
    RNTRT <- ifelse(TCNPKT * NDEMTO == 0, 0, ((NDEMR / NDEMTO)  *  ATN - ATNRT)/TCNPKT)    # g N m-2 d-1
    
    RPTLV <- ifelse(TCNPKT * PDEMTO == 0, 0, ((PDEML / PDEMTO)  *  ATP - ATPLV)/TCNPKT)    # g P m-2 d-1
    RPTST <- ifelse(TCNPKT * PDEMTO == 0, 0, ((PDEMS / PDEMTO)  *  ATP - ATPST)/TCNPKT)    # g P m-2 d-1
    RPTSO <- ifelse(TCNPKT * PDEMTO == 0, 0, ((PDEMSO/ PDEMTO)  *  ATP - ATPSO)/TCNPKT)    # g P m-2 d-1
    RPTRT <- ifelse(TCNPKT * PDEMTO == 0, 0, ((PDEMR / PDEMTO)  *  ATP - ATPRT)/TCNPKT)    # g P m-2 d-1
    
    RKTLV <- ifelse(TCNPKT * KDEMTO == 0, 0, ((KDEML / KDEMTO)  *  ATK - ATKLV)/TCNPKT)    # g K m-2 d-1
    RKTST <- ifelse(TCNPKT * KDEMTO == 0, 0, ((KDEMS / KDEMTO)  *  ATK - ATKST)/TCNPKT)    # g K m-2 d-1
    RKTSO <- ifelse(TCNPKT * KDEMTO == 0, 0, ((KDEMSO/ KDEMTO)  *  ATK - ATKSO)/TCNPKT)    # g K m-2 d-1
    RKTRT <- ifelse(TCNPKT * KDEMTO == 0, 0, ((KDEMR / KDEMTO)  *  ATK - ATKRT)/TCNPKT)    # g K m-2 d-1

    
    #---------------
    TINY=10E-9
    if(abs(RNTLV+RNTST+RNTSO+RNTRT)> TINY){
      print("UNRELIABLE RESULTS!! Internal N reallocation must be net 0")
      print(c(RNTLV,RNTST,RNTSO,RNTRT))
    }
    if(abs(RPTLV+RPTST+RPTSO+RPTRT)> TINY){
      print("UNRELIABLE RESULTS!! Internal P reallocation must be net 0")
      print(c(RPTLV,RPTST,RPTSO,RPTRT))
    }
    if(abs(RKTLV+RKTST+RKTSO+RKTRT)> TINY){
      print("UNRELIABLE RESULTS!! Internal K reallocation must be net 0")
      print(c(RKTLV,RKTST,RKTSO,RKTRT))
    }
    
    #--------------- Nutrient uptake
    # Nutrient uptake from the soil depends on the soil moisture. It is assumed 
    # that nutrient uptake reduces monod-like with growth rate reduction due to:
    # 1) Low soil water supply: uptake rates are 50% when soil supply rates equals max-uptake rates
    # 2) Low soil nutrient supply: uptake rates are 50% when max soil supply rates equals max-uptake rates
    WLIMIT <- TRANRF / (K_WATER + TRANRF)
    
    #Maximum amounts of nutrients for the given amount of biomass
    NMAX <- NMAXLV * WLVG + NMAXST * WST + NMAXRT * WRT + NMAXSO * WSO
    PMAX <- PMAXLV * WLVG + PMAXST * WST + PMAXRT * WRT + PMAXSO * WSO
    KMAX <- KMAXLV * WLVG + KMAXST * WST + KMAXRT * WRT + KMAXSO * WSO
    
    #Nutrient equivalents in the soil and maximum uptake of equivalents based on optimum ratios
    #g m-2
    NUTEQ_SOIL <- NMINT + PMINT * ifelse(PMAX == 0,1, NMAX/PMAX) + KMINT * ifelse(KMAX == 0,1,NMAX/KMAX)
    
    #g m-2
    NUTEQ_DEMAND <- NDEMTO + PDEMTO * ifelse(PMAX==0,1,NMAX/PMAX) + KDEMTO * ifelse(KMAX == 0,1,NMAX/KMAX)
    
    #The parameter SLOPE_NEQ_SOIL_PEQUPTAKE determines the ratio between biomass and nutrients
    #Plant uptake is proportional to nutrient supply under optimum ratios.
    #uptake rates increase when soil supply is larger.
    #Interaction effects are not accounted for here. 
    
    #If uptake isn't adequate, concentrations will decrease first, later growth rates decrease.
    #gNEQ m-2 d-1     g m-2           d-1,       d-1                              g m-2
    RMAX_UPRATE = min(NUTEQ_DEMAND / DELT, SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE * NUTEQ_SOIL)
    
    #If actual ratios are suboptimal, uptake of excess nutrients is relatively increased.
    #A suboptimal ratio in the soil results in a suboptimal ratio in the plant when supply is smaller than demand.
    #For example, a soil full of N gives large N uptake, but relatively small P and K uptake. 
    #Actual uptake of N is limited by maximum N contensts, reducing uptake of P and K contents in the plant 
    #to suboptimal levels. In extremis, oversupply of one nutrient can reduce uptake of another.
    RNUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * NMINT  / NUTEQ_SOIL) * WLIMIT 

#### changed per advise from Tom Schut 2026-01-13
	##RPUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * (ifelse(PMAX == 0,1,NMAX/PMAX) * PMINT) / NUTEQ_SOIL) * WLIMIT 
    ##RKUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * (ifelse(KMAX == 0,1,NMAX/KMAX) * KMINT) / NUTEQ_SOIL) * WLIMIT 
    RPUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * PMINT / NUTEQ_SOIL) * WLIMIT 
    RKUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * KMINT / NUTEQ_SOIL) * WLIMIT 	
####

    #Actual uptake is limited by demand based on maximum concentrations for standing biomass
    #Uptake rate should not exceed maximum soil supply.
    RNUPTR = pmin(NMINT / DELT, RNUPTR, NDEMTO / DELT)
    RPUPTR = pmin(PMINT / DELT, RPUPTR, PDEMTO / DELT)
    RKUPTR = pmin(KMINT / DELT, RKUPTR, KDEMTO / DELT)
    
    
    #-------------
  
    #------------- Partitioning
    # to compute the partitioning of the total N/P/K uptake rates (NUPTR, PUPTR, KUPTR)
    # over the leaves, stem, and roots (kg N/P/K ha-1 d-1)
    # concentrations are balanced, so distribution based on weight proportions
      WTOT <- WLVG + WST + WSO + WRT
    
      RNULV <- ifelse(WTOT == 0, 0, (WLVG / WTOT) * RNUPTR)  # g N m-2 d-1
      RNUST <- ifelse(WTOT == 0, 0,  (WST / WTOT) * RNUPTR)  # g N m-2 d-1
      RNUSO <- ifelse(WTOT == 0, 0,  (WSO / WTOT) * RNUPTR)   # g N m-2 d-1
      RNURT <- ifelse(WTOT == 0, 0,  (WRT / WTOT) * RNUPTR)  # g N m-2 d-1
   
      RPULV <- ifelse(WTOT == 0, 0, (WLVG / WTOT) * RPUPTR)  # g P m-2 d-1
      RPUST <- ifelse(WTOT == 0, 0,  (WST / WTOT) * RPUPTR)  # g P m-2 d-1
      RPUSO <- ifelse(WTOT == 0, 0,  (WSO / WTOT) * RPUPTR)   # g P m-2 d-1
      RPURT <- ifelse(WTOT == 0, 0,  (WRT / WTOT) * RPUPTR)  # g P m-2 d-1
  
      RKULV <- ifelse(WTOT == 0, 0, (WLVG / WTOT) * RKUPTR)  # g K m-2 d-1
      RKUST <- ifelse(WTOT == 0, 0,  (WST / WTOT) * RKUPTR)  # g K m-2 d-1
      RKUSO <- ifelse(WTOT == 0, 0,  (WSO / WTOT) * RKUPTR)   # g K m-2 d-1
      RKURT <- ifelse(WTOT == 0, 0,  (WRT / WTOT) * RKUPTR)  # g K m-2 d-1
    #------------
    
    
    #------------ Nutrient redistribution because of cutting. 
    #A negative rate in e.g. RNCUTTING adds to other components.
    RANCUTLV <- -RNCUTTING * FLV  # g N m-2 d-1
    RANCUTST <- -RNCUTTING * FST  # g N m-2 d-1
    RANCUTRT <- -RNCUTTING * FRT  # g N m-2 d-1
    RANCUTSO <- -RNCUTTING * FSO  # g N m-2 d-1
  
    RAPCUTLV <- -RPCUTTING * FLV  # g P m-2 d-1
    RAPCUTST <- -RPCUTTING * FST  # g P m-2 d-1
    RAPCUTRT <- -RPCUTTING * FRT  # g P m-2 d-1
    RAPCUTSO <- -RPCUTTING * FSO  # g P m-2 d-1
  
    RAKCUTLV <- -RKCUTTING * FLV  # g K m-2 d-1
    RAKCUTST <- -RKCUTTING * FST  # g K m-2 d-1
    RAKCUTRT <- -RKCUTTING * FRT  # g K m-2 d-1
    RAKCUTSO <- -RKCUTTING * FSO  # g K m-2 d-1
	    
    #------------ Nutrient redistribution because of leaf death
    if( WLVG > 0){
      #Nutrients lost due to dying leaves
      RANLVD <- RWLVD * NFLVD # g N m-2 d-1
      RAPLVD <- RWLVD * PFLVD # g P m-2 d-1
      RAKLVD <- RWLVD * KFLVD # g K m-2 d-1
      #Total nutrients in dying leaves
      RNDLVG <- RWLVD * (ANLVG / WLVG) # g N m-2 d-1
      RPDLVG <- RWLVD * (APLVG / WLVG) # g P m-2 d-1
      RKDLVG <- RWLVD * (AKLVG / WLVG) # g K m-2 d-1
    }else{
      RNDLVG <- 0
      RPDLVG <- 0
      RKDLVG <- 0
      RANLVD <- 0
      RAPLVD <- 0
      RAKLVD <- 0
    }
    #What is not lost to dead leaves most be redistributed to other organs
    RNDLV_REDIST <- pmax(0, RNDLVG - RANLVD) # g N m-2 d-1
    RPDLV_REDIST <- pmax(0, RPDLVG - RAPLVD)
    RKDLV_REDIST <- pmax(0, RKDLVG - RAKLVD)
    
    #------------
    
    #------------ Nutrient redistribution because of storage root DM redistribution after dormancy
    # DM to the leaves, with new at maximum NPK concentrations
    #             g DM m-2 d-1 * (gN m-2 d-1 * gDM-1 m2 d)
    RANSO2LVLV <- RREDISTLVG * NMAXLV * PUSHREDIST  # g N m-2 d-1
    RAPSO2LVLV <- RREDISTLVG * PMAXLV * PUSHREDIST  # g P m-2 d-1
    RAKSO2LVLV <- RREDISTLVG * KMAXLV * PUSHREDIST  # g K m-2 d-1
    
    # DM loss of the storage roots
      RANSO2LVSO <- ifelse(WSO == 0, 0, RREDISTSO * (ANSO / WSO))  # g N m-2 d-1 
      RAPSO2LVSO <- ifelse(WSO == 0, 0, RREDISTSO * (APSO / WSO))  # g P m-2 d-1
      RAKSO2LVSO <- ifelse(WSO == 0, 0, RREDISTSO * (AKSO / WSO))  # g K m-2 d-1
    #-----------
    
    #------------- Rate of change of N/P/K in crop organs
    #        uptake + net translocation + cutting
    # N relocated to stem, P+K to storate roots
	
    RANLVG <- RNULV + RNTLV + RANCUTLV + RANSO2LVLV  - RNDLVG # g N m-2 d-1
    RANST <- RNUST + RNTST + RANCUTST + RNDLV_REDIST          # g N m-2 d-1
    RANRT <- RNURT + RNTRT + RANCUTRT                         # g N m-2 d-1
    RANSO <- RNUSO + RNTSO + RANCUTSO - RANSO2LVSO            # g N m-2 d-1
    
    RAPLVG <- RPULV + RPTLV + RAPCUTLV + RAPSO2LVLV  - RPDLVG # g P m-2 d-1
    RAPST <- RPUST + RPTST + RAPCUTST                         # g P m-2 d-1
    RAPRT <- RPURT + RPTRT + RAPCUTRT                         # g P m-2 d-1
    RAPSO <- RPUSO + RPTSO + RAPCUTSO - RAPSO2LVSO + RPDLV_REDIST   # g P m-2 d-1
    
    RAKLVG <- RKULV + RKTLV + RAKCUTLV + RAKSO2LVLV  - RKDLVG # g K m-2 d-1
    RAKST <- RKUST + RKTST + RAKCUTST                         # g K m-2 d-1
    RAKRT <- RKURT + RKTRT + RAKCUTRT                         # g K m-2 d-1
    RAKSO <- RKUSO + RKTSO + RAKCUTSO - RAKSO2LVSO + RKDLV_REDIST   # g K m-2 d-1
    #----------
    
	
    #------------ Soil supply
    # Soil nutrient supply through mineralization during crop growth(not affected by water supply)
    #The reason for this is that soil supply isn't modelled but a given from control plots. 
    #With unknown number of days with water limitations it is impossible to know the potential uptake rate from this pool.
    RNMINS <- ifelse(NMINS < RTNMINS, -NMINS/DELT, -RTNMINS)   # g N m-2 d-1
    RPMINS <- ifelse(NMINS < RTPMINS, -PMINS/DELT, -RTPMINS)   # g P m-2 d-1
    RKMINS <- ifelse(NMINS < RTKMINS, -KMINS/DELT, -RTKMINS)   # g K m-2 d-1
    
    #------------ Fertilizer supply
    #Fertilizer nutrient supply 
    #Pool in the soil which is not yet avalable for plant uptake
    #        supply rate      rate that becomes available for uptake
    RNMINF <- RFERTN - RTNMINF * NMINF * WLIMIT   # g N m-2 d-1
    RPMINF <- RFERTP - RTPMINF * PMINF * WLIMIT   # g P m-2 d-1 
    RKMINF <- RFERTK - RTKMINF * KMINF * WLIMIT   # g K m-2 d-1
    
    # Change in total inorganic N/P/K in soil as function of fertilizer input, 
    # soil N/P/K mineralization and crop uptake.
    RNMINT <- RTNMINF * NMINF * WLIMIT + (-RNMINS)  - RNUPTR   # g N m-2 d-1
    RPMINT <- RTPMINF * PMINF * WLIMIT + (-RPMINS)  - RPUPTR   # g P m-2 d-1
    RKMINT <- RTKMINF * KMINF * WLIMIT + (-RKMINS)  - RKUPTR   # g K m-2 d-1
  
    return(data.frame(RANLVG = RANLVG, RANLVD = RANLVD, RANST = RANST, RANRT = RANRT, RANSO = RANSO, 
                      RAPLVG = RAPLVG, RAPLVD = RAPLVD, RAPST = RAPST, RAPRT = RAPRT, RAPSO = RAPSO, 
                      RAKLVG = RAKLVG, RAKLVD = RAKLVD, RAKST = RAKST, RAKRT = RAKRT, RAKSO = RAKSO, 
                      RNMINT = RNMINT, RPMINT = RPMINT, RKMINT = RKMINT, 
                      RNMINS = RNMINS, RPMINS = RPMINS, RKMINS = RKMINS,
                      RNMINF = RNMINF, RPMINF = RPMINF, RKMINF = RKMINF))
  })
}
