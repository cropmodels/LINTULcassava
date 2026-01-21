#-------------------------------------------------------------------------------------------------#
# FUNCTION nutrientdyn
#
# Author:       Rob van den Beuken, adapted by AGT Schut 18 DEC 2019
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         18-12-2019
#
# This file contains a component of the LINTUL-CASSAVA_NPK model. The purpose of this function is to
# compute the rates of the crop nutrient amounts and the soil nutrient amounts available for crop 
# uptake.   
#
# modified by RH, 2026-01-13
#--------------------------------------------------------------------------------------------------#

nutrientdyn2 <- function (Time, S, R, crop, soil, management, EMERG, DELT, 
			NMINLV, PMINLV, KMINLV, NMINST, PMINST, KMINST, NMINSO, PMINSO, KMINSO, 
			NMINRT, PMINRT, KMINRT, NMAXLV, PMAXLV, KMAXLV, NMAXST, PMAXST, KMAXST, 
			NMAXSO, PMAXSO, KMAXSO, NMAXRT, PMAXRT, KMAXRT, TRANRF, 
			NPKICAL, FLV, FST, FRT, FSO, PUSHREDIST) {

	NNI <- NPKICAL$NNI
	PNI <- NPKICAL$PNI
	KNI <- NPKICAL$KNI
  
    #---------------- Fertilizer application
    # Ferilizer N/P/K application (kg N/P/K ha-1 d-1)
    RFERTN <- approx(management$FERNTAB[,1], management$FERNTAB[,2], Time)$y    # kg N ha-1 d-1
    RFERTN <- (RFERTN * 1000) / 10000                     # g N m-2 d-1 
    
    RFERTP <- approx(management$FERPTAB[,1], management$FERPTAB[,2], Time)$y    # kg P ha-1 d-1
    RFERTP <- (RFERTP * 1000) / 10000                     # g P m-2 d-1
    
    RFERTK <- approx(management$FERKTAB[,1], management$FERKTAB[,2], Time)$y    # kg N ha-1 d-1
    RFERTK <- (RFERTK * 1000) / 10000                     # g N m-2 d-1
 

    #---------------- Translocatable nutrient amounts
    # The amount of translocatable nutrients to the storage organs is the actual nutrient amount minus the 
    # optimal amount in the plant leaves, stems and roots. For the roots it is the minimum between the 
    # amount of nutrients available and as a fraction of the amount of translocatable nutrients from the
    # stem and leaves. The total translocatable nutrients is the sum of this.

	ATNLV <- max(0, S$ANLVG - S$WLVG * (NMINLV + crop$FR_MAX * (NMAXLV - NMINLV)))
	ATNST <- max(0, S$ANST - S$WST * (NMINST + crop$FR_MAX * (NMAXST - NMINST)))
	ATNSO <- max(0, S$ANSO - S$WSO * (NMINSO + crop$FR_MAX * (NMAXSO - NMINSO)))
	ATNRT <- max(0, S$ANRT - S$WRT * (NMINRT + crop$FR_MAX * (NMAXRT - NMINRT)))
    ATN   <- ATNLV + ATNST + ATNSO + ATNRT # g N m-2   


	ATPLV <- max(0, S$APLVG - S$WLVG * (PMINLV + crop$FR_MAX * (PMAXLV - PMINLV)))
	ATPST <- max(0, S$APST - S$WST * (PMINST + crop$FR_MAX * (PMAXST - PMINST)))
	ATPSO <- max(0, S$APSO - S$WSO * (PMINSO + crop$FR_MAX * (PMAXSO - PMINSO)))
	ATPRT <- max(0, S$APRT - S$WRT * (PMINRT + crop$FR_MAX * (PMAXRT - PMINRT)))
    ATP   <- ATPLV + ATPST + ATPSO + ATPRT # g P m-2  
    
	ATKLV <- max(0, S$AKLVG - S$WLVG * (KMINLV + crop$FR_MAX * (KMAXLV - KMINLV)))
	ATKST <- max(0, S$AKST - S$WST * (KMINST + crop$FR_MAX * (KMAXST - KMINST)))
	ATKSO <- max(0, S$AKSO - S$WSO * (KMINSO + crop$FR_MAX * (KMAXSO - KMINSO)))
	ATKRT <- max(0, S$AKRT - S$WRT * (KMINRT + crop$FR_MAX * (KMAXRT - KMINRT)))
    ATK   <- ATKLV + ATKST + ATKSO + ATKRT # g K m-2  
  
    #---------------- Nutrient demand
    # The nutrient demand is calculated as the difference between the amount of translocatable nutrients and
    # the maximum nutrient content. The total nutrient demand is the sum of the demands of the different organs.

	NDEML <- max(NMAXLV * S$WLVG - S$ANLVG, 0) # g N m-2
	NDEMS <- max(NMAXST * S$WST - S$ANST, 0)
	NDEMR <- max(NMAXRT * S$WRT - S$ANRT, 0)
	NDEMSO <- max(NMAXSO * S$WSO - S$ANSO, 0)
	NDEMTO <- max(0, (NDEML + NDEMS + NDEMSO + NDEMR))

	PDEML <- max(PMAXLV * S$WLVG - S$APLVG, 0) # g P m-2
	PDEMS <- max(PMAXST * S$WST - S$APST, 0)
	PDEMR <- max(PMAXRT * S$WRT - S$APRT, 0)
	PDEMSO <- max(PMAXSO * S$WSO - S$APSO, 0)
	PDEMTO <- max(0, (PDEML + PDEMS + PDEMSO + PDEMR))

	KDEML <- max(KMAXLV * S$WLVG - S$AKLVG, 0) # g K m-2
	KDEMS <- max(KMAXST * S$WST - S$AKST, 0)
	KDEMR <- max(KMAXRT * S$WRT - S$AKRT, 0)
	KDEMSO <- max(KMAXSO * S$WSO - S$AKSO, 0)
	KDEMTO <- max(0, (KDEML + KDEMS + KDEMSO + KDEMR))

 
    #--------------- Net nutrient translocation in the crop
    #Internal relocation of nutrients depends on relative content
    #When there is no demand, there should also be no redistribution
    
    # Computation of the translocation of nutrients from the different organs
    # Daily redistribution to balance nutrient contents between all organs
    # Based on relative demand of organs

	RNTLV <- ifelse(crop$TCNPKT * NDEMTO == 0, 0, ((NDEML/NDEMTO) * ATN - ATNLV)/crop$TCNPKT) # g N m-2 d-1
	RNTST <- ifelse(crop$TCNPKT * NDEMTO == 0, 0, ((NDEMS/NDEMTO) * ATN - ATNST)/crop$TCNPKT)
	RNTSO <- ifelse(crop$TCNPKT * NDEMTO == 0, 0, ((NDEMSO/NDEMTO) * ATN - ATNSO)/crop$TCNPKT)
	RNTRT <- ifelse(crop$TCNPKT * NDEMTO == 0, 0, ((NDEMR/NDEMTO) * ATN - ATNRT)/crop$TCNPKT)
	
	RPTLV <- ifelse(crop$TCNPKT * PDEMTO == 0, 0, ((PDEML/PDEMTO) * ATP - ATPLV)/crop$TCNPKT) # g P m-2 d-1
	RPTST <- ifelse(crop$TCNPKT * PDEMTO == 0, 0, ((PDEMS/PDEMTO) * ATP - ATPST)/crop$TCNPKT)
	RPTSO <- ifelse(crop$TCNPKT * PDEMTO == 0, 0, ((PDEMSO/PDEMTO) * ATP - ATPSO)/crop$TCNPKT)
	RPTRT <- ifelse(crop$TCNPKT * PDEMTO == 0, 0, ((PDEMR/PDEMTO) * ATP - ATPRT)/crop$TCNPKT)
	
	RKTLV <- ifelse(crop$TCNPKT * KDEMTO == 0, 0, ((KDEML/KDEMTO) * ATK - ATKLV)/crop$TCNPKT) # g K m-2 d-1
	RKTST <- ifelse(crop$TCNPKT * KDEMTO == 0, 0, ((KDEMS/KDEMTO) * ATK - ATKST)/crop$TCNPKT)
	RKTSO <- ifelse(crop$TCNPKT * KDEMTO == 0, 0, ((KDEMSO/KDEMTO) * ATK - ATKSO)/crop$TCNPKT)
	RKTRT <- ifelse(crop$TCNPKT * KDEMTO == 0, 0, ((KDEMR/KDEMTO) * ATK - ATKRT)/crop$TCNPKT)
   
    #---------------
	TINY = 1e-08
	if (abs(RNTLV + RNTST + RNTSO + RNTRT) > TINY) {
		print("UNRELIABLE RESULTS!! Internal N reallocation must be net 0")
		print(c(RNTLV, RNTST, RNTSO, RNTRT))
	}
	if (abs(RPTLV + RPTST + RPTSO + RPTRT) > TINY) {
		print("UNRELIABLE RESULTS!! Internal P reallocation must be net 0")
		print(c(RPTLV, RPTST, RPTSO, RPTRT))
	}
	if (abs(RKTLV + RKTST + RKTSO + RKTRT) > TINY) {
		print("UNRELIABLE RESULTS!! Internal K reallocation must be net 0")
		print(c(RKTLV, RKTST, RKTSO, RKTRT))
	}
    
    #--------------- Nutrient uptake
    # Nutrient uptake from the soil depends on the soil moisture. It is assumed 
    # that nutrient uptake reduces monod-like with growth rate reduction due to:
    # 1) Low soil water supply: uptake rates are 50% when soil supply rates equals max-uptake rates
    # 2) Low soil nutrient supply: uptake rates are 50% when max soil supply rates equals max-uptake rates
 	WLIMIT <- TRANRF/(crop$K_WATER + TRANRF)
   
    #Maximum amounts of nutrients for the given amount of biomass
	NMAX <- NMAXLV * S$WLVG + NMAXST * S$WST + NMAXRT * S$WRT + NMAXSO * S$WSO
	PMAX <- PMAXLV * S$WLVG + PMAXST * S$WST + PMAXRT * S$WRT + PMAXSO * S$WSO
	KMAX <- KMAXLV * S$WLVG + KMAXST * S$WST + KMAXRT * S$WRT + KMAXSO * S$WSO
    
    #Nutrient equivalents in the soil and maximum uptake of equivalents based on optimum ratios
    #g m-2
	NUTEQ_SOIL <- S$NMINT + S$PMINT * ifelse(PMAX == 0, 1, NMAX/PMAX) + S$KMINT * ifelse(KMAX == 0, 1, NMAX/KMAX)
    
    #g m-2
	NUTEQ_DEMAND <- NDEMTO + PDEMTO * ifelse(PMAX == 0, 1, NMAX/PMAX) + KDEMTO * ifelse(KMAX == 0, 1, NMAX/KMAX)
    
    #The parameter SLOPE_NEQ_SOIL_PEQUPTAKE determines the ratio between biomass and nutrients
    #Plant uptake is proportional to nutrient supply under optimum ratios.
    #uptake rates increase when soil supply is larger.
    #Interaction effects are not accounted for here. 
    
    #If uptake isn't adequate, concentrations will decrease first, later growth rates decrease.
    #gNEQ m-2 d-1     g m-2           d-1,       d-1                              g m-2
	RMAX_UPRATE = min(NUTEQ_DEMAND/DELT, crop$SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE * NUTEQ_SOIL)
    
    #If actual ratios are suboptimal, uptake of excess nutrients is relatively increased.
    #A suboptimal ratio in the soil results in a suboptimal ratio in the plant when supply is smaller than demand.
    #For example, a soil full of N gives large N uptake, but relatively small P and K uptake. 
    #Actual uptake of N is limited by maximum N contensts, reducing uptake of P and K contents in the plant 
    #to suboptimal levels. In extremis, oversupply of one nutrient can reduce uptake of another.
	RNUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * S$NMINT/NUTEQ_SOIL) * WLIMIT


#### changed per advise from Tom Schut 2026-01-13
	##RPUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * (ifelse(PMAX == 0,1,NMAX/PMAX) * PMINT) / NUTEQ_SOIL) * WLIMIT 
    ##RKUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * (ifelse(KMAX == 0,1,NMAX/KMAX) * KMINT) / NUTEQ_SOIL) * WLIMIT 
	RPUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * S$PMINT/NUTEQ_SOIL) * WLIMIT
	RKUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * S$KMINT/NUTEQ_SOIL) * WLIMIT
####

    #Actual uptake is limited by demand based on maximum concentrations for standing biomass
    #Uptake rate should not exceed maximum soil supply.
	RNUPTR = min(S$NMINT/DELT, RNUPTR, NDEMTO/DELT)
	RPUPTR = min(S$PMINT/DELT, RPUPTR, PDEMTO/DELT)
	RKUPTR = min(S$KMINT/DELT, RKUPTR, KDEMTO/DELT)
  
    #------------- Partitioning
    # to compute the partitioning of the total N/P/K uptake rates (NUPTR, PUPTR, KUPTR)
    # over the leaves, stem, and roots (kg N/P/K ha-1 d-1)
    # concentrations are balanced, so distribution based on weight proportions
	WTOT <- S$WLVG + S$WST + S$WSO + S$WRT

	RNULV <- ifelse(WTOT == 0, 0, (S$WLVG/WTOT) * RNUPTR) # g N m-2 d-1
	RNUST <- ifelse(WTOT == 0, 0, (S$WST/WTOT) * RNUPTR)
	RNUSO <- ifelse(WTOT == 0, 0, (S$WSO/WTOT) * RNUPTR)
	RNURT <- ifelse(WTOT == 0, 0, (S$WRT/WTOT) * RNUPTR)

	RPULV <- ifelse(WTOT == 0, 0, (S$WLVG/WTOT) * RPUPTR) # g P m-2 d-1
	RPUST <- ifelse(WTOT == 0, 0, (S$WST/WTOT) * RPUPTR)
	RPUSO <- ifelse(WTOT == 0, 0, (S$WSO/WTOT) * RPUPTR)
	RPURT <- ifelse(WTOT == 0, 0, (S$WRT/WTOT) * RPUPTR)

	RKULV <- ifelse(WTOT == 0, 0, (S$WLVG/WTOT) * RKUPTR) # g K m-2 d-1
	RKUST <- ifelse(WTOT == 0, 0, (S$WST/WTOT) * RKUPTR)
	RKUSO <- ifelse(WTOT == 0, 0, (S$WSO/WTOT) * RKUPTR)
	RKURT <- ifelse(WTOT == 0, 0, (S$WRT/WTOT) * RKUPTR)

    
    #------------ Nutrient redistribution because of cutting. 
    #A negative rate in e.g. RNCUTTING adds to other components.
    RANCUTLV <- -R$NCUTTING * FLV  # g N m-2 d-1
    RANCUTST <- -R$NCUTTING * FST  # g N m-2 d-1
    RANCUTRT <- -R$NCUTTING * FRT  # g N m-2 d-1
    RANCUTSO <- -R$NCUTTING * FSO  # g N m-2 d-1
  
    RAPCUTLV <- -R$PCUTTING * FLV  # g P m-2 d-1
    RAPCUTST <- -R$PCUTTING * FST  # g P m-2 d-1
    RAPCUTRT <- -R$PCUTTING * FRT  # g P m-2 d-1
    RAPCUTSO <- -R$PCUTTING * FSO  # g P m-2 d-1
  
    RAKCUTLV <- -R$KCUTTING * FLV  # g K m-2 d-1
    RAKCUTST <- -R$KCUTTING * FST  # g K m-2 d-1
    RAKCUTRT <- -R$KCUTTING * FRT  # g K m-2 d-1
    RAKCUTSO <- -R$KCUTTING * FSO  # g K m-2 d-1
    
	
    #------------ Nutrient redistribution because of leaf death
    if (S$WLVG > 0) {
      #Nutrients lost due to dying leaves
      R$ANLVD <- R$WLVD * crop$NFLVD # g N m-2 d-1
      R$APLVD <- R$WLVD * crop$PFLVD # g P m-2 d-1
      R$AKLVD <- R$WLVD * crop$KFLVD # g K m-2 d-1
      #Total nutrients in dying leaves
      RNDLVG <- R$WLVD * (S$ANLVG / S$WLVG) # g N m-2 d-1
      RPDLVG <- R$WLVD * (S$APLVG / S$WLVG) # g P m-2 d-1
      RKDLVG <- R$WLVD * (S$AKLVG / S$WLVG) # g K m-2 d-1
    } else {
      RNDLVG <- 0
      RPDLVG <- 0
      RKDLVG <- 0
      R$ANLVD <- 0
      R$APLVD <- 0
      R$AKLVD <- 0
    }
    #What is not lost to dead leaves most be redistributed to other organs
    RNDLV_REDIST <- max(0, RNDLVG - R$ANLVD) # g N m-2 d-1
    RPDLV_REDIST <- max(0, RPDLVG - R$APLVD)
    RKDLV_REDIST <- max(0, RKDLVG - R$AKLVD)

    
    #------------ Nutrient redistribution because of storage root DM redistribution after dormancy
    # DM to the leaves, with new at maximum NPK concentrations
    #             g DM m-2 d-1 * (gN m-2 d-1 * gDM-1 m2 d)
    RANSO2LVLV <- R$REDISTLVG * NMAXLV * PUSHREDIST  # g N m-2 d-1
    RAPSO2LVLV <- R$REDISTLVG * PMAXLV * PUSHREDIST  # g P m-2 d-1
    RAKSO2LVLV <- R$REDISTLVG * KMAXLV * PUSHREDIST  # g K m-2 d-1
    
    # DM loss of the storage roots
    RANSO2LVSO <- ifelse(S$WSO == 0, 0, R$REDISTSO * (S$ANSO / S$WSO))  # g N m-2 d-1 
    RAPSO2LVSO <- ifelse(S$WSO == 0, 0, R$REDISTSO * (S$APSO / S$WSO))  # g P m-2 d-1
    RAKSO2LVSO <- ifelse(S$WSO == 0, 0, R$REDISTSO * (S$AKSO / S$WSO))  # g K m-2 d-1
   
    #------------- Rate of change of N/P/K in crop organs
    # uptake + net translocation + cutting
    # N relocated to stem, P+K to storate roots
	
    R$ANLVG <- RNULV + RNTLV + RANCUTLV + RANSO2LVLV  - RNDLVG # g N m-2 d-1
    R$ANST <- RNUST + RNTST + RANCUTST + RNDLV_REDIST          # g N m-2 d-1
    R$ANRT <- RNURT + RNTRT + RANCUTRT                         # g N m-2 d-1
    R$ANSO <- RNUSO + RNTSO + RANCUTSO - RANSO2LVSO            # g N m-2 d-1
    
    R$APLVG <- RPULV + RPTLV + RAPCUTLV + RAPSO2LVLV  - RPDLVG # g P m-2 d-1
    R$APST <- RPUST + RPTST + RAPCUTST                         # g P m-2 d-1
    R$APRT <- RPURT + RPTRT + RAPCUTRT                         # g P m-2 d-1
    R$APSO <- RPUSO + RPTSO + RAPCUTSO - RAPSO2LVSO + RPDLV_REDIST   # g P m-2 d-1
    
    R$AKLVG <- RKULV + RKTLV + RAKCUTLV + RAKSO2LVLV  - RKDLVG # g K m-2 d-1
    R$AKST <- RKUST + RKTST + RAKCUTST                         # g K m-2 d-1
    R$AKRT <- RKURT + RKTRT + RAKCUTRT                         # g K m-2 d-1
    R$AKSO <- RKUSO + RKTSO + RAKCUTSO - RAKSO2LVSO + RKDLV_REDIST   # g K m-2 d-1
    
	
    #------------ Soil supply
    # Soil nutrient supply through mineralization during crop growth(not affected by water supply)
    #The reason for this is that soil supply isn't modelled but a given from control plots. 
    #With unknown number of days with water limitations it is impossible to know the potential uptake rate from this pool.
	# Rate of the nutrient amount which becomes available due to soil mineralization.  
	R$NMINS <- ifelse(S$NMINS < soil$RTNMINS, -S$NMINS/DELT, -soil$RTNMINS) # g N m-2 d-1
	R$PMINS <- ifelse(S$NMINS < soil$RTPMINS, -S$PMINS/DELT, -soil$RTPMINS) # g P m-2 d-1
	R$KMINS <- ifelse(S$NMINS < soil$RTKMINS, -S$KMINS/DELT, -soil$RTKMINS) # g K m-2 d-1
  
    #------------ Fertilizer supply
    #Fertilizer nutrient supply 
    #Pool in the soil which is not yet avalable for plant uptake
    #        supply rate rate that becomes available for uptake
	# Rate of the nutrient amount which becomes available due to fertilization.  
    R$NMINF <- RFERTN - crop$RTNMINF * S$NMINF * WLIMIT   # g N m-2 d-1
    R$PMINF <- RFERTP - crop$RTPMINF * S$PMINF * WLIMIT   # g P m-2 d-1 
    R$KMINF <- RFERTK - crop$RTKMINF * S$KMINF * WLIMIT   # g K m-2 d-1

    # Change in total inorganic N/P/K in soil as function of fertilizer input, 
    # soil N/P/K mineralization and crop uptake.
	# Rate of change of the total mineral N, P and K available for crop uptake. 
    R$NMINT <- crop$RTNMINF * S$NMINF * WLIMIT + (-R$NMINS) - RNUPTR   # g N m-2 d-1
    R$PMINT <- crop$RTPMINF * S$PMINF * WLIMIT + (-R$PMINS) - RPUPTR   # g P m-2 d-1
    R$KMINT <- crop$RTKMINF * S$KMINF * WLIMIT + (-R$KMINS) - RKUPTR   # g K m-2 d-1
   
	if ((EMERG == 1) && (S$WST == 0)) {
		R$ANLVG <- R$ANLVD <- R$ANST <- R$ANRT <- R$ANSO <- 0
		R$APLVG <- R$APLVD <- R$APST <- R$APRT <- R$APSO <- 0
		R$AKLVG <- R$AKLVD <- R$AKST <- R$AKRT <- R$AKSO <- 0
	}	
	R
}
