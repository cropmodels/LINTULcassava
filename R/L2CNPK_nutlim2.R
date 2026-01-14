#-------------------------------------------------------------------------------------------------#
# FUNCTION npkical
#
# Author:       Rob van den Beuken, adapted by AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         06-02-2020
#
# This file contains a component of the LINTUL-CASSAVA_NPK model. The purpose of this function is to
# compute the nutrient limitations due to nitrogen, phosphorus and potassium.  
#
#--------------------------------------------------------------------------------------------------#

#source('./Components/Mirrored_Monod_function.r')

npkical2 <- function(S, crop, NMINLV, PMINLV, KMINLV, NMINST, PMINST, KMINST, NMINSO, PMINSO, KMINSO, 
                 NMAXLV, PMAXLV, KMAXLV, NMAXST, PMAXST, KMAXST, NMAXSO, PMAXSO, KMAXSO) {
                 #ANLVG, ANST, ANSO, APLVG, APST, APSO, AKLVG, AKST, AKSO) {
    
  #---------------- Nutrient concentrations
  # Minimum nutrient content in the living biomass
  NMIN <- S$WLVG * NMINLV + S$WST * NMINST + S$WSO * NMINSO    # g N m-2
  PMIN <- S$WLVG * PMINLV + S$WST * PMINST + S$WSO * PMINSO    # g P m-2
  KMIN <- S$WLVG * KMINLV + S$WST * KMINST + S$WSO * KMINSO    # g K m-2
  
  # Maximum nutrient content in the living biomass
  NMAX <- NMAXLV * S$WLVG + NMAXST * S$WST + NMAXSO * S$WSO   # g N m-2 
  PMAX <- PMAXLV * S$WLVG + PMAXST * S$WST + PMAXSO * S$WSO   # g P m-2 
  KMAX <- KMAXLV * S$WLVG + KMAXST * S$WST + KMAXSO * S$WSO   # g K m-2 
  
  
  # Optimal nutrient content in the living biomass
  NOPT <- NMIN + crop$FR_MAX * (NMAX - NMIN)   # g N m-2 
  POPT <- PMIN + crop$FR_MAX * (PMAX - PMIN)   # g P m-2 
  KOPT <- KMIN + crop$FR_MAX * (KMAX - KMIN)   # g K m-2 
  #----------------
  
  #---------------- Actual nutrient concentrations

  # Actual nutrient amounts in the living biomass 
  NACT <- S$ANLVG + S$ANST + S$ANSO  # g N m-2
  PACT <- S$APLVG + S$APST + S$APSO   # g P m-2
  KACT <- S$AKLVG + S$AKST + S$AKSO   # g K m-2 
  
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
  NPKI = Mirrored_Monod(x = NNI*PNI*KNI, K = crop$K_NPK_NI, Kmax = crop$K_MAX)
  
  data.frame(NNI = NNI, PNI = PNI, KNI = KNI, NPKI = NPKI)
}
