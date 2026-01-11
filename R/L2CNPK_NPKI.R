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


Mirrored_Monod <- function(x, K, Kmax = 4){
  if(K <= Kmax){
    C=K+1
    y=ifelse(x == 0, 0, C * x/(x+K))
  }else{
    K=max(0, 2 * Kmax - K)
    C=K+1
    x=1-x
    y=ifelse(x == 0, 0, C * x/(x+K))
    y=1-y
  }
}


npkical <- function(WLVG, WST, WSO,
                    NMINLV, PMINLV, KMINLV,
                    NMINST, PMINST, KMINST,
                    NMINSO, PMINSO, KMINSO, 
                 NMAXLV, PMAXLV, KMAXLV,
                 NMAXST, PMAXST, KMAXST,
                 NMAXSO, PMAXSO, KMAXSO,
                 FR_MAX, K_NPK_NI, K_MAX,
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
  
  NNI <- pmin(1, pmax(0,NNI))
  PNI <- pmin(1, pmax(0,PNI))
  KNI <- pmin(1, pmax(0,KNI))
  

  #Combined effect. 
  #Multiplication allows to have extra growth reduction if multiple nutrients are deficient
  #The "Monod" acts as scalar to reduce effect of minor deficiencies that do not affect growth rates 
  #but are compensated by dilution. 
  #A mirrored Monod function to determine effect of N, P and K stress on NPKI 
  NPKI = Mirrored_Monod(x = NNI*PNI*KNI, K = K_NPK_NI, Kmax = K_MAX)
  
  return(data.frame(NNI = NNI, PNI = PNI, KNI = KNI, NPKI = NPKI))
  #-----------------
}
