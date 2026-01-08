#-------------------------------------------------------------------------------------------------#
# FUNCTION penman
#
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         29-01-2019
#
# This file contains a component of the LINTUL-CASSAVA model. The purpose of this function is to
# compute the penman equation. 
# 
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
#--------------------------------------------------------------------------------------------------#
penman <-function(TAVG,VAPR,SRAD,LAI,WIND,RNINTC) {
  
  DTRJM2 <-SRAD * 1E6        # J m-2 d-1     :    Daily radiation in Joules 
  BOLTZM <-5.668E-8 	      # J m-1 s-1 K-4 :    Stefan-Boltzmann constant 
  LHVAP  <-2.4E6            # J kg-1        :    Latent heat of vaporization 
  PSYCH  <-0.067            # kPa deg. C-1  :    Psychrometric constant
  
  BBRAD  <-BOLTZM * (TAVG+273)^4 * 86400                 # J m-2 d-1   :     Black body radiation 
  SVP    <-0.611 * exp(17.4 * TAVG / (TAVG + 239))     # kPa         :     Saturation vapour pressure
  SLOPE  <-4158.6 * SVP / (TAVG + 239)^2                 # kPa dec. C-1:     Change of SVP per degree C
  RLWN   <-BBRAD * max(0, 0.55 * (1 - VAPR / SVP))     # J m-2 d-1   :     Net outgoing long-wave radiation
  WDF    <-2.63 * (1.0 + 0.54 * WIND)                  # kg m-2 d-1  :     Wind function in the Penman equation
  
  # Net radiation (J m-2 d-1) for soil (1) and crop (2)
  NRADS  <-DTRJM2 * (1 - 0.15) - RLWN     # (1)
  NRADC  <-DTRJM2 * (1 - 0.25) - RLWN     # (2)
  
  # Radiation terms (J m-2 d-1) of the Penman equation for soil (1) and crop (2)
  PENMRS <-NRADS * SLOPE / (SLOPE + PSYCH)    # (1)
  PENMRC <-NRADC * SLOPE / (SLOPE + PSYCH)    # (2)
  
  # Drying power term (J m-2 d-1) of the Penman equation
  PENMD  <-LHVAP * WDF * (SVP - VAPR) * PSYCH / (SLOPE + PSYCH)
  
  # Potential evaporation and transpiration are weighed by a factor representing the plant canopy (exp(-0.5 * LAI)).
  PEVAP  <-exp(-0.5 * LAI)  * (PENMRS + PENMD) / LHVAP       # mm d-1
  PTRAN  <-(1 - exp(-0.5 * LAI)) * (PENMRC + PENMD) / LHVAP  # mm d-1
  PTRAN  <-max(0, PTRAN - 0.5 * RNINTC)                     # mm d-1

  data.frame(cbind(PEVAP,PTRAN))
}