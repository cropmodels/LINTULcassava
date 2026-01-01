#-------------------------------------------------------------------------------------------------#
# FUNCTION drunir
#
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         29-01-2019
#
# This file contains a component of the LINTUL-CASSAVA_NPK model. The purpose of this function is to
# compute rates of drainage, runoff and irrigation. 
#
#--------------------------------------------------------------------------------------------------#
drunir <-function(RAIN,RNINTC,EVAP,TRAN,IRRIGF,DRATE,DELT,WA,ROOTD,WCFC,WCST) {
  
  # Soil water content      
  WC   <-0.001 * WA / ROOTD   # m3 m-3
  # The amount of soil water at air dryness (AD) and field capacity (FC).
  WAFC <- 1000 * WCFC * ROOTD   # mm
  WAST <- 1000 * WCST * ROOTD # mm
  
  # Drainage below the root zone occurs when the amount of water in the soil exceeds field capacity
  # or when the amount of rainfall in excess of interception and evapotranspiration fills up soil
  # water above field capacity.
  DRAIN <-(WA-WAFC)/DELT + (RAIN - (RNINTC + EVAP + TRAN))  # mm d-1
  
  DRAIN <- ifelse(DRAIN < 0, 0, DRAIN)                      # mm d-1
  DRAIN <- ifelse(DRAIN > DRATE, DRATE, DRAIN)              # mm d-1
  
  # Surface runoff occurs when the amount of soil water exceeds total saturation or when the amount
  # of rainfall in excess of interception, evapotranspiration and drainage fills up soil water
  # above total saturation.
  RUNOFF = max(0, (WA - WAST) / DELT + (RAIN - (RNINTC + EVAP + TRAN + DRAIN))) # mm d-1
  
  
  # The irrigation rate is the extra amount of water that is needed to keep soil water at a fraction
  # of field capacity that is defined by setting the parameter IRRIGF. If IRRIGF is set to 1, the
  # soil will be irrigated every timestep to keep the amount of water in the soil at field capacity.
  # IRRIGF = 0 implies rainfed conditions.
  IRRIG  = IRRIGF * max(0, (WAFC - WA) / DELT - (RAIN - (RNINTC + EVAP + TRAN + DRAIN + RUNOFF))) # mm d-1
  
  DRUNIR <- data.frame(DRAIN = DRAIN,RUNOFF=RUNOFF, IRRIG=IRRIG)
  
  return(DRUNIR)
}