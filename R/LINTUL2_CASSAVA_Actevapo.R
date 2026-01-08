#-------------------------------------------------------------------------------------------------#
# FUNCTION evaptr
#
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         29-01-2019
#
# This file contains a component of the LINTUL-CASSAVA model. The purpose of this function is to
# compute the actual rates of evaporation and transpiration. 
# 
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
#--------------------------------------------------------------------------------------------------#
evaptr <-function(PEVAP,PTRAN,ROOTD,WA,WCAD,WCWP,TWCSD,WCFC,WCWET,WCST,TRANCO,DELT=1){
    
    # Soil water content      
    WC   <-0.001 * WA / ROOTD   # m3 m-3
    # The amount of soil water at air dryness (AD) and field capacity (FC).
    WAAD <-1000 * WCAD * ROOTD   # mm
    #RH not used
	#WAFC <-1000 * WCFC * ROOTD   # mm
    
    # Evaporation is decreased when water content is below field capacity, 
    # but continues until WC = WCAD. It is ensured to stay within 0-1 range
    limit.evap <-(WC-WCAD)/(WCFC-WCAD)         # (-)
    limit.evap <- min(1,pmax(0,limit.evap))   # (-)
    EVAP <-PEVAP * limit.evap                  # mm d-1
    
    # Water content at severe drought
    WCSD <- WCWP * TWCSD
    # Critical water content
    WCCR <- WCWP + max(WCSD-WCWP, PTRAN/(PTRAN+TRANCO) * (WCFC-WCWP))

    # If water content is below the critical soil water content a correction factor is calculated
    #that reduces the transpiration until it stops at WC = WCWP.
    FR <- (WC-WCWP) / (WCCR - WCWP)  # (-)
    
    # If water content is above the critical soil water content a correction factor is calculated
    # that reduces the transpiration when the crop is hampered by waterlogging (WC > WCWET).
    FRW <- (WCST-WC) / (WCST - WCWET)  # (-)
    
    #Replace values for wet days with a higher water content than the critical water content.
    #FR[WC > WCCR] <- FRW[WC > WCCR]    # (-)
    FR <- ifelse(WC > WCCR, FRW, FR)
    #Ensure to stay within the 0-1 range
    FR <- min(1, max(0,FR))   # (-)
    
    # Actual transpration
    TRAN <- PTRAN * FR # mm d-1

    # A final correction term is calculated to reduce evaporation and transpiration when evapotranspiration exceeds the amount of water in soil present in excess of air dryness.
    aux <- EVAP + TRAN    # mm d-1
    #aux[aux <= 0] <- 1  # mm d-1
    aux = ifelse(aux <= 0, 1, aux)
	AVAILF <- min(1, (WA-WAAD)/(DELT*aux)) # mm
    data.frame(EVAP = EVAP * AVAILF, TRAN = TRAN * AVAILF)
  }    