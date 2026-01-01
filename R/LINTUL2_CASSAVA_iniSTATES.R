#----------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA_iniSTATES                                  
# 
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         28-01-2019
#
# This code lists the initial conditions used for the LINTUL_Cassava model.
#
# Developer LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
#----------------------------------------------------------------------------------------------------#
LINTUL2_CASSAVA_iniSTATES <-function(pars){
    return( c(ROOTD   = pars[['ROOTDI']],                              # m     :    initial rooting depth (at crop emergence)
              WA      = 1000 * pars[['ROOTDI']] * pars[['WCFC']],  # mm    :    initial soil water amount available for crop
              TSUM    = 0,                                                  # deg. C:    temperature sum 
              TSUMCROP =0,                                       # deg. C  :    Crop temperature sum
              TSUMCROPLEAFAGE =0, # deg. C  :    Temperature sum of the leafs
              DORMTSUM = 0, # deg. C  :    Temperature sum in dormancy
              PUSHDORMRECTSUM = 0, # deg. C  :    Temperature sum when water content is higher as the critical water content in dormancy
              PUSHREDISTENDTSUM = 0, # deg. C  :    Temperature sum of the recovery of dormancy 
              DORMTIME = 0,
              WCUTTING = pars[['WCUTTINGUNIT']] * pars[['NCUTTINGS']], 
              TRAIN   = 0,                                                 # mm    :    rain sum
              PAR     = 0,                                                 # MJ m-2:    PAR sum
              LAI     = 0,                                                 # m2 m-2:    leaf area index 
              WLVD    = 0,                                                 # g m-2 :    dry weight of dead leaves
              WLV     = 0,
              WST     = 0,                                                 # g m-2 :    dry weight of stems
              WSO     = 0,                                                 # g m-2 :    dry weight of storage organs
              WRT     = 0,                                                 # g m-2 :    dry weight of roots
              WLVG    = 0,
              TRAN    = 0,                                                 # mm    :    actual transpiration
              EVAP    = 0,                                                # mm    :    actual evaporation
              PTRAN   = 0,                                                 # mm    :    potential transpiration
              PEVAP   = 0,
              RUNOFF = 0,
              RNINTC = 0, 
              RDRAIN = 0, # mm    :    potential evaporation
              REDISTLVG = 0,
              REDISTSO = 0,
              PUSHREDISTSUM = 0, # deg. C  :    Temperature sum of the redistribution after dormancy
              WSOFASTRANSLSO =0 
    ))
}

  

