#----------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA_NPK_iniSTATES                                  
# 
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         28-01-2019
#
# This code lists the initial conditions used for the LINTUL_Cassava_NPK model.
#
#----------------------------------------------------------------------------------------------------#
LINTUL2_CASSAVA_NPK_iniSTATES <- function(parvalues){
  
    return( c(
              # LINTUL2-CASSAVA: 30 states
              ROOTD   = parvalues[['ROOTDI']],                                   # m      : Initial rooting depth (at crop emergence)
              WA      = 1000 * parvalues[['ROOTDI']] * parvalues[['WCFC']],      # mm     : Initial soil water amount available for crop
              TSUM    = 0,                                                       # deg. C : Temperature sum 
              TSUMCROP =0,                                                       # deg. C : Crop temperature sum
              TSUMCROPLEAFAGE =0,                                                # deg. C : Temperature sum of the leafs
              DORMTSUM = 0,                                                      # deg. C : Temperature sum in dormancy
              PUSHDORMRECTSUM = 0,                                               # deg. C : Temperature sum when water content is higher as the critical water content in dormancy
              PUSHREDISTENDTSUM = 0,                                             # deg. C : Temperature sum of the recovery of dormancy 
              DORMTIME = 0,                                                      # d      : Time in dormancy
              WCUTTING = parvalues[['WCUTTINGUNIT']] * parvalues[['NCUTTINGS']], # g      : Cutting weight
              TRAIN   = 0,                                                       # mm     : Rain sum
              PAR     = 0,                                                       # MJ m-2 : PAR sum
              LAI     = 0,                                                       # m2 m-2 : Leaf area index 
              WLVD    = 0,                                                       # g m-2  : Dry weight of dead leaves
              WLV     = 0,                                                       # g m-2  : Dry weight of the total leaves
              WST     = 0,                                                       # g m-2  : Dry weight of stems
              WSO     = 0,                                                       # g m-2  : Dry weight of storage organs
              WRT     = 0,                                                       # g m-2  : Dry weight of the fibrousroots
              WLVG    = 0,                                                       # g m-2  : Dry weight of the living leaves
              TRAN    = 0,                                                       # mm     :    actual transpiration
              EVAP    = 0,                                                       # mm     :    actual evaporation
              PTRAN   = 0,                                                       # mm     :    potential transpiration
              PEVAP   = 0,                                                       # mm     : Potential evaporation
              RUNOFF = 0,                                                        # mm     : Runoff       
              RNINTC = 0,                                                        # mm     : Intercepted rainfall
              RDRAIN = 0,                                                        # mm     : Drainage
              REDISTLVG = 0,                                                     # g m-2  : Leaf DM redistributed to storage roots
              REDISTSO = 0,                                                      # g m-2  : Storage root DM redistributed to leaves
              PUSHREDISTSUM = 0,                                                 # deg. C : Temperature sum of the redistribution after dormancy
              WSOFASTRANSLSO = 0,                                                 # g m-2  : Storage root growth due leaf redistribution
              
              # LINTUL2-CASSAVA_NPK: 27 states
              NCUTTING = 0.015 * parvalues[['WCUTTINGUNIT']] * parvalues[['NCUTTINGS']], #g N m-2
              PCUTTING = 0.0015 * parvalues[['WCUTTINGUNIT']] * parvalues[['NCUTTINGS']], #g P m-2
              KCUTTING = 0.010 * parvalues[['WCUTTINGUNIT']] * parvalues[['NCUTTINGS']], #g K m-2            
              ANLVG = 0.0,                      # g N m-2: Actual nitrogen amount in the green leaves                                                     
              ANLVD = 0.0,                      # g N m-2: Actual nitrogen amount in the dead leaves                                                     
              ANST = 0.0,                      # g N m-2: Actual nitrogen amount in the stems
              ANRT = 0.0,                      # g N m-2: Actual nitrogen amount in the fibrous roots
              ANSO = 0.0,                      # g N m-2: Actual nitrogen amount in the storage roots
              APLVG = 0.0,                      # g P m-2; Actual phosphorus amount in the leaves
              APLVD = 0.0,                      # g P m-2; Actual phosphorus amount in the leaves
              APST = 0.0,                      # g P m-2: Actual phosphorus amount in the stems
              APRT = 0.0,                      # g P m-2: Actual phosphorus amount in the fibrous roots
              APSO = 0.0,                      # g P m-2; Actual phosphorus amount in the storage roots
              AKLVG = 0.0,                      # g K m-2: Actual potassium amount in the leaves 
              AKLVD = 0.0,                      # g K m-2: Actual potassium amount in the leaves 
              AKST = 0.0,                      # g K m-2: Actual potassium amount in the stems
              AKRT = 0.0,                      # g K m-2: Actual potassium amount in the fibrous roots
              AKSO = 0.0,                      # g K m-2: Actual potassium amount in the storage roots
              NMINT = 0.25*parvalues[['NMINI']],  # g N m-2: Available nitrogen amount in the soil for crop uptake
              PMINT = 0.25*parvalues[['PMINI']],  # g P m-2: Available phosphorus amount in the soil for crop uptake
              KMINT = 0.25*parvalues[['KMINI']],  # g K m-2: Available potassium amount in the soil for crop uptake
              NMINS = 0.75*parvalues[['NMINI']],  # g N m-2: Available organic nitrogen in the soil
              PMINS = 0.75*parvalues[['PMINI']],  # g P m-2: Available organic phosphorus in the soil
              KMINS = 0.75*parvalues[['KMINI']],  # g K m-2: Available organic potassium in the soil
              NMINF = 0,                     # g N m-2: Available N from fertilizer 
              PMINF = 0,                     # g P m-2: Available P from fertilizer 
              KMINF = 0                      # g K m-2: Available K from fertilizer 
    ))
}

  

