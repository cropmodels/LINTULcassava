#----------------------------------------------------------------------------------------------------#
# LINTUL2_CASSAVA_PARAMETERS_EZUI                                  
#
# Author:       Rob van den Beuken / A.G.T. Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         19-12-2019
#
# This code lists the default parameters required to run the LINTUL-Cassava model
# 
# Parameters taken from: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
#
#----------------------------------------------------------------------------------------------------#
LINTUL2_CASSAVA_PARAMETERS_EZUI <- function() {
  #All defaults
  PARAM = c( 
    #-------------------------------LINTUL2-Cassava--------------------------------------------------#
    # Day of planting
    DELT         = 1,      # d           : Model time step

    # Water balance
    TWCSD        = 1.05,   # (-)         : Proportion of WCWP at which water content at severe drought (WCSD) is assumed to be reached
    FRACRNINTC   = 0.25,   # (-)         : Fraction of rain intercepted
    RECOV        = 0.7,    # (-)         : Proportion of critical soil water content above which the crop recovers from drought
    TRANCO       = 8,      # mm d-1      : Transpiration constant (indicating level of drought tolerance)
   
    IRRIGF       = 0,      # (-)         : Irrigation present (1 = yes, 0 = no)
    
    # Stem cutting information
    WCUTTINGUNIT = 18,     # g                 :    Average weight per cutting
    NCUTTINGS    = 1.5625, # m-2               :    Number of cuttings planted per m2
    WCUTTINGIP   = 21.875, # g :    Stem cutting weight at planting (WCUTTINGUNIT*NCUTTINGS)
    
    # Conditions at emergence
    ROOTDI       = 0.1,         # m           :     Rooting depth at crop emergence
    SLAI         = 0.017,       # m2 g-1      :     Specific leaf area at crop emergence
    WLVI         = 1.531,       # g           :     Leaf weight at crop emergence (WCUTTINGIP*FLV_CUTT)
    LAII         = 0.026,       # m2 m-2      :     Inital leaf area index at crop emergence (WLVI*SLAI)
    
    # Cutting weight decrease
    WCUTTINGMINPRO= 0.15,  # g m-2                  :     Minimum proportion of the stem cutting weight that remains unchanged
    FST_CUTT     = 0.02,   # g stem g cuttings-1    :    Fraction of initial cutting allocated to stem
    FRT_CUTT     = 0.03,   # g roots g cuttings-1   :    Fraction of initial cutting allocated to adventitious roots
    FLV_CUTT     = 0.07,   # g leaves g cuttings-1  :    Fraction of initial cutting allocated to leaves
    FSO_CUTT     = 0,      # g storage g cuttings-1 :    Fraction of initial cutting allocated to storage roots
    RDRWCUTTING  = 0.017,  # g d-1                  :    Relative decrease rate of cutting weight
    
    # Light interception and conversion to assimilates
    FPAR         = 0.5,    # (-)         :     Fraction PAR, MJ PAR/MJ DTR
    K_EXT        = 0.67,   # (-)         :     extinction coefficient for PAR
    LUE_OPT      = 1.5,    # g MJ PAR-1  :    Light use effeciency at optimum growing conditions
    
    # Root growth
    ROOTDM       = 0.35,   # m           :     maximum rooting depth
    RRDMAX       = 0.012,  # m  d-1      :     max rate increase of rooting depth
    
    # Leaf death
    RDRB          = 0.09,   # d-1         :    Basic relative death rate of the leaves
    LAICR         = 3.5,    # m2 m-2      :    Critical LAI beyond which leaf shedding is stimulated
    RDRSHM        = 0.09,   # d-1         :    Max relative death rate of leaves due to shading
    FRACTLLFENHSH = 0.85,   # (-)         :    Fraction of leaf life at which enhanced shedding can be induced
    FASTRANSLSO   = 0.45,   # (-)         :    Proportion of senesced leaf weight translocated to storage roots before shedding of the leaf
    
    # Leaf growth
    SLA_MAX      = 0.03,   # m2 g-1       :    maximum specific leaf area
    RGRL         = 0.004,  # 1/(deg. C d) :     relative growth rate of LAI during exponential growth
    LAIEXPOEND   = 0.75,   # m2 m-2       :    maximum leaf area index for exponential growth phase
    
    # Temperature sums for initiation
    TBASE          = 15,     # deg. C      :    Base temperature 
    OPTEMERGTSUM   = 170,    # deg. c      :    Optimal amount of Tsum accumulated from planting to emergence
    TSUMLA_MIN     = 168,    # deg. C d    :    Crop temperature sum accumulation for minimum leaf area
    TSUMSBR        = 776,    # deg. C      :    Crop temperature sum accumulation to first branching 
    TSUMLLIFE      = 1200,   # deg. C d    :    Temperature sum accumulation to leaf life
    TSUMREDISTMAX  = 144,    # deg. C      :    Maximum temperature sum accumulation to indicate the duration of dry matter redistribution
    FINTSUM        = 8000,   # deg. C      :    Temperature sum at which simulation stops
    
    # Dormancy effect
    LAI_MIN          = 0.09,     # m2 m-2                   :    Minimum leaf area index
    WSOREDISTFRACMAX = 0.05,     # (-)                      :    Maximum proportion of dry matter redistribution from storage roots for the formation of new leaves
    WLVGNEWN         = 10,       # g DM m-2                 :    Minimum amount of new leaves weight produced in the redistribution phase
    SO2LV            = 0.8,      # g leaves DM g-1 storage  :    Converstion rate of storage organs dry matter to leaf dry matter
    RRREDISTSO       = 0.01,     # d-1                      :    Relative rate of redistribution of dry matter from storage roots to leaves
    DELREDIST        = 12       # deg. C d                 :    Delay for redistribution of dry matter
  )
  #Look-up tables:
  
  # Fraction of SLA_MAX at different physiological times 
  FRACSLATB <- matrix(c(0,     0.57, 
                        1440,  0.57, 
                        2880,  0.65, 
                        3864,  1, 
                        4320,  1,
                        8000,  1),ncol=2,byrow=TRUE)  # (-)
  
  # Relative death rate of the leaves at different temperatures 
  RDRT <- matrix(c(-10, 0.02 * (1 - 0.45), #NEEDS A COMMENT. WHERE DID 1- 0.45 come from??? 
                   10,  0.02 * (1 - 0.45), 
                   15,  0.03 * (1 - 0.45), 
                   30,  0.06 * (1 - 0.45), 
                   50,  0.06 * (1 - 0.45)),ncol = 2,byrow = TRUE)  # d-1
  
  # fraction of LUE_OPT at different temperatures
  TTB  <- matrix(c(-10, 0, 
                   15,  0, 
                   25,  1, 
                   29,  1,
                   40,  0, 
                   50,  0),ncol=2,byrow=TRUE)   # (-)
  
  #Partitioning of the adventious roots at different physiological times
  FRTTB <- matrix(c(   0,    0.11,   
                      540,  0.10,   
                      720,  0.094,   
                      900,  0.01,
                      1488, 0.01,  
                      1980, 0.01,  
                      2676, 0.01,  
                      3864, 0.01,  
                      4320, 0.01,
                      8000, 0.01),ncol=2,byrow=TRUE)  # (-)
  
  #Partitioning of the leaves at different physiological times 
  FLVTB <- matrix(c(   0,    0.71,   
                      540,  0.515,   
                      720,  0.393,   
                      900,  0.24,
                      1488, 0.21,  
                      1980, 0.18,
                      2676, 0.13,
                      3864, 0.21,
                      4320, 0.21,
                      8000, 0.21),ncol=2,byrow=TRUE)  # (-)
  
  #Partitioning of the stems at different physiological times 
  FSTTB <- matrix(c(   0,    0.18,   
                      540,  0.385,   
                      720,  0.393,   
                      900,  0.26, 
                      1488, 0.26,  
                      1980, 0.19,  
                      2676, 0.29,
                      3864, 0.29,
                      4320, 0.29,
                      8000, 0.29),ncol=2,byrow=TRUE)   # (-)
  
  #Partitioning of the storage organs at different physiological times 
  FSOTB <- matrix(c(   0,    0,
                      540,  0,  
                      720,  0.12,  
                      900,  0.49,  
                      1488, 0.52,
                      1980, 0.62,
                      2676, 0.57,
                      3864, 0.49, 
                      4320, 0.49,
                      8000, 0.49),ncol=2,byrow=TRUE)  # (-)  
  
  return( c(PARAM,
            list(FRACSLATB = FRACSLATB, 
                 RDRT = RDRT,
                 TTB = TTB),
            list(FLVTB = FLVTB,
                FSTTB = FSTTB,
                FSOTB = FSOTB,
                FRTTB = FRTTB)))
}



