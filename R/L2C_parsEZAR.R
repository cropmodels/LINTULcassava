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
L2C_PARAMETERS_EZUI <- function() {
  #All defaults
  PARAM = c( 
    #-------------------------------LINTUL2-Cassava--------------------------------------------------#
    # Water balance
    TWCSD        = 1.05,   # (-)         : Proportion of WCWP at which water content at severe drought (WCSD) is assumed to be reached
    FRACRNINTC   = 0.25,   # (-)         : Fraction of rain intercepted
    RECOV        = 0.7,    # (-)         : Proportion of critical soil water content above which the crop recovers from drought
    TRANCO       = 8,      # mm d-1      : Transpiration constant (indicating level of drought tolerance)
       
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
    FPAR         = 0.5,    # (-)         :     Fraction PAR, MJ PAR/MJ SRAD
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
  p <- as.list(PARAM)

  #Look-up tables: 
  # Fraction of SLA_MAX at different physiological times 
  p$FRACSLATB <- matrix(c(0,     0.57, 
                        1440,  0.57, 
                        2880,  0.65, 
                        3864,  1, 
                        4320,  1,
                        8000,  1),ncol=2,byrow=TRUE)  # (-)
  
  # Relative death rate of the leaves at different temperatures 
  p$RDRT <- matrix(c(-10, 0.02 * (1 - 0.45), #NEEDS A COMMENT. WHERE DID 1- 0.45 come from??? 
                   10,  0.02 * (1 - 0.45), 
                   15,  0.03 * (1 - 0.45), 
                   30,  0.06 * (1 - 0.45), 
                   50,  0.06 * (1 - 0.45)),ncol = 2,byrow = TRUE)  # d-1
  
  # fraction of LUE_OPT at different temperatures
  p$TTB  <- matrix(c(-10, 0, 
                   15,  0, 
                   25,  1, 
                   29,  1,
                   40,  0, 
                   50,  0),ncol=2,byrow=TRUE)   # (-)
  
  #Partitioning of the adventious roots at different physiological times
  p$FRTTB <- matrix(c(  0,    0.11,   
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
  p$FLVTB <- matrix(c(   0,    0.71,   
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
  p$FSTTB <- matrix(c(  0,    0.18,   
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
  p$FSOTB <- matrix(c(  0,    0,
                      540,  0,  
                      720,  0.12,  
                      900,  0.49,  
                      1488, 0.52,
                      1980, 0.62,
                      2676, 0.57,
                      3864, 0.49, 
                      4320, 0.49,
                      8000, 0.49),ncol=2,byrow=TRUE)  # (-)  
  
	p
}



#----------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA_PARAMETERS_ADIELE                                  
#
# Author:       AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         18-12-2019
#
# This code lists the default parameters required to run the LINTUL-Cassava model
# 
# Parameters taken from: Adiele et al. (2019). EJA, under review.
#
#----------------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------#
# FUNCTION adapted parameters                                               #
# Purpose: Listing the input parameters for Lintul2                   #
#---------------------------------------------------------------------#
L2C_PARAMETERS_ADIELE <- function() {
  #get the defaults
  PARAM <- L2C_PARAMETERS_EZUI() 
  
  #Adjust with values listed in Adiele et al. (2019)
  PARAM[["LAICR"]]   <- 3.5   #  m2 m-2     :     critical LAI beyond which leaf shedding is stimulated at Edo 
  PARAM[["LUE_OPT"]] <- 2.76   # g MJ PAR-1   :     Light use effeciency at optimum growing conditions at Edo
  PARAM[["SLAII"]]   <- 0.017 # 0.0051
  PARAM[["SLA_MAX"]] <- 0.03  # 0.022
  PARAM[["K_EXT"]]   <- 0.67    # 0.67            :    Light extinction coefficient
  PARAM[["WSOREDISTFRACMAX"]] <- 0.05   # DEF=0.05
  PARAM[["RRREDISTSO"]]       <- 0.01   # DEF=0.01
  PARAM[["FASTRANSLSO"]]      <- 0.65   # DEF=0.45 (-)   :    Proportion of senesced leaf weight translocated to storage roots before shedding of the leaf 
  PARAM[["RDRB"]]             <- 0.0495   # d-1          :    basic relative death rate of the leaves
  PARAM[["RDRSHM"]]           <- 0.0495   # d-1          :    shading 
  PARAM[["LAII"]]             <-  0.1    # 0.04 m2 m-2
  PARAM[["TRANCO"]]           <-  4# mm d-1 

  # Stem cutting information
  PARAM[["WCUTTINGUNIT"]] = 18     # g                 :    Average weight per cutting
  PARAM[["NCUTTINGS"]]    = 1.5625 # m-2               :    Number of cuttings planted per m2
  

  # #Look-up tables oC, FRACSLAB
  PARAM[["FRACSLATB"]] <- matrix(c(0,  0.47,  #Estimated, from measured leaves and LI
                        1440,  0.57,
                        2880,  0.65,
                        3864,  1,
                        4320,  1,
                        8000,  1),ncol=2,byrow=TRUE) # (-)  : table of the fraction of SLA_MAX at different physiological times
  
  #Based on measured data from EDO, 2016 where water was not limiting
  #Constant ratio between stems, fibrous roots and tuber
  #Leaves were calibrated
  PARAM[["FRTTB"]]     <- matrix(c(0,    0.11,
                        175,  0.11,#Until 180dC, LAI is derived from stem assimilates
                        185,  0.11,#Until 180dC, LAI is derived from stem assimilates
                        540,  0.11,
                        750,  0.11,
                        1000, 0.11,
                        1500, 0.11,
                        3000, 0.11,
                        5522, 0.11,
                        8000, 0.11), ncol = 2, byrow=TRUE) # -  : fraction of roots
  PARAM[["FLVTB"]]    <- matrix(c(0,    0.85,
                       175,  0.75,#Until 180dC, LAI is derived from stem assimilates
                       185,  0.65,#Until 180dC, LAI is derived from stem assimilates
                       540,  0.45,
                       750,  0.35,
                       1000, 0.10,
                       1500, 0.05,
                       3000, 0.40,
                       5522, 0.40, #DEF = 0.3
                       8000, 0.40), ncol = 2, byrow=TRUE) # - : DEF=0.15 fraction of leaves
  PARAM[["FSTTB"]]    <- matrix(c(0,    0.04,
                       175,  0.14,#Until 180dC, LAI is derived from stem assimilates
                       185,  0.24,#Until 180dC, LAI is derived from stem assimilates
                       540,  0.44,
                       750,  0.30,
                       1000, 0.35,
                       1500, 0.35,
                       3000, 0.25,
                       5522, 0.20,
                       8000, 0.20), ncol = 2, byrow=TRUE) # - : fraction of stems
  PARAM[["FSOTB"]] = PARAM[["FSTTB"]]
  PARAM[["FSOTB"]][,2] = 1 - (PARAM[["FSTTB"]][,2]
                              + PARAM[["FLVTB"]][,2]
                              + PARAM[["FRTTB"]][,2]) # - : fraction of storage roots  
    
  return(PARAM)
}



L2C_NPK_PARAMETERS <- function() {
  
  #-------------------------------------LINTUL2_Cassava_NPK-------------------------------------------#
  # Nutrient (N/P/K) use (taken from Wolf, J. (2002), LINTUL5: Simple generic model for simulation of 
  # crop growth under potential, water limited and nitrogen, phosphorus and potassium limited conditions)
  # Derived from WOFOST41 data set published in Diepen, C.A. van, C. Rappoldt, J. Wolf & H. van Keulen, 1998. 
  # Crop growth simulation model WOFOST. Documentation version 4.1, Centre for world foord studies, Wageningen, 299 pp. 
  NPK_PARAM <- c(
    #Switch to turn nutrient limitaitons on or off
#    NUTRIENT_LIMITED = nutrient_limited,

    #VERY IMPOPRTANT, If NLUE=0, no effect of nutrient stress on growth rate
    NLAI   =  0.0,     # -          : Coefficient for the reduction due to nutrient stress of the LAI increase (during juvenile phase)
    RDRNS  = 0.05,     # d-1        : maximum relative death rate of the leaves due to nutrient stress
#PARAMETERS FOR CALIBRATION
    K_MAX = 4.157277,         # Maximum value of K, a value for K_NPK_NI larger than K_MAX 
                       # will mirror the Monod function. 
                       # A value larger than 2* K_MAX is the same as a K of 0.
    K_NPK_NI = 5.885810, # K value in Monod relationship to reduce influence of slightly 
                       # lower NI value. A higher values give quicker stress.
    TSUM_NPKI = 218.232616,   #  dC, minimal TSUM: before this TSUM nutrient limitations do not reduce growth rates.

    K_WATER = 0.2,     #  K value in Monod relationship with TRANSRF to reduce uptake and mineralisation rates at drought stress

    #Measured maximum uptake rates are:
    #N: 0.21 g N m-1 d-1, P: 0.025 g m-2 d-1,  K: 0.124 g m-2 d-1
    #measured in Edo (N+P) and CRS (K), see table 2 in Adiele et al.
    #This equates to max. NEQ uptake rates of 
    #NEQ= 0.21 * 0.055 + 0.025 * 0.055/0.0044 + 0.124 * 0.055/0.021 = 0.649 g NEQ m-2 d-1
    #With soil supply in the first fase of the season for the NfPfKf (with 100, 100, 100 kg NPK with recovery of 0.7, 0.3, 0.6) 
    #of about 70 kg N ha-1, 30 kg P ha-1, 60 kg K ha-1, the slope suply with uptake then becomes
    #NEQ supply: 7 * 0.055 + 3 * 0.055/0.0044 + 6 * 0.055/0.021 =  53.6 g NEQ m-1 
    #The slope of the relationship of NEQ supply vs. NEQ uptake then becomes 0.649 / 53.6 = 0.0121   

    SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE = 0.01207,  # d-1 nutrient equivalent uptake rate  as function of soil nutrient equivalents

    #Guestimated from figure in Adiele et al.
    FR_MAX   =  0.8,     # 0.8 -    : Optimal NPK concentration as fraction of maximum NPK concentration

    N_RECOV = 0.75, # measured values
    P_RECOV = 0.28, # measured values
    K_RECOV = 0.7,  # measured values

#END PARAMETERS FOR CALIBRATION #

    #NPART  =  1.0,     # -          : Coefficient for the effect of N stress on leaf allocation
    #NSLA   =  0.5,     # -          : Coefficient for the effect of nutrient stress on SLA reduction
    #Nutrient contents of fallen leaves 
    #Numbers from Howeler: http://ciat-library.ciat.cgiar.org/articulos_ciat/cabi_10ch7.pdf
    #Table 7.2 for fertilized conditions
    #Slight differences due to fertilization ar ignored here
    #SET TO ZERO as we do not have measured amounts in dead leaves.
    NFLVD  =  0, #30.5/1860,   # g N g-1 DM : N fraction in fallen leaves 
    PFLVD  =  0, # 2.0/1860,   # g P g-1 DM : P fraction in fallen leaves
    KFLVD  =  0, # 7.1/1860,   # g K g-1 DM : K fraction in fallen leaves
    
    

    #It takes on average TCNPKT days for nutrients to move from one organ to the other
    TCNPKT    =  10.0,      # d  : Time coefficient for NPK translocation. Must be >5 to prevent oscillating nutrient contents  between organs   

    #Fertilizer becomes available with decreasing rates: Fav=Frec*Fsupp*exp(-rFa*t) and dFav/dt=-rFa * Fa
    RTNMINF = 1 / 10,   # d-1  : relative rate of fertilizer N becoming available per day
    RTPMINF = 1 / 100,  # d-1  : relative rate of fertilizer P becoming available per day
    RTKMINF = 1 / 25    # d-1  : relative rate of fertilizer K becoming available per day 
  )
  
  # Maximum nutrient N/P/K concentration as function of development stage (kg N kg-1 DM). The development stage as it is used 
  # in LINTUL5 is converted to temperature sums using the results of Ezui et al. It is assumed that maturity (Development stage = 2)
  # is reached when the TSUMCROP = 4320 Deg. C. 
  #The values are taken from Adiele et al., from minimum and maximum values for H1, H2 and H3 data.
  #Leaves, min and max contents
  NMINMAXLV <- matrix(c( 0, 0.034,  0.055,
                      1500, 0.027, 0.049,
                      8000, 0.026, 0.059), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXLV <- matrix(c( 0, 0.0015, 0.0044,
                      1500, 0.0014, 0.0027, 
                      8000, 0.0018, 0.0048), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXLV <- matrix(c(  0, 0.0053, 0.0211,
                      1500, 0.0048, 0.0126,
                      8000, 0.0041, 0.0172), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM
  #Stems
  NMINMAXST <- matrix(c( 0, 0.0061, 0.0116,
                      1500, 0.0055, 0.0145,
                      8000, 0.0043, 0.0131), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXST <- matrix(c( 0, 0.00107,0.0026,
                      1500, 0.00057,0.0019, 
                      8000, 0.00038,0.0019), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXST <- matrix(c( 0, 0.0038,0.0126,
                      1500, 0.0021,0.0081,
                      8000, 0.0015,0.0095), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM

  #Storage organs
  NMINMAXSO <- matrix(c( 0, 0.0039, 0.0158,
                      1500, 0.0040, 0.0089,
                      8000, 0.0021, 0.0091), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXSO <- matrix(c( 0, 0.00057, 0.0022,
                      1500, 0.00056, 0.0013, 
                      8000, 0.00046, 0.0014), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXSO <- matrix(c( 0, 0.0057, 0.0125,
                      1500, 0.0025, 0.0092,
                      8000, 0.0024, 0.0100), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM
  #Roots: not based on data, simply assumed 0%N, 0%P, 0%K
  #NOW SET TO VERY SMALL NUMBERS AS RECOVERY ISN'T KNOWN WHEN INCLUDING ROOTS
  NMINMAXRT <- matrix(c(  0,    0.0, 0.0,
                      1500, 0.0, 0.0,
                      8000, 0.0, 0.0), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXRT <- matrix(c(  0,    0.0, 0.0,
                      1500, 0.0, 0.0, 
                      8000, 0.0, 0.0), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXRT <- matrix(c(  0,    0.0, 0.0,
                      1500, 0.0, 0.0,
                      8000, 0.0, 0.0), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM
  
  c(NPK_PARAM, list( NMINMAXLV = NMINMAXLV,
                 PMINMAXLV = PMINMAXLV,
                 KMINMAXLV = KMINMAXLV,
                 NMINMAXST = NMINMAXST,
                 PMINMAXST = PMINMAXST,
                 KMINMAXST = KMINMAXST,
                 NMINMAXSO = NMINMAXSO,
                 PMINMAXSO = PMINMAXSO,
                 KMINMAXSO = KMINMAXSO,
                 NMINMAXRT = NMINMAXRT,
                 PMINMAXRT = PMINMAXRT,
                 KMINMAXRT = KMINMAXRT))
}



