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
LINTUL2_CASSAVA_PARAMETERS_ADIELE <- function() {
  #get the defaults
  PARAM <- LINTUL2_CASSAVA_PARAMETERS_EZUI() 
  
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


