#----------------------------------------------------------------------------------------------------#
# Site related settings                                  
#
# Author:       AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         18-12-2019
#
#----------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA_FIELD_MANAGEMENT                           #
# Purpose: Listing the field related input parameters for Lintul2     #
#---------------------------------------------------------------------#

LC_management <- function(SiteInfo, MODEL_PARAM){ 

  soil_param = c(
    ROOTDM = SiteInfo[1,"Maximum_rooting_depth.m"] ,                                # m            :     maximum rooting depth
    DOYPL  = SiteInfo[1,"Day_of_planting.DOY"],                                     # Day nr of planting and basal NPK application
    DOYHAR = SiteInfo[1,"Day_of_harvest.Days_since_1_january_of_planting_year"],    # Day nr of harvest, counted from 1 Jan in year of planting
    WCAD   = SiteInfo[1,"Water_content_air_dry.m3.H2O..m.3.soil."],                 # m3 m-3      :     soil water content at air dryness 
    WCWP   = SiteInfo[1,"Water_content_wilting_point.m3.H2O..m.3.soil."],           # m3 m-3      :     soil water content at wilting point
    WCFC   = SiteInfo[1,"Water_content_field_capacity.m3.H2O..m.3.soil."],          # m3 m-3      :     soil water content at field capacity 
    WCWET  = SiteInfo[1,"Water_content_wet.m3.H2O..m.3.soil."],                     # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
    WCST   = SiteInfo[1,"Water_content_saturated.m3.H2O..m.3.soil."],               # m3 m-3      :     soil water content at full saturation 
    DRATE  = SiteInfo[1,"Drainage_rate.mm.d.1"]                                     # mm d-1      :     max drainage rate
  )

  #NOTE: the order of concatenation matters!!!! SOIL PARAM needs to go first.
  return( c(soil_param, MODEL_PARAM))
}
