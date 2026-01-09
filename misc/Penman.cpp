/*
License: EUPL

Author:       Rob van den Beuken
Copyright:    Copyright 2019, PPS
Email:        rob.vandenbeuken@wur.nl
Date:         29-01-2019

This file contains a component of the LINTUL-CASSAVA model. 
The purpose of this function is to compute the evaporation and transpiration with the Penman equation. 
 
*/

#include "LINTcas.h"

void LINcasModel::Penman() {
	//double TAVG, double VAPR, double SRAD, double LAI, double WIND, double RNINTC) {
  
  double DTRJM2 = A.SRAD * 1E6;        // J m-2 d-1     :    Daily radiation in Joules 
  double BOLTZM = 5.668E-8; 	      // J m-1 s-1 K-4 :    Stefan-Boltzmann constant 
  double LHVAP  = 2.4E6;            // J kg-1        :    Latent heat of vaporization 
  double PSYCH  = 0.067;            // kPa deg. C-1  :    Psychrometric constant
  
  double BBRAD  = BOLTZM * pow((A.TAVG+273), 4) * 86400;           // J m-2 d-1 : Black body radiation 
  double SVP    = 0.611 * std::exp(17.4 * A.TAVG / (A.TAVG + 239)); // kPa : Saturation vapour pressure
  double SLOPE  = 4158.6 * SVP / pow((A.TAVG + 239), 2);        // kPa dec. C-1:  Change of SVP per degree C
  double RLWN   = BBRAD * std::max(0., 0.55 * (1 - A.VAPR / SVP)); // J m-2 d-1 : Net outgoing long-wave radiation
  double WDF    = 2.63 * (1.0 + 0.54 * A.WIND);      // kg m-2 d-1 : Wind function in the Penman equation
  
  // Net radiation (J m-2 d-1) for soil (1) and crop (2)
  double NRADS  = DTRJM2 * (1 - 0.15) - RLWN;     // (1)
  double NRADC  = DTRJM2 * (1 - 0.25) - RLWN;     // (2)
  
  // Radiation terms (J m-2 d-1) of the Penman equation for soil (1) and crop (2)
  double PENMRS = NRADS * SLOPE / (SLOPE + PSYCH);    // (1)
  double PENMRC = NRADC * SLOPE / (SLOPE + PSYCH);    // (2)
  
  // Drying power term (J m-2 d-1) of the Penman equation
  double PENMD  = LHVAP * WDF * (SVP - A.VAPR) * PSYCH / (SLOPE + PSYCH);
  
  // Potential evaporation and transpiration are weighed by a factor representing the plant canopy (exp(-0.5 * LAI)).
  R.PEVAP  = std::exp(-0.5 * S.LAI)  * (PENMRS + PENMD) / LHVAP;       // mm d-1
  double PTRAN  = (1 - std::exp(-0.5 * S.LAI)) * (PENMRC + PENMD) / LHVAP;  // mm d-1
  R.PTRAN  = std::max(0., PTRAN - 0.5 * R.NINTC);                     // mm d-1

}

