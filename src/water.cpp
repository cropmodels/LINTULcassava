/*
License: EUPL

Author:       Rob van den Beuken
Copyright:    Copyright 2019, PPS
Email:        rob.vandenbeuken@wur.nl
Date:         29-01-2019

Thee functions are part of the LINTUL-CASSAVA_NPK model. 

The Penman function computes the evaporation and transpiration with the Penman equation. 
The evaptr function computes the actual rates of evaporation and transpiration. 
The drunir function computes rates of drainage, runoff and irrigation. 

*/

#include "LINTcas.h"


void LINcasModel::Penman() {
  
	double DTRJM2 = A.SRAD * 1E6;   // J m-2 d-1     :    Daily radiation in Joules 
	double BOLTZM = 5.668E-8; 	    // J m-1 s-1 K-4 :    Stefan-Boltzmann constant 
	double LHVAP  = 2.4E6;          // J kg-1        :    Latent heat of vaporization 
	double PSYCH  = 0.067;          // kPa deg. C-1  :    Psychrometric constant
	
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


void LINcasModel::evaptr() {
//      double EVA = evaptr(R.PEVAP, R.PTRAN, S.ROOTD, S.WA, soil.WCAD,
//		soil.WCWP,crop.TWCSD,soil.WCFC, soil.WCWET, soil.WCST, crop.TRANCO, DELT);
	  
	// Soil water content      
	double WC   = 0.001 * S.WA / S.ROOTD;   // m3 m-3
	// The amount of soil water at air dryness (AD) and field capacity (FC).
	double WAAD = 1000 * soil.WCAD * S.ROOTD;   // mm
	//double WAFC = 1000 * soil.WCFC * S.ROOTD;   // mm
	
	// Evaporation is decreased when water content is below field capacity, 
	// but continues until WC = WCAD. It is ensured to stay within 0-1 range
	double limitevap = (WC-soil.WCAD)/(soil.WCFC - soil.WCAD);         // (-)
	limitevap = std::min(1., std::max(0., limitevap));   // (-)
	double EVAP = R.PEVAP * limitevap;                  // mm d-1
	
	// Water content at severe drought
	double WCSD = soil.WCWP * crop.TWCSD;
	// Critical water content
	double WCCR = soil.WCWP + std::max(WCSD - soil.WCWP, R.PTRAN/(R.PTRAN+crop.TRANCO) * (soil.WCFC-soil.WCWP));

	// If water content is below the critical soil water content a correction factor is calculated
	//that reduces the transpiration until it stops at WC = WCWP.
	double FR = (WC-soil.WCWP) / (WCCR - soil.WCWP);  // (-)
	
	// If water content is above the critical soil water content a correction factor is calculated
	// that reduces the transpiration when the crop is hampered by waterlogging (WC > WCWET).
	double FRW = (soil.WCST-WC) / (soil.WCST - soil.WCWET);  // (-)
	
	//Replace values for wet days with a higher water content than the critical water content.
	//FR[WC > WCCR] = FRW[WC > WCCR]    // (-)
	FR = WC > WCCR ? FRW : FR;
	
	//Ensure to stay within the 0-1 range
	FR = std::min(1., std::max(0., FR));   // (-)
	
	// Actual transpration
	double TRAN = R.PTRAN * FR; // mm d-1

	// A final correction term is calculated to reduce evaporation and transpiration when evapotranspiration exceeds 
	// the amount of water in soil present in excess of air dryness.
	double aux = EVAP + TRAN;    // mm d-1
	//aux[aux <= 0] = 1;  // mm d-1
	aux = aux <= 0 ? 1 : aux;
	double AVAILF = std::min(1., (S.WA-WAAD)/(control.DELT*aux)); // mm
	
	R.EVAP = EVAP * AVAILF;
	R.TRAN = TRAN * AVAILF;
}


void LINcasModel::drunir() {
	
	// Soil water content      
	// double WC   = 0.001 * WA / ROOTD;  // m3 m-3
	// The amount of soil water at air dryness (AD) and field capacity (FC).
	double WAFC = 1000 * soil.WCFC * S.ROOTD; // mm
	double WAST = 1000 * soil.WCST * S.ROOTD; // mm
	
	// Drainage below the root zone occurs when the amount of water in the soil exceeds field capacity
	// or when the amount of rainfall in excess of interception and evapotranspiration fills up soil
	// water above field capacity.
	double DRAIN =(S.WA-WAFC)/control.DELT + (A.PREC - (R.NINTC + R.EVAP + R.TRAN));  // mm d-1 
	R.DRAIN = std::min(soil.DRATE, std::max(0., DRAIN));              // mm d-1
	
	// Surface runoff occurs when the amount of soil water exceeds total saturation or when the amount
	// of rainfall in excess of interception, evapotranspiration and drainage fills up soil water
	// above total saturation.
	R.RUNOFF = std::max(0., (S.WA - WAST) / control.DELT + (A.PREC - (R.NINTC + R.EVAP + R.TRAN + R.DRAIN))); // mm d-1

	// The irrigation rate is the extra amount of water that is needed to keep soil water at a fraction
	// of field capacity that is defined by setting the parameter IRRIGF. If IRRIGF is set to 1, the
	// soil will be irrigated every timestep to keep the amount of water in the soil at field capacity.
	// IRRIGF = 0 implies rainfed conditions.
	
	 R.IRRIG = control.IRRIGF ? std::max(0., (WAFC - S.WA) / control.DELT - (A.PREC - (R.NINTC + R.EVAP + R.TRAN + R.DRAIN + R.RUNOFF))) : 0; // mm d-1 
}


