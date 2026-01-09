/*

Author:       Rob van den Beuken
Copyright:    Copyright 2019, PPS
Email:        rob.vandenbeuken@wur.nl
Date:         29-01-2019

This file contains a component of the LINTUL-CASSAVA model. 
The purpose of the evaptr function is to compute the actual rates of evaporation and transpiration. 
*/

#include "LINTcas.h"

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