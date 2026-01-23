/*
License: EUPL

Author:       Rob van den Beuken, adapted by AGT Schut 18 DEC 2019
Copyright:    Copyright 2019, PPS
Date:         18-12-2019

compute the rates of the crop nutrient amounts and the soil nutrient amounts available for crop uptake.   

C++ version by RH, 2026-01-20
*/


#include "LINTcas.h"
#include "Rcpp.h"

inline double approx2(std::vector<double> X, std::vector<double> Y, double v) {
	int n = X.size();
	double r = NAN;
	if (v <= X[0]) {
		r = Y[0];
	} else if (v >= X[n-1]) {
		r = Y[n-1];
	} else {
		for(int i=1; i<n; i++) {
			if (X[i] >= v) {
				double slope = (Y[i] - Y[i-1]) / (X[i] - X[i-1]);
				r = Y[i-1] + (v - X[i-1]) * slope;
				break;
			}
		}
	}
	return(r);
}	




inline double Mirrored_Monod(double x, double K, double Kmax) {
	if (K <= Kmax){
		return(x == 0 ? 0 : (K+1) * x/(x+K));
	} else {
		K = std::max(0., 2 * Kmax - K);
		x = 1-x;
		return(1. - (x == 0 ? 0 : (K+1) * x/(x+K)));
	}
}


    

std::vector<double> LINcasModel::npkical(
		double NMINLV, double PMINLV, double KMINLV,
		double NMINST, double PMINST, double KMINST, 
		double NMINSO, double PMINSO, double KMINSO, 
		double NMAXLV, double PMAXLV, double KMAXLV, 
		double NMAXST, double PMAXST, double KMAXST,
		double NMAXSO, double PMAXSO, double KMAXSO) {

	//---------------- Nutrient concentrations
	// Minimum nutrient content in the living biomass
	double NMIN = S.WLVG * NMINLV + S.WST * NMINST + S.WSO * NMINSO;    // g N m-2
	double PMIN = S.WLVG * PMINLV + S.WST * PMINST + S.WSO * PMINSO;    // g P m-2
	double KMIN = S.WLVG * KMINLV + S.WST * KMINST + S.WSO * KMINSO;    // g K m-2

	// Maximum nutrient content in the living biomass
	double NMAX = NMAXLV * S.WLVG + NMAXST * S.WST + NMAXSO * S.WSO;   // g N m-2 
	double PMAX = PMAXLV * S.WLVG + PMAXST * S.WST + PMAXSO * S.WSO;   // g P m-2 
	double KMAX = KMAXLV * S.WLVG + KMAXST * S.WST + KMAXSO * S.WSO;   // g K m-2 

	// Optimal nutrient content in the living biomass
	double NOPT = NMIN + crop.FR_MAX * (NMAX - NMIN);   // g N m-2 
	double POPT = PMIN + crop.FR_MAX * (PMAX - PMIN);   // g P m-2 
	double KOPT = KMIN + crop.FR_MAX * (KMAX - KMIN);   // g K m-2 

	//---------------- Actual nutrient concentrations
	// Actual nutrient amounts in the living biomass 
	double NACT = S.ANLVG + S.ANST + S.ANSO;  // g N m-2
	double PACT = S.APLVG + S.APST + S.APSO;   // g P m-2
	double KACT = S.AKLVG + S.AKST + S.AKSO;   // g K m-2 

	//-------------- Nutrition Indices
	double NNI = (NOPT - NMIN) == 0 ? 0 : (NACT - NMIN) / (NOPT - NMIN);  // (-)
	double PNI = (POPT - PMIN) == 0 ? 0 : (PACT - PMIN) / (POPT - PMIN);  // (-)
	double KNI = (KOPT - KMIN) == 0 ? 0 : (KACT - KMIN) / (KOPT - KMIN);  // (-)

	NNI = std::min(1., std::max(0., NNI));
	PNI = std::min(1., std::max(0., PNI));
	KNI = std::min(1., std::max(0., KNI));

	//Combined effect. 
	//Multiplication allows to have extra growth reduction if multiple nutrients are deficient
	//The "Monod" acts as scalar to reduce effect of minor deficiencies that do not affect growth rates but are compensated by dilution. A mirrored Monod function to determine effect of N, P and K stress on NPKI 
	double NPKI = Mirrored_Monod(NNI*PNI*KNI, crop.K_NPK_NI, crop.K_MAX);
  
	return std::vector<double> {NNI, PNI, KNI, NPKI};
}


//Time, S, R, crop, soil, management, DELT
void LINcasModel::nutrientdyn(bool EMERG, 
			double NMINLV, double PMINLV, double KMINLV, double NMINST, double PMINST, double KMINST, 
			double NMINSO, double PMINSO, double KMINSO, double NMINRT, double PMINRT, double KMINRT, 
			double NMAXLV, double PMAXLV, double KMAXLV, double NMAXST, double PMAXST, double KMAXST, 
			double NMAXSO, double PMAXSO, double KMAXSO, double NMAXRT, double PMAXRT, double KMAXRT, 
			double TRANRF, double NNI, double PNI, double KNI, double FLV, double FST, double FRT, double FSO, 
			bool PUSHREDIST) {

    //---------------- Fertilizer application;
    // Ferilizer N/P/K application (kg N/P/K ha-1 d-1);
    double RFERTN = approx2(management.FERTAB[0], management.FERTAB[1], time);    // kg N ha-1 d-1;
	RFERTN = (RFERTN * 1000) / 10000;             // g N m-2 d-1 ;

    double RFERTP = approx2(management.FERTAB[0], management.FERTAB[2], time);    // kg P ha-1 d-1;
	RFERTP = (RFERTP * 1000) / 10000;              // g P m-2 d-1;

    double RFERTK = approx2(management.FERTAB[0], management.FERTAB[3], time);    // kg K ha-1 d-1;
	RFERTK = (RFERTK * 1000) / 10000;             // g N m-2 d-1;

	//---------------- Translocatable nutrient amounts;
	// The amount of translocatable nutrients to the storage organs is the actual nutrient amount minus the ;
	// optimal amount in the plant leaves, stems and roots. For the roots it is the minimum between the ;
	// amount of nutrients available and as a fraction of the amount of translocatable nutrients from the;
	// stem and leaves. The total translocatable nutrients is the sum of this.;

	double ATNLV = std::max(0., S.ANLVG - S.WLVG * (NMINLV + crop.FR_MAX * (NMAXLV - NMINLV)));
	double ATNST = std::max(0., S.ANST - S.WST * (NMINST + crop.FR_MAX * (NMAXST - NMINST)));
	double ATNSO = std::max(0., S.ANSO - S.WSO * (NMINSO + crop.FR_MAX * (NMAXSO - NMINSO)));
	double ATNRT = std::max(0., S.ANRT - S.WRT * (NMINRT + crop.FR_MAX * (NMAXRT - NMINRT)));
	double ATN   = ATNLV + ATNST + ATNSO + ATNRT ; // g N m-2  

	double ATPLV = std::max(0., S.APLVG - S.WLVG * (PMINLV + crop.FR_MAX * (PMAXLV - PMINLV)));
	double ATPST = std::max(0., S.APST - S.WST * (PMINST + crop.FR_MAX * (PMAXST - PMINST)));
	double ATPSO = std::max(0., S.APSO - S.WSO * (PMINSO + crop.FR_MAX * (PMAXSO - PMINSO)));
	double ATPRT = std::max(0., S.APRT - S.WRT * (PMINRT + crop.FR_MAX * (PMAXRT - PMINRT)));
	double ATP   = ATPLV + ATPST + ATPSO + ATPRT; // g P m-2  

	double ATKLV = std::max(0., S.AKLVG - S.WLVG * (KMINLV + crop.FR_MAX * (KMAXLV - KMINLV)));
	double ATKST = std::max(0., S.AKST - S.WST * (KMINST + crop.FR_MAX * (KMAXST - KMINST)));
	double ATKSO = std::max(0., S.AKSO - S.WSO * (KMINSO + crop.FR_MAX * (KMAXSO - KMINSO)));
	double ATKRT = std::max(0., S.AKRT - S.WRT * (KMINRT + crop.FR_MAX * (KMAXRT - KMINRT)));
	double ATK   = ATKLV + ATKST + ATKSO + ATKRT; // g K m-2  ;

	//---------------- Nutrient demand;
	// The nutrient demand is calculated as the difference between the amount of translocatable nutrients and;
	// the std::maximum nutrient content. The total nutrient demand is the sum of the demands of the different organs.;

	double NDEML = std::max(NMAXLV * S.WLVG - S.ANLVG, 0.); // g N m-2;
	double NDEMS = std::max(NMAXST * S.WST - S.ANST, 0.);
	double NDEMR = std::max(NMAXRT * S.WRT - S.ANRT, 0.);
	double NDEMSO = std::max(NMAXSO * S.WSO - S.ANSO, 0.);
	double NDEMTO = std::max(0., NDEML + NDEMS + NDEMSO + NDEMR);

	double PDEML = std::max(PMAXLV * S.WLVG - S.APLVG, 0.); // g P m-2
	double PDEMS = std::max(PMAXST * S.WST - S.APST, 0.);
	double PDEMR = std::max(PMAXRT * S.WRT - S.APRT, 0.);
	double PDEMSO = std::max(PMAXSO * S.WSO - S.APSO, 0.);
	double PDEMTO = std::max(0., PDEML + PDEMS + PDEMSO + PDEMR);

	double KDEML = std::max(KMAXLV * S.WLVG - S.AKLVG, 0.); // g K m-2;
	double KDEMS = std::max(KMAXST * S.WST - S.AKST, 0.);
	double KDEMR = std::max(KMAXRT * S.WRT - S.AKRT, 0.);
	double KDEMSO = std::max(KMAXSO * S.WSO - S.AKSO, 0.);
	double KDEMTO = std::max(0., KDEML + KDEMS + KDEMSO + KDEMR);

	//--------------- Net nutrient translocation in the crop;
	//Internal relocation of nutrients depends on relative content;
	//When there is no demand, there should also be no redistribution;
	// Computation of the translocation of nutrients from the different organs;
	// Daily redistribution to balance nutrient contents between all organs;
	// Based on relative demand of organs;

	double RNTLV = crop.TCNPKT * NDEMTO == 0 ? 0 : ((NDEML/NDEMTO) * ATN - ATNLV)/crop.TCNPKT; // g N m-2 d-1
	double RNTST = crop.TCNPKT * NDEMTO == 0 ? 0 : ((NDEMS/NDEMTO) * ATN - ATNST)/crop.TCNPKT;
	double RNTSO = crop.TCNPKT * NDEMTO == 0 ? 0 : ((NDEMSO/NDEMTO) * ATN - ATNSO)/crop.TCNPKT;
	double RNTRT = crop.TCNPKT * NDEMTO == 0 ? 0 : ((NDEMR/NDEMTO) * ATN - ATNRT)/crop.TCNPKT;
									  
	double RPTLV = crop.TCNPKT * PDEMTO == 0 ? 0 : ((PDEML/PDEMTO) * ATP - ATPLV)/crop.TCNPKT; // g P m-2 d-1
	double RPTST = crop.TCNPKT * PDEMTO == 0 ? 0 : ((PDEMS/PDEMTO) * ATP - ATPST)/crop.TCNPKT;
	double RPTSO = crop.TCNPKT * PDEMTO == 0 ? 0 : ((PDEMSO/PDEMTO) * ATP - ATPSO)/crop.TCNPKT;
	double RPTRT = crop.TCNPKT * PDEMTO == 0 ? 0 : ((PDEMR/PDEMTO) * ATP - ATPRT)/crop.TCNPKT;
									  
	double RKTLV = crop.TCNPKT * KDEMTO == 0 ? 0 : ((KDEML/KDEMTO) * ATK - ATKLV)/crop.TCNPKT; // g K m-2 d-1
	double RKTST = crop.TCNPKT * KDEMTO == 0 ? 0 : ((KDEMS/KDEMTO) * ATK - ATKST)/crop.TCNPKT;
	double RKTSO = crop.TCNPKT * KDEMTO == 0 ? 0 : ((KDEMSO/KDEMTO) * ATK - ATKSO)/crop.TCNPKT;
	double RKTRT = crop.TCNPKT * KDEMTO == 0 ? 0 : ((KDEMR/KDEMTO) * ATK - ATKRT)/crop.TCNPKT;

	//---------------;
	double TINY = 1e-08;
	if (abs(RNTLV + RNTST + RNTSO + RNTRT) > TINY) {
		Rcpp::Rcout << "UNRELIABLE RESULTS!! Internal N reallocation must be net 0\n";
		Rcpp::Rcout << RNTLV << " " << RNTST << " " << RNTSO << " " << RNTRT << std::endl;
	}
	if (abs(RPTLV + RPTST + RPTSO + RPTRT) > TINY) {
		Rcpp::Rcout << "UNRELIABLE RESULTS!! Internal P reallocation must be net 0\n";
		Rcpp::Rcout << RPTLV << " " << RPTST << " " << RPTSO << " " << RPTRT << std::endl;
	}
	if (abs(RKTLV + RKTST + RKTSO + RKTRT) > TINY) {
		Rcpp::Rcout << "UNRELIABLE RESULTS!! Internal K reallocation must be net 0\n";
		Rcpp::Rcout << RKTLV << " " << RKTST << " " << RKTSO << " " << RKTRT << std::endl;
	}

	//--------------- Nutrient uptake;
	// Nutrient uptake from the soil depends on the soil moisture. It is assumed ;
	// that nutrient uptake reduces monod-like with growth rate reduction due to:;
	// 1) Low soil water supply: uptake rates are 50% when soil supply rates equals std::max-uptake rates;
	// 2) Low soil nutrient supply: uptake rates are 50% when std::max soil supply rates equals std::max-uptake rates;
 	double WLIMIT = TRANRF/(crop.K_WATER + TRANRF);
 
	//std::maximum amounts of nutrients for the given amount of biomass;
	double NMAX = NMAXLV * S.WLVG + NMAXST * S.WST + NMAXRT * S.WRT + NMAXSO * S.WSO;
	double PMAX = PMAXLV * S.WLVG + PMAXST * S.WST + PMAXRT * S.WRT + PMAXSO * S.WSO;
	double KMAX = KMAXLV * S.WLVG + KMAXST * S.WST + KMAXRT * S.WRT + KMAXSO * S.WSO;

	//Nutrient equivalents in the soil and std::maximum uptake of equivalents based on optimum ratios
	double NUTEQ_SOIL = S.NMINT + S.PMINT * (PMAX == 0 ? 1 : NMAX/PMAX) + S.KMINT * (KMAX == 0 ? 1 : NMAX/KMAX); //g m-2;
	double NUTEQ_DEMAND = NDEMTO + PDEMTO * (PMAX == 0 ? 1 : NMAX/PMAX) + KDEMTO * (KMAX == 0 ? 1 : NMAX/KMAX);
  
	//The parameter SLOPE_NEQ_SOIL_PEQUPTAKE determines the ratio between biomass and nutrients
	//Plant uptake is proportional to nutrient supply under optimum ratios.
	//uptake rates increase when soil supply is larger. Interaction effects are not accounted for here. 
	//If uptake isn't adequate, concentrations will decrease first, later growth rates decrease.
	//gNEQ m-2 d-1     g m-2           d-1,       d-1                              g m-2
	double RMAX_UPRATE = std::min(NUTEQ_DEMAND/control.DELT, crop.SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE * NUTEQ_SOIL);

	//If actual ratios are suboptimal, uptake of excess nutrients is relatively increased.
	//A suboptimal ratio in the soil results in a suboptimal ratio in the plant when supply is smaller than demand.
	//For example, a soil full of N gives large N uptake, but relatively small P and K uptake.
	//Actual uptake of N is limited by std::maximum N contensts, reducing uptake of P and K contents in the plant
	//to suboptimal levels. In extremis, oversupply of one nutrient can reduce uptake of another.
	double RNUPTR = (NUTEQ_SOIL == 0 ? 0 : RMAX_UPRATE * S.NMINT/NUTEQ_SOIL) * WLIMIT;

	// changed per advise from Tom Schut 2026-01-13;
	//RPUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * (ifelse(PMAX == 0,1,NMAX/PMAX) * PMINT) / NUTEQ_SOIL) * WLIMIT 
	//RKUPTR = ifelse(NUTEQ_SOIL == 0, 0, RMAX_UPRATE * (ifelse(KMAX == 0,1,NMAX/KMAX) * KMINT) / NUTEQ_SOIL) * WLIMIT 
	double RPUPTR = (NUTEQ_SOIL == 0 ? 0 : RMAX_UPRATE * S.PMINT/NUTEQ_SOIL) * WLIMIT;
	double RKUPTR = (NUTEQ_SOIL == 0 ? 0 : RMAX_UPRATE * S.KMINT/NUTEQ_SOIL) * WLIMIT;

	//Actual uptake is limited by demand based on std::maximum concentrations for standing biomass
	//Uptake rate should not exceed std::maximum soil supply.
	RNUPTR = std::min(S.NMINT/control.DELT, std::min(RNUPTR, NDEMTO/control.DELT));
	RPUPTR = std::min(S.PMINT/control.DELT, std::min(RPUPTR, PDEMTO/control.DELT));
	RKUPTR = std::min(S.KMINT/control.DELT, std::min(RKUPTR, KDEMTO/control.DELT));

	//------------- Partitioning;
	// to compute the partitioning of the total N/P/K uptake rates (NUPTR, PUPTR, KUPTR);
	// over the leaves, stem, and roots (kg N/P/K ha-1 d-1);
	// concentrations are balanced, so distribution based on weight proportions;
	double WTOT = S.WLVG + S.WST + S.WSO + S.WRT;

	double RNULV = WTOT == 0 ? 0 : (S.WLVG/WTOT) * RNUPTR; // g N m-2 d-1
	double RNUST = WTOT == 0 ? 0 : (S.WST/WTOT) * RNUPTR;
	double RNUSO = WTOT == 0 ? 0 : (S.WSO/WTOT) * RNUPTR;
	double RNURT = WTOT == 0 ? 0 : (S.WRT/WTOT) * RNUPTR;
					   
	double RPULV = WTOT == 0 ? 0 : (S.WLVG/WTOT) * RPUPTR; // g P m-2 d-1
	double RPUST = WTOT == 0 ? 0 : (S.WST/WTOT) * RPUPTR;
	double RPUSO = WTOT == 0 ? 0 : (S.WSO/WTOT) * RPUPTR;
	double RPURT = WTOT == 0 ? 0 : (S.WRT/WTOT) * RPUPTR;
					   
	double RKULV = WTOT == 0 ? 0 : (S.WLVG/WTOT) * RKUPTR; // g K m-2 d-1
	double RKUST = WTOT == 0 ? 0 : (S.WST/WTOT) * RKUPTR;
	double RKUSO = WTOT == 0 ? 0 : (S.WSO/WTOT) * RKUPTR;
	double RKURT = WTOT == 0 ? 0 : (S.WRT/WTOT) * RKUPTR;

	//------------ Nutrient redistribution because of cutting. ;
	//A negative rate in e.g. RNCUTTING adds to other components.;
	double RANCUTLV = -R.NCUTTING * FLV;  // g N m-2 d-1
	double RANCUTST = -R.NCUTTING * FST;  // g N m-2 d-1
	double RANCUTRT = -R.NCUTTING * FRT;  // g N m-2 d-1
	double RANCUTSO = -R.NCUTTING * FSO;  // g N m-2 d-1

	double RAPCUTLV = -R.PCUTTING * FLV;  // g P m-2 d-1
	double RAPCUTST = -R.PCUTTING * FST;  // g P m-2 d-1
	double RAPCUTRT = -R.PCUTTING * FRT;  // g P m-2 d-1
	double RAPCUTSO = -R.PCUTTING * FSO;  // g P m-2 d-1

	double RAKCUTLV = -R.KCUTTING * FLV;  // g K m-2 d-1
	double RAKCUTST = -R.KCUTTING * FST;  // g K m-2 d-1
	double RAKCUTRT = -R.KCUTTING * FRT;  // g K m-2 d-1
	double RAKCUTSO = -R.KCUTTING * FSO;  // g K m-2 d-1

	//------------ Nutrient redistribution because of leaf death;
	double RNDLVG, RPDLVG, RKDLVG;
	if (S.WLVG > 0) {
	//Nutrients lost due to dying leaves
		R.ANLVD = R.WLVD * crop.NFLVD; // g N m-2 d-1
		R.APLVD = R.WLVD * crop.PFLVD; // g P m-2 d-1
		R.AKLVD = R.WLVD * crop.KFLVD; // g K m-2 d-1
		//Total nutrients in dying leaves
		RNDLVG = R.WLVD * (S.ANLVG / S.WLVG); // g N m-2 d-1
		RPDLVG = R.WLVD * (S.APLVG / S.WLVG); // g P m-2 d-1
		RKDLVG = R.WLVD * (S.AKLVG / S.WLVG); // g K m-2 d-1
	} else {
		RNDLVG = 0;
		RPDLVG = 0;
		RKDLVG = 0;
		R.ANLVD = 0;
		R.APLVD = 0;
		R.AKLVD = 0;
	}
	//What is not lost to dead leaves most be redistributed to other organs;
	double RNDLV_REDIST = std::max(0., RNDLVG - R.ANLVD); // g N m-2 d-1
	double RPDLV_REDIST = std::max(0., RPDLVG - R.APLVD);
	double RKDLV_REDIST = std::max(0., RKDLVG - R.AKLVD);

	//------------ Nutrient redistribution because of storage root DM redistribution after dormancy;
	// DM to the leaves, with new at std::maximum NPK concentrations;
	//             g DM m-2 d-1 * (gN m-2 d-1 * gDM-1 m2 d)
	double RANSO2LVLV = R.REDISTLVG * NMAXLV * PUSHREDIST;  // g N m-2 d-1
	double RAPSO2LVLV = R.REDISTLVG * PMAXLV * PUSHREDIST;  // g P m-2 d-1
	double RAKSO2LVLV = R.REDISTLVG * KMAXLV * PUSHREDIST;  // g K m-2 d-1

	// DM loss of the storage roots
	double RANSO2LVSO = S.WSO == 0 ? 0 : R.REDISTSO * (S.ANSO / S.WSO);  // g N m-2 d-1
	double RAPSO2LVSO = S.WSO == 0 ? 0 : R.REDISTSO * (S.APSO / S.WSO);  // g P m-2 d-1
	double RAKSO2LVSO = S.WSO == 0 ? 0 : R.REDISTSO * (S.AKSO / S.WSO);  // g K m-2 d-1
   
	//------------- Rate of change of N/P/K in crop organs;
	// uptake + net translocation + cutting;
	// N relocated to stem, P+K to storate roots;

	R.ANLVG = RNULV + RNTLV + RANCUTLV + RANSO2LVLV  - RNDLVG; // g N m-2 d-1
	R.ANST = RNUST + RNTST + RANCUTST + RNDLV_REDIST;          // g N m-2 d-1
	R.ANRT = RNURT + RNTRT + RANCUTRT;                         // g N m-2 d-1
	R.ANSO = RNUSO + RNTSO + RANCUTSO - RANSO2LVSO;            // g N m-2 d-1

	R.APLVG = RPULV + RPTLV + RAPCUTLV + RAPSO2LVLV  - RPDLVG; // g P m-2 d-1
	R.APST = RPUST + RPTST + RAPCUTST;                         // g P m-2 d-1
	R.APRT = RPURT + RPTRT + RAPCUTRT;                         // g P m-2 d-1
	R.APSO = RPUSO + RPTSO + RAPCUTSO - RAPSO2LVSO + RPDLV_REDIST;   // g P m-2 d-1
	R.AKLVG = RKULV + RKTLV + RAKCUTLV + RAKSO2LVLV  - RKDLVG; // g K m-2 d-1
	R.AKST = RKUST + RKTST + RAKCUTST;                         // g K m-2 d-1
	R.AKRT = RKURT + RKTRT + RAKCUTRT;                         // g K m-2 d-1
	R.AKSO = RKUSO + RKTSO + RAKCUTSO - RAKSO2LVSO + RKDLV_REDIST;   // g K m-2 d-1
 
	//------------ Soil supply;
	// Soil nutrient supply through mineralization during crop growth(not affected by water supply)
	//The reason for this is that soil supply isn't modelled but a given from control plots. 
	//With unknown number of days with water limitations it is impossible to know the potential uptake rate from this pool.
	// Rate of the nutrient amount which becomes available due to soil mineralization.  
	R.NMINS = S.NMINS < soil.RTNMINS ? -S.NMINS/control.DELT : -soil.RTNMINS; // g N m-2 d-1
	R.PMINS = S.NMINS < soil.RTPMINS ? -S.PMINS/control.DELT : -soil.RTPMINS; // g P m-2 d-1
	R.KMINS = S.NMINS < soil.RTKMINS ? -S.KMINS/control.DELT : -soil.RTKMINS; // g K m-2 d-1
  
	//------------ Fertilizer supply
	//Fertilizer nutrient supply 
	//Pool in the soil which is not yet avalable for plant uptake
	//        supply rate rate that becomes available for uptake
	// Rate of the nutrient amount which becomes available due to fertilization.
	R.NMINF = RFERTN - crop.RTNMINF * S.NMINF * WLIMIT;   // g N m-2 d-1
	R.PMINF = RFERTP - crop.RTPMINF * S.PMINF * WLIMIT;   // g P m-2 d-1
	R.KMINF = RFERTK - crop.RTKMINF * S.KMINF * WLIMIT;   // g K m-2 d-1

	// Change in total inorganic N/P/K in soil as function of fertilizer input,
	// soil N/P/K mineralization and crop uptake.
	// Rate of change of the total mineral N, P and K available for crop uptake. 
	R.NMINT = crop.RTNMINF * S.NMINF * WLIMIT + (-R.NMINS) - RNUPTR;   // g N m-2 d-1
	R.PMINT = crop.RTPMINF * S.PMINF * WLIMIT + (-R.PMINS) - RPUPTR;   // g P m-2 d-1
	R.KMINT = crop.RTKMINF * S.KMINF * WLIMIT + (-R.KMINS) - RKUPTR;   // g K m-2 d-1
   
	if ((EMERG) && (S.WST == 0)) {
		R.ANLVG = R.ANLVD = R.ANST = R.ANRT = R.ANSO = 0;
		R.APLVG = R.APLVD = R.APST = R.APRT = R.APSO = 0;
		R.AKLVG = R.AKLVD = R.AKST = R.AKRT = R.AKSO = 0;
	}
}

