/*
License: EUPL

LINTUL-CASSACA-NPK 

Orignally developed by: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in cassava using the LINTUL model. Field Crops Research 219: 256-272.

Original R code by Rob van den Beuken; rob.vandenbeuken@wur.nl; (c) 2019, PPS

*/


#include "LINTcas.h"
#include "SimUtil.h"

void LINcasModel::states() {
	S.ROOTD = S.ROOTD + R.ROOTD;
	S.WA = S.WA + R.WA;
	S.TSUM = S.TSUM + R.TSUM;
	S.TSUMCROP = S.TSUMCROP + R.TSUMCROP;
	S.TSUMCROPLEAFAGE = S.TSUMCROPLEAFAGE + R.TSUMCROPLEAFAGE;
	S.DORMTSUM = S.DORMTSUM + R.DORMTSUM;
	S.PUSHDORMRECTSUM = S.PUSHDORMRECTSUM + R.PUSHDORMRECTSUM;
	S.PUSHREDISTENDTSUM = S.PUSHREDISTENDTSUM + R.PUSHREDISTENDTSUM;
	S.DORMTIME = S.DORMTIME + R.DORMTIME;
	S.WCUTTING = S.WCUTTING + R.WCUTTING;
	//S.TRAIN = S.TRAIN + R.TRAIN;
	S.PAR = S.PAR + R.PAR;
	S.LAI = S.LAI + R.LAI;
	S.WLVD = S.WLVD + R.WLVD;
	S.WLV = S.WLV + R.WLV;
	S.WST = S.WST + R.WST;
	S.WSO = S.WSO + R.WSO;
	S.WRT = S.WRT + R.WRT;
	S.WLVG = S.WLVG + R.WLVG;
	S.TRAN = S.TRAN + R.TRAN;
	S.EVAP = S.EVAP + R.EVAP;
	S.PTRAN = S.PTRAN + R.PTRAN;
	S.PEVAP = S.PEVAP + R.PEVAP;
	S.RUNOFF = S.RUNOFF + R.RUNOFF;
	S.NINTC = S.NINTC + R.NINTC;
	S.DRAIN = S.DRAIN + R.DRAIN;
	S.IRRIG = S.IRRIG + R.IRRIG;
	S.REDISTLVG = S.REDISTLVG + R.REDISTLVG;
	S.REDISTSO = S.REDISTSO + R.REDISTSO;
	S.PUSHREDISTSUM = S.PUSHREDISTSUM + R.PUSHREDISTSUM;
	S.WSOFASTRANSLSO = S.WSOFASTRANSLSO + R.WSOFASTRANSLSO;
}

void LINcasModel::output(){
	out.values.insert(out.values.end(),
		{ double(step), 
			S.ROOTD, S.WA, S.TSUM, S.TSUMCROP, S.TSUMCROPLEAFAGE, S.DORMTSUM, 
			S.PUSHDORMRECTSUM, S.PUSHREDISTENDTSUM, S.DORMTIME, S.WCUTTING, S.PAR, S.LAI, 
			S.WLVD, S.WLV, S.WST, S.WSO, S.WRT, S.WLVG, S.TRAN, S.EVAP, S.PTRAN, S.PEVAP, 
			S.RUNOFF, S.NINTC, S.DRAIN, S.REDISTLVG, S.REDISTSO, S.PUSHREDISTSUM, S.WSOFASTRANSLSO, S.IRRIG
		}
	);
}



inline double SatVP(const double &tmp) {
	return 0.611 * std::exp(17.4 * tmp / (tmp + 239)) ;
}

bool LINcasModel::weather_step() {
	A.SRAD = W.srad[time] / 1000.;
	A.WIND = W.wind[time];
	A.VAPR = W.vapr[time] * 10;
	A.PREC = W.prec[time] / 10 ; // cm !

	double SatVP_TMMN = SatVP(W.tmin[time]);
	double SatVP_TMMX = SatVP(W.tmax[time]);
  // vapour pressure deficits;
	A.VPD_MN = std::max(0., SatVP_TMMN - A.VAPR);
	A.VPD_MX = std::max(0., SatVP_TMMX - A.VAPR);
	A.TAVG = 0.5 * (W.tmin[time] + W.tmax[time]);   // Deg. C     :     daily average temperature
	return true;
}

void LINcasModel::rates() {

    if (S.TSUM >= crop.FINTSUM) {;
		// If the plant is not growing anymore all plant related rates are set to 0.;
		return;
    } 

	double DTEFF  = std::max(0., A.TAVG - crop.TBASE); // Deg. C   : effective daily temperature
	R.PAR  = crop.FPAR * A.SRAD;        // PAR MJ m-2 d-1   : PAR radiation

    // Temperature sum after planting;
	R.TSUM = ((time - management.PLDATE) >= 0) ? DTEFF : 0; // Deg. C 

	// Determine water content of rooted soil
	double WC = 0.001 * S.WA/S.ROOTD;	 // (-) 
 
//---EMERGENCE-----------------------------------------------//;
	// emergence occurs (1) when the temperature sum exceeds the temperature sum needed for emergence. And (2)
	// when enough water is available in the soil. 

	bool EMERG = ((S.TSUMCROP > 0) || ((WC-soil.WCWP) >= 0 && (S.TSUM-crop.OPTEMERGTSUM) >= 0));

	// Emergence of the crop is used to calculate the temperature sum of the crop.
	R.TSUMCROP = DTEFF * EMERG ;     // Deg. C

//---FIBROUS ROOT GROWTH------------------------------------------//;
	// If the soil water content drops to, or below, wilting point fibrous root growth stops.;
	// Root growth continues till the maximum rooting depth is reached.;
	// The rooting depth (m) is calculated from a maximum rate of change in rooting depth, ;
	// the emergence of the crop and the constraints mentioned above.;

	if (((S.ROOTD-soil.ROOTDM) < 0) && ((WC-soil.WCWP) >= 0)) {
		R.ROOTD = crop.RRDMAX * EMERG;		 // mm d-1;
	} else { 
		R.ROOTD = 0;
	}

//---WATER BALANCE---------------------------------------------//;
	// Explored water of new soil water layers by the roots, explored soil is assumed to have a FC soil moisture content).
	
	double EXPLOR = 1000 * R.ROOTD * soil.WCFC ; // mm d-1

	// Interception of the canopy, depends on the amount of rainfall and the LAI. 
	R.NINTC = std::min(A.PREC, (crop.FRACRNINTC * S.LAI)) ;    // mm d-1

	// Potential evaporation and transpiration are calculated using the Penman equation.
	Penman();
	//	std::vector<double> PENM = Penman(A.TAVG, A.VAPR, A.SRAD, S.LAI, A.WIND, R.NINTC);
	//	R.PTRAN = PENM[0];				// mm d-1
	//	R.PEVAP = PENM[1];				// mm d-1
	// Soil moisture content at severe drought and the critical soil moisture content are calculated to see if drought stress occurs in the crop. The critical soil moisture content depends on the transpiration coefficient which is a measure of how drought resistant the crop is. ;
	double WCSD = soil.WCWP * crop.TWCSD;
	double WCCR = soil.WCWP + std::max(WCSD-soil.WCWP, 
		(R.PTRAN/(R.PTRAN+crop.TRANCO) * (soil.WCFC-soil.WCWP)));

	// The actual evaporation and transpiration is based on the soil moisture contents and the potential evaporation and transpiration rates.;
	//double EVA = evaptr(R.PEVAP, R.PTRAN, S.ROOTD, S.WA, soil.WCAD,
	// soil.WCWP,crop.TWCSD,soil.WCFC, soil.WCWET, soil.WCST, crop.TRANCO, control.DELT);
	//R.TRAN = EVA[0];				   // mm d-1
	//R.EVAP = EVA[1];				   // mm d-1

	evaptr();

	// The transpiration reduction factor is defined as the ratio between actual and potential transpiration;
	double TRANRF = R.PTRAN <= 0 ? 1 : R.TRAN/R.PTRAN; // (-)

	// Drainage and Runoff is calculated using the drunir function.;
	//	std::vector<double> DRUNIR  = drunir(A.PREC, R.NINTC, R.EVAP, R.TRAN, management.IRRIGF, 
	//			soil.DRATE, control.DELT, S.WA, S.ROOTD, soil.WCFC, soil.WCST);
	//	R.DRAIN  = DRUNIR[0];			  // mm d-1
	//	R.RUNOFF = DRUNIR[1];			 // mm d-1
	drunir();
	
	// Rate of change of soil water amount;
	R.WA = (A.PREC + EXPLOR + R.IRRIG) - (R.NINTC + R.RUNOFF + R.TRAN + R.EVAP + R.DRAIN);  // mm d-1;

	if (!EMERG) return;


//---DORMANCY AND RECOVERY-------------------------------------------//;
	// The crop enters the dormancy phase as the soil water content is lower than the soil water content at ;
	// severe drought and as the LAI is lower than the minimal LAI.
	bool dormancy = ((WC-WCSD) <= 0) && ((S.LAI - crop.LAI_MIN) <= 0);

	// The crop goes out of dormancy if the water content is higher than a certain recovery water content and as the water content is larger than the wilting point soil moisture content. 
	bool pushdor = ((WC - crop.RECOV * WCCR) >= 0) && ((WC - soil.WCWP) >= 0);

	// The redistributed fraction of storage root DM to the leaves. 
	bool WSOREDISTFRAC = S.WSO == 0 ? 1 : S.REDISTSO/S.WSO;

	// Three push functions are used to determine the redistribution and recovery from dormancy, a final function DORMANCY is used to indicate if the crop is still in dormancy:
	// (1) PUSHREDISTEND: The activation of the PUSHREDISTEND function ends the redistribution phase. Redistribution stops when the redistributed fraction reached the maximum redistributed fraction or when the minimum amount of new leaves is produced after dormancy or when the Tsum during the recovery exceeds the maximum redistribution temperature sum.
	// (2) PUSHREDIST: The activation of the PUSHREDIST function ends the dormancy phase including the delay temperature sum needed for the redistribution of DM.
	// (3) PUSHDORMREC: Indicates if the the crop is still in dormancy. Dormancy can only when the temperature sum of the crop exceeds the temperature sum of the branching.
	bool PUSHREDISTEND = ((((WSOREDISTFRAC - crop.WSOREDISTFRACMAX) >= 0) ||
			((S.REDISTLVG - crop.WLVGNEWN)>= 0) ||
			((S.PUSHREDISTSUM - crop.TSUMREDISTMAX) >= 0)) &&
			(S.PUSHREDISTSUM > 0));

	bool PUSHREDIST  = (S.PUSHDORMRECTSUM - crop.DELREDIST) >= 0 ? !PUSHREDISTEND : false;  // (-)
	bool PUSHDORMREC = pushdor && (S.DORMTSUM > 0) && (!PUSHREDIST) && ((S.TSUMCROP - crop.TSUMSBR) >= 0); // (-)

	bool DORMANCY = (dormancy || PUSHDORMREC) && (!PUSHREDIST) && ((S.TSUMCROP - crop.TSUMSBR) >= 0); // (-)

	// The temperature sums related to the dormancy and recovery periods.
	R.DORMTSUM = DTEFF * DORMANCY - (S.DORMTSUM/control.DELT) * PUSHREDIST; // Deg. C
	R.PUSHDORMRECTSUM = DTEFF * PUSHDORMREC - (S.PUSHDORMRECTSUM/control.DELT) * (!(PUSHDORMREC || PUSHREDIST));  // Deg. C;
	
	R.PUSHREDISTSUM = DTEFF * PUSHREDIST - (S.PUSHREDISTSUM/control.DELT) * PUSHREDISTEND;  // Deg. C
	R.PUSHREDISTENDTSUM = DTEFF * PUSHREDIST - (S.PUSHREDISTENDTSUM/control.DELT) * (!PUSHREDISTEND); // Deg. C

	// No. of days in dormancy
	R.DORMTIME = DORMANCY;  // d

	// Dry matter redistribution after dormancy. The rate of redistribution of the storage roots dry matter to leaf dry matter. A certain fraction is lost for the conversion of storage organs dry matter to leaf dry matter.
	R.REDISTSO = crop.RRREDISTSO * S.WSO * PUSHREDIST - (S.REDISTSO/control.DELT) * (S.DORMTSUM > 0); // g DM m-2 d-1
	R.REDISTLVG = crop.SO2LV * R.REDISTSO * (!DORMANCY);  // g DM m-2 d-1
 
//---LIGHT INTERCEPTION AND GROWTH-----------------------------------------//
	// Light interception and total crop growth rate.
	double PARINT = R.PAR * (1 - std::exp(-crop.K_EXT * S.LAI));  // MJ m-2 d-1
	double LUE = crop.LUE_OPT * approx(crop.TTB, A.TAVG);   // g DM m-2 d-1

	double GTOTAL = LUE * PARINT * TRANRF * (!DORMANCY);  // g DM m-2 d-1

//---LEAF SENESCENCE------------------------------------------------------//;

//--- AGE;
	// The calculation of the physiological leaf age.  ;
	R.TSUMCROPLEAFAGE = DTEFF * EMERG - (S.TSUMCROPLEAFAGE/control.DELT) * PUSHREDIST;     // Deg. C

	// Relative death rate due to aging depending on leaf age and the daily average temperature.
	double RDRDV = (S.TSUMCROPLEAFAGE - crop.TSUMLLIFE >= 0) ? approx(crop.RDRT, A.TAVG) : 0; // d-1

//--- SHEDDING;
	// Relative death rate due to self shading, depending on a critical leaf area index at which leaf shedding is;
	// induced. Leaf shedding is limited to a maximum leaf shedding per day. 
	double RDRSH = crop.RDRSHM * (S.LAI-crop.LAICR) / crop.LAICR;  // d-1
	RDRSH = std::min(std::max(0., RDRSH), crop.RDRSHM) ;

//--- DROUGHT;
	// ENSHED triggers enhanced leaf senescence due to severe drought or excessive soil water. It assumes that drought or excessive water does not affect young leaves. It only affects leaves that have a reached a given fraction of the leaf age.

	bool ENHSHED = (((WC-WCSD) < 0) || ((WC-soil.WCWET) >= 0)) && 
		((S.TSUMCROPLEAFAGE - crop.FRACTLLFENHSH * crop.TSUMLLIFE) >= 0);    // (-)

	// Relative death rate due to severe drought
	double RDRSD = crop.RDRB * ENHSHED;    // d-1
	//---;

	// Effective relative death rate and the resulting decrease in LAI.
	double RDR = ((S.TSUMCROPLEAFAGE - crop.TSUMLLIFE) >= 0) ? std::max(RDRDV, std::max(RDRSH, RDRSD)) : 0; 	// d-1
	//	DLAI  = LAI * RDR * (1 - FASTRANSLSO) * (1 - DORMANCY)    // m2 m-2 d-1
	double DLAI  = S.LAI * RDR * (!DORMANCY);    // m2 m-2 d-1

	// Fraction of the maximum specific leaf area index depending on the temperature sum of the crop. And its specific leaf area index.
	double FRACSLACROPAGE = approx(crop.FRACSLATB, S.TSUMCROP);  // (-)
	double SLA = crop.SLA_MAX * FRACSLACROPAGE ;   // m2 g-1 DM

	// The rate of storage root DM production with DM supplied by the leaves before abscission.
	R.WSOFASTRANSLSO = S.WLVG * RDR * crop.FASTRANSLSO * (!DORMANCY);   // g storage root DM m-2 d-1

	// Decrease in leaf weight due to leaf senesence.
	//	DLV = (WLVG * RDR - RWSOFASTRANSLSO) * (1 - DORMANCY)  // g leaves DM m-2 d-1
	double DLV = S.WLVG * RDR * (!DORMANCY);			   // g leaves DM m-2 d-1
	R.WLVD = (DLV - R.WSOFASTRANSLSO);				   // g leaves DM m-2 d-1

//---PARTITIONING---------------------------------------------------//;
	// Allocation of assimilates to the different organs. The fractions are modified for water availability.;
	double FRTMOD = std::max(1., 1/(TRANRF+0.5));			// (-);;
	// Fibrous roots;
	double FRT    = approx(crop.FRTTB, S.TSUMCROP) * FRTMOD; // (-)
	double FSHMOD = (1 - FRT) / (1 - FRT / FRTMOD); // (-)
	// Leaves;
	double FLV    = approx(crop.FLVTB, S.TSUMCROP) * FSHMOD; // (-)
	// Stems;
	double FST    = approx(crop.FSTTB, S.TSUMCROP) * FSHMOD; // (-)
	// Storage roots;
	double FSO    = approx(crop.FSOTB, S.TSUMCROP) * FSHMOD; // (-)

	//When plants emerge from dormancy, leaf growth may go far too quickly. ;
	//Adjust partitioning if LAI too large;
	double FLV_ADJ  = FLV * std::max(0., std::min(1., (S.LAI-crop.LAICR) / crop.LAICR));
	FLV = FLV - FLV_ADJ;
	FSO = FSO + 0.66 * FLV_ADJ; //Not used assimilated go for 2/3 to storage roots;
	FST = FST + 0.34 * FLV_ADJ; //Not used assimilated go for 1/3 to stem;
     
	// Minimal stem cutting weight. ;
	double WCUTTINGMIN = crop.WCUTTINGMINPRO * crop.WCUTTINGIP;

	// Stem cutting partioning at emergence. ;
	if (EMERG && (S.WST == 0)) {
		R.WCUTTING = S.WCUTTING *(-crop.FST_CUTT - crop.FRT_CUTT - crop.FLV_CUTT - crop.FSO_CUTT) ;
		R.WRT = crop.WCUTTINGIP * crop.FRT_CUTT;	// g fibrous root DM m-2 d-1
		R.WST = crop.WCUTTINGIP * crop.FST_CUTT;	// g stem DM m-2 d-1
		R.WLVG = crop.WCUTTINGIP * crop.FLV_CUTT;     // g leaves DM m-2 d-1
		R.WSO  = crop.WCUTTINGIP * crop.FSO_CUTT;     // g storage root DM m-2 d-1
	} else if (S.TSUM > crop.OPTEMERGTSUM) { ;
		R.WCUTTING = -crop.RDRWCUTTING * S.WCUTTING * ((S.WCUTTING-WCUTTINGMIN) >= 0) * TRANRF * EMERG * (!DORMANCY);  // g stem cutting DM m-2 d-1;
		R.WRT   = (std::abs(GTOTAL)+std::abs(R.WCUTTING)) * FRT;	// g fibrous root DM m-2 d-1
		R.WST   = (std::abs(GTOTAL)+std::abs(R.WCUTTING)) * FST;	// g stem DM m-2 d-1
		R.WLVG  = (std::abs(GTOTAL)+std::abs(R.WCUTTING)) * FLV - DLV + R.REDISTLVG * PUSHREDIST; // g leaves DM m-2 d-1 
		R.WSO   = (std::abs(GTOTAL)+std::abs(R.WCUTTING)) * FSO + R.WSOFASTRANSLSO - R.REDISTSO; // g storage root DM m-2 d-1
	} else {
		R.WCUTTING = 0;   // g stem cutting DM m-2 d-1
		R.WRT = 0;	// g fibrous root DM m-2 d-1
		R.WST = 0;	// g stem DM m-2 d-1
		R.WLVG = 0;	 // g leaves DM m-2 d-1
		R.WSO  = 0;	 // g storage root DM m-2 d-1
	};
	// Growth of the leaf weight
	R.WLV = R.WLVG + R.WLVD;			// g leaves DM m-2 d-1

//---LEAF GROWTH---------------------------------------------------//;
	// Green leaf weight ;
	double GLV = FLV * (GTOTAL + abs(R.WCUTTING)) + R.REDISTLVG * PUSHREDIST;  // g green leaves DM m-2 d-1;

	// Growth of the leaf are index;
	double GLAI;
	if (S.TSUMCROP == 0) {
		// Growth before seedling emergence
		GLAI =  0;     // m2 m-2 d-1
	} else if (S.LAI == 0 && WC > soil.WCWP) {
		// Growth at day of seedling emergence
		GLAI =  crop.LAII / control.DELT;  // m2 m-2 d-1
	} else if (S.TSUMCROP < crop.TSUMLA_MIN && S.LAI < crop.LAIEXPOEND) {
		 // Growth during juvenile stage
		GLAI = ((S.LAI * (std::exp(crop.RGRL * DTEFF * control.DELT) - 1) / control.DELT) 
				+ std::abs(R.WCUTTING) * FLV * SLA) * TRANRF;  // m2 m-2 d-1
	} else {
		GLAI = SLA * GLV * (!DORMANCY);  // m2 m-2 d-1  
	}
	// Change in LAI due to new growth of leaves
	R.LAI = GLAI - DLAI;    // m2 m-2 d-1
}



void LINcasModel::run() {
	step = 1;
	//unsigned cropstart_step = step + control.cropstart;
	while (step < maxdur) {
		if (! weather_step()) break;
		rates();
		output();
		states();
		time++;
		step++;
		if (fatalError) {
			return;
		}
	}
}




/*
LINTCAS2 = function(weather, crop, soil, management, control){;

	control.DELT = control.timestep;
	pars = c(crop, soil, management.IRRIGF, control.DELT=control.DELT);
	iR = iniRates() 	;
	wth = weather[weather.DATE >= control.startDATE, ];
	;
	season = seq(control.startDATE, management.HVDATE, by = control.DELT);
	out = vector(length=length(season), mode="list");
	S = as.list(LC_iniSTATES(pars));
	for (i in 1:length(season)) {;
		R = get_rates(season[i], wth[i, ], S, iR, crop, soil, management, control.DELT);
		states = unlist(S);
		AUX = c(WSOTHA = S[["WSO"]] * 0.01, TRANRF = ifelse(R.PTRAN <= 0, 1, R.TRAN/R.PTRAN), ;
				HI = states[["WSO"]] / sum(states[c("WSO", "WLV", "WST", "WRT")]));
		// order ;
		R = R[names(S)];
		rates = unlist(R);
		names(rates) = paste0("R", names(rates));
		out[[i]] = c(states, AUX, rates);
		//S = S + rates;
		S = get_states(S, R);
    };
	out = do.call(rbind, out);

// for compatability with the original output;
	steps = seq(control.startDOY, management.DOYHAR, by = control.DELT);
	j = 1:length(steps);
	DAYS = (weather.DOY[1] + (1:nrow(weather))-1)[steps];

	out = data.frame(year_planting=wth.YEAR[1], year=wth.YEAR[j], DOY=weather.DOY[steps], time=DAYS, out);
	out;
};

*/