/*
Authors: Robert Hijmans and Huang Fang
Date: June 2016

License: GNU General Public License (GNU GPL) v. 2
*/

#include <Rcpp.h>
//using namespace Rcpp;
#include "R_interface_util.h"
#include "LINTcas.h"


// [[Rcpp::export(".LC")]]
Rcpp::List LC(List crop, DataFrame weather, List soil, List management, List control) {

	LINcasControl cntr;
	LINcasManagement mgm;
	LINcasCropParameters crp;
	LINcasSoilParameters sol;
	LINcasWeather wth;

	cntr.modelstart = valueFromList<long>(control, "startDATE");
	cntr.outvars = valueFromListDefault<std::string>(control, "outvars", "full");
	cntr.water_limited = valueFromListDefault<bool>(control, "water_limited", true); 
	cntr.NPKmodel = valueFromList<bool>(control, "NPKmodel");

// management
	mgm.PLDATE = valueFromList<long>(management, "PLDATE");
	mgm.HVDATE = valueFromList<long>(management, "HVDATE");
	if (cntr.NPKmodel) {
		mgm.FERTAB = TableFromList2(management, "FERTAB", 4);
	}
	
// crop
	crp.TWCSD = valueFromList<double>(crop, "TWCSD");
	crp.FRACRNINTC = valueFromList<double>(crop, "FRACRNINTC");
	crp.RECOV = valueFromList<double>(crop, "RECOV");
	crp.TRANCO = valueFromList<double>(crop, "TRANCO");
	crp.WCUTTINGUNIT = valueFromList<double>(crop, "WCUTTINGUNIT");
	crp.NCUTTINGS = valueFromList<double>(crop, "NCUTTINGS");
	crp.WCUTTINGIP = valueFromList<double>(crop, "WCUTTINGIP");
	crp.ROOTDI = valueFromList<double>(crop, "ROOTDI");
	crp.SLAI = valueFromList<double>(crop, "SLAI");
	crp.WLVI = valueFromList<double>(crop, "WLVI");
	crp.LAII = valueFromList<double>(crop, "LAII");
	crp.WCUTTINGMINPRO = valueFromList<double>(crop, "WCUTTINGMINPRO");
	crp.FST_CUTT = valueFromList<double>(crop, "FST_CUTT");
	crp.FRT_CUTT = valueFromList<double>(crop, "FRT_CUTT");
	crp.FLV_CUTT = valueFromList<double>(crop, "FLV_CUTT");
	crp.FSO_CUTT = valueFromList<double>(crop, "FSO_CUTT");
	crp.RDRWCUTTING = valueFromList<double>(crop, "RDRWCUTTING");
	crp.FPAR = valueFromList<double>(crop, "FPAR");
	crp.K_EXT = valueFromList<double>(crop, "K_EXT");
	crp.LUE_OPT = valueFromList<double>(crop, "LUE_OPT");
	crp.RRDMAX = valueFromList<double>(crop, "RRDMAX");
	crp.RDRB = valueFromList<double>(crop, "RDRB");
	crp.LAICR = valueFromList<double>(crop, "LAICR");
	crp.RDRSHM = valueFromList<double>(crop, "RDRSHM");
	crp.FRACTLLFENHSH = valueFromList<double>(crop, "FRACTLLFENHSH");
	crp.FASTRANSLSO = valueFromList<double>(crop, "FASTRANSLSO");
	crp.SLA_MAX = valueFromList<double>(crop, "SLA_MAX");
	crp.RGRL = valueFromList<double>(crop, "RGRL");
	crp.LAIEXPOEND = valueFromList<double>(crop, "LAIEXPOEND");
	crp.TBASE = valueFromList<double>(crop, "TBASE");
	crp.OPTEMERGTSUM = valueFromList<double>(crop, "OPTEMERGTSUM");
	crp.TSUMLA_MIN = valueFromList<double>(crop, "TSUMLA_MIN");
	crp.TSUMSBR = valueFromList<double>(crop, "TSUMSBR");
	crp.TSUMLLIFE = valueFromList<double>(crop, "TSUMLLIFE");
	crp.TSUMREDISTMAX = valueFromList<double>(crop, "TSUMREDISTMAX");
	crp.FINTSUM = valueFromList<double>(crop, "FINTSUM");
	crp.LAI_MIN = valueFromList<double>(crop, "LAI_MIN");
	crp.WSOREDISTFRACMAX = valueFromList<double>(crop, "WSOREDISTFRACMAX");
	crp.WLVGNEWN = valueFromList<double>(crop, "WLVGNEWN");
	crp.SO2LV = valueFromList<double>(crop, "SO2LV");
	crp.RRREDISTSO = valueFromList<double>(crop, "RRREDISTSO");
	crp.DELREDIST = valueFromList<double>(crop, "DELREDIST");
	crp.FRACSLATB = TableFromList2(crop, "FRACSLATB");
	crp.RDRT = TableFromList2(crop, "RDRT");
	crp.TTB = TableFromList2(crop, "TTB");
	crp.FLVTB = TableFromList2(crop, "FLVTB");
	crp.FSTTB = TableFromList2(crop, "FSTTB");
	crp.FSOTB = TableFromList2(crop, "FSOTB");
	crp.FRTTB = TableFromList2(crop, "FRTTB");
	crp.SLAII = valueFromList<double>(crop, "SLAII");

	if (cntr.NPKmodel) {
		crp.NLAI = valueFromList<double>(crop, "NLAI");
		crp.RDRNS = valueFromList<double>(crop, "RDRNS");
		crp.K_MAX = valueFromList<double>(crop, "K_MAX");
		crp.K_NPK_NI = valueFromList<double>(crop, "K_NPK_NI");
		crp.TSUM_NPKI = valueFromList<double>(crop, "TSUM_NPKI");
		crp.K_WATER = valueFromList<double>(crop, "K_WATER");
		crp.SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE = valueFromList<double>(crop, "SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE");
		crp.FR_MAX = valueFromList<double>(crop, "FR_MAX");
		crp.N_RECOV = valueFromList<double>(crop, "N_RECOV");
		crp.P_RECOV = valueFromList<double>(crop, "P_RECOV");
		crp.K_RECOV = valueFromList<double>(crop, "K_RECOV");
		crp.NFLVD = valueFromList<double>(crop, "NFLVD");
		crp.PFLVD = valueFromList<double>(crop, "PFLVD");
		crp.KFLVD = valueFromList<double>(crop, "KFLVD");
		crp.TCNPKT = valueFromList<double>(crop, "TCNPKT");
		crp.RTNMINF = valueFromList<double>(crop, "RTNMINF");
		crp.RTPMINF = valueFromList<double>(crop, "RTPMINF");
		crp.RTKMINF = valueFromList<double>(crop, "RTKMINF");
		crp.NMINMAXLV = TableFromList2(crop, "NMINMAXLV", 3);
		crp.PMINMAXLV = TableFromList2(crop, "PMINMAXLV", 3);
		crp.KMINMAXLV = TableFromList2(crop, "KMINMAXLV", 3);
		crp.NMINMAXST = TableFromList2(crop, "NMINMAXST", 3);
		crp.PMINMAXST = TableFromList2(crop, "PMINMAXST", 3);
		crp.KMINMAXST = TableFromList2(crop, "KMINMAXST", 3);
		crp.NMINMAXSO = TableFromList2(crop, "NMINMAXSO", 3);
		crp.PMINMAXSO = TableFromList2(crop, "PMINMAXSO", 3);
		crp.KMINMAXSO = TableFromList2(crop, "KMINMAXSO", 3);
		crp.NMINMAXRT = TableFromList2(crop, "NMINMAXRT", 3);
		crp.PMINMAXRT = TableFromList2(crop, "PMINMAXRT", 3);
		crp.KMINMAXRT = TableFromList2(crop, "KMINMAXRT", 3);
	}
	
//soil 
	sol.ROOTDM = valueFromList<double>(soil, "ROOTDM"); 
	sol.WCAD = valueFromList<double>(soil, "WCAD");
	sol.WCWP = valueFromList<double>(soil, "WCWP");
	sol.WCFC = valueFromList<double>(soil, "WCFC");
	sol.WCWET = valueFromList<double>(soil, "WCWET");
	sol.WCST = valueFromList<double>(soil, "WCST");
	sol.DRATE = valueFromList<double>(soil, "DRATE");
	if (cntr.NPKmodel) {
		sol.NMINI = valueFromList<double>(soil, "NMINI");
		sol.PMINI = valueFromList<double>(soil, "PMINI");
		sol.KMINI = valueFromList<double>(soil, "KMINI");
	}
	
// weather
	wth.tmin = vectorFromDF<double>(weather, "tmin");
	wth.tmax = vectorFromDF<double>(weather, "tmax");
	wth.srad = vectorFromDF<double>(weather, "srad");
	wth.prec = vectorFromDF<double>(weather, "prec");
	wth.vapr = vectorFromDF<double>(weather, "vapr");
	wth.wind = vectorFromDF<double>(weather, "wind");
	wth.date = vectorFromDF<long>(weather, "date");
	
	LINcasModel m;
	m.crop = crp;
	m.soil = sol;
	m.control = cntr;
	m.management = mgm;
	m.weather = wth;
	m.run();

	if (m.fatalError) {
		for (size_t i = 0; i < m.messages.size(); i++) {
			Rcout << m.messages[i] << std::endl;
		}
	}

	Rcpp::List out = Rcpp::List::create(m.out.values, m.out.names);

/*
	size_t nc = m.out.names.size();
	size_t nr = m.out.values.size() / nc;
	NumericMatrix mat(nr, nc);
	
	CharacterVector cnames = wrap(m.out.names);
	colnames(mat) = cnames;

	size_t k=0;
	for (size_t i = 0; i < nr; i++) {
		for (size_t j = 0; j < nc; j++) {
			mat(i, j) = m.out.values[k];
			k++;
		}
	}

	return(mat);
*/

	return out;
}

