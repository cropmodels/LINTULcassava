#include <algorithm>
#include "LINTcas.h"


void LINcasModel::initialize(long maxdur) {

	S.ROOTD = crop.ROOTDI; 
	S.WA = 1000 * crop.ROOTDI * soil.WCFC; // should be separate parameter
	S.WCUTTING = crop.WCUTTINGUNIT * crop.NCUTTINGS; 

	if (control.NPKmodel) {
		soil.RTNMINS = (1/0.9) * soil.NMINI / season_length;
		soil.RTPMINS = (1/0.9) * soil.PMINI / season_length;
		soil.RTKMINS = (1/0.9) * soil.KMINI / season_length;
		
		// LINTUL2-CASSAVA_NPK: 27 states
		S.NCUTTING = 0.015 * crop.WCUTTINGUNIT * crop.NCUTTINGS; // g N m-2
		S.PCUTTING = 0.0015 * crop.WCUTTINGUNIT * crop.NCUTTINGS; // g P m-2
		S.KCUTTING = 0.010 * crop.WCUTTINGUNIT * crop.NCUTTINGS; // g K m-2            

		S.NMINT = 0.25 * soil.NMINI;  // g N m-2: Available nitrogen amount in the soil for crop uptake
		S.PMINT = 0.25 * soil.PMINI;  // g P m-2: Available phosphorus amount in the soil for crop uptake
		S.KMINT = 0.25 * soil.KMINI;  // g K m-2: Available potassium amount in the soil for crop uptake
		S.NMINS = 0.75 * soil.NMINI;  // g N m-2: Available organic nitrogen in the soil
		S.PMINS = 0.75 * soil.PMINI;  // g P m-2: Available organic phosphorus in the soil
		S.KMINS = 0.75 * soil.KMINI;  // g K m-2: Available organic potassium in the soil
		
		if (control.outvars == "batch") {
			out.names = {"step", "WSO"};
		} else {
			out.names = {"step", "ROOTD", "WA", "TSUM", "TSUMCROP", "TSUMCROPLEAFAGE", "DORMTSUM", "PUSHDORMRECTSUM", "PUSHREDISTENDTSUM", "DORMTIME", "WCUTTING", "TRAIN", "PAR", "LAI", "WLVD", "WLV", "WST", "WSO", "WRT", "WLVG", "TRAN", "EVAP", "PTRAN", "PEVAP", "RUNOFF", "NINTC", "DRAIN", "REDISTLVG", "REDISTSO", "PUSHREDISTSUM", "WSOFASTRANSLSO", "IRRIG", "NCUTTING", "PCUTTING", "KCUTTING", "ANLVG", "ANLVD", "ANST", "ANRT", "ANSO", "APLVG", "APLVD", "APST", "APRT", "APSO", "AKLVG", "AKLVD", "AKST", "AKRT", "AKSO", "NMINT", "PMINT", "KMINT", "NMINS", "PMINS", "KMINS", "NMINF", "PMINF", "KMINF"};
			if (control.outvars == "full") {
				out.names.insert(out.names.end(), {"RROOTD", "RWA", "RTSUM", "RTSUMCROP", "RTSUMCROPLEAFAGE", "RDORMTSUM", "RPUSHDORMRECTSUM", "RPUSHREDISTENDTSUM", "RDORMTIME", "RWCUTTING", "RTRAIN", "RPAR", "RLAI", "RWLVD", "RWLV", "RWST", "RWSO", "RWRT", "RWLVG", "RTRAN", "REVAP", "RPTRAN", "RPEVAP", "RRUNOFF", "RNINTC", "RDRAIN", "RREDISTLVG", "RREDISTSO", "RPUSHREDISTSUM", "RWSOFASTRANSLSO", "RIRRIG", "RNCUTTING", "RPCUTTING", "RKCUTTING", "RANLVG", "RANLVD", "RANST", "RANRT", "RANSO", "RAPLVG", "RAPLVD", "RAPST", "RAPRT", "RAPSO", "RAKLVG", "RAKLVD", "RAKST", "RAKRT", "RAKSO", 
				"RNMINT", "RPMINT", "RKMINT", "RNMINS", "RPMINS", "RKMINS", "RNMINF", "RPMINF", "RKMINF"});
			}
		}
	} else {
		if (control.outvars == "batch") {
			out.names = {"step", "WSO"};
		} else {
			out.names = {"step", "ROOTD", "WA", "TSUM", "TSUMCROP", "TSUMCROPLEAFAGE", "DORMTSUM", "PUSHDORMRECTSUM", "PUSHREDISTENDTSUM", "DORMTIME", "WCUTTING", "TRAIN", "PAR", "LAI", "WLVD", "WLV", "WST", "WSO", "WRT", "WLVG", "TRAN", "EVAP", "PTRAN", "PEVAP", "RUNOFF", "NINTC", "DRAIN", "REDISTLVG", "REDISTSO", "PUSHREDISTSUM", "WSOFASTRANSLSO", "IRRIG"};
			if (control.outvars == "full") {
				out.names.insert(out.names.end(), {"RROOTD", "RWA", "RTSUM", "RTSUMCROP", "RTSUMCROPLEAFAGE", "RDORMTSUM", "RPUSHDORMRECTSUM", "RPUSHREDISTENDTSUM", "RDORMTIME", "RWCUTTING", "RTRAIN", "RPAR", "RLAI", "RWLVD", "RWLV", "RWST", "RWSO", "RWRT", "RWLVG", "RTRAN", "REVAP", "RPTRAN", "RPEVAP", "RRUNOFF", "RNINTC", "RDRAIN", "RREDISTLVG", "RREDISTSO", "RPUSHREDISTSUM", "RWSOFASTRANSLSO", "RIRRIG"} );
			}
		}
	}
	out.values.reserve(maxdur * out.names.size());
}


bool LINcasModel::weather_step() {
	A.date = weather.date[time];

	A.SRAD = weather.srad[time] / 1000.;
	A.WIND = weather.wind[time];
	A.VAPR = weather.vapr[time];
	A.PREC = weather.prec[time];

	double SatVP_TMMN = SatVP(weather.tmin[time]);
	double SatVP_TMMX = SatVP(weather.tmax[time]);
  // vapour pressure deficits;
	A.VPD_MN = std::max(0., SatVP_TMMN - A.VAPR);
	A.VPD_MX = std::max(0., SatVP_TMMX - A.VAPR);
	A.TAVG = 0.5 * (weather.tmin[time] + weather.tmax[time]);   // Deg. C     :     daily average temperature

	return true;
}

void LINcasModel::run() {

	if (control.modelstart < weather.date[0]) {
		std::string m = "model cannot start before beginning of the weather data";
	    messages.push_back(m);
	    fatalError = true;
		return;
	} else if (control.modelstart > weather.date[weather.date.size()-1]) {
		std::string m = "model cannot start after the end of the weather data";
	    messages.push_back(m);
	    fatalError = true;
		return;
	} else {
// get start time relative to weather data
		auto it = std::find(weather.date.begin(), weather.date.end(), control.modelstart);
		if (it != weather.date.end()) {
			time = std::distance(weather.date.begin(), it);
		} else {
			messages.push_back("startdate not found in weather file");
		}		
	}

	if (control.modelstart > management.PLDATE) {
		messages.push_back("model cannot start after the planting date");
	    fatalError = true;
		return;		
	}		
	if (management.PLDATE >= management.HVDATE) {
		messages.push_back("harvest date must be after the planting date");
	    fatalError = true;
		return;
	}

	unsigned maxdur = management.HVDATE - control.modelstart + 1;
	if (weather.date.size() < (time + maxdur)) {
		messages.push_back("harvest date beyond the end of weather data");
	    fatalError = true;
		return;		
	}
	
	season_length = management.HVDATE - management.PLDATE;	
	initialize(maxdur);
	
	step = 1;	
	if (control.NPKmodel) {
		while (step <= maxdur) {
			if (!weather_step()) break;
			ratesNPK();
			outputNPK();
			statesNPK();
			if (S.TSUM >= crop.FINTSUM) break;
			time++;
			step++;
			if (fatalError) return;
		}		
	} else {
		while (step <= maxdur) {
			if (!weather_step()) break;
			rates();
			output();
			states();
			if (S.TSUM >= crop.FINTSUM) break;
			time++;
			step++;
			if (fatalError) return;
		}
	}
	
	if (control.outvars == "batch") {
		out.values = {double(step), S.WSO};		
	}
}

