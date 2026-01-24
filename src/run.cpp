#include <algorithm>
#include "LINTcas.h"

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

