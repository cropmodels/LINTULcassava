/*
Author Robert Hijmans
Date: January 2026
License: EUPL
*/

#include <Rcpp.h>
#include "LINTcas.h"

void setWeather(LINcasModel* m, Rcpp::NumericVector date, Rcpp::NumericVector tmin, Rcpp::NumericVector tmax, Rcpp::NumericVector srad, Rcpp::NumericVector prec, Rcpp::NumericVector wind, Rcpp::NumericVector vapr) {
	LINcasWeather wth;
	wth.tmin = Rcpp::as<std::vector<double>>(tmin);
	wth.tmax = Rcpp::as<std::vector<double>>(tmax);
	wth.srad = Rcpp::as<std::vector<double>>(srad);
	wth.wind = Rcpp::as<std::vector<double>>(wind);
	wth.vapr = Rcpp::as<std::vector<double>>(vapr);
	wth.prec = Rcpp::as<std::vector<double>>(prec);
	wth.date = Rcpp::as<std::vector<long>>(date);
	m->weather = wth;
}

RCPP_EXPOSED_CLASS(LINcasWeather)
RCPP_EXPOSED_CLASS(LINcasCropParameters)
RCPP_EXPOSED_CLASS(LINcasSoilParameters)
RCPP_EXPOSED_CLASS(LINcasManagement)
RCPP_EXPOSED_CLASS(LINcasControl)
RCPP_EXPOSED_CLASS(LINcasModel)
RCPP_EXPOSED_CLASS(LINcasOutput)

RCPP_MODULE(LINcas){
    using namespace Rcpp;

    class_<LINcasControl>("LINcasControl")
		.field("modelstart", &LINcasControl::modelstart) 
		.field("water_limited", &LINcasControl::water_limited)
		.field("nutrient_limited", &LINcasControl::nutrient_limited)
		.field("outvars",  &LINcasControl::outvars)
	;

    class_<LINcasManagement>("LINcasManagement")
		.field("PLDATE", &LINcasManagement::PLDATE) 
		.field("HVDATE", &LINcasManagement::HVDATE)
	;

    class_<LINcasWeather>("LINcasWeather")
		.constructor()
		.field("date", &LINcasWeather::date) 
		.field("srad", &LINcasWeather::srad) 
		.field("tmin", &LINcasWeather::tmin) 
		.field("tmax", &LINcasWeather::tmax) 
		.field("prec", &LINcasWeather::prec) 
		.field("wind", &LINcasWeather::wind) 
		.field("vapr", &LINcasWeather::vapr) 
	;
	
	class_<LINcasCropParameters>("LINcasCropParameters")
		.field("TWCSD", &LINcasCropParameters::TWCSD)
		.field("FRACRNINTC", &LINcasCropParameters::FRACRNINTC)
		.field("RECOV", &LINcasCropParameters::RECOV)
		.field("TRANCO", &LINcasCropParameters::TRANCO)
		.field("WCUTTINGUNIT", &LINcasCropParameters::WCUTTINGUNIT)
		.field("NCUTTINGS", &LINcasCropParameters::NCUTTINGS)
		.field("WCUTTINGIP", &LINcasCropParameters::WCUTTINGIP)
		.field("ROOTDI", &LINcasCropParameters::ROOTDI)
		.field("SLAI", &LINcasCropParameters::SLAI)
		.field("WLVI", &LINcasCropParameters::WLVI)
		.field("LAII", &LINcasCropParameters::LAII)
		.field("WCUTTINGMINPRO", &LINcasCropParameters::WCUTTINGMINPRO)
		.field("FST_CUTT", &LINcasCropParameters::FST_CUTT)
		.field("FRT_CUTT", &LINcasCropParameters::FRT_CUTT)
		.field("FLV_CUTT", &LINcasCropParameters::FLV_CUTT)
		.field("FSO_CUTT", &LINcasCropParameters::FSO_CUTT)
		.field("RDRWCUTTING", &LINcasCropParameters::RDRWCUTTING)
		.field("FPAR", &LINcasCropParameters::FPAR)
		.field("K_EXT", &LINcasCropParameters::K_EXT)
		.field("LUE_OPT", &LINcasCropParameters::LUE_OPT)
		.field("RRDMAX", &LINcasCropParameters::RRDMAX)
		.field("RDRB", &LINcasCropParameters::RDRB)
		.field("LAICR", &LINcasCropParameters::LAICR)
		.field("RDRSHM", &LINcasCropParameters::RDRSHM)
		.field("FRACTLLFENHSH", &LINcasCropParameters::FRACTLLFENHSH)
		.field("FASTRANSLSO", &LINcasCropParameters::FASTRANSLSO)
		.field("SLA_MAX", &LINcasCropParameters::SLA_MAX)
		.field("RGRL", &LINcasCropParameters::RGRL)
		.field("LAIEXPOEND", &LINcasCropParameters::LAIEXPOEND)
		.field("TBASE", &LINcasCropParameters::TBASE)
		.field("OPTEMERGTSUM", &LINcasCropParameters::OPTEMERGTSUM)
		.field("TSUMLA_MIN", &LINcasCropParameters::TSUMLA_MIN)
		.field("TSUMSBR", &LINcasCropParameters::TSUMSBR)
		.field("TSUMLLIFE", &LINcasCropParameters::TSUMLLIFE)
		.field("TSUMREDISTMAX", &LINcasCropParameters::TSUMREDISTMAX)
		.field("FINTSUM", &LINcasCropParameters::FINTSUM)
		.field("LAI_MIN", &LINcasCropParameters::LAI_MIN)
		.field("WSOREDISTFRACMAX", &LINcasCropParameters::WSOREDISTFRACMAX)
		.field("WLVGNEWN", &LINcasCropParameters::WLVGNEWN)
		.field("SO2LV", &LINcasCropParameters::SO2LV)
		.field("RRREDISTSO", &LINcasCropParameters::RRREDISTSO)
		.field("DELREDIST", &LINcasCropParameters::DELREDIST)
		.field("FRACSLATB", &LINcasCropParameters::FRACSLATB)
		.field("RDRT", &LINcasCropParameters::RDRT)
		.field("TTB", &LINcasCropParameters::TTB)
		.field("FLVTB", &LINcasCropParameters::FLVTB)
		.field("FSTTB", &LINcasCropParameters::FSTTB)
		.field("FSOTB", &LINcasCropParameters::FSOTB)
		.field("FRTTB", &LINcasCropParameters::FRTTB)
		.field("SLAII", &LINcasCropParameters::SLAII)
	;

    class_<LINcasSoilParameters>("LINcasSoilParameters")
		.field("ROOTDM", &LINcasSoilParameters::ROOTDM)
		.field("WCAD", &LINcasSoilParameters::WCAD)
		.field("WCWP", &LINcasSoilParameters::WCWP)
		.field("WCFC", &LINcasSoilParameters::WCFC)
		.field("WCWET", &LINcasSoilParameters::WCWET)
		.field("WCST", &LINcasSoilParameters::WCST)
		.field("DRATE", &LINcasSoilParameters::DRATE)	
	;
	
    class_<LINcasOutput>("LINcasOutput")
		.field("names", &LINcasOutput::names, "names")
		.field("values", &LINcasOutput::values, "values")
	;

    class_<LINcasModel>("LINcasModel")
		.constructor()
		.method("run", &LINcasModel::run, "run the model")		
//		.method("run_batch", &LINcasModel::run_batch, "run the model")		
		.field("control", &LINcasModel::control, "control")
		.field("weather", &LINcasModel::weather, "weather")
		.field("output", &LINcasModel::out, "output")
		.field("messages", &LINcasModel::messages, "messages")
//		.field("fatalError", &LINcasModel::fatalError, "fatalError")
	;			

};

