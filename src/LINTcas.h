/*
Author: Robert Hijmans
2026
License: GNU General Public License (GNU GPL) v. 2
*/

#include <vector>
#include <cmath>
#include <string>

class LINcasWeather {
public:
	virtual ~LINcasWeather(){}
	std::vector<long> date;
	std::vector<double> srad, tmin, tmax, prec, wind, vapr;
};

class LINcasAtmosphere {
public:
	virtual ~LINcasAtmosphere(){}
	long date;
	double TAVG, PREC, VPD_MN, VPD_MX, SRAD, VAPR, WIND; 
};


class LINcasControl {
public:
	virtual ~LINcasControl(){}
	double DELT=1;
	long modelstart;
	bool IRRIGF=false;	
	std::string outvars;
};


class LINcasCropParameters {
public:
	virtual ~LINcasCropParameters(){}	
	double TWCSD, FRACRNINTC, RECOV, TRANCO, WCUTTINGUNIT, NCUTTINGS, WCUTTINGIP, ROOTDI, SLAI, WLVI, LAII, WCUTTINGMINPRO, FST_CUTT, FRT_CUTT, FLV_CUTT, FSO_CUTT, RDRWCUTTING, FPAR, K_EXT, LUE_OPT, RRDMAX, RDRB, LAICR, RDRSHM, FRACTLLFENHSH, FASTRANSLSO, SLA_MAX, RGRL, LAIEXPOEND, TBASE, OPTEMERGTSUM, TSUMLA_MIN, TSUMSBR, TSUMLLIFE, TSUMREDISTMAX, FINTSUM, LAI_MIN, WSOREDISTFRACMAX, WLVGNEWN, SO2LV, RRREDISTSO, DELREDIST, SLAII;
	std::vector<std::vector<double>> FRACSLATB, RDRT, TTB, FLVTB, FSTTB, FSOTB, FRTTB;
} ;


class LINcasRates {
public:
	double ROOTD=0; // m d-1
	double WA=0;   // mm d-1  
	double TSUM=0; double TSUMCROP=0; // Deg. C d-1
	double TSUMCROPLEAFAGE=0; double DORMTSUM=0;// Deg. C d-1
	double PUSHDORMRECTSUM=0; double PUSHREDISTENDTSUM=0; // Deg. C d-1
	double DORMTIME=0; // d d-1
	double WCUTTING=0; // g DM m-2 d-1
	//double TRAIN=0;// mm d-1
	double PAR=0;// MJ m-2 d-1
	double LAI=0;// m2 m-2 d-1
	double WLVD=0; double WLV=0; double WST=0; double WSO=0; double WRT=0; double WLVG=0; // g DM m-2 d-1
	double TRAN=0; double EVAP=0; double PTRAN=0; double PEVAP=0; 
	double RUNOFF=0; double NINTC=0; double DRAIN=0; // mm d-1
	double REDISTLVG=0; double REDISTSO=0;// g DM m-2 d-1
	double PUSHREDISTSUM=0;// Deg. C d-1
	double WSOFASTRANSLSO=0; // g DM m-2 d-1
	double IRRIG=0;
};

class LINcasStates {
public:
	double ROOTD=0; // m d-1
	double WA=0;   // mm d-1  
	double TSUM=0; double TSUMCROP=0; // Deg. C d-1
	double TSUMCROPLEAFAGE=0; double DORMTSUM=0;// Deg. C d-1
	double PUSHDORMRECTSUM=0; double PUSHREDISTENDTSUM=0; // Deg. C d-1
	double DORMTIME=0; // d d-1
	double WCUTTING=0; // g DM m-2 d-1
	//double TRAIN=0;// mm d-1
	double PAR=0;// MJ m-2 d-1
	double LAI=0;// m2 m-2 d-1
	double WLVD=0; double WLV=0; double WST=0; double WSO=0; double WRT=0; double WLVG=0; // g DM m-2 d-1
	double TRAN=0; double EVAP=0; double PTRAN=0; double PEVAP=0; 
	double RUNOFF=0; double NINTC=0; double DRAIN=0; // mm d-1
	double REDISTLVG=0; double REDISTSO=0;// g DM m-2 d-1
	double PUSHREDISTSUM=0;// Deg. C d-1
	double WSOFASTRANSLSO=0; // g DM m-2 d-1
	double IRRIG=0;
};


class LINcasSoilParameters {
public:
	virtual ~LINcasSoilParameters(){}
	double ROOTDM, WCAD, WCWP, WCFC, WCWET, WCST, DRATE;
};


class LINcasOutput {
public:
	virtual ~LINcasOutput(){}
	std::vector<std::string> names;
	std::vector<double> values;
};


class LINcasManagement {
public:
	virtual ~LINcasManagement(){}
	long PLDATE, HVDATE;
};


class LINcasModel {
public:
	virtual ~LINcasModel(){}

	unsigned step, time;

	std::vector<std::string> messages;
	bool fatalError=false;

	LINcasWeather weather;
	LINcasAtmosphere A;
	LINcasRates R;
	LINcasStates S;
	LINcasSoilParameters soil;
	LINcasCropParameters crop;
	LINcasManagement management;
	LINcasControl control;

	LINcasOutput out;
	
	bool weather_step();
	void rates();
	void states();
	void output();
	void initialize(long int maxdur);
	void run();

	void Penman();
	void evaptr();
	void GLAI();
	void drunir();
};


