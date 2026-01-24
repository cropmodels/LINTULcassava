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
	bool NPKmodel=false;
	long modelstart;
	bool water_limited=false;	
	bool nutrient_limited=false;	
	double WCI; // not yet used
	std::string outvars;
};


class LINcasCropParameters {
public:
	virtual ~LINcasCropParameters(){}	
	double TWCSD, FRACRNINTC, RECOV, TRANCO, WCUTTINGUNIT, NCUTTINGS, WCUTTINGIP, ROOTDI, SLAI, WLVI, LAII, WCUTTINGMINPRO, FST_CUTT, FRT_CUTT, FLV_CUTT, FSO_CUTT, RDRWCUTTING, FPAR, K_EXT, LUE_OPT, RRDMAX, RDRB, LAICR, RDRSHM, FRACTLLFENHSH, FASTRANSLSO, SLA_MAX, RGRL, LAIEXPOEND, TBASE, OPTEMERGTSUM, TSUMLA_MIN, TSUMSBR, TSUMLLIFE, TSUMREDISTMAX, FINTSUM, LAI_MIN, WSOREDISTFRACMAX, WLVGNEWN, SO2LV, RRREDISTSO, DELREDIST, SLAII;
	std::vector<std::vector<double>> FRACSLATB, RDRT, TTB, FLVTB, FSTTB, FSOTB, FRTTB;

// nutrients	
	double NLAI, RDRNS, K_MAX, K_NPK_NI, TSUM_NPKI, K_WATER, SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE, FR_MAX, N_RECOV, P_RECOV, K_RECOV, NFLVD, PFLVD, KFLVD, TCNPKT, RTNMINF, RTPMINF, RTKMINF;
	std::vector<std::vector<double>> NMINMAXLV, PMINMAXLV, KMINMAXLV, NMINMAXST, PMINMAXST, KMINMAXST, NMINMAXSO, PMINMAXSO, KMINMAXSO, NMINMAXRT, PMINMAXRT, KMINMAXRT;

} ;

class LINcasSoilParameters {
public:
	virtual ~LINcasSoilParameters(){}

	double ROOTDM, WCAD, WCWP, WCFC, WCWET, WCST, DRATE;

// nutrients	
	double NMINI=0, PMINI=0, KMINI=0, RTNMINS=0, RTPMINS=0, RTKMINS=0;
};


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

	double NCUTTING=0;
	double PCUTTING=0;
	double KCUTTING=0;
	double ANLVG=0; // g N m-2 d-1
	double ANLVD=0;
	double ANST=0;
	double ANRT=0;
	double ANSO=0;
	double APLVG=0; // g P m-2 d-1
	double APLVD=0;
	double APST=0;
	double APRT=0;
	double APSO=0;
	double AKLVG=0; // g K m-2 d-1
	double AKLVD=0;
	double AKST=0;
	double AKRT=0;
	double AKSO=0;
	double NMINT=0; // g N,P,K m-2 d-1
	double PMINT=0;
	double KMINT=0;
	double NMINS=0; // g N,P,K m-2 d-1
	double PMINS=0;
	double KMINS=0;
	double NMINF=0; // # g N,P,K m-2 d-1
	double PMINF=0;
	double KMINF=0;
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

	double NCUTTING=0;
	double PCUTTING=0;
	double KCUTTING=0;
	double ANLVG=0; // g N m-2 d-1
	double ANLVD=0;
	double ANST=0;
	double ANRT=0;
	double ANSO=0;
	double APLVG=0; // g P m-2 d-1
	double APLVD=0;
	double APST=0;
	double APRT=0;
	double APSO=0;
	double AKLVG=0; // g K m-2 d-1
	double AKLVD=0;
	double AKST=0;
	double AKRT=0;
	double AKSO=0;
	double NMINT=0; // g N,P,K m-2 d-1
	double PMINT=0;
	double KMINT=0;
	double NMINS=0; // g N,P,K m-2 d-1
	double PMINS=0;
	double KMINS=0;
	double NMINF=0; // # g N,P,K m-2 d-1
	double PMINF=0;
	double KMINF=0;
	
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
	std::vector<std::vector<double>> FERTAB;
};


class LINcasModel {
public:
	virtual ~LINcasModel(){}

	unsigned step, time, season_length;

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

	void ratesNPK();
	void statesNPK();
	void outputNPK();

	void Penman();
	void evaptr();
	void GLAI();
	void drunir();


	std::vector<double> npkical(
		double NMINLV, double PMINLV, double KMINLV,
		double NMINST, double PMINST, double KMINST, 
		double NMINSO, double PMINSO, double KMINSO, 
		double NMAXLV, double PMAXLV, double KMAXLV, 
		double NMAXST, double PMAXST, double KMAXST,
		double NMAXSO, double PMAXSO, double KMAXSO);


	void nutrientdyn(bool EMERG, 
		double NMINLV, double PMINLV, double KMINLV, double NMINST, double PMINST, double KMINST, 
		double NMINSO, double PMINSO, double KMINSO, double NMINRT, double PMINRT, double KMINRT, 
		double NMAXLV, double PMAXLV, double KMAXLV, double NMAXST, double PMAXST, double KMAXST, 
		double NMAXSO, double PMAXSO, double KMAXSO, double NMAXRT, double PMAXRT, double KMAXRT, 
		double TRANRF, double NNI, double PNI, double KNI, double FLV, double FST, double FRT, double FSO, 
		bool PUSHREDIST);

};



inline double SatVP(const double &tmp) {
	return 0.611 * std::exp(17.4 * tmp / (tmp + 239)) ;
}



inline double approx(std::vector<std::vector<double>> tb, double x) {
	int n = tb[0].size();
	double y = NAN;
	if (x <= tb[0][0]) {
		y = tb[1][0];
	} else if (x >= tb[0][n-1]) {
		y = tb[1][n-1];
	} else {
		for(int i=1; i<n; i++) {
			if (tb[0][i] >= x) {
				double slope = (tb[1][i] - tb[1][i-1]) / (tb[0][i] - tb[0][i-1]);
				y = tb[1][i-1] + (x - tb[0][i-1]) * slope;
				break;
			}
		}
	}
	return(y);
}	

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


