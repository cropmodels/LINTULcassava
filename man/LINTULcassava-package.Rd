\name{LINTULcassava-package}

\alias{LINTULcassava-package}

\alias{LINTULcassava}

\docType{package}

\title{
\packageTitle{LINTULcassava}
}

\description{

This package imlements the LINTUL-CASSAVA crop growth simulation model that was developed by 

Ezui, K.S., P.A. Leffelaar, A.C. Franke, A. Mando & K.E. Giller (2018). Simulating drought impact and mitigation in cassava using the LINTUL model. Field Crops Research 219: 256-272. 
\href{https://doi.org/10.1016/j.fcr.2018.01.033}{doi:10.1016/j.fcr.2018.01.033}. 

The model was calibrated with data from three locations in Nigeria by

Adiele, J.G., A.G.T. Schut, R.P.M. van den Beuken, K.S. Ezui, P. Pypers, A.O. Ano, C.N. Egesi & K.E. Giller (2021). A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high yield potential of cassava under rainfed conditions. European Journal of Agronomy 124:126242.  \href{https://doi.org/10.1016/j.eja.2021.126242}{doi:10.1016/j.eja.2021.126242}. 

The orginal *R* implementation by Rob van den Beuken was a translation from the FST implementation of the model developed by Guillaume Ezui under supervision of Peter Leffelaar. Joy Adiele calibrated the model for Nigerian conditions using cultivar 'TME 419'. Tom Schut checked, simplified and adapted the code which is available as R scripts \href{https://models.pps.wur.nl/index.php/lintulcassavanpk}{here} (2024-10-21).

Robert Hijmans used the R scripts to create this R package. He tweaked the interface to make it easier to use, and he added an alternative R implementation of the model, as well as a C++ implementation that is more than 1000 times faster than the original R implementation.
}
