\name{LC_crop}

\alias{LC_crop}

\title{Get crop parameters}

\description{
Get crop parameters from by Ezui et al. or from by Adiele et al.
}

\usage{
LC_crop(x)
}

\arguments{
  \item{x}{character. Either "Adiele" or "Ezui"}
}

\references{
Adiele, J.G., A.G.T. Schut, K.S. Ezui, P. Pypers and K.E. Giller (2021). A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high yield potential of cassava under rainfed conditions. European Journal of Agronomy 124: 126242.

Ezui, K.S., P.A. Leffelaar, A.C. Franke, A. Mando, K.E. Giller (2018). Simulating drought impact and mitigation in cassava using the LINTUL model. Field Crops Research 219: 256-272.
}


\value{
list
}

\examples{
crop <- LC_crop("Adiele")
str(crop)
}
