\name{Adiele}

\alias{Adiele}

\title{Example data}

\description{
Get example weather data, and soil, management and model control parameters from the Adiele et al study.
}

\usage{
Adiele(site, year)
}

\arguments{
  \item{site}{character. Site name. One of "Edo", "Cross River", or "Benue"}
  \item{year}{integer. Year. Either 2016 or 2017}
}

\value{
list
}

\references{
Adiele, J.G., A.G.T. Schut, K.S. Ezui, P. Pypers and K.E. Giller (2021). A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high yield potential of cassava under rainfed conditions. European Journal of Agronomy 124: 126242.
}


\examples{
w <- Adiele("Edo", 2016)
str(w)
}
