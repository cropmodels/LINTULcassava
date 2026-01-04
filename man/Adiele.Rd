\name{Adiele}

\alias{Adiele_soil}
\alias{Adiele_weather}

\title{Get example soil and weather data}

\description{
Get example weather data and soil parameters (and also two management parameters) from the Adiele et al study.
}

\usage{
Adiele_weather(site, year)
Adiele_soil(site, year)
}

\arguments{
  \item{site}{character. Site name}
  \item{year}{integer. Year}
}

\value{
data.frame with weather data or list with soil parameters
}

\references{
Adiele, J.G., A.G.T. Schut, K.S. Ezui, P. Pypers and K.E. Giller (2021). A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high yield potential of cassava under rainfed conditions. European Journal of Agronomy 124: 126242.
}


\examples{
w <- Adiele_weather("Benin", 2016)
head(w)

s <- Adiele_soil("Edo", 2017)
str(s)
}
