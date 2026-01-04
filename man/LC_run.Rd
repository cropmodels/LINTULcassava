\name{LC_run}

\alias{LC_run}

\title{run the model}

\description{
Run the model
}

\usage{
LC_run(wdata, pars, year, starttime, endtime)
}

\arguments{
  \item{wdata}{data.frame with weather data}
  \item{pars}{list with crop, soil, and management parameters}
  \item{year}{start year}
  \item{starttime}{start DOY}
  \item{endtime}{end DOY}
}

\value{
data.frame
}

\examples{
crop <- LC_crop("Adiele")
year <- 2016
wth <- Adiele_weather("Benin", year)
soil <- Adiele_soil("Edo", year)
  
pot <- LC_run(wth, pars=c(soil, crop, IRRIGF=TRUE), year=year, starttime=45, endtime=582)
wlim <- LC_run(wth, pars=c(soil, crop, IRRIGF=FALSE), year=year, starttime=45, endtime=582)
}
