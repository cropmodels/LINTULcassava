\name{LC_run}

\alias{LC_run}

\title{run the model}

\description{
Run the model
}

\usage{
LC_run(weather, crop, soil, management)
}

\arguments{
  \item{weather}{data.frame with weather data}
  \item{crop}{list with crop parameters}
  \item{soil}{list with soil parameters}
  \item{management}{list with management parameters (year, starttime, endtime, IRRIGF)}
}

\value{
data.frame
}

\examples{
crop <- LC_crop("Adiele")
year <- 2016
wth <- Adiele_weather("Benin", year)
soil <- Adiele_soil("Edo", year)
mng <-  list(year=year, starttime=45, endtime=582)
  
pot <- LC_run(wth, crop, soil, c(mng, IRRIGF=TRUE))
wlim <- LC_run(wth, crop, soil, c(mng, IRRIGF=FALSE))
}
