\name{LC_run}

\alias{LC_run}

\title{run the model}

\description{
Run the model
}

\usage{
LC_run(weather, crop, soil, management, control)
}

\arguments{
  \item{weather}{data.frame with weather data}
  \item{crop}{list with crop parameters}
  \item{soil}{list with soil parameters}
  \item{management}{list with management parameters (year, starttime, endtime, IRRIGF)}
  \item{control}{list with model control parameters (timestep)}
}

\value{
data.frame
}

\examples{

crop <- LC_crop("Adiele")
p <- Adiele("Edo", 2016)

pot <- LC_run(p$weather, crop, p$soil, c(p$management, IRRIGF=TRUE), p$control)
wlim <- LC_run(p$weather, crop, p$soil, c(p$management, IRRIGF=FALSE), p$control)
}
