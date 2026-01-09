\name{LINTCAS}

\alias{LINTCAS}

\title{Run the LINTCAS model}

\description{
Run the LINTCAS model
}

\usage{
LINTCAS(weather, crop, soil, management, control, level=3)
}

\arguments{
  \item{weather}{data.frame with weather data}
  \item{crop}{list with crop parameters}
  \item{soil}{list with soil parameters}
  \item{management}{list with management parameters (PLDATE, HVDATE)}
  \item{control}{list with model control parameters (starttime, timestep, IRRIGF)}
  \item{level}{1, 2, or 3. With 1 you get the original R implementation; 2 is a modified R implementation; and 3 is the C++ implementation). The results should be exactly the same. Level 2 is about 3 times faster than level 1, and level 3 is > 1000 times faster than level 1}
}

\value{
data.frame
}

\examples{

# get example parameters and environmental data
crop <- LC_crop("Adiele")
p <- Adiele("Edo", 2016)

pot <- LINTCAS(p$weather, crop, p$soil, p$management, c(p$control, IRRIGF=TRUE))
wlm <- LINTCAS(p$weather, crop, p$soil, p$management, c(p$control, IRRIGF=FALSE))
tail(wlm)
}
