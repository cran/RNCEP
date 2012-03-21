\name{NCEP.loxodrome}

\alias{NCEP.loxodrome}

\title{ Calculate the loxodrome angle between two points on Earth. }

\description{
  This function calculates the loxodrome angle (i.e rhumb line or constant compass heading) between two points on a sphere.  
}

\usage{
NCEP.loxodrome(lat1,lat2,lon1,lon2)
}

\arguments{
  \item{lat1}{ A numeric value giving the starting latitude in decimal degrees. }
  \item{lat2}{ A numeric value giving the ending latitude in decimal degrees. }
  \item{lon1}{ A numeric value giving the starting longitude in decimal degrees. }
  \item{lon2}{ A numeric value giving the ending longitude in decimal degrees. }
  }

\details{
 This function calculates the loxodrome angle (i.e. rhumb line or constant compass heading) between two points on a sphere.  Output is given in degrees from north.  
}

\value{
  A numeric value indicating the loxodrome angle between the two input points in degrees from north. }

\references{ \url{http://en.wikipedia.org/wiki/Rhumb_line} }

\author{ Michael U. Kemp \email{mukemp+RNCEP@gmail.com} }

\examples{
library(RNCEP)
## Using NCEP.loxodrome ##
NCEP.loxodrome(lat1=45,lat2=40,lon1=4,lon2=5)
}