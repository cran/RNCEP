\name{NCEP.bind}

\alias{NCEP.bind}

\title{ Bind Two 3-D Arrays of Weather Data Along the Prime Meridian }

\description{
  This function is applied automatically by \code{\link{NCEP.gather}} whenever it is needed.  It binds the results from either side of the Prime Meridian.
}

\usage{
NCEP.bind(data.west, data.east)
}

\arguments{
  \item{data.west}{ a 3-D array of weather data, as returned by \code{ NCEP.gather }, from the West side of the Prime Meridian }
  \item{data.east}{ a 3-D array of weather data, as returned by \code{ NCEP.gather }, from the East side of the Prime Meridian }
}

\details{
  This function is applied automatically by \code{\link{NCEP.gather}} whenever it is needed.
  
  The arrays specified in \code{data.west} and \code{data.east} must have the same latitude and datetime intervals and extents.
  
  This function depends on the package \pkg{\link{abind}}
  
  The maximum longitudinal extent of the NCEP dataset is 357.5 not 360.
}

\value{
  A 3-D array with the same latitude and datetime intervals and extent as \code{data.west} and \code{data.east}.  Row names (i.e. longitudes) for data from the west of the Prime Meridian are converted from positive to negative values.
}

\references{ Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Methods in Ecology and Evolution. DOI:10.1111/j.2041-210X.2011.00138.x. }

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\examples{
\dontrun{
library(RNCEP)
## Using NCEP.gather(), query weather data from both sides of
## the Prime Meridian ##
## NCEP.bind() is applied automatically ##
wx <- NCEP.gather(variable='air', level=925,
    months.minmax=10, years.minmax=2003,
    lat.southnorth=c(50, 52.5), lon.westeast=c(-2.5, 2.5),
    reanalysis2=FALSE, return.units=TRUE)
}
}