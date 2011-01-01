\name{NCEP.restrict}

\alias{NCEP.restrict}

\title{ Temporally Filters Weather Data }

\description{
This function removes unwanted datetime intervals (i.e. layers) of weather data from the NCEP/NCAR Reanalysis or NCEP/DOE Reanalysis II data array as returned by \code{\link{NCEP.gather}}.  The spatial structure of the data is retained.
}
\usage{
NCEP.restrict(wx.data, years2remove = NULL, months2remove = NULL,
    days2remove = NULL, hours2remove = NULL, other2remove = NULL,
    set2na = TRUE)
}

\arguments{
  \item{wx.data}{ A 3-D weather dataset as returned by \code{\link{NCEP.gather}} }
  \item{years2remove}{ Numeric.  Specifies which years should be removed from the dataset. }
  \item{months2remove}{ Numeric.  Specifies which months should be removed from the dataset. }
  \item{days2remove}{ Numeric.  Specifies which days of the month should be removed from the dataset. }
  \item{hours2remove}{ Numeric.  Specifies which hours of the day should be removed from the dataset. }
  \item{other2remove}{ Character.  Specifies any specific combinations of year, month, day, and hour to remove from the dataset. }
  \item{set2na}{ Logical.  Should the data matching the year, month, day, or hour specified in the function call be set to \code{NA} (default) or should they be removed completely from the dataset. }
}

\details{
	\code{other2remove} is for specific combinations of year, month, day, and hour and must be given in the format \code{"\%Y-\%m-\%d \%H"}.  
	If \code{\link{NCEP.aggregate}} has been applied, some datetime components will need to be replaced with \code{"XX"} or \code{"XXXX"} when specifying \code{other2remove}.  
	Use \code{\link{dimnames}} to determine how to specify an aggregated datetime.
	
	If \code{set2na} is anything other than \code{TRUE} or \code{FALSE}, the function replaces items to be removed with the value of \code{set2na}.
}

\value{
  This function returns a three dimensional array (or a 2-D matrix if all but a single timestep is removed) of weather data.  The three dimensions are latitude, longitude, and datetime reflected in the dimnames of the output array.
}

\references{ Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Ecography submitted. }

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl}}

\examples{
library(RNCEP)
## First query the U component of the wind from the 850mb
## pressure level
uwnd <- NCEP.gather(variable='uwnd', level=850,
    months.minmax=c(5,7), years.minmax=c(2000,2001),
    lat.southnorth=c(50,55), lon.westeast=c(0,5))

## Then remove all observations except those made at midnight from
## the first half of either May or July
uwnd.r <- NCEP.restrict(wx.data=uwnd, hours2remove=c(6,12,18),
    days2remove=seq(17,31), months2remove=6, set2na=FALSE)

## Then remove the observation from 1 May 2000 at midnight ##
uwnd.r2 <- NCEP.restrict(wx.data=uwnd.r,
    other2remove="2000-05-01 00", set2na=FALSE)
}