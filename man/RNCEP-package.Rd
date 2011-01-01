\name{RNCEP-package}

\alias{RNCEP-package}

\alias{RNCEP}

\docType{package}

\title{
This package of functions retrieves, organizes, and visualizes weather data from either the NCEP/NCAR Reanalysis or NCEP/DOE Reanalysis II datasets
}

\description{
This package contains functions to...

\enumerate{
         \item Query data from these two NCEP datasets for a specified range of space and time, maintaining both the spatial and temporal structure of the data
         \item Remove any unwanted time intervals of the returned data
         \item Temporally aggregate the data and apply any function to the subsets (i.e. calculate user-defined climatic variables)
         \item Create a contour map from the data
		 \item Query data from these two NCEP datasets interpolated to a particular point in time and space
		 \item Visualize these interpolated data as points on a map using color to represent the interpolated value
		 }
}

\details{
\tabular{ll}{
Package: \tab RNCEP\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2011-01-01\cr
License: \tab GPL (>=2) \cr
LazyLoad: \tab yes\cr
}
}

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\references{ \bold{To cite package 'RNCEP' in publications use:}

  Kemp, M.U., van Loon, E.E., Shamoun-Baranes, J.,and Bouten, W.
  (2011). RNCEP: global weather and climate data at your fingertips.
  Methods in Ecology and Evolution, submitted.

\bold{To cite the NCEP/NCAR Reanalysis dataset use:}

  Kalnay et al. (1996), The NCEP/NCAR 40-year reanalysis project, Bull.
  Amer. Meteor. Soc., 77, 437-470

\bold{To cite the NCEP/DOE Reanalysis II dataset use:}

  Kanamitsu et al. (2002), NCEP-DOE AMIP-II Reanalysis (R-2). Bull.
  Amer. Meteor. Soc., 83, 1631-1643


Please acknowledge the use of NCEP data in any publications by
including text such as, \dQuote{NCEP Reanalysis data provided by the
NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at
\url{http://www.esrl.noaa.gov/psd/}}. They would also appreciate a copy of any publications using NCEP data. 
}

\keyword{ package }

%% \seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%% }

\examples{
library(RNCEP)
## Retrieve the temperature from a particular pressure level for
## a specified spatial and temporal extent
wx.extent <- NCEP.gather(variable='air', level=850,
    months.minmax=c(8,9), years.minmax=c(2000,2001),
    lat.southnorth=c(50,55), lon.westeast=c(0,5),
    reanalysis2 = FALSE, return.units = TRUE)

## Retrive the temperature from a particular pressure level
## interpolated in space and time
wx.interp <- NCEP.interp(variable='air', level=850, lat=55.1,
    lon=11.3, dt='2006-10-12 17:23:12')
}
