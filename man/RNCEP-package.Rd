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
         \item Temporally aggregate the data and apply any user defined function to the subsets
         \item Create a contour map from the data
		 \item Query data from these two NCEP datasets interpolated to a particular point in time and space
		 \item Visualize these interpolated data as points on a map using color to represent the interpolated value
		 }
}

\details{
\tabular{ll}{
Package: \tab RNCEP\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2011-01-01\cr
License: \tab GPL (>=2) \cr
LazyLoad: \tab yes\cr
}
}

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\references{ Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Ecography submitted. }

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
