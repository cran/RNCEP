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
         
         
         \item Perform trajectory simulations according to specified behavior using wind data from NCEP or data specified by the user.
		 }
}

\details{
\tabular{ll}{
Package: \tab RNCEP\cr
Type: \tab Package\cr
Version: \tab 1.0.4\cr
Date: \tab 2012-04-05\cr
License: \tab GPL (>=2) \cr
LazyLoad: \tab yes\cr
}
}

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\references{ \bold{To cite package 'RNCEP' in publications use:}

Kemp, M. U., Emiel van Loon, E., Shamoun-Baranes, J., Bouten, W., 2012. RNCEP: global weather and climate data at your fingertips. Methods in Ecology and Evolution (3), 65-70., DOI: 10.1111/j.2041-210X.2011.00138.x.
  
\bold{For more information on flow-assistance and NCEP.flight see:}

Kemp, M.U., Shamoun-Baranes, J., van Loon, E. E., and Bouten, W. 2012. Quantifying flow-assistance and implications for movement research. -- Journal of Theoretical Biology. In prep.

\bold{To cite the NCEP/NCAR Reanalysis dataset use:}

Kalnay et al. (1996), The NCEP/NCAR 40-year reanalysis project, Bull. Amer. Meteor. Soc., 77, 437-470

\bold{To cite the NCEP/DOE Reanalysis II dataset use:}

Kanamitsu et al. (2002), NCEP-DOE AMIP-II Reanalysis (R-2). Bull. Amer. Meteor. Soc., 83, 1631-1643


Please acknowledge the use of NCEP data in any publications by including text such as, \dQuote{NCEP Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at \url{http://www.esrl.noaa.gov/psd/}}. They would also appreciate a copy of any publications using NCEP data. 
}

\keyword{ package }

%% \seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%% }

\examples{
\dontrun{
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

## Simulate a flight trajectory using NCEP wind data
flight <- NCEP.flight(beg.loc=c(58.00,7.00), 
  end.loc=c(53.00,7.00), begin.dt='2007-10-01 18:00:00',
  flow.assist='NCEP.Tailwind', fa.args=list(airspeed=12),
  path='loxodrome', calibrate.dir=FALSE, calibrate.alt=FALSE,
  cutoff=0, when2stop='latitude', levels2consider=c(850,925),
  hours=12, evaluation.interval=60, id=1, land.if.bad=FALSE,
  reanalysis2 = FALSE, query=TRUE)
}
}
