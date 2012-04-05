\name{NCEP.interp}

\alias{NCEP.interp}
\alias{NCEP.interp.gaussian}
\alias{NCEP.interp.pressure}
\alias{NCEP.interp.surface}
\alias{robust.NCEP.interp.gaussian}
\alias{robust.NCEP.interp.pressure}
\alias{robust.NCEP.interp.surface}

\title{ Interpolates Weather Data to a point in space and time }

\description{ This function queries a weather variable via the Internet from the NCEP/NCAR Reanalysis or NCEP/DOE Reanalysis II datasets and subsequently calculates values at desired locations in space and time by interpolation. }

\usage{ NCEP.interp(variable, level, lat, lon, dt, reanalysis2 = FALSE,
    interpolate.space = TRUE, interpolate.time = TRUE,
    keep.unpacking.info = FALSE, return.units = TRUE,
    interp = 'linear', p = 1, status.bar=TRUE) 

## NCEP.interp is a wrapper function that calls one of the
## following functions based on the value of level.
## Users should avoid using these functions directly.

NCEP.interp.gaussian(variable, lat, lon, dt, reanalysis2 = FALSE,
    interpolate.space = TRUE, interpolate.time = TRUE,
    keep.unpacking.info = FALSE, return.units = TRUE,
    interp = 'linear', p = 1, status.bar=TRUE) 
NCEP.interp.pressure(variable, lat, lon, dt, pressure,
    reanalysis2 = FALSE, interpolate.space = TRUE, 
    interpolate.time = TRUE, keep.unpacking.info = FALSE, 
    return.units = TRUE, interp = 'linear', p = 1, status.bar=TRUE) 
NCEP.interp.surface(variable, lat, lon, dt, reanalysis2 = FALSE,
    interpolate.space = TRUE, interpolate.time = TRUE,
    keep.unpacking.info = FALSE, return.units = TRUE,
    interp = 'linear', p = 1, status.bar=TRUE) 
}

\arguments{
  \item{variable}{ Character. The name of the weather variable to be obtained.  See \sQuote{Variable Naming Conventions} below for possible variable names. }
  \item{level}{ A numeric pressure level or one of either \sQuote{gaussian} or \sQuote{surface}. See \sQuote{Details}. }
  \item{lat}{ Numeric.  The latitude to which the weather variable should be interpolated. }
  \item{lon}{ Numeric.  The longitude to which the weather variable should be interpolated. }
  \item{dt}{ Character.  The datetime (specified in UTC) to which the weather variable should be interpolated.  Must use the format \code{"\%Y-\%m-\%d \%H:\%M:\%S"}. }
  \item{reanalysis2}{ Logical. Should the data be obtained from the Reanalysis II dataset or from Reanalysis I(default)? }
  \item{interpolate.space}{ Logical.  Should interpolation be done in space? }
  \item{interpolate.time}{ Logical.  Should interpolation be done in time? }
  \item{keep.unpacking.info}{ Logical.  Should the information needed to unpack the data be used for all queries in the function call? }
  \item{return.units}{ Logical. Should the units of the variable being obtained be printed after the query finishes? }
  \item{interp}{ Method of interpolation.  One of 'linear' (default) or 'IDW'.  See \sQuote{Details}.  }
  \item{p}{ A positive real number. The power parameter controlling interpolation. Only matters when \code{interp} is 'IDW'. See \sQuote{Details}.  }
  \item{pressure}{ Numeric.  A pressure level in millibars that is assigned automatically from the value of \code{level} when needed. }    
  \item{status.bar}{ Logical.  Should a status bar be shown indicating the percentage of completion? }
 }

\details{
  \code{NCEP.interp} is a wrapper function that applies one of \code{NCEP.interp.gaussian},\cr \code{NCEP.interp.pressure}, or \code{NCEP.interp.surface} depending
  on the value of \code{level}.

  
  \code{level} must specify one of either \sQuote{gaussian} or \sQuote{surface} or give a numerical pressure level in millibars.  Numeric pressure levels must be one of 1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10.
  See \sQuote{Variable Naming Conventions} below to determine if your variable of interest is stored relative to the surface, a pressure level, or a T62 Gaussian grid.
  Note that variables on a T62 Gaussian grid are evenly spaced in longitude but unevenly spaced in latitude while variables from either the surface or a particular pressure level are evenly spaced in both latitude and longitude (2.5 deg. x 2.5 deg.).
  
  
  All arguments except \code{keep.unpacking.info}, \code{return.units}, and \code{status.bar} can be vectors.  The remaining arguments are recycled to the length of the longest arguement.

  
  \code{lat} and \code{lon} should be given in decimal degrees.  Latitudes south of the equator should be negative.  Longitudes west of the Prime Meridian can be specified using either positive (i.e. 350) or negative (i.e. -10) notation.

  
  All interpolation is performed assuming a spherical (rather than a planar) grid.
  
  
  When \code{interp} is 'IDW', 2-D spatial interpolation is done using inverse distance weighting followed by a 1-D linear interpolation in time.
  When \code{interp} is 'linear', the function performs a trilinear interpolation in latitude, longitude, and time.
  If \code{interpolate.space} or\cr \code{interpolate.time} is \code{FALSE}, the function performs \sQuote{nearest neighbor} interpolation and returns data from the grid point closest in space or time, respectively.
  The numerical value of \code{p} controls the degree of smoothing in the interpolation only when \code{interp} is 'IDW'.  Greater values of \code{p} assign greater influence to values closest to the interpolated point. For 0 < \code{p} < 1 peaks over the interpolated point remain smooth.  As \code{p} increases beyond 1, the peaks become sharper.
  
  
  Variables in these datasets on a T62 Gaussian grid describe conditions over an interval of time rather than at a particular point in time. (see \sQuote{Variable Naming Conventions} below)
  As such, it is not appropriate to perform temporal interpolation on these varaibles.
  Therefore, \code{NCEP.interp} automatically sets \code{interpolate.time} to \code{FALSE} when querying one of these variables, 
  and returns the data corresponding to the interval within which the specified datetime falls.  
  Spatial interpolation is still performed as long as \code{interpolate.space} is \code{TRUE}.
  
  
  Unpacking information is unique to each variable and dataset.  Therefore,\cr \code{keep.unpacking.info} can be made \code{TRUE} as long as only one variable from one dataset (i.e. Reanalysis I or II) is queried in a single function call, even for multiple times and locations.  \code{keep.unpacking.info} will be made \code{FALSE}, if necessary, with a warning.


  The function will run faster when \code{keep.unpacking.info} is \code{TRUE}.

  
  The robust alternative to \code{NCEP.interp} is applied automatically.  These robust functions are applied when interpolation requires data from two different years or from both sides of the Prime Meridian. 

  
  Some variables are not in both the Reanalysis I and II datasets.  If a variable is chosen that is not in the specified dataset, the other dataset will be used... with a warning.

  The function also returns, as an attribute, standard deviation calculated on all data used in the interpolation.  
  This provides an indication of the precision of an interpolated result described in the same units as the interpolated variable.
  Smaller values indicate that there is less variability among the points used in interpolation.
  Standard deviation is only calcuated on the points used in the interpolation.  Therefore, if \code{interpolate.time} and \code{interpolate.space} are both \code{TRUE}, 
  standard deviation is calculated on eight points.\cr
  If \code{interpolate.time} is \code{FALSE} and \code{interpolate.space} is \code{TRUE},
  standard deviation is calculated on four points. If \code{interpolate.time} is \code{TRUE} and \code{interpolate.space} is \code{FALSE}, standard deviation
  is calculated on only two points.  If \code{interpolate.time} and \code{interpolate.space} are both \code{FALSE}, standard deviation is not calculated and \code{NA} is returned.
  This measure of precision is the same irrespective of \code{interp}.
  
  
  Note that the status bar may be hidden behind an active R window.
  
  
  \code{variable} must be specified using one of the names found in the section \sQuote{Variable Naming Conventions} below...
}

\section{Variable Naming Conventions}{
\bold{VARIABLES IN REFERENCE TO A PARTICULAR PRESSURE LEVEL}
  
\tabular{lll}{
	\bold{\sQuote{air}} \tab Air Temperature \tab deg K \cr
	\bold{\sQuote{hgt}} \tab Geopotential Height \tab m \cr
	\bold{\sQuote{rhum}} \tab Relative Humidity \tab \% \cr
	\bold{\sQuote{shum}} \tab Specific Humidity \tab kg/kg \cr
	\bold{\sQuote{omega}} \tab Omega [Vertical Velocity] \tab Pascal/s \cr
	\bold{\sQuote{uwnd}} \tab U-Wind Component [East/West] \tab m/s \cr
	\bold{\sQuote{vwnd}} \tab V-Wind Component [North/South] \tab m/s
}

\bold{VARIABLES IN REFERENCE TO THE SURFACE}
  
 \tabular{llll}{
	\bold{\sQuote{air.sig995}} \tab Air Temperature \tab (Near Surface) \tab deg K \cr
	\bold{\sQuote{lftx.sfc}} \tab Surface Lifted Index \tab (At Surface) \tab deg K \cr
	\bold{\sQuote{lftx4.sfc}} \tab Best (4-layer) Lifted Index \tab (At Surface) \tab deg K \cr
	\bold{\sQuote{omega.sig995}} \tab Omega [Vertical Velocity] \tab (Near Surface) \tab Pascal/s \cr
	\bold{\sQuote{pottmp.sig995}} \tab Potential Temperature \tab (Near Surface) \tab deg K \cr
	\bold{\sQuote{pr_wtr.eatm}} \tab Precipitable Water \tab (Entire Atmosphere) \tab kg/m^2 \cr
	\bold{\sQuote{pres.sfc}} \tab Pressure \tab (At Surface) \tab Pascals \cr
	\bold{\sQuote{rhum.sig995}} \tab Relative Humidity \tab (Near Surface) \tab \% \cr
	\bold{\sQuote{slp}} \tab Sea Level Pressure \tab (Sea Level) \tab Pascals \cr
	\bold{\sQuote{mslp}} \tab Mean Sea Level Pressure \tab (Sea Level) \tab Pascals \cr
	\bold{\sQuote{uwnd.sig995}} \tab U-Wind Component [East/West] \tab (Near Surface) \tab m/s \cr
	\bold{\sQuote{vwnd.sig995}} \tab V-Wind Component [North/South] \tab (Near Surface) \tab m/s
}
  
\bold{VARIABLES IN REFERENCE TO A T62 GAUSSIAN GRID}
  
  ----- These variables are forecasts valid 6 hours after the reference time -----
  
 \tabular{llllll}{
         \bold{\sQuote{air.2m}} \tab \tab Air Temperature  \tab \tab (At 2 meters) \tab deg K \cr
         \bold{\sQuote{icec.sfc}} \tab \tab Ice Concentration \tab \tab (At Surface) \tab fraction \cr
		 \bold{\sQuote{pevpr.sfc}} \tab \tab	Potential Evaporation Rate \tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{pres.sfc}} \tab \tab	Pressure  \tab \tab (At Surface) \tab Pascals \cr
		 \bold{\sQuote{runof.sfc}} \tab \tab	Water Runoff						\tab \tab (At Surface) \tab kg/m^2 \cr
		 \bold{\sQuote{sfcr.sfc}} \tab \tab	Surface Roughness					\tab \tab (At Surface) \tab m \cr
		 \bold{\sQuote{shum.2m}} \tab \tab	Specific Humidity					\tab \tab (At 2 meters) \tab kg/kg \cr
		 \bold{\sQuote{soilw.0-10cm}} \tab \tab	Soil Moisture 						\tab \tab (From 0-10 cm) \tab fraction \cr
		 \bold{\sQuote{soilw.10-200cm}} \tab \tab Soil Moisture 						\tab \tab (From 10-200 cm) \tab fraction \cr
		 \bold{\sQuote{skt.sfc}} \tab \tab  	Skin Temperature					\tab \tab (At Surface) \tab deg K \cr
		 \bold{\sQuote{tmp.0-10cm}} \tab \tab 	Temperature of 0-10 cm layer		\tab \tab (From 0-10 cm) \tab deg K \cr
		 \bold{\sQuote{tmp.10-200cm}} \tab \tab 	Temperature of 10-200 cm layer		\tab \tab (From 10-200 cm) \tab deg K \cr
		 \bold{\sQuote{tmp.300cm}} \tab \tab  	Temperature at 300 cm				\tab \tab (From 300 cm) \tab deg K \cr
		 \bold{\sQuote{uwnd.10m}} \tab \tab  	U-wind								\tab \tab (At 10 meters) \tab m/s \cr
		 \bold{\sQuote{vwnd.10m}} \tab \tab  	V-wind								\tab \tab (At 10 meters) \tab m/s \cr
		 \bold{\sQuote{weasd.sfc}} \tab \tab 	Water equivalent of snow depth		\tab \tab (At Surface) \tab kg/m^2
		 }
		 
----- These variables are 6 hour hindcasts from the reference time -----

 \tabular{llllll}{
		 \bold{\sQuote{tmax.2m}} \tab \tab 	Maximum temperature					\tab \tab (At 2 meters) \tab deg K \cr
		 \bold{\sQuote{tmin.2m}} \tab \tab 	Minimum temperature					\tab \tab (At 2 meters) \tab deg K
		 }
		 
----- These variables are 6 hour averages starting at the reference time -----

 \tabular{llllll}{
		 \bold{\sQuote{cfnlf.sfc}} \tab \tab	Cloud forcing net longwave flux		\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{cfnsf.sfc}} \tab \tab	Cloud forcing net solar flux		\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{cprat.sfc}} \tab \tab 	Convective precipitation rate		\tab \tab (At Surface) \tab Kg/m^2/s \cr
		 \bold{\sQuote{csdlf.sfc}} \tab \tab 	Clear sky downward longwave flux	\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{csdsf.sfc}} \tab \tab 	Clear sky downward solar flux		\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{dlwrf.sfc}} \tab \tab 	Downward longwave radiation flux	\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{dswrf.sfc}} \tab \tab 	Downward solar radiation flux		\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{dswrf.ntat}} \tab \tab 	Downward solar radiation flux		\tab \tab (Nominal Top of Atmosphere) \tab W/m^2 \cr
		 \bold{\sQuote{gflux.sfc}} \tab \tab 	Ground heat flux					\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{lhtfl.sfc}} \tab \tab 	Latent heat net flux				\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{nbdsf.sfc}} \tab \tab 	Near IR beam downward solar flux	\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{nddsf.sfc}} \tab \tab 	Near IR diffuse downward solar flux	\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{nlwrs.sfc}} \tab \tab 	Net longwave radiation				\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{nswrs.sfc}} \tab \tab 	Net shortwave radiation				\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{prate.sfc}} \tab \tab 	Precipitation rate					\tab \tab (At Surface) \tab Kg/m^2/s \cr
		 \bold{\sQuote{shtfl.sfc}} \tab \tab 	Sensible heat net flux				\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{uflx.sfc}} \tab \tab 	Momentum flux (zonal)				\tab \tab (At Surface) \tab N/m^2 \cr
		 \bold{\sQuote{ugwd.sfc}} \tab \tab 	Zonal gravity wave stress			\tab \tab (At Surface) \tab N/m^2 \cr
		 \bold{\sQuote{ulwrf.sfc}} \tab \tab 	Upward longwave radiation flux		\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{ulwrf.ntat}} \tab \tab 	Upward longwave radiation flux		\tab \tab (Nominal Top of Atmosphere) \tab W/m^2 \cr
		 \bold{\sQuote{uswrf.sfc}} \tab \tab 	Upward solar radiation flux			\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{uswrf.ntat}} \tab \tab 	Upward solar radiation flux			\tab \tab (Nominal Top of Atmosphere) \tab W/m^2 \cr
		 \bold{\sQuote{vbdsf.sfc}} \tab \tab  	Visable beam downward solar flux	\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{vddsf.sfc}} \tab \tab 	Visable diffuse downward solar flux	\tab \tab (At Surface) \tab W/m^2 \cr
		 \bold{\sQuote{vflx.sfc}} \tab \tab 	Momentum flux (meridional)			\tab \tab (At Surface) \tab N/m^2 \cr
		 \bold{\sQuote{vgwd.sfc}} \tab \tab 	Meridional gravity wave stress		\tab \tab (At Surface) \tab N/m^2
		 }
		 
----- These variables are 6 hour averages starting at the reference time -----

 \tabular{llllll}{
		 \bold{\sQuote{csulf.ntat}} \tab \tab 	Clear Sky Upward Longwave Flux		\tab \tab (Nominal Top of Atmosphere) \tab W/m^2 \cr
		 \bold{\sQuote{csusf.ntat}} \tab \tab 	Clear Sky Upward Solar Flux			\tab \tab (Nominal Top of Atmosphere) \tab W/m^2 \cr
		 \bold{\sQuote{dswrf.ntat}} \tab \tab 	Downward Solar Radiation Flux		\tab \tab (Nominal Top of Atmosphere) \tab W/m^2 \cr
		 \bold{\sQuote{pres.hcb}} \tab \tab 	Pressure							\tab \tab (High Cloud Bottom) \tab Pascals \cr
		 \bold{\sQuote{pres.hct}} \tab \tab 	Pressure							\tab \tab (High Cloud Top) \tab Pascals \cr
		 \bold{\sQuote{pres.lcb}} \tab \tab 	Pressure							\tab \tab (Low Cloud Bottom) \tab Pascals \cr
		 \bold{\sQuote{pres.lct}} \tab \tab 	Pressure							\tab \tab (Low Cloud Top) \tab Pascals \cr
		 \bold{\sQuote{pres.mcb}} \tab \tab 	Pressure							\tab \tab (Middle Cloud Bottom) \tab Pascals \cr
		 \bold{\sQuote{pres.mct}} \tab \tab 	Pressure							\tab \tab (Middle Cloud Top) \tab Pascals \cr
		 \bold{\sQuote{tcdc.eatm}} \tab \tab 	Total Cloud Cover					\tab \tab (Entire Atmosphere) \tab \% \cr
		 \bold{\sQuote{ulwrf.ntat}} \tab \tab 	Upward Longwave Radiation Flux		\tab \tab (Nominal Top of Atmosphere) \tab W/m^2 \cr
		 \bold{\sQuote{uswrf.ntat}} \tab \tab 	Upward Solar Radiation Flux			\tab \tab (Nominal Top of Atmosphere) \tab W/m^2
		 }


}

\value{
A vector of interpolated results with the associated standard deviation of the points used to perform the interpolation as an attribute.

Optionally, the units of the variable being queried are printed when the function completes.
}

\references{ \bold{To cite package 'RNCEP' in publications use:}

  Kemp, M.U., van Loon, E.E., Shamoun-Baranes, J.,and Bouten, W.
  (2011). RNCEP: global weather and climate data at your fingertips.
  Methods in Ecology and Evolution, DOI:10.1111/j.2041-210X.2011.00138.x.

\bold{To cite the NCEP/NCAR Reanalysis dataset use:}

  Kalnay et al. (1996), The NCEP/NCAR 40-year reanalysis project, Bull.
  Amer. Meteor. Soc., 77, 437-470

\bold{To cite the NCEP/DOE Reanalysis II dataset use:}

  Kanamitsu et al. (2002), NCEP-DOE AMIP-II Reanalysis (R-2). Bull.
  Amer. Meteor. Soc., 83, 1631-1643


Please acknowledge the use of NCEP data in any publications by
including text such as, \dQuote{NCEP Reanalysis data provided by the
NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at
\url{http://www.esrl.noaa.gov/psd/}}. They would also appreciate a copy of any publications using NCEP data.}

\author{Michael U. Kemp \email{M.U.Kemp@UvA.nl}}


\examples{
\dontrun{
library(RNCEP)
###############################################################
###############################################################
## The function can be applied to interpolate a single variable 
## to a single point in space and time ##
## Interpolate temperature from the 850 mb pressure level ##
wx.interp <- NCEP.interp(variable='air', level=850, lat=55.1,
    lon=11.3, dt='2006-10-12 17:23:12',
    interp='linear')

## Interpolate precipitable water (for the entire atmosphere, but
## described in reference to the surface)
wx.interp <- NCEP.interp(variable='pr_wtr.eatm', level='surface',
    lat=55.1, lon=11.3, dt='2006-10-12 17:23:12',
    interp='linear')

## Interpolate specific humidity (at the surface, but in 
## reference to a T62 Gaussian grid) using the IDW interpolation
wx.interp <- NCEP.interp(variable='shum.2m', level='gaussian',
    lat=55.1, lon=11.3, dt='2006-10-12 17:23:12',
    interp='IDW', p=1)

###################################################################
###################################################################	
## The function can also be applied to interpolate several variables,
## locations, datetimes, and/or methods of interpolation in a single
## function call ##
## Interpolate temperature from the 850 and 700 mb pressure levels ## 
## for the same time and location ##
wx.interp <- NCEP.interp(variable='air', level=c(850,700), lat=55.1,
    lon=11.3, dt='2006-10-12 17:23:12',
    interp='linear')

## Interpolate temperature and relative humidity from the 1000 mb 
## pressure level ##
wx.interp <- NCEP.interp(variable=c('air','rhum'), level=1000, 
    lat=55.1, lon=11.3, dt='2006-10-12 17:23:12', interp='linear')

## Interpolate temperature and relative humidity 
## from the 1000 and 700 mb pressure levels, respectively
## for the same datetime ##
wx.interp <- NCEP.interp(variable=c('air','rhum'), 
    level=c(1000,700), lat=55.1, lon=11.3, 
    dt='2006-10-12 17:23:12', interp='linear')

## Interpolate temperature and relative humidity 
## from the 1000 and 700 mb pressure levels, respectively
## for different datetimes ##
wx.interp <- NCEP.interp(variable=c('air','rhum'), level=c(1000,700), lat=55.1,
    lon=11.3, dt=c('2006-10-12 17:23:12', '2006-10-12 18:05:31'),
    interp='linear')
	
## Interpolate geopotential height using 'linear', 'IDW', and 
## 'nearest neighbor' interpolation ##
wx.interp <- NCEP.interp(variable='hgt', level=700, lat=55.1,
    lon=11.3, dt='2006-10-12 17:23:12',
    interp=c('linear','IDW','IDW'), 
    interpolate.space=c(TRUE,TRUE,FALSE))

###############################################################
###############################################################
## Alternatively the function can be applied to interpolate a
## weather variable to multiple datetime and point locations
## in a single function call ##  

## In this example, we use datetime and locational data obtained 
## from a GPS device attached to a lesser black-backed gull.
## We interpolate wind information to to each point in the dataset
data(gull)

## Take a subset of the data based on the datetime of 
## the measurement ##
ss <- subset(gull, format(gull$datetime, "\%Y-\%m-\%d \%H:\%M:\%S") >=
    "2008-09-19 16:00:00" & format(gull$datetime, 
    "\%Y-\%m-\%d \%H:\%M:\%S") <= "2008-09-19 19:30:00")


## Now collect wind information for each point in the subset ##
uwind <- NCEP.interp(variable='uwnd', level=925, 
    lat=ss$latitude, lon=ss$longitude, dt=ss$datetime,
    reanalysis2=TRUE, keep.unpacking.info=TRUE)
vwind <- NCEP.interp(variable='vwnd', level=925, 
    lat=ss$latitude, lon=ss$longitude, dt=ss$datetime, 
    reanalysis2=TRUE, keep.unpacking.info=TRUE)	

	
## Now calculate the tailwind component from the U and V
## wind components assuming that the bird's preferred 
## direction is 225 degrees
tailwind <- (sqrt(uwind^2 + vwind^2)*cos(((atan2(uwind,vwind)*
    (180/pi))-225)*(pi/180)))

## Now visualize the subset of the GPS track using color
## to indicate the tailwind speed ##
NCEP.vis.points(wx=tailwind, lats=ss$latitude, lons=ss$longitude,
    cols=rev(heat.colors(64)),
    title.args=list(main='Lesser black-backed gull'),
    image.plot.args=list(legend.args=list(text='Tailwind m/s',
    adj=0, padj=-2, cex=1.15)),
    map.args=list(xlim=c(-7,4), ylim=c(40,50)))
}
}