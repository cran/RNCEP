\name{NCEP.gather}

\alias{NCEP.gather}
\alias{NCEP.gather.gaussian}
\alias{NCEP.gather.pressure}
\alias{NCEP.gather.surface}
\alias{robust.NCEP.gather.gaussian}
\alias{robust.NCEP.gather.pressure}
\alias{robust.NCEP.gather.surface}

\title{Queries Weather Data }

\description{
  This function queries weather data over the Internet from the NCEP/NCAR Reanalysis\cr or NCEP/DOE Reanalysis II datasets for the spatial and temporal extents specified in the function call and maintains the spatial and temporal structure of the data in a 3-D array.
}

\usage{
NCEP.gather(variable, level, months.minmax, years.minmax,
    lat.southnorth, lon.westeast, reanalysis2 = FALSE,
    return.units = TRUE, status.bar=TRUE)

## NCEP.gather is a wrapper function that calls one of the
## following functions based on the value of level.
## Users should avoid using these functions directly.
NCEP.gather.gaussian(variable, months.minmax, years.minmax,
    lat.minmax, lon.minmax, reanalysis2 = FALSE, 
    return.units = TRUE, increments=NULL, pb=NULL)
NCEP.gather.pressure(variable, months.minmax, years.minmax,
    lat.minmax, lon.minmax, pressure, reanalysis2 = FALSE, 
    return.units = TRUE, increments=NULL, pb=NULL)
NCEP.gather.surface(variable, months.minmax, years.minmax, 
    lat.minmax, lon.minmax, reanalysis2 = FALSE, 
    return.units = TRUE, increments=NULL, pb=NULL)
}

\arguments{
  \item{variable}{ Character. The name of the weather variable to be obtained.  See \sQuote{Details} for possible variable names. }
  \item{level}{ A numeric pressure level or one of either \sQuote{gaussian} or \sQuote{surface}. See \sQuote{Details}. }
  \item{months.minmax}{ Numeric.  Specifies the range of months to be obtained from each year. }
  \item{years.minmax}{ Numeric.  Specifies the range of years to be obtained. }
  \item{lat.southnorth}{ Numeric. Specifies the range of latitudes to be obtained in the order\cr \code{c(southernmost, northernmost)}. }
  \item{lat.minmax}{ Same as \code{lat.southnorth}. }
  \item{lon.westeast}{ Numeric. Specifies the range of longitudes to be obtained in the order\cr \code{c(westernmost, easternmost)}. }
  \item{lon.minmax}{ Same as \code{lat.westeast}. }
  \item{reanalysis2}{ Logical.  Should the data be obtained from the Reanalysis I dataset (default) or from Reanalysis II? }
  \item{return.units}{ Logical.  Should the units of the variable being obtained be printed after the query finishes? }
  \item{pressure}{ Numeric.  A pressure level in millibars that is assigned automatically from the value of \code{level} when needed. }  
  \item{status.bar}{ Logical.  Should a status bar be shown indicating the percentage of completion? }
  \item{increments}{ Numeric.  This value, which is assigned automatically when using \code{NCEP.gather}, indicates the number of queries necessary to retrieve all data. }
  \item{pb}{ An object of class "tkProgressBar", which is assigned automatically when using \code{NCEP.gather}. }
}
\details{
  \code{NCEP.gather} is a wrapper function that applies one of \code{NCEP.gather.gaussian},\cr \code{NCEP.gather.pressure}, or \code{NCEP.gather.surface} depending on the value of \code{level}.
  
  
  \code{level} must specify one of either \sQuote{gaussian} or \sQuote{surface} or give a numerical pressure level in millibars.  Numeric pressure levels must be one of 1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10
  See \sQuote{Variable Naming Conventions} below to determine if your variable of interest is stored relative to the surface, a pressure level, or a T62 Gaussian grid.
  Note that variables on a T62 Gaussian grid are evenly spaced in longitude but unevenly spaced in latitude while variables from either the surface or a particular pressure level are evenly spaced in both latitude and longitude (2.5 deg. x 2.5 deg.)
  
  Months and years in \code{months.minmax} and \code{years.minmax} must be numeric and given in the order \code{c(minimum,maximum)}.
  
  
  Latitude and longitude ranges should be given in decimal degrees.  If the latitudes or longitudes given do not match a grid point in the NCEP dataset, the function moves to the next grid point such that the specified range is always included.  Latitude should always be given in the order \code{c(southernmost, northernmost)} and longitude should always be given in the order \code{c(westernmost, easternmost)}.
  
  
  Latitudes below the equator are negative.  Longitudes west of the Prime Meridian can be specified using either positive (i.e. 350) or negative (i.e. -10) notation.
  
  
  Some variables are not in both the Reanalysis I and II datasets.  If a variable is chosen that is not in the specified dataset, the other dataset will be used... with a warning.
  
  
  Very large queries may cause errors due to memory limitations in R.  
  See \code{\link[base]{Memory-limits}} for more information or \code{\link[utils]{memory.limit}} to increase available memory.
  Alternatively, consider querying subsets of your total desired range.
  For instance, subset a very large spatial domain into several smaller regions, perform analyses on each geographical subset independently, and then combine the results.
  Functions in the \code{\link[raster]{raster-package}} may be useful for managing subsets in the temporal domain.
   
  
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
  This function returns a three dimensional array of weather data.  The three dimensions are latitude, longitude, and datetime reflected in the dimnames of the array.  Datetimes are always expressed in UTC with the format \code{"\%Y_\%m_\%d_\%H"}.
  
  
  Optionally, the units of the variable being queried are printed upon completion.
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
\url{http://www.esrl.noaa.gov/psd/}}. They would also appreciate a copy of any publications using NCEP data.
}

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\examples{
\dontrun{
library(RNCEP)
## Query the temperature from a particular pressure level ##
wx.extent1 <- NCEP.gather(variable='air', level=850,
    months.minmax=c(9,10), years.minmax=c(1996,1997),
    lat.southnorth=c(50,55), lon.westeast=c(5,10),
    reanalysis2 = FALSE, return.units = TRUE)

## Query the temperature at 2 meters altitude with reference to
## the surface
wx.extent2 <- NCEP.gather(variable='air.sig995', level='surface',
    months.minmax=c(2,3), years.minmax=c(2000,2001),
    lat.southnorth=c(50,55), lon.westeast=c(0,5),
    reanalysis2 = FALSE, return.units = TRUE)

## Query the temperature at 2 meters altitude with reference to
## a T62 Gaussian grid
wx.extent3 <- NCEP.gather(variable='air.2m', level='gaussian',
    months.minmax=c(4,5), years.minmax=c(2006,2007),
    lat.southnorth=c(32,35), lon.westeast=c(-35,-32),
    reanalysis2 = FALSE, return.units = TRUE)

## Note that the dimnames of the data array indicate the 
## latitudes, longitudes, and datetimes	of the data. ##
dimnames(wx.extent1)
## Therefore, the latitudes, longitudes, and datetimes
## can be called. ##
dimnames(wx.extent1)[[1]] ## latitudes
dimnames(wx.extent1)[[2]] ## longitudes
dimnames(wx.extent1)[[3]] ## datetimes

#################################################################
#################################################################
#################################################################
## THERE ARE MANY OPTIONS FOR CREATING DIFFERENT R OBJECTS  
## AND/OR FOR EXPORTING THESE WEATHER DATA.
## HERE ARE A FEW EXAMPLES

#########################################################
#########################################################
## The data array may be saved directly as an R object ##
save(wx.extent, file='wx_extent.Rdata')
## And then later recalled ##
load(file='wx_extent.Rdata')


################################################################
################################################################
## Another option is to create a raster object from the array ##
## For more info, see package raster
library(raster)

## Using the data from a query above ##
## First create a stacked raster object using the first 
## layer (i.e. datetime) of the weather data array ##
## Notice the offset of 1.25 degrees (1/2 the spatial resolution)
## to describe the limits of the bounding box not the points
ras <- stack(raster(wx.extent1[,,1], crs="+proj=longlat +datum=WGS84", 
    xmn=min(as.numeric(dimnames(wx.extent1)[[2]])) - 1.25,
    xmx=max(as.numeric(dimnames(wx.extent1)[[2]])) + 1.25, 
    ymn=min(as.numeric(dimnames(wx.extent1)[[1]])) - 1.25,
    ymx=max(as.numeric(dimnames(wx.extent1)[[1]])) + 1.25))

## Then add each subsequent layer to the raster stack ##
for(i in 2:length(dimnames(wx.extent1)[[3]])){
    ras <- addLayer(ras, raster(wx.extent1[,,i], 
        crs="+proj=longlat +datum=WGS84", 
        xmn=min(as.numeric(dimnames(wx.extent1)[[2]])) - 1.25,
        xmx=max(as.numeric(dimnames(wx.extent1)[[2]])) + 1.25, 
        ymn=min(as.numeric(dimnames(wx.extent1)[[1]])) - 1.25,
        ymx=max(as.numeric(dimnames(wx.extent1)[[1]])) + 1.25))
    }

#######################################################
## Optionally, export a layer from the raster stack to 
## a format that can be imported by Esri's ArcGIS products ##

## First, select a layer from the raster stack ##
ras1 <- raster(ras, layer=1)

## Then write the data from that layer to a .bil file ##
writeRaster(ras, filename='ras_example.bil', format="EHdr")
## The file will be saved in your current working directory ##

## The resulting file can be imported into ArcGIS ##
## by using the "Raster to Other Format" tool in the 
## "To Raster" section of the ArcToolbox.

###########################################################
## NOTE: Weather data obtained from a Gaussian grid must 
## first be resampled onto a regular grid !!! ##
## Here we use the interp.loess() function
## from the tgp package

## Using data from a T62 Gaussian grid queried above 
## Interpolate the data from the first layer (i.e. datetime)
## onto a regular grid ##
library(tgp)
wx.reg <- interp.loess(x=rep(as.numeric(dimnames(wx.extent3)[[2]]), 
        each=length(dimnames(wx.extent3)[[1]])),
    y=rep(as.numeric(dimnames(wx.extent3)[[1]]), 
         length(dimnames(wx.extent3)[[2]])), 
    z=as.vector(wx.extent3[,,1]), span=0.6, 
    gridlen=c(length(dimnames(wx.extent3)[[2]]),
        length(dimnames(wx.extent3)[[1]])))

## Create a stacked raster object from the first layer 
## (i.e. datetime) after interpolation ##
## Again, notice the offset (1/2 the resolution) ##
## Also note that the matrix (i.e. wx.reg$z) must be flipped 
## along the y axis and transposed ##
## This is required b/c of the interpolation performed above ##
ras <- stack(raster(t(wx.reg$z[,length(wx.reg$y):1]), 
    crs="+proj=longlat +datum=WGS84", 
    xmn=min(as.numeric(wx.reg$x)) - abs(diff(wx.reg$x)[1]/2),
    xmx=max(as.numeric(wx.reg$x)) + abs(diff(wx.reg$x)[1]/2), 
    ymn=min(as.numeric(wx.reg$y)) - abs(diff(wx.reg$y)[1]/2),
    ymx=max(as.numeric(wx.reg$y)) + abs(diff(wx.reg$y)[1]/2)))
	
## Add each subsequent layer in the array to the raster stack ##
## after interpolating onto a regular grid ##

for(i in 2:length(dimnames(wx.extent3)[[3]])){

  ## Interpolate
  t.wx.reg <- interp.loess(x=rep(as.numeric(dimnames(wx.extent3)[[2]]), 
      each=length(dimnames(wx.extent3)[[1]])), 
      y=rep(as.numeric(dimnames(wx.extent3)[[1]]), 
      length(dimnames(wx.extent3)[[2]])), 
      z=as.vector(wx.extent3[,,i]), span=0.6, 
      gridlen=c(length(dimnames(wx.extent3)[[2]]),
      length(dimnames(wx.extent3)[[1]])))
  
  ## Note the offset ##
  ## Note flipping the matrix along the y axis and transposing ##
  ## Add layer to stack
  ras <- addLayer(ras, raster(t(t.wx.reg$z[,length(t.wx.reg$y):1]),
    crs="+proj=longlat +datum=WGS84",
    xmn=min(as.numeric(t.wx.reg$x)) - abs(diff(t.wx.reg$x)[1]/2),
    xmx=max(as.numeric(t.wx.reg$x)) + abs(diff(t.wx.reg$x)[1]/2), 
    ymn=min(as.numeric(t.wx.reg$y)) - abs(diff(t.wx.reg$y)[1]/2),
    ymx=max(as.numeric(t.wx.reg$y)) + abs(diff(t.wx.reg$y)[1]/2)))
  }

##################################################################
##################################################################
## Another option is to create a Spatial object
## using the sp package
## Again, data from a Gaussian grid may require special attention
## as the grid points are unevenly spaced
library(sp)

## Using the data from a query above
## Convert the array to a data.frame ##
wx.df <- NCEP.array2df(wx.extent2)

## Specify that the data.frame is a spatial object
library(sp)
coordinates(wx.df) <- ~longitude+latitude
gridded(wx.df) <- TRUE
proj4string(wx.df) <- CRS('+proj=longlat + datum=WGS84')

#########################################################
## A Spatial object of a single datetime (i.e. layer) can 
## be written to .asc, a format that may then be 
## imported into ArcGIS.

## First, convert the first layer of the array to a data.frame ##
wx.df <- NCEP.array2df(wx.extent2[,,1])

## Specify that the data.frame is a spatial object
library(sp)
coordinates(wx.df) <- ~longitude+latitude
gridded(wx.df) <- TRUE
proj4string(wx.df) <- CRS('+proj=longlat + datum=WGS84')

## Save the data in .asc format
write.asciigrid(wx.df, fname='wx.asc')
## Note: Data will be written to your working directory ##

## The resulting .asc file can be imported into ArcMap
## using ArcMap's "ASCII to Raster" tool in the "Conversion Tools"
## section of the ArcToolbox. ##

##################################################################
## There are still more options for writing these data to files ##
## See e.g. writeMat() in the R.matlab package for writing Matlab files
## Also see the RSAGA package for GIS functionality in R
## One could even write the data array back to NetCDF 
## (see packages RNetCDF and ncdf)
}
}