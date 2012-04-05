\name{NCEP.aggregate}

\alias{NCEP.aggregate}

\title{ Temporally Aggregate Weather Data }

\description{
  This function temporally aggregates weather data from the NCEP/NCAR Reanalysis or NCEP/DOE Reanalysis II datasets as returned by \code{\link{NCEP.gather}}.  The spatial structure of the data is retained.  Data can be aggregated by any combination of year, month, day, and hour.
}

\usage{
NCEP.aggregate(wx.data, YEARS = TRUE, MONTHS = TRUE, DAYS = TRUE,
    HOURS = TRUE, fxn = "\%>0")
}

\arguments{
  \item{wx.data}{ the 3-D array of weather data as returned by \code{\link{NCEP.gather}} to be aggregated }
  \item{YEARS}{ Logical.  Should the years portion of the datetime be retained in the aggregation? }
  \item{MONTHS}{ Logical.  Should the months portion of the datetime be retained in the aggregation? }
  \item{DAYS}{ Logical.  Should the days portion of the datetime be retained in the aggregation? }
  \item{HOURS}{ Logical.  Should the hours portion of the datetime be retained in the aggregation? }
  \item{fxn}{ A scalar function to be applied to all aggregated subsets of the data. }
}

\details{
  Each latitude and longitude combination in the array is subset according to the logical datetime arguements in the function call, and \code{fxn} is applied to the subsets.  \code{fxn} can be an internal R function such as \code{\link[base]{mean}}, \code{\link[base]{sum}}, or \code{\link[stats]{sd}}, or it can be a function created by the user.
  
  The default setting of \code{fxn} (i.e. \code{"\%>0"}) calculates the percentage of observations in each subset greater than zero.
  
  To calculate some variables, sequential aggregations may be needed.  For instance, if the user wants to calculate monthly averages of maximum daily relative humidity, two aggregations would be needed.
  In the first aggregation, the user would find the maximum relative humidity per day.
  A second aggregation would then be required to find the monthly average of those maximum daily values.  See the examples below for a demonstration.
}

\value{
  A three-dimensional array (or 2-D if all layers are completely aggregated) containing the same latitude and longitude ranges and intervals as the input data specified in \code{wx.data}.
  The names of the aggregated time components will be replaced with \code{"XX"} or \code{"XXXX"} in the output array.
}

\references{ Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Methods in Ecology and Evolution. DOI:10.1111/j.2041-210X.2011.00138.x. }

\author{ Michael U. Kemp \email{ M.U.Kemp@UvA.nl } }

\examples{
\dontrun{
library(RNCEP)
######################################################
## In the first example, we use the internal R function
## 'mean' to calculate average temperatures per
## month and year

## First gather temperature data from a spatial extent
## for January and February from 2000-2001. 
wx.extent <- NCEP.gather(variable='air', level=850, 
    months.minmax=c(1,2), years.minmax=c(2000,2001), 
    lat.southnorth=c(50, 55), lon.westeast=c(0, 5),
    reanalysis2=FALSE, return.units=TRUE)

## Now calculate the average temperature per month and year ##
wx.ag <- NCEP.aggregate(wx.data=wx.extent, YEARS=TRUE, MONTHS=TRUE,
    DAYS=FALSE, HOURS=FALSE, fxn='mean')

## Notice that aggregated time components have been replaced 
## with "XX" or "XXXX" ##
dimnames(wx.ag)[[3]][1]
	
#######################################################
## In the second example, we create our own function to
## calculate the percentage of observations at each grid
## point with a temperature less than -5 degrees Celsius ##

## First create the function ##
## Note: temperature is in degrees Kelvin in NCEP database ##
COLD <- function(x){
    return(length(which(x < 273.15-5))/(length(x) - sum(is.na(x))))
    }

## Now calculate the percentage of occuence of temperatures
## less than -5 degrees Celsius per month and year ##
wx.cold <- NCEP.aggregate(wx.data=wx.extent, YEARS=TRUE, MONTHS=TRUE,
    DAYS=FALSE, HOURS=FALSE, fxn='COLD')

	
##########################################################
## As explained in the Details section above,
## calculating some variables requires sequential aggregations ##
## Here we calculate the monthly mean of daily average
## relative humidity.

## First gather relative humidity data from near the surface
## for a spatial extent for October through November 
## from 2001-2002. 
wx.extent <- NCEP.gather(variable='rhum.sig995', level='surface',
    months.minmax=c(10,11), years.minmax=c(2001,2002), 
    lat.southnorth=c(50, 55), lon.westeast=c(0, 5),
    reanalysis2=FALSE, return.units=FALSE)

## First calculate maximum daily relative humidity ##
wx.ag <- NCEP.aggregate(wx.data=wx.extent, 
    HOURS=FALSE, fxn='max')

## Then calculate the monthly average of those daily maximums ##
wx.ag2 <- NCEP.aggregate(wx.data=wx.ag, 
    DAYS=FALSE, fxn='mean')

	

##########################################################
## Data that have been aggregated may then be visualized
## or exported in various formats ##

###########################################
## Visualize the aggregated temperatures ##
NCEP.vis.area(wx.ag2, title.args=
    list(main='Monthly average of daily maximum relative humidity
    \\n October 2000'), image.plot.args=
    list(legend.args=list(text='\%',
    cex=1.15, padj=-1, adj=-.25)))

###########################################################
## Export a layer of the data to a format that can then be
## imported into ArcGIS ##

## Convert the first layer of the aggregated array
## to a data.frame ##
wx.df <- NCEP.array2df(wx.ag[,,1])

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
}
}
