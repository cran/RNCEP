\name{NCEP.array2df}

\alias{NCEP.array2df}

\title{ Convert data from an array to a data frame }

\description{
  This function takes a 3-dimensional array of weather data, as returned by \code{\link{NCEP.gather}}, and converts it to a dataframe composed of latitude, longitude, datetime, and the weather data.
}

\usage{
NCEP.array2df(wx.data, var.names=NULL)
}

\arguments{
  \item{wx.data}{ either a single 3-D array of weather data, as returned by \code{NCEP.gather}, or a list of multiple 3-D arrays of different variables but with the exact same spatial and temporal dimensions and intervals.  }
  \item{var.names}{ an optional vector of names for the weather variables in the 3-D arrays to be used in the output data frame }
}

\details{
 This is a function to convert the data contained in one or many arrays to a single data.frame.

 The order of the names specified in \code{var.names} should correspond to the order of the list of input arrays in \code{wx.data}.  Latitude, longitude, and datetime are named automatically and cannot be changed in the function call.
 
 When converting the data from two or more 3-D arrays to a single data frame, all 3-D arrays must contain the exact same spatial and temporal dimensions and intervals.
}

\value{
  A data frame with the components of latitudes, longitude, datetime, and the weather variable(s) from the input data array(s).
}

\references{ Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Methods in Ecology and Evolution submitted. }

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\examples{
library(RNCEP)
############################################################
############################################################
## In this first example, we take data from a single 3-D array
## and arrange them in a data.frame ##
#############################################################

## First query the temperature for a particular pressure level
## and datetime range ##
wx.extent <- NCEP.gather(variable='air', level=850,
    months.minmax=c(8,9), years.minmax=c(2006,2007),
    lat.southnorth=c(50,55), lon.westeast=c(0,5),
    reanalysis2 = FALSE, return.units = TRUE)
	
## Then convert the 3-D array to a data.frame ##
wx.df <- NCEP.array2df(wx.data=wx.extent, var.names='Temperature')

############################################################
############################################################
## In this second example, we take data from two 3-D arrays 
## and arrange them in a single data.frame ##
#############################################################

## Query the U (east/west) and V (north/south) wind components
## for a particular pressure level and datetime range ##
wx.uwnd <- NCEP.gather(variable='uwnd', level=850,
    months.minmax=c(8,9), years.minmax=c(2006,2007),
    lat.southnorth=c(50,55), lon.westeast=c(0,5),
    reanalysis2 = FALSE, return.units = TRUE)
wx.vwnd <- NCEP.gather(variable='vwnd', level=850,
    months.minmax=c(8,9), years.minmax=c(2006,2007),
    lat.southnorth=c(50,55), lon.westeast=c(0,5),
    reanalysis2 = FALSE, return.units = TRUE)	
	
## Then convert the two 3-D arrays to a single data.frame ##
wx.df <- NCEP.array2df(wx.data=list(wx.uwnd, wx.vwnd), 
    var.names=c('Uwind', 'Vwind'))

}