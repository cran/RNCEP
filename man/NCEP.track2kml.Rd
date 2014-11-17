\name{NCEP.track2kml}

\alias{NCEP.track2kml}

\title{ Plot a track in Google Earth }

\description{
This function creates a .kml file from a time series of point locations (e.g. as returned by \code{\link{NCEP.flight}} or measured with a GPS device) that can be viewed as a track in Google Earth.}

\usage{
NCEP.track2kml(latitude, longitude, datetime, altitude=NULL, 
  col.variable=NULL, col.scheme=NULL, point.alpha=255, 
  line.color='goldenrod', line.alpha=255, size.variable=NULL,
  point.names=NULL, data.variables=NULL, output.filename='track',
  descriptive.filename=NULL)
}

\arguments{
  \item{latitude}{ A numeric vector of latitudes in decimal degrees }
  \item{longitude}{ A numeric vector of longitudes in decimal degrees }
  \item{datetime}{ A character vector of datetime increments in the format "\%Y-\%M-\%D \%H:\%M:\%S". }
  \item{altitude}{ An optional vector of altitudes in meters. }
  \item{col.variable}{ An optional numeric vector upon which the color of each point should be based }
  \item{col.scheme}{ A character description of the color scheme to use in coloring the points.  Several options, see Details. }
  \item{point.alpha}{ A numeric vector of length one indicating the transparency of all points on a scale from 0 (transparent) to 255 (opaque) }
  \item{line.color}{ An character expression (any of \code{\link[grDevices]{colors}} or hexadecimal notation), or numeric indicating the color of the line connecting the point locations.}
  \item{line.alpha}{ A numeric vector of length one indicating the transparency of the line connecting the point locations on a scale from 0 (transparent) to 255 (opaque) }  
  \item{size.variable}{ An optional numeric vector upon which the size of the points should be based }
  \item{point.names}{ An optional character vector of containing names for each point }
  \item{data.variables}{ An optional data.frame containing any descriptor information for each point.  See Details.  }
  \item{output.filename}{ A character expression giving the name of the resulting output file. This should not include the .kml extension. }
  \item{descriptive.filename}{ The name of the object in the .kml file.  Defaults to \code{output.filename}. }
}

\details{
  There are several options for specifying \code{col.scheme}.  A single color may be specified for all points (using e.g. \code{'red'}, \code{2}, or \code{"#FF0000"}), specific colors may be given for each point (e.g. \code{c('red','blue','green', etc.)} or code{c('#FF0000','#0000FF','#00FF00', etc.)}), or the function can automatically assign colors, according to the values in \code{clo.variable}, if \code{col.scheme} is any color palette in \code{\link[RColorBrewer]{display.brewer.all}} or one of the recognizezd R color palettes (i.e. \code{'rainbow'}, \code{'heat.colors'}, \code{'terrain.colors'}, \code{'topo.colors'}, \code{'cm.colors'}, or \code{'bpy.colors'}).  An alpha value (0-255) may be supplied to \code{point.alpha} to adjust transparencies.

  
  In the output .kml file, each point along the track contains a table of values.  By default, this table contains the latitude, longitude, and datetime. Variables contained in the \code{data.frame} described in \code{data.variables} will also be included in this table.  
  
  
  The altitude associated with each point, i.e. those passed to the \code{altitude} argument, should be supplied in meters.  For an example of how to assign general altitudes from pressure levels, see the Examples below.
 
 
  Depending on \code{col.scheme}, this function may require \pkg{RColorBrewer} or \pkg{sp}. 
  }

 \value{
  This function returns no data.  It creates a .kml file in the current working directory.
}
\references{ \bold{To cite package 'RNCEP' in publications use:}

Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Methods in Ecology and Evolution. DOI:10.1111/j.2041-210X.2011.00138.x. }

\author{ Michael U. Kemp <\email{mukemp+RNCEP@gmail.com}> }
\seealso{ \code{\link{NCEP.flight}} }
\examples{
\dontrun{
## Load the gull dataset ##
data(gull)

## Create a subset of the full dataset ##
g <- gull[1:100,]

## Create a .kml file from a portion of the GPS track ##
NCEP.track2kml(latitude=g$latitude, longitude=g$longitude,
    datetime=as.character(g$datetime), altitude=g$altitude, 
    col.variable=g$altitude, col.scheme='heat.colors',
    point.alpha=255, line.color='goldenrod', line.alpha=255,
	size.variable=NULL, point.names=NULL, 
    data.variables=data.frame(g$altitude),
	output.filename='track', descriptive.filename=NULL)
}
}
