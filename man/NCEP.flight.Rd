\name{NCEP.flight}

\alias{NCEP.flight}

\title{ Simulate Trajectories }

\description{
  This function simulates a single trajectory for any time and location by default using wind data returned by \code{\link{NCEP.interp}}.  Various flight behaviors are possible.
}

\usage{
NCEP.flight(beg.loc, end.loc, begin.dt, flow.assist='NCEP.Tailwind', 
  fa.args=list(airspeed=12), path='loxodrome', calibrate.dir=FALSE, 
  calibrate.alt=TRUE, cutoff=0, when2stop=list('latitude','longitude',50),
  levels2consider=c(850,925), hours=12, evaluation.interval=60, id=1, 
  land.if.bad=FALSE, reanalysis2 = FALSE, query=TRUE)
}

\arguments{
  \item{beg.loc}{ A numeric vector of length two giving the starting location in decimal degrees i.e. c(latitude, longitude)}
  \item{end.loc}{ A numeric vector of length two giving the end location (i.e. goal location) in decimal degrees i.e. c(latitude, longitude)}
  \item{begin.dt}{ A character string in the format '\%Y-\%m-\%d \%H:\%M:\%S' indicating the day and time at which to initiate take-off. }
  \item{flow.assist}{ A character string giving the name of the flow-assistance function to apply, which also specifies the behavior of the animal in relation to the flow conditions.  See Details below. }
  \item{fa.args}{ A list of arguments passed to the flow-assistance function.  See Details below. }
  \item{path}{ Either a numeric value indicating the preferred direction of movement in degrees from North, or one of 'loxodrome' or 'great.circle' describing the method of calculating the preferred direction. }
  \item{calibrate.dir}{ A logical expression indicating whether or not to re-calibrate the \code{path} to the goal at each evaluation interval. }
  \item{calibrate.alt}{ A logigal expression indicating whether or not to re-calibrate the altitude from which to obtain wind data at each evaluation interval.  }
  \item{cutoff}{ A numeric value indicating the minimum acceptable flow-assistance to initiate the simulation (and to continue the simulation if \code{land.if.bad = TRUE}). }
  \item{when2stop}{ A list containing at least one of 'latitude', 'longitude', or a numeric value. See Details below.}
  \item{levels2consider}{ A vector indicating the altitudes at which to consider wind conditions.  Values may be numeric and describe specific pressure levels or may be a character string describing 'surface' and/or 'gaussian'.  See \code{\link{NCEP.interp}} }
  \item{hours}{ A numeric value indicating the maximum number of hours to continue the simulation, in case the spatial goal is not reached. }
  \item{evaluation.interval}{ A numeric value indicating the interval (in minutes) in which to reassess the situation and recalibrate (if \code{calibrate.dir} or \code{calibrate.alt} is \code{TRUE}).  Also the interval at which movement statistics are calculated. }
  \item{id}{ A character string or numeric value giving an id for the simulation. }
  \item{land.if.bad}{ A logical expression describing whether or not the simulation should be interrupted if the flow-assistance is \code{NA} or below the \code{cutoff}. }
  \item{reanalysis2}{ A logical expression indicating whether wind data should come from the Reanalysis I dataset (\code{FALSE}) or from Reanalysis II (\code{TRUE}). See \code{\link{NCEP.interp}}. }
  \item{query}{ A logical expression indicating whether or not to use \code{\link{NCEP.interp}} to retrieve wind data.  See Details below. }
  }

\details{
 This function simulates a single trajectory according to the behavioral rules specified in \code{flow.assist}.

 
 The argument \code{flow.assist} gives the flow-assistance equation to apply.  It also determines how the animal will behave in relation to the flow conditions and thus how the animal will move.  Internal options for \code{flow.assist} include "\code{\link{NCEP.FlowSpeed}}", "\code{\link{NCEP.NegFlowSpeed}}", "\code{\link{NCEP.Tailwind}}", "\code{\link{NCEP.Groundspeed}}", "\code{\link{NCEP.M.Groundspeed}}", "\code{\link{NCEP.Airspeed}}",and "\code{\link{NCEP.PartialSpeed}}".  Each flow-assistance equation requires further arguements that should be given as a list to \code{fa.args}.
 
 
 Users are invited to create their own flow-assistance functions using one of the existing functions as a template.  Any flow-assistance function that is used in the context of \code{NCEP.flight} must contain at least the arguments \code{u}, \code{v}, and \code{direction}, as these arguments are passed directly by \code{NCEP.flight} if \code{query} == \code{TRUE}.  If \code{query} == \code{FALSE}, only the argument \code{direction} is passed directly by \code{NCEP.flight}.  Any other arguments are possible, and their value may be set using \code{fa.args}.  As well, any flow-assistance function that is used in the context of \code{NCEP.flight} must produce a \code{data.frame} as output.  This \code{data.frame} must contain at least the variables \code{fa}, \code{forward.move}, and \code{side.move}, but may also contain as many other variables as desired.  Note that \code{forward.move} and \code{side.move} should give forward and sideways speeds relative to the specified \code{direction} in meters per second and should already account for the animal's own airspeed.   
 
 
 If \code{calibrate.dir} is \code{TRUE}, the animal will adjust its \code{direction} at each \code{evaluation.interval} if necessary to reorient to its \code{end.loc}.  The new direction is calculated according to the specification of \code{path}.  If \code{calibrate.dir} is \code{FALSE} and \code{path} is not numeric, the direction to the \code{end.loc} is calculated before take-off but not again during the simulation.
 If \code{path} is \code{'loxodrome'}, the rhumb line or constant compass direction is calculated to the \code{end.loc}.  See \code{\link{NCEP.loxodrome}}.  If \code{path} is \code{'great.circle'}, the angle describing the shortest path to the \code{end.loc} is calculated using \code{\link[fossil]{earth.bear}}.
 A warning is issued if \code{path} is numeric and \code{calibrate.dir} is \code{TRUE}.
 
 
 If \code{calibrate.alt} is \code{TRUE}, the animal will select at each \code{evaluation.interval} from \code{levels2consider} the most supportive flow conditions according to the specified \code{flow.assist} equation.
 If \code{calibrate.alt} is \code{FALSE} and the length of \code{levels2consider} is greater than one, the animal will select from \code{levels2consider} the most supportive flow conditions according to the specified \code{flow.assist} equation only when initiating the simulation and will continue using conditions at that level thereafter.
 
 
 If the list passed to \code{when2stop} includes the character strings 'latitude' or 'longitude' the simulation will stop when the latitude or longitude, respectively, of the \code{end.loc} is reached.  A numeric argument passed to the \code{when2stop} list indicates a distance from the \code{end.loc} in kilometers at which to stop the simulation. The simulation will end when any of the \code{when2stop} arguments are satisfied.
 
 
 The argument \code{query} indicates whether or not to use \code{\link{NCEP.interp}} to retrieve wind data from the NCEP database via the Internet.  This is intended to facilitate the use of flow conditions from sources other than NCEP.  For example, one could use the time and location of the animal at each timestep to retrieve data from an oceanic dataset or an atmospheric dataset with higher resolution than NCEP.  Alternatively, one could examine the trajectory of an animal that encounters consistent flow conditions of a given intensity for its entire journey.  Since the user supplies the flow information, there is no need to query wind data from NCEP, and the function will run much faster.  See the examples for a demonstration.
}

\value{
  A \code{data.frame} containing at least the flow-assistance (i.e. \code{fa}), \code{forward.move}, and \code{side.move} each in meters per second. }

\references{ \bold{To cite package 'RNCEP' in publications use:}

Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Methods in Ecology and Evolution. DOI:10.1111/j.2041-210X.2011.00138.x. 

\bold{For more information on flow-assistance and NCEP.flight see:}

Kemp, M.U., Shamoun-Baranes, J., van Loon, E. E., and Bouten, W. 2012. Quantifying flow-assistance and implications for movement research. -- Journal of Theoretical Biology. In prep. }

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\examples{
library(RNCEP)
## Use NCEP.flight(), to simulate a flight over the North Sea 
## from Norway to the Netherlands using NCEP wind data
## queried automatically from the NCEP Reanalysis datset ##
\dontrun{tst <- NCEP.flight(beg.loc=c(58.00,7.00), 
  end.loc=c(53.00,7.00), begin.dt='2007-10-01 18:00:00',
  flow.assist='NCEP.Tailwind', fa.args=list(airspeed=12),
  path='loxodrome', calibrate.dir=FALSE, calibrate.alt=FALSE,
  cutoff=0, when2stop='latitude', levels2consider=c(850,925),
  hours=12, evaluation.interval=60, id=1, land.if.bad=FALSE,
  reanalysis2 = FALSE)}
  

#######################################################
## Use NCEP.flight(), to simulate a flight without 
## querying the NCEP database.  In this case, we use
## constant wind conditions (i.e. u=2 and v=0). ##
tst2 <- NCEP.flight(beg.loc=c(58.00,7.00), end.loc=c(53.00,7.00), 
  begin.dt='2007-10-01 18:00:00', flow.assist='NCEP.Tailwind', 
  fa.args=list(u=2, v=0, airspeed=12), path='loxodrome',
  calibrate.dir=TRUE, calibrate.alt=FALSE, cutoff=-10, when2stop='latitude', 
  levels2consider=850, hours=12, evaluation.interval=60, 
  id=1, land.if.bad=FALSE, reanalysis2 = FALSE, query=FALSE)

#######################################################
## Use NCEP.flight(), to simulate a flight without 
## querying the NCEP database.  In this case, we use
## wind conditions that depend on the animal's location ##

## First create a flow assistance function that will
## generate u and v flow components based on the animal's
## location
## Note that to use the datetime and the animal's location,
## the function MUST have the arguments lat.x, lon.x, and dt.x
Ex.FA <- function(direction, lat.x, lon.x, dt.x, airspeed){
    deg2rad = pi/180
    rad2deg = 180/pi
    ## Generate U and V flow component based on lat and lon
    u.x <- sin(lat.x*deg2rad) 
    v.x <- cos(lon.x*deg2rad)
    ## Apply NCEP.Tailwind with generated flow data
    return(cbind(NCEP.Tailwind(u=u.x, v=v.x,
        direction=direction, airspeed=airspeed), u.x, v.x))
    }

## Now use the function we just created in NCEP.flight
## The location and datetime are passed to the flow-
## assistance function automatically as lat.x, lon.x, and dt.x
tst3 <- NCEP.flight(beg.loc=c(58.00,7.00), end.loc=c(53.00,7.00), 
  begin.dt='2007-10-01 18:00:00', flow.assist='Ex.FA', 
  fa.args=list(airspeed=12), path='loxodrome',calibrate.dir=TRUE, 
  calibrate.alt=FALSE, cutoff=-10, when2stop='latitude',
  levels2consider=850, hours=12, evaluation.interval=60, 
  id=1, land.if.bad=FALSE, reanalysis2 = FALSE, query=FALSE)

## Confirm that the U and V wind components were determined
## by the latitude and longitude at each timestep
sin(tst3$lat[3] * (pi/180)) == tst3$u.x[3]
cos(tst3$lon[7] * (pi/180)) == tst3$v.x[7]

}