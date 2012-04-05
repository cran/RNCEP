\name{NCEP.Airspeed}

\alias{NCEP.Airspeed}

\title{ Calculate flow-assistance according to equation 'Airspeed' }

\description{
  This function calculates flow-assistance according to equation Airspeed and determines the speed of forward and sideways movement if an animal behaves according to the rules of equation Airspeed.
}

\usage{
NCEP.Airspeed(u, v, direction, airspeed,...)
}

\arguments{
  \item{u}{ A numeric value indicating the U (i.e. zonal or east/west) flow component in meters per second, toward east being positive.  Values must describe the direction into which the flow is moving.    }
  \item{v}{ A numeric value indicating the V (i.e. meridional or north/south) flow component in meters per second, toward north being positive.  Values must describe the direction into which the flow is moving. }
  \item{direction}{ A numeric value indicating a preferred direction of movement in degrees from North. }
  \item{airspeed}{ The airspeed (i.e. speed relative to the flow) of the animal in meters per second. }  
  \item{...}{ Any extra arguments passed to the flow-assistance equation. }  
  }

\details{
 This function calculates flow-assistance and forward and sideways movement according to equation Airspeed.  Equation Airspeed stipulates that the animal fully compensates for any lateral drift, altering its heading and groundspeed (i.e. speed relative to the fixed Earth), in order to maintain its \code{direction}.  If, with the given \code{airspeed}, the animal is incapable of maintaining its \code{direction}, the equation produces no real solution.  
}

\value{
   A \code{data.frame} containing flow-assistance (\sQuote{fa}), the animal's forward speed (\sQuote{forward.move} which includes the animal's own airspeed), the animal's sideways speed (\sQuote{side.move} which includes the animal's own airspeed), the component of the flow parallel to preferred direction of movement(\sQuote{tailwind}), the component of the flow perpendicular to the preferred direction of movement (\sQuote{sidewind}), the animal's speed relative to the flow (\sQuote{airspeed}), and the animal's speed relative to the fixed Earth (\sQuote{groundspeed}) each in meters per second, presuming \code{u}, \code{v}, and \code{airspeed} were given in meters per second. }

\references{ \bold{To cite package 'RNCEP' in publications use:}

Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Methods in Ecology and Evolution. DOI:10.1111/j.2041-210X.2011.00138.x. 

\bold{For more information on flow-assistance and equation Airspeed see:}

Kemp, M.U., Shamoun-Baranes, J., van Loon, E. E., and Bouten, W. 2012. Quantifying flow-assistance and implications for movement research. -- Journal of Theoretical Biology. In prep.
}

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\examples{
library(RNCEP)
## Using NCEP.Airspeed to calculate flow-assistance ##
tst <- NCEP.Airspeed(u=-2, v=-1, direction=225, airspeed=12)
}