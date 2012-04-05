\name{NCEP.uv.revert}

\alias{NCEP.uv.revert}

\title{ Reverts speed and direction to U and V components. }

\description{
  This function calculates U (i.e. zonal or east/west) and V (i.e. meridional or north/south) components from a specified speed and direction.
}

\usage{
NCEP.uv.revert(spd, dir, radians=FALSE)
}

\arguments{
  \item{spd}{ A numeric value indicating speed. }
  \item{dir}{ A numeric value indicating direction in degrees from north or radians if \code{radians} is TRUE. }
  \item{radians}{ A logical indicating whether \code{dir} is given in degrees from north (FALSE) or radians (TRUE). }
  }

\details{
 This function calculates U (i.e. zonal or east/west) and V (i.e. meridional or north/south) components from a specified speed and direction.  U and V components describe the direction into which movement occurs with east and north being positive, respectively.  If directions are given in radians, \code{radians} must be set to \code{TRUE}.  
}

\value{
  A \code{data.frame} containing U and V components given in the same units as \code{speed}. }

\references{ \bold{To cite package 'RNCEP' in publications use:}

Kemp, M. U., van Loon, E. E., Shamoun-Baranes, J., and Bouten, W. 2011. RNCEP:global weather and climate data at your fingertips. -- Methods in Ecology and Evolution. DOI:10.1111/j.2041-210X.2011.00138.x. }

\author{ Michael U. Kemp \email{M.U.Kemp@UvA.nl} }

\examples{
library(RNCEP)
## Using NCEP.uv.revert ##
NCEP.uv.revert(spd=12, dir=225, radians=FALSE)
}