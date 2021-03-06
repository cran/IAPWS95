% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooHZ.R
\name{phi0}
\alias{phi0}
\title{Ideal-Gas part of the Dimensionless Helmholtz Energy Equation, Function of Temperature and Density}
\usage{
phi0(T, D)
}
\arguments{
\item{T}{Temperature [ K ]}

\item{D}{Density [ kg m-3 ]}
}
\value{
The Ideal-gas part of the Helmholtz Energy Equation: phi0 and an Error
     Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{phi0(T,D)} returns the Ideal-gas part of the
     dimensionless Helmholtz Energy Equation, phi0, for given T [K] and D [kg/m3].
}
\details{
This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
    in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
    Thermodynamic Properties of Ordinary Water Substance for General and Scientific
    Use (June 2014) developed by the International Association for the Properties of
    Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
    from the triple point to the pressure of 1000 MPa and temperature of 1273.
}
\examples{
T <- 500.
D <- 838.025
phi_0 <- phi0(T,D)
phi_0

}
