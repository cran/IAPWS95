% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooDeriv.R
\name{ThrcTD}
\alias{ThrcTD}
\title{Isothermal Throttling Coefficient, Function of Tenoerature and Density}
\usage{
ThrcTD(T, D)
}
\arguments{
\item{T}{Temperature [ K ]}

\item{D}{Density [ kg m-3 ]}
}
\value{
The Isothermal Throttling Coefficient: Thrc [ kJ kg-1 MPa-1 ] and an Error
     Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{ThrcTD(T,D)} returns the Isothermal Throttling Coefficient, 
    Thrc, for given T [K] and D [kg m-3].
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
Thrc <- ThrcTD(T,D)
Thrc

}
