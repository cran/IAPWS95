% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooDeriv.R
\name{dpdDTD}
\alias{dpdDTD}
\title{Pressure Derivative with respect to Density, Function of Temperature and Density}
\usage{
dpdDTD(Temp, D, digits = 9)
}
\arguments{
\item{Temp}{Temperature [ K ]}

\item{D}{Density [ kg m-3 ]}

\item{digits}{Digits of results (optional)}
}
\value{
The pressure derivative with respect to D: dp/dD [ MPa kg-1 m3  ] and an Error
     Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{dpdDTD(Temp,D,digits=9)} returns the pressure derivative with 
    respect to Density, dpdD, for given T [K] and D [kg m-3].
}
\details{
This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
    in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
    Thermodynamic Properties of Ordinary Water Substance for General and Scientific
    Use (June 2014) developed by the International Association for the Properties of
    Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
    from the triple point to the pressure of 1000 MPa and temperature of 1273.
}
\examples{
Temp <- 500.
D <- 838.025
dpdD <- dpdDTD(Temp,D)
dpdD

}
