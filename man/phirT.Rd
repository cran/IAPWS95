% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooHZ.R
\name{phirT}
\alias{phirT}
\title{First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz Energy Equation
    with respect to Temperature, Function of Temperature and Density}
\usage{
phirT(Temp, D, digits = 9)
}
\arguments{
\item{Temp}{Temperature [ K ]}

\item{D}{Density [ kg m-3 ]}

\item{digits}{Digits of results (optional)}
}
\value{
The First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
    Energy Equation with respect to Temp: phirT, and an Error Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{phirT(Temp,D,digits=9)} returns the First Derivative of the 
    Residual-Gas Part of the Dimensionless Helmholtz Energy Equation with respect to Temp,
    for given Temp [K] and D [kg/m3].
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
phir_T <- phirT(Temp,D)
phir_T

}
