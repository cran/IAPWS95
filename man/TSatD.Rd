% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooSatpDs.R
\name{TSatD}
\alias{TSatD}
\title{Saturation Temperature, Function of Density}
\usage{
TSatD(D, digits = 9)
}
\arguments{
\item{D}{Density [ kg m-3 ]}

\item{digits}{Digits of results (optional)}
}
\value{
The first saturation Temperature: TSat_1 [ K ]

The second saturation pressure: TSat_2 [ K ]

An Error Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{TsatD(D,digits=9)} returns the temperature [K], 
    TSat, for given D [ kg m-3 ]: it may have two different values!
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
D <- 890.341250
T_Sat <- TSatD(D)
T_Sat

D <- 999.887406
T_Sat <- TSatD(D)
T_Sat

}
