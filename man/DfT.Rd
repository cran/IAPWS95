% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooSatT.R
\name{DfT}
\alias{DfT}
\title{Saturated Liquid Density, Function of Temperature}
\usage{
DfT(T)
}
\arguments{
\item{T}{Temperature [ K ]}
}
\value{
The saturated liquid density: Df [ kg m-3 ] and an Error
     Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{DfT(T)} returns the saturated liquid density [kg m-3], 
    Df, for given T [K].
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
T <- 450.
Df <- DfT(T)
Df

}
