% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooOth.R
\name{DTs}
\alias{DTs}
\title{Density, Function of Temperature and Entropy}
\usage{
DTs(T, s)
}
\arguments{
\item{T}{Temperature [ K ]}

\item{s}{Entropy [ kJ kg-1 K-1 ]}
}
\value{
The Density: D [ kg m-3 ] and an Error Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{DTs(T,s)} returns the water density, D [ kg m-3 ],
     for given T [K] and s [ kJ k-1 K-1 ].
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
s <- 2.56690919
D_Ts <- DTs(T,s)
D_Ts

}
