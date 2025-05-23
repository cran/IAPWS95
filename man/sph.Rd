% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fooOth.R
\name{sph}
\alias{sph}
\title{Entropy, Function of Pressure and Enthalpy}
\usage{
sph(p, h, digits = 9)
}
\arguments{
\item{p}{Pressure [ MPa ]}

\item{h}{Enthalpy [ kJ kg-1 ]}

\item{digits}{Digits of results (optional)}
}
\value{
The Entropy: s [ kJ kg-1 K-1 ] and an Error Message (if an error occur: \link{errorCodes})
}
\description{
The function \code{sph(p,h,digits=9)} returns the water entropy, s [ kJ kg-1 K-1 ],
     for given p [MPa] and h [ kJ k-1 ].
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
p <- 10.0003858
h <- 977.181624
s_ph <- sph(p,h)
s_ph

}
