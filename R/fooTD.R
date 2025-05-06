#' Pressure, Function of Temperature and Density
#'
#' @description The function \code{pTD(T,D,digits=9)} returns the water pressure, p [ MPa ],
#'      for given Temp [K] and D [kg/m3], returning also an error message, if any error occur.
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @useDynLib IAPWS95
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Pressure: p [ MPa ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' p <- pTD(Temp,D)
#' p
#' 
#' Temp <- 647.096
#' D <- 322.
#' p <- pTD(Temp,D)
#' p
#' 
#' @export
#' 
  pTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('pTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
  }
  return(round(res[[3]],digits = digits))
  }

#' Helmholtz Free Energy, Function of Temperature and Density
#'
#' @description The function \code{fTD(T,D,digits=9)} returns the Helmholtz Free Energy, f [ kJ kg-1 ],
#'      for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Helmholtz Free Energy: f [ kJ kg-1 ] and an Error Message if an error occur:
#'   \link{errorCodes}
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' f <- fTD(Temp,D)
#' f
#' @export
#' 
 fTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('fTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
  }
  return(round(res[[3]],digits = digits))
} 
 
#' Specific Enthalpy, Function of Temperature and Density
#'
#' @description The function \code{hTD(Temp,D,digits=9)} returns the Specific Enthalpy, h [ kJ kg-1 ],
#'      for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Enthalpy: h [ kJ kg-1 ] and an Error Message (if an error occur:
#'       \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' h <- hTD(Temp,D)
#' h
#' 
#' @export
#'  
 hTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('hTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Entropy, Function of Temperature and Density
#'
#' @description The function \code{sTD(Temp,D,digits=9)} returns the Specific Entropy, h [ kJ kg-1 k-1 ],
#'      for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Entropy: s [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' s <- sTD(Temp,D)
#' s
#' 
#' @export
#'  
 sTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('sTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Internal Energy, Function of Temperature and Density
#'
#' @description The function \code{uTD(Temp,D,digits=9)} returns the Specific Internal Energy, h [ kJ kg-1 ],
#'      for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Internal Energy: u [ kJ kg-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' u <- uTD(Temp,D)
#' u
#' 
#' @export
#'  
 uTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('uTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Isochoric Heat Capacity, Function of Temperature and Density
#'
#' @description The function \code{CvTD(Temp,D,digits=9)} returns the Specific Isochoric Heat Capacity, 
#'     Cv [ kJ kg-1 K-1 ], for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Isochoric Heat Capacity: Cv [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' Cv <- CvTD(Temp,D)
#' Cv
#' 
#' @export
#'  
 CvTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CvTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Isobaric Heat Capacity, Function of Temperature and Density
#'
#' @description The function \code{CpTD(Temp,D,digits=9)} returns the Specific Isobaric Heat Capacity, 
#'     Cp [ kJ kg-1 K-1 ], for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Isobaric Heat Capacity: Cp [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' Cp <- CpTD(Temp,D)
#' Cp
#' 
#' @export
#'  
 CpTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CpTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits=digits))
 }
 
#' Speed of Sound, Function of Temperature and Density
#'
#' @description The function \code{wTD(Temp,D,digits=9)} returns the Speed of Sound in water, 
#'     w [ m s-1 ], for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Speed of Sound: w [ m s-1 ]
#' @return Error message (if an error occur)
#' 
#' @return The Speed of Sound: w [ m s-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 0.435
#' w <- wTD(Temp,D)
#' w
#' 
#' @export
#'  
 wTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('wTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Compressibility Factor, Function of Temperature and Density
#'
#' @description The function \code{ZTD(Temp,D,digits=9)} returns the Compressibility Factor, 
#'     Z [ - ], for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Compressibility Factor and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' z <- ZTD(Temp,D)
#' z
#' 
#' @export
#'  
 ZTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('ZTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Joule-Thomson Coefficient, Function of Temperature and Density
#'
#' @description The function \code{JTcTD(Temp,D,digits=9)} returns the Joule-Thomson coefficient 
#'     for given Temp [K] and D [kg/m3].
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     The temperature change produced during a Joule-Thomson expansion is quantified by 
#'     the Joule-Thomson coefficient, which may be positive (cooling) or negative (heating).
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Joule-Thomson coefficient and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' JT <- JTcTD(Temp,D)
#' JT
#' 
#' @export
#'  
 JTcTD <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('JTcTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Error Codes
#' 
#' Error codes due values out of validity range, incorrect inputs,
#'     and/or convergence issues
#' 
# #' @format 
#'  
#' @source errorCodes.rda
#' 
  "errorCodes"
# globalVariables("errorCodes", "IAPWS95", add = TRUE)
 globalVariables("errorCodes")
 