#' Pressure, Function of Temperature and Density
#'
#' @description The function \code{pTD(T,D)} returns the water pressure, p [ MPa ],
#'      for given T [K] and D [kg/m3], returning also an error message, 
#'      if any error occur. \link{errorCodes}
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @useDynLib IAPWS95
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Pressure: p [ MPa ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' pTD(T,D)
#' 
#' T <- 647.096
#' D <- 322.
#' pTD(T,D)
#' 
#' @export
#' 
  pTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('pTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Density=D, Pressure=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[3]])
  }

#' Helmholtz Free Energy, Function of Temperature and Density
#'
#' @description The function \code{fTD(T,D)} returns the Helmholtz Free Energy, f [ kJ kg-1 ],
#'      for given T [K] and D [kg/m3].
#'      
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Helmholtz Free Energy: f [ kJ kg-1 ] and an Error Message 
#'     (if an error occur:  \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' fTD(T,D)
#' 
#' @export
#' 
 fTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('fTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Density=D, f=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[3]])
} 
 
#' Specific Enthalpy, Function of Temperature and Density
#'
#' @description The function \code{hTD(T,D)} returns the Specific Enthalpy, h [ kJ kg-1 ],
#'      for given T [K] and D [kg/m3].
#'      
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Specific Enthalpy: h [ kJ kg-1 ] and an Error Message (if an error occur:
#'       \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' hTD(T,D)
#' 
#' @export
#'  
 hTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('hTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, Enthalpy=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Entropy, Function of Temperature and Density
#'
#' @description The function \code{sTD(T,D)} returns the Specific Entropy, h [ kJ kg-1 k-1 ],
#'      for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Specific Entropy: s [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' sTD(T,D)
#' 
#' @export
#'  
 sTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('sTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, Entropy=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Internal Energy, Function of Temperature and Density
#'
#' @description The function \code{uTD(T,D)} returns the Specific Internal Energy, h [ kJ kg-1 ],
#'      for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Specific Internal Energy: u [ kJ kg-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' uTD(T,D)
#' 
#' @export
#'  
 uTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('uTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, InternalEnergy=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Isochoric Heat Capacity, Function of Temperature and Density
#'
#' @description The function \code{CvTD(T,D)} returns the Specific Isochoric Heat Capacity, 
#'     Cv [ kJ kg-1 K-1 ], for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Specific Isochoric Heat Capacity: Cv [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' CvTD(T,D)
#' 
#' @export
#'  
 CvTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CvTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, IsochoricHeatCapacity=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Isobaric Heat Capacity, Function of Temperature and Density
#'
#' @description The function \code{CpTD(T,D)} returns the Specific Isobaric Heat Capacity, 
#'     Cp [ kJ kg-1 K-1 ], for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Specific Isobaric Heat Capacity: Cp [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' CpTD(T,D)
#' 
#' @export
#'  
 CpTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CpTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, IsobaricHeatCapacity=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Speed of Sound, Function of Temperature and Density
#'
#' @description The function \code{wTD(T,D)} returns the Speed of Sound in water, 
#'     w [ m s-1 ], for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Speed of Sound: w [ m s-1 ]
#' @return Error message (if an error occur)
#' 
#' @return The Speed of Sound: w [ m s-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' wTD(T,D)
#' 
#' T <- 500.
#' D <- 0.435
#' wTD(T,D)
#' 
#' @export
#'  
 wTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('wTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, SoundSpeed=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Compressibility Factor, Function of Temperature and Density
#'
#' @description The function \code{ZTD(T,D)} returns the Compressibility Factor, 
#'     Z [ - ], for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Compressibility Factor and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' ZTD(T,D)
#' 
#' @export
#'  
 ZTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('ZTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, CompFactor=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Joule-Thomson Coefficient, Function of Temperature and Density
#'
#' @description The function \code{JTcTD(T,D)} returns the Joule-Thomson coefficient 
#'     for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     The temperature change produced during a Joule-Thomson expansion is quantified by 
#'     the Joule-Thomson coefficient, which may be positive (cooling) or negative (heating).
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Joule-Thomson coefficient and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' JTcTD(T,D)
#' 
#' @export
#'  
 JTcTD <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('JTcTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, JouleThomson=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
 #  class(out) <- "IAPWS95"
   print(res[[3]])
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
 