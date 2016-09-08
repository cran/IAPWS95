#' Density, Function of Temperature and Pressure
#'
#' @description The function \code{DTp(T,p)} returns the water density, D [ kg m-3 ],
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Density: D [ kg m-3 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' DTp(T,p)
#' 
#' @export
#' 
  DTp <- function(T,p) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Pressure=p, Density=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
#     print(out[[5]])
  }
#  class(out) <- "IAPWS95"
  print(res[[3]])
  }

#' Helmholtz Free Energy, Function of Temperature and Pressure
#'
#' @description The function \code{fTp(T,p)} returns the Helmholtz Free Energy, f [ kJ kg-1 ],
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Helmholtz Free Energy: f [ kJ kg-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' fTp(T,p)
#' 
#' @export
#' 
 fTp <- function(T,p) {
  y <- 0.
  icode <- 0
  res <- .Fortran('fTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Pressure=p, f=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[3]])
} 
 
#' Specific Enthalpy, Function of Temperature and Pressure
#'
#' @description The function \code{hTp(T,p)} returns the Specific Enthalpy, h [ kJ kg-1 ],
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Specific Enthalpy: h [ kJ kg-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' hTp(T,p)
#' 
#' @export
#'  
 hTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('hTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, Enthalpy=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Entropy, Function of Temperature and Pressure
#'
#' @description The function \code{sTp(T,p)} returns the Specific Entropy, h [ kJ kg-1 K-1 ],
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Specific Entropy: s [ kJ kg-1 K-1] and an Error message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' sTp(T,p)
#' 
#' @export
#'  
 sTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('sTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, Entropy=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Internal Energy, Function of Temperature and Pressure
#'
#' @description The function \code{uTp(T,p)} returns the Specific Internal Energy, h [ kJ kg-1 ],
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Specific Internal Energy: u [ kJ kg-1 ] and an Error message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' uTp(T,p)
#' 
#' @export
#'  
 uTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('uTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, InternalEnergy=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }

#' Specific Isochoric Heat Capacity, Function of Temperature and Pressure
#'
#' @description The function \code{CvTp(T,p)} returns the Specific Isochoric Heat Capacity,
#'      Cv [ kJ kg-1 K-1 ], for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' @param p Pressure [ MPa ]
#' 
#' @return The Specific Isochoric Heat Capacity: Cv [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' CvTp(T,p)
#' 
#' @export
#'  
 CvTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CvTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, IsochoricHeatCapacity=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Isobaric Heat Capacity, Function of Temperature and Pressure
#'
#' @description The function \code{CpTp(T,p)} returns the Specific Isobaric Heat Capacity,
#'      Cp [ kJ kg-1 K-1 ], for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' @param p Pressure [ MPa ]
#' 
#' @return The Specific Isobaric Heat Capacity: Cp [ kJ kg-1 K-1 ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' CpTp(T,p)
#' 
#' @export
#'  
 CpTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CpTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, IsobaricHeatCapacity=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Speed of Sound, Function of Temperature and Pressure
#'
#' @description The function \code{wTp(T,p)} returns the Speed of Sound, [ m s-1 ], 
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Speed of Sound: w [ m s-1 ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' wTp(T,p)
#' 
#' @export
#'  
 wTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('wTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
 #  out <- list(Temperature=T, Density=D, SoundSpeed=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
 #  class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Gibbs Energy, Function of Temperature and Pressure
#'
#' @description The function \code{GibbsTp(T,p)} returns the Specific Gibbs Energy, [ MPa ], 
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Specific Gibbs Energy: Gibbs [ MPa ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' GibbsTp(T,p)
#' 
#' @export
#'  
 GibbsTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('GibbsTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, GibbsEnergy=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Fugacity, Function of Temperature and Pressure
#'
#' @description The function \code{FugaTp(T,p)} returns the Fugacity, [ MPa ], 
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Fugacityy: Fuga [ MPa ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' FugaTp(T,p)
#' 
#' @export
#'  
 FugaTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('FugaTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, Fugacity=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }
 
#' Specific Volume, Function of Temperature and Pressure
#'
#' @description The function \code{vTp(T,p)} returns the Specific Volume, [ m3 kg-1 ], 
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
#' @param p Pressure [ MPa ]
#' 
#' @return The Specifiv Volume: v [ m3 kg-1 ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' vTp(T,p)
#' 
#' @export
#'  
 vTp <- function(T,p) {
   y <- 0.
   icode <- 0
   res <- .Fortran('vTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, SpecVolume=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }