#' Density, Function of Temperature and Pressure
#'
#' @description The function \code{DTp(Temp,p,digits=9)} returns the water density, D [ kg m-3 ],
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Density: D [ kg m-3 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' D <- DTp(Temp,p)
#' D
#' 
#' @export
#' 
  DTp <- function(Temp,p,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
  }
  return(round(res[[3]],digits = digits))
  }

#' Helmholtz Free Energy, Function of Temperature and Pressure
#'
#' @description The function \code{fTp(Temp,p,digits=9)} returns the Helmholtz Free Energy, f [ kJ kg-1 ],
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Helmholtz Free Energy: f [ kJ kg-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' f <- fTp(Temp,p)
#' f
#' 
#' @export
#' 
 fTp <- function(Temp,p,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('fTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
  }
  return(round(res[[3]],digits = digits))
} 
 
#' Specific Enthalpy, Function of Temperature and Pressure
#'
#' @description The function \code{hTp(Temp,p,digits=9)} returns the Specific Enthalpy, h [ kJ kg-1 ],
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Enthalpy: h [ kJ kg-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' h <- hTp(Temp,p)
#' h
#' 
#' @export
#'  
 hTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('hTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Entropy, Function of Temperature and Pressure
#'
#' @description The function \code{sTp(Temp,p,digits=9)} returns the Specific Entropy, h [ kJ kg-1 K-1 ],
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Entropy: s [ kJ kg-1 K-1] and an Error message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' s <- sTp(Temp,p)
#' s
#' 
#' @export
#'  
 sTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('sTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Internal Energy, Function of Temperature and Pressure
#'
#' @description The function \code{uTp(Temp,p,digits=9)} returns the Specific Internal Energy, h [ kJ kg-1 ],
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Internal Energy: u [ kJ kg-1 ] and an Error message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' u <- uTp(Temp,p)
#' u
#' 
#' @export
#'  
 uTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('uTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }

#' Specific Isochoric Heat Capacity, Function of Temperature and Pressure
#'
#' @description The function \code{CvTp(Temp,p,digits=9)} returns the Specific Isochoric Heat Capacity,
#'      Cv [ kJ kg-1 K-1 ], for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Isochoric Heat Capacity: Cv [ kJ kg-1 K-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' Cv <- CvTp(Temp,p)
#' Cv
#' 
#' @export
#'  
 CvTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CvTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Isobaric Heat Capacity, Function of Temperature and Pressure
#'
#' @description The function \code{CpTp(Temp,p,digits=9)} returns the Specific Isobaric Heat Capacity,
#'      Cp [ kJ kg-1 K-1 ], for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{https://iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Isobaric Heat Capacity: Cp [ kJ kg-1 K-1 ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' Cp <- CpTp(Temp,p)
#' Cp
#' 
#' @export
#'  
 CpTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('CpTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Speed of Sound, Function of Temperature and Pressure
#'
#' @description The function \code{wTp(Temp,p,digits=9)} returns the Speed of Sound, [ m s-1 ], 
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Speed of Sound: w [ m s-1 ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' w <- wTp(Temp,p)
#' w
#' 
#' @export
#'  
 wTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('wTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Gibbs Energy, Function of Temperature and Pressure
#'
#' @description The function \code{GibbsTp(Temp,p,digits=9)} returns the Specific Gibbs Energy, [ MPa ], 
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specific Gibbs Energy: Gibbs [ MPa ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' Gibbs <- GibbsTp(Temp,p)
#' Gibbs
#' 
#' @export
#'  
 GibbsTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('GibbsTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Fugacity, Function of Temperature and Pressure
#'
#' @description The function \code{FugaTp(Temp,p,digits=9)} returns the Fugacity, [ MPa ], 
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Fugacity: Fuga [ MPa ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' Fuga <- FugaTp(Temp,p)
#' Fuga
#' 
#' @export
#'  
 FugaTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('FugaTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }
 
#' Specific Volume, Function of Temperature and Pressure
#'
#' @description The function \code{vTp(Temp,p,digits=9)} returns the Specific Volume, [ m3 kg-1 ], 
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
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Specifiv Volume: v [ m3 kg-1 ] and an 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
 #' Temp <- 500.
 #' p <- 10.0003858
 #' v <- vTp(Temp,p)
 #' v
 #' 
 #' @export
#'  
 vTp <- function(Temp,p,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('vTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }