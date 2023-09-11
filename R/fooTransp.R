#' Thermal Conductivity, Function of Temperature and Density
#'
#' @description The function \code{CndTD(Temp,D,digits=9)} calculates the Thermal Conductivity,
#'     k [ W m-1 K-1 ] for given Temp [K] and D [kg/m3], returning the computed
#'      thermal conductivity and an error message if an error occur.
#'
#' @details This function calls a Fortran DLL that solves the equations developed by
#'     the International Association for the Properties of Water and Steam, valid from 
#'     the triple point to the pressure of 1000 MPa and temperature of 1173.15K. 
#'     \url{http://www.iapws.org/relguide/ThCond.html}
#'     
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Thermal Conductivity: k [ W m-1 K-1 ] and an Error message if necessary
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' Cond <- CndTD(Temp,D)
#' Cond
#' 
#' @export
#' 
CndTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CNDTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    warning(error)
  }
  return(round(res[[3]],digits = digits))
}

#' Dynamic Viscosity, Function of Temperature and Density
#'
#' @description The function \code{ViscTD(Temp,D,digits=9)} computes the Dynamic Viscosity
#'      [ Pa s ] for given Temp [K] and D [kg/m3], returning the computed
#'      viscosity and an error message, if an error occur. \link{errorCodes}
#'
#' @details This function calls a Fortran DLL that solves the equations developed by
#'     the International Association for the Properties of Water and Steam, valid from 
#'     the triple point to the pressure of 1000 MPa and temperature of 1173.15K.
#'      \url{http://www.iapws.org/relguide/viscosity.html}
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Dynamic viscosity: [ Pa s ] and an Error Message (if an error occur)
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' Vis <- ViscTD(Temp,D)
#' Vis
#' 
#' @export
#' 
ViscTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('VISCTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    warning(error)
  }
  return(round(res[[3]],digits = digits))
}

#' Kinematic Viscosity, Function of Temperature and Density
#'
#' @description The function \code{KViscTD(Temp,D,digits=9)} computes the Kinematic Viscosity
#'      [ m2 s-1 ] for given T [K] and D [kg/m3], returning the calculated
#'      viscosity and an error message, if an error occur. \link{errorCodes}
#'
#' @details This function calculates the Kinematic Viscosity that is the relation
#'      \code{ViscTD(D,Temp)/D}, valid from the triple point to the pressure of 1000 
#'      MPa and temperature of 1173.15K.
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Kinematic viscosity: [ m2 s-1 ] and an Error Message (if an error occur)
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' KVis <- KViscTD(Temp,D)
#' KVis
#' 
#' @export
#' 
KViscTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('KVISCTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    warning(error)
  }
  return(round(res[[3]],digits = digits))
}

#' Surface Tension, Function of Temperature
#'
#' @description The function \code{SigmaT(Temp,digits=9)} computes the Surface Tension [ mN m-1 ]
#'      for a given Temp [K], returning the calculated Surface Tension and an 
#'      error message, if an error occur. \link{errorCodes}
#'
#' @details This function calls a Fortran DLL that solves the equations developed by
#'     the International Association for the Properties of Water and Steam, valid from 
#'     the triple point to the critical temperature [ 273.13K to 647.096K].
#'      \url{http://www.iapws.org/relguide/Surf-H2O.html}
#' 
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The Surface Tension: Sigma [ mN m-1 ] and an Error Message (if an error occur)
#' 
#' @examples
#' Temp <- 500.
#' Sig <- SigmaT(Temp)
#' Sig
#' 
#' @export
#' 
SigmaT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('SigmaT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    warning(error)
  }
  return(round(res[[2]],digits = digits))
}

#' Prandt Number, Function of Temperature and Density
#'
#' @description The function \code{PrandtTD(Temp,D,digits=9)} computes the Prandt Number, i.e., 
#'      the product of the dynamic viscosity by the specific isobaric heat capacity,
#'      divided by the thermal conductivity of water for given T [K] and D [kg/m3]. 
#'
#' @details This function calls a Fortran DLL that computes the Prandt Number, valid from 
#'     the triple point to the pressure of 1000 MPa and temperature of 1173.15K. 
#' 
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Prandt Number: Pr [ - ]
#' @return Error message (if an error occur)
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' Pran <- PrandtTD(Temp,D)
#' Pran
#' 
#' @export
#' 
PrandtTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('PrandtTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    warning(error)
  }
  return(round(res[[3]],digits = digits))
}
