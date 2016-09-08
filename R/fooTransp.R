#' Thermal Conductivity, Function of Temperature and Density
#'
#' @description The function \code{CndTD(T,D)} calculates the Thermal Conductivity,
#'     k [ W m-1 K-1 ] for given T [K] and D [kg/m3], returning the calculated
#'      thermal conductivity and an error message, if an error occur. \link{errorCodes}
#'
#' @details This function calls a Fortran DLL that solves the equations developed by
#'     the International Association for the Properties of Water and Steam, valid from 
#'     the triple point to the pressure of 1000 MPa and temperature of 1173.15K. \url{http://www.iapws.org/relguide/ThCond.html}
#'     
#' @param T - Temperature [ K ]
#' @param D - Density [ kg m-3 ]
#' 
#' @return The calculated Thermal Conductivity: k [ W m-1 K-1 ] and an Error message
#'      (if an error occur)
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' CndTD(T,D)
#' 
#' T <- 0.
#' D <- 200.
#' CndTD(T,D)
#' 
#' @export
#' 
CndTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CNDTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Density=D, Thermal_Cond=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
#    out[[5]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  print(res[[3]])
#  class(out) <- "IAPWS95"
#  print(out)
}

#' Dynamic Viscosity, Function of Temperature and Density
#'
#' @description The function \code{ViscTD(T,D)} calculates the Dynamic Viscosity
#'      [ Pa s ] for given T [K] and D [kg/m3], returning the calculated
#'      viscosity and an error message, if an error occur. \link{errorCodes}
#'
#' @details This function calls a Fortran DLL that solves the equations developed by
#'     the International Association for the Properties of Water and Steam, valid from 
#'     the triple point to the pressure of 1000 MPa and temperature of 1173.15K.
#'      \url{http://www.iapws.org/relguide/viscosity.html}
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Dynamic viscosity: [ Pa s ] and an Error Message (if an error occur)
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' ViscTD(T,D)
#' 
#' T <- 500.
#' D <- 0.
#' ViscTD(T,D)
#' 
#' @export
#' 
ViscTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('VISCTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Density=D, Dyn_Visc=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
#    out[[5]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  print(res[[3]])
#  class(out) <- "IAPWS95"
#  print(out)
}

#' Kinematic Viscosity, Function of Temperature and Density
#'
#' @description The function \code{KViscTD(T,D)} calculates the Kinematic Viscosity
#'      [ m2 s-1 ] for given T [K] and D [kg/m3], returning the calculated
#'      viscosity and an error message, if an error occur. \link{errorCodes}
#'
#' @details This function calculates the Kinematic Viscosity that is the relation
#'      \code{ViscTD(D,T)/D}, valid from the triple point to the pressure of 1000 
#'      MPa and temperature of 1173.15K.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Kinematic viscosity: [ m2 s-1 ] and an Error Message (if an error occur)
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' KViscTD(T,D)
#' 
#' T <- 500.
#' D <- 0.
#' KViscTD(T,D)
#' 
#' @export
#' 
KViscTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('KVISCTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Density=D, Kin_Visc=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  print(res[[3]])
#  class(out) <- "IAPWS95"
#  print(out)
}

#' Surface Tension, Function of Temperature
#'
#' @description The function \code{SigmaT(T)} calculates the Surface Tension [ mN m-1 ]
#'      for a given T [K], returning the calculated Surface Tension and an 
#'      error message, if an error occur. \link{errorCodes}
#'
#' @details This function calls a Fortran DLL that solves the equations developed by
#'     the International Association for the Properties of Water and Steam, valid from 
#'     the triple point to the critical temperature [ 273.13K to 647.096K].
#'      \url{http://www.iapws.org/relguide/Surf-H2O.html}
#' 
#' @param T Temperature [ K ]
#' 
#' @return The Surface Tension: Sigma [ mN m-1 ] and an Error Message (if an error occur)
#' 
#' @examples
#' T <- 500.
#' SigmaT(T)
#' 
#' T <- 700.
#' SigmaT(T)
#' 
#' @export
#' 
SigmaT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('SigmaT', as.double(T), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Sigma=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  print(res[[2]])
#  class(out) <- "IAPWS95"
#  print(out)
}

#' Prandt Number, Function of Temperature and Density
#'
#' @description The function \code{PrandtTD(T,D)} computes the Prandt Number, i.e., 
#'      the product of the dynamic viscosity by the specific isobaric heat capacity,
#'      divided by the thermal conductivity of water for given T [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that computes the Prandt Number, valid from 
#'     the triple point to the pressure of 1000 MPa and temperature of 1173.15K.
#' 
#' @param T Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' 
#' @return The Prandt Number: Pr [ - ]
#' @return Error message (if an error occur)
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' PrandtTD(T,D)
#' 
#' @export
#' 
PrandtTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('PrandtTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Density=D, Prandt=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  print(res[[3]])
#  class(out) <- "IAPWS95"
#  print(out)
}
