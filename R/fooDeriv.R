#' Pressure Derivative with Respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{dpdTTD(T,D)} returns the pressure derivative with 
#'     respect to Temperature, dpdT, for given T [K] and D [kg/m3].
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
#' @return The pressure derivative with respect to T: dp/dT [ MPa K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' dpdT <- dpdTTD(T,D)
#' dpdT
#' 
#' @export
#' 
  dpdTTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('dpdTTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
  }
  return(res[[3]])
  }

#' Pressure Derivative with respect to Temperature, Function of Temperature and Pressure
#'
#' @description The function \code{dpdTTp(T,p)} returns the pressure derivative with 
#'     respect to Temperature, dpdT, for given T [K] and p [MPa].
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
#' @return The pressure derivative with respect to T: dp/dT [ MPa K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' dpdT <- dpdTTp(T,p)
#' dpdT
#' 
#' @export
#' 
  dpdTTp <- function(T,p) {
    y <- 0.
    icode <- 0
    res <- .Fortran('dpdTTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
    options(digits=9)
    if (res[[4]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
      print(error)
    }
    return(res[[3]])
  }
  
#' Pressure Derivative with respect to Density, Function of Temperature and Density
#'
#' @description The function \code{dpdDTD(T,D)} returns the pressure derivative with 
#'     respect to Density, dpdD, for given T [K] and D [kg m-3].
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
#' @return The pressure derivative with respect to D: dp/dD [ MPa kg-1 m3  ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' dpdD <- dpdDTD(T,D)
#' dpdD
#' 
#' @export
#' 
   dpdDTD <- function(T,D) {
    y <- 0.
    icode <- 0
    res <- .Fortran('dpdDTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
    options(digits=9)
    if (res[[4]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
      print(error)
    }
    return(res[[3]])
  }
  
#' Pressure Derivative with respect to Density, Function of Temperature and Pressure
#'
#' @description The function \code{dpdDTp(T,p)} returns the pressure derivative with 
#'     respect to Density, dpdD, for given T [K] and p [MPa].
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
#' @return The pressure derivative with respect to d: dp/dD [ MPa kg-1 m3 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' dpdD <- dpdDTp(T,p)
#' dpdD
#' 
#' @export
#' 
   dpdDTp <- function(T,p) {
     y <- 0.
     icode <- 0
     res <- .Fortran('dpdDTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
     return(res[[3]])
   }

#' Density Derivative with respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{dDdTTD(T,D)} returns the pressure derivative with 
#'     respect to Density, dpdD, for given T [K] and D [kg m-3].
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
#' @return The Density Derivative with respect to T: dD/dT [ kg m-3 K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' dDdT <- dDdTTD(T,D)
#' dDdT
#' 
#' @export
#' 
   dDdTTD <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('dDdTTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
     return(res[[3]])
   }

#' Density Derivative with respect to Temperature, Function of Temperature and Pressure
#'
#' @description The function \code{dDdTTp(T,p)} returns the Density derivative with 
#'     respect to Temperature, dDdT, for given T [K] and p [MPa].
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
#' @return The Density derivative with respect to T: dD/dT [ kg m-3 K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' p <- 10.0003858
#' dDdT <- dDdTTp(T,p)
#' dDdT
#' 
#' @export
#' 
   dDdTTp <- function(T,p) {
     y <- 0.
     icode <- 0
     res <- .Fortran('dDdTTp', as.double(T), as.double(p), as.double(y), as.integer(icode))
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
     return(res[[3]])
   }

#' Isothermal Throttling Coefficient, Function of Tenoerature and Density
#'
#' @description The function \code{ThrcTD(T,D)} returns the Isothermal Throttling Coefficient, 
#'     Thrc, for given T [K] and D [kg m-3].
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
#' @return The Isothermal Throttling Coefficient: Thrc [ kJ kg-1 MPa-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' Thrc <- ThrcTD(T,D)
#' Thrc
#' 
#' @export
#' 
   ThrcTD <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('ThrcTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
     return(res[[3]])
   }

#' Isothermal Compressibility, Function of Temperature and Density
#'
#' @description The function \code{KapaTD(T,D)} returns the Isothermal Compressibility, Kapa, 
#'     for given T [K] and D [kg m-3].
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
#' @return The Isothermal Compressibility: Kapa [ MPa-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' Kapa <- KapaTD(T,D)
#' Kapa
#' 
#' @export
#' 
KapaTD <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('kapaTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

   