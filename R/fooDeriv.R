#' Pressure Derivative with Respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{dpdTTD(Temp,D,digits=9)} returns the pressure derivative with 
#'     respect to Temperature, dpdT, for given Temp [K] and D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The pressure derivative with respect to Temp: dp/dTemp [ MPa K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' dpdTemp <- dpdTTD(Temp,D)
#' dpdTemp
#' 
#' @export
#' 
  dpdTTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('dpdTTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
  }
  return(round(res[[3]],digits=digits))
  }

#' Pressure Derivative with respect to Temperature, Function of Temperature and Pressure
#'
#' @description The function \code{dpdTTp(Temp,p,digits=9)} returns the pressure derivative with 
#'     respect to Temperature, dpdTemp, for given Temp [K] and p [MPa].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The pressure derivative with respect to Temp: dp/dTemp [ MPa K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' dpdTemp <- dpdTTp(Temp,p)
#' dpdTemp
#' 
#' @export
#' 
  dpdTTp <- function(Temp,p,digits=9) {
    y <- 0.
    icode <- 0
    res <- .Fortran('dpdTTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
    if (res[[4]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
      warning(error)
    }
    return(round(res[[3]],digits=digits))
  }
  
#' Pressure Derivative with respect to Density, Function of Temperature and Density
#'
#' @description The function \code{dpdDTD(Temp,D,digits=9)} returns the pressure derivative with 
#'     respect to Density, dpdD, for given T [K] and D [kg m-3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' @return The pressure derivative with respect to D: dp/dD [ MPa kg-1 m3  ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' dpdD <- dpdDTD(Temp,D)
#' dpdD
#' 
#' @export
#' 
   dpdDTD <- function(Temp,D,digits=9) {
    y <- 0.
    icode <- 0
    res <- .Fortran('dpdDTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
    if (res[[4]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
      warning(error)
    }
    return(round(res[[3]],digits=digits))
  }
  
#' Pressure Derivative with respect to Density, Function of Temperature and Pressure
#'
#' @description The function \code{dpdDTp(Temp,p)} returns the pressure derivative with 
#'     respect to Density, dpdD, for given Temp [K] and p [MPa].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The pressure derivative with respect to d: dp/dD [ MPa kg-1 m3 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' dpdD <- dpdDTp(Temp,p)
#' dpdD
#' 
#' @export
#' 
   dpdDTp <- function(Temp,p,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('dpdDTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits=digits))
   }

#' Density Derivative with respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{dDdTTD(Temp,D,digits=9)} returns the pressure derivative with 
#'     respect to Density, dpdD, for given Temp [K] and D [kg m-3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Density Derivative with respect to T: dD/dTemp [ kg m-3 K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' dDdTemp <- dDdTTD(Temp,D)
#' dDdTemp
#' 
#' @export
#' 
   dDdTTD <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('dDdTTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits=digits))
   }

#' Density Derivative with respect to Temperature, Function of Temperature and Pressure
#'
#' @description The function \code{dDdTTp(Temp,p,digits=9)} returns the Density derivative with 
#'     respect to Temperature, dDdTemp, for given Temp [K] and p [MPa].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param digits Digits of results (optional)
#' 
#' @return The Density derivative with respect to Temp: dD/dTemp [ kg m-3 K-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' p <- 10.0003858
#' dDdTemp <- dDdTTp(Temp,p)
#' dDdTemp
#' 
#' @export
#' 
   dDdTTp <- function(Temp,p,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('dDdTTp', as.double(Temp), as.double(p), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits=digits))
   }

#' Isothermal Throttling Coefficient, Function of Temperature and Density
#'
#' @description The function \code{ThrcTD(Temp,D,digits=9)} returns the Isothermal Throttling Coefficient, 
#'     Thrc, for given Temp [K] and D [kg m-3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Isothermal Throttling Coefficient: Thrc [ kJ kg-1 MPa-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' Thrc <- ThrcTD(Temp,D)
#' Thrc
#' 
#' @export
#' 
   ThrcTD <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('ThrcTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits=digits))
   }

#' Isothermal Compressibility, Function of Temperature and Density
#'
#' @description The function \code{KapaTD(Temp,D,disgits=9)} returns the Isothermal Compressibility, Kapa, 
#'     for given Temp [K] and D [kg m-3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Isothermal Compressibility: Kapa [ MPa-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' Kapa <- KapaTD(Temp,D)
#' Kapa
#' 
#' @export
#' 
KapaTD <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('kapaTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    warning(error)
  }
  return(round(res[[3]],digits=digits))
}

   