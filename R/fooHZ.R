#' Ideal-Gas part of the Dimensionless Helmholtz Energy Equation, Function of Temperature and Density
#'
#' @description The function \code{phi0(Temp,D,digits=9)} returns the Ideal-gas part of the
#'      dimensionless Helmholtz Energy Equation, phi0, for given Temp [K] and D [kg/m3].
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
#' @return The Ideal-gas part of the Helmholtz Energy Equation: phi0 and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phi_0 <- phi0(Temp,D)
#' phi_0
#' 
#' @export
#' 
  phi0 <- function(Temp,D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('phi0TD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
  }
  return(round(res[[3]],digits=digits))
  }

#' First Derivative of the Ideal-Gas part of the Dimensionless Helmholtz Energy
#'      Equation with respect to Density, Function of Density
#'
#' @description The function \code{phi0D(D,digits=9)} returns the First Derivative of the
#'     Ideal-gas part of the dimensionless Helmholtz Energy Equation for a given D [kg/m3]. 
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m-3 ]  
#' @param digits Digits of results (optional)
#' 
#' @return The First D Derivative of Ideal-gas part of the Helmholtz Energy: phi0D and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 838.025
#' phi_0 <- phi0D(D)
#' phi_0
#'
#' @export
#' 
  phi0D <- function(D,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('phi0DD', as.double(D), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    warning(error)
  }
  return(round(res[[2]],digits=digits))
} 
  
#' Second Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density, Function of Density
#'
#' @description The function \code{phi0DD(D,digits=9)} returns the Second Derivative of the
#'     Ideal-gas part of the dimensionless Helmholtz Energy Equation for a given D [kg/m3].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m-3 ]
#' @param digits Digits of results (optional)
#' 
#' @return The Second D Derivative of Ideal-gas part of the Helmholtz Energy: phi0DD and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 838.025
#' phi_0 <- phi0DD(D)
#' phi_0
#' 
#' @export
#' 
  phi0DD <- function(D,digits=9) {
    y <- 0.
    icode <- 0
    res <- .Fortran('phi0DDD', as.double(D), as.double(y), as.integer(icode))
    if (res[[3]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
      warning(error)
    }
    return(round(res[[2]],digits=digits))
  } 
  
#' First Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{phi0T(Temp,D,digits=9)} returns the First Derivative of the
#'     Ideal-gas Part of the dimensionless Helmholtz Energy Equation with respect to 
#'     Temperature, for given Temp [K] and D [kg/m3].
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
#' @return The First Temp Derivative of Ideal-gas part of the Helmholtz Energy: phi0T and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phi0_T <- phi0T(Temp,D)
#' phi0_T
#' 
#' @export
#'  
   phi0T <- function(Temp,D,digits=9) {
   y <- 0.
   icode <- 0
   res <- .Fortran('phi0TTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     warning(error)
   }
   return(round(res[[3]],digits = digits))
 }

#' Second Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{phi0TT(Temp,D,digits =9)} returns the Second Derivative of the
#'     Ideal-gas Part of the Dimensionless Helmholtz Energy Equation with respect to 
#'     Temperature, for given Temp [K] and D [kg/m3].
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
#' @return The Second Temp Derivative of Ideal-gas part of the Helmholtz Energy: phi0TT and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phi0_TT <- phi0TT(Temp,D)
#' phi0_TT
#' 
#' @export
#'  
   phi0TT <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phi0TTTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits = digits))
   }
   
#' Second Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density and Temperature
#'
#' @description The function \code{phi0DT(digits=9)} returns the Second Derivative of the
#'     Ideal-gas Part of the Dimensionless Helmholtz Energy Equation with respect to 
#'     Density and Temperature.
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @return The Second DT Derivative of Ideal-gas Part of the Helmholtz Energy: phi0DT and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @param digits Digits of results (optional)
#' 
#' @examples
#' phi0_DT <- phi0DT()
#' phi0_DT
#' 
#' @export
#'  
  phi0DT <- function(digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phi0DT', as.double(y), as.integer(icode))
     if (res[[2]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[2]]),2])
       warning(error)
     }
     return(round(res[[1]],digits = digits))
  }
   
#' Residual-Gas Part of the Dimensionless Helmholtz Energy Equation, Function 
#'     of Temperature and Density
#'
#' @description The function \code{phir(Temp,D,digits=9)} returns the Residual-Gas Part of the Dimensionless 
#'     Helmholtz Energy Equation for given Temp [K] and D [kg/m3].
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
#' @return The Residual-Gas Part of the Dimensionless Helmholtz Energy Equation: phir
#'      and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phir_TD <- phir(Temp,D)
#' phir_TD
#' 
#' @export
#' 
   phir <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits = digits))
   }
   
#' First Derivative of the Residual-Gas part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density, Function of Temperature and Density
#' 
#' @description The function \code{phirD(Temp,D,digits=9)} returns the First Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation for given Temp [K] and D [kg/m3].
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
#' @return The First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation: phirD, and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phir_D <- phirD(T,D)
#' phir_D
#' 
#' @export
#' 
   phirD <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRDTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits))
   }
   
#' Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to Density, Function of Temperature and Density
#' 
#' @description The function \code{phirDD(Temp,D,digits=9)} returns the Second Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation for given Temp [K] and D [kg/m3].
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
#' @return The Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation: phirDD, and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phir_DD <- phirDD(Temp,D)
#' phir_DD
#' 
#' @export
#' 
   phirDD <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRDDTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits = digits))
   }
   
#' First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#' 
#' @description The function \code{phirT(Temp,D,digits=9)} returns the First Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation with respect to Temp,
#'     for given Temp [K] and D [kg/m3].
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
#' @return The First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to Temp: phirT, and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phir_T <- phirT(Temp,D)
#' phir_T
#' 
#' @export
#' 
  phirT <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRTTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits = digits))
  }
   
#' Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#' 
#' @description The function \code{phirTT(Temp,D,digits=9)} returns the Second Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation with respect to Temp,
#'     for given Temp [K] and D [kg/m3].
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
#' @return The Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to T: phirTT, and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phir_TT <- phirTT(Temp,D)
#' phir_TT
#' 
#' @export
#' 
  phirTT <- function(Temp,D,digits=9) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRTTTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       warning(error)
     }
     return(round(res[[3]],digits = digits))
  }
  
#' Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density and Temperature, Function of Temperature and Density
#' 
#' @description The function \code{phirDT(Temp,D,digits=9)} returns the Second Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation with respect to D and Temp,
#'     for given Temp [K] and D [kg/m3].
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
#' @return The Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to D and Temp: phirTT, and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 500.
#' D <- 838.025
#' phir_DT <- phirDT(Temp,D)
#' phir_DT
#' 
#' @export
#' 
  phirDT <- function(Temp,D,digits) {
    y <- 0.
    icode <- 0
    res <- .Fortran('phiRDTTD', as.double(Temp), as.double(D), as.double(y), as.integer(icode))
    if (res[[4]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
      warning(error)
    }
    return(round(res[[3]],digits = digits))
  }
  