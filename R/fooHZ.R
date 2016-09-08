#' Ideal-Gas part of the Dimensionless Helmholtz Energy Equation, Function of Temperature and Density
#'
#' @description The function \code{phi0(T,D)} returns the Ideal-gas part of the
#'      dimensionless Helmholtz Energy Equation, phi0, for given T [K] and D [kg/m3]
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
#' @return The Ideal-gas part of the Helmholtz Energy Equation: phi0 and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phi0(T,D)
#' 
#' @export
#' 
  phi0 <- function(T,D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('phi0TD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#  out <- list(Temperature=T, Density=D, phi0=res[[3]], ErrorCode=res[[4]])
  options(digits=9)
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[3]])
  }

#' First Derivative of the Ideal-Gas part of the Dimensionless Helmholtz Energy
#'      Equation with respect to Density, Function of Density
#'
#' @description The function \code{phi0D(D)} returns the First Derivative of the
#'     Ideal-gas part of the dimensionless Helmholtz Energy Equation for a given D [kg/m3]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m-3 ]
#' 
#' @return The First D Derivative of Ideal-gas part of the Helmholtz Energy: phi0D and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 838.025
#' phi0D(D)
#' 
#' @export
#' 
  phi0D <- function(D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('phi0DD', as.double(D), as.double(y), as.integer(icode))
#  out <- list(Density=D, phi0D=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 
  
#' Second Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density, Function of Density
#'
#' @description The function \code{phi0DD(D)} returns the Second Derivative of the
#'     Ideal-gas part of the dimensionless Helmholtz Energy Equation for a given D [kg/m3]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m-3 ]
#' 
#' @return The Second D Derivative of Ideal-gas part of the Helmholtz Energy: phi0DD and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 838.025
#' phi0DD(D)
#' 
#' @export
#' 
  phi0DD <- function(D) {
    y <- 0.
    icode <- 0
    res <- .Fortran('phi0DDD', as.double(D), as.double(y), as.integer(icode))
 #   out <- list(Density=D, phi0DD=res[[2]], ErrorCode=res[[3]])
    options(digits=9)
    if (res[[3]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
      print(error)
    }
#    class(out) <- "IAPWS95"
    print(res[[2]])
  } 
  
#' First Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{phi0T(T,D)} returns the First Derivative of the
#'     Ideal-gas Part of the dimensionless Helmholtz Energy Equation with respect to 
#'     Temperature, for given T [K] and D [kg/m3]
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
#' @return The First T Derivative of Ideal-gas part of the Helmholtz Energy: phi0T and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phi0T(T,D)
#' 
#' @export
#'  
   phi0T <- function(T,D) {
   y <- 0.
   icode <- 0
   res <- .Fortran('phi0TTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#   out <- list(Temperature=T, Density=D, phi0T=res[[3]], ErrorCode=res[[4]])
   options(digits=9)
   if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
   }
#   class(out) <- "IAPWS95"
   print(res[[3]])
 }

#' Second Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#'
#' @description The function \code{phi0TT(T,D)} returns the Second Derivative of the
#'     Ideal-gas Part of the Dimensionless Helmholtz Energy Equation with respect to 
#'     Temperature, for given T [K] and D [kg/m3]
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
#' @return The Second T Derivative of Ideal-gas part of the Helmholtz Energy: phi0TT and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phi0TT(T,D)
#' 
#' @export
#'  
   phi0TT <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phi0TTTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#     out <- list(Temperature=T, Density=D, phi0TT=res[[3]], ErrorCode=res[[4]])
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
 #    class(out) <- "IAPWS95"
     print(res[[3]])
   }
   
#' Second Derivative of the Ideal-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density and Temperature
#'
#' @description The function \code{phi0DT()} returns the Second Derivative of the
#'     Ideal-gas Part of the Dimensionless Helmholtz Energy Equation with respect to 
#'     Density and Temperature
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
#' @examples
#' phi0DT()
#' 
#' @export
#'  
  phi0DT <- function() {
     y <- 0.
     icode <- 0
     res <- .Fortran('phi0DT', as.double(y), as.integer(icode))
#    out <- list(phi0DT=res[[1]], ErrorCode=res[[2]])
     options(digits=9)
     if (res[[2]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[2]]),2])
       print(error)
     }
#     class(out) <- "IAPWS95"
     print(res[[1]])
   }
   
#' Residual-Gas Part of the Dimensionless Helmholtz Energy Equation, Function 
#'     of Temperature and Density
#'
#' @description The function \code{phir(T,D)} returns the Residual-Gas Part of the Dimensionless 
#'     Helmholtz Energy Equation for given T [K] and D [kg/m3]
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
#' @return The Residual-Gas Part of the Dimensionless Helmholtz Energy Equation: phir
#'      and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phir(T,D)
#' 
#' @export
#' 
   phir <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#     out <- list(Temperature=T, Density=D, phir=res[[3]], ErrorCode=res[[4]])
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
#     class(out) <- "IAPWS95"
     print(res[[3]])
   }
   
#' First Derivative of the Residual-Gas part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density, Function of Temperature and Density
#' 
#' @description The function \code{phirD(T,D)} returns the First Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation for given T [K] and D [kg/m3]
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
#' @return The First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation: phirD, and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phirD(T,D)
#' 
#' @export
#' 
   phirD <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRDTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#     out <- list(Temperature=T, Density=D, phirD=res[[3]], ErrorCode=res[[4]])
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
#     class(out) <- "IAPWS95"
     print(res[[3]])
   }
   
#' Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to Density, Function of Temperature and Density
#' 
#' @description The function \code{phirDD(T,D)} returns the Second Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation for given T [K] and D [kg/m3]
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
#' @return The Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation: phirDD, and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phirDD(T,D)
#' 
#' @export
#' 
   phirDD <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRDDTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#     out <- list(Temperature=T, Density=D, phirDD=res[[3]], ErrorCode=res[[4]])
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
#     class(out) <- "IAPWS95"
     print(res[[3]])
   }
   
#' First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#' 
#' @description The function \code{phirT(T,D)} returns the First Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation with respect to T,
#'     for given T [K] and D [kg/m3]
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
#' @return The First Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to T: phirT, and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phirT(T,D)
#' 
#' @export
#' 
  phirT <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRTTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#     out <- list(Temperature=T, Density=D, phirT=res[[3]], ErrorCode=res[[4]])
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
#     class(out) <- "IAPWS95"
     print(res[[3]])
   }
   
#' Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Temperature, Function of Temperature and Density
#' 
#' @description The function \code{phirTT(T,D)} returns the Second Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation with respect to T,
#'     for given T [K] and D [kg/m3]
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
#' @return The Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to T: phirTT, and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phirTT(T,D)
#' 
#' @export
#' 
  phirTT <- function(T,D) {
     y <- 0.
     icode <- 0
     res <- .Fortran('phiRTTTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#     out <- list(Temperature=T, Density=D, phirTT=res[[3]], ErrorCode=res[[4]])
     options(digits=9)
     if (res[[4]] != 0) { 
       error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
       print(error)
     }
#     class(out) <- "IAPWS95"
     print(res[[3]])
  }
  
#' Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz Energy Equation
#'     with respect to Density and Temperature, Function of Temperature and Density
#' 
#' @description The function \code{phirDT(T,D)} returns the Second Derivative of the 
#'     Residual-Gas Part of the Dimensionless Helmholtz Energy Equation with respect to D and T,
#'     for given T [K] and D [kg/m3]
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
#' @return The Second Derivative of the Residual-Gas Part of the Dimensionless Helmholtz 
#'     Energy Equation with respect to D and T: phirTT, and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' D <- 838.025
#' phirDT(T,D)
#' 
#' @export
#' 
  phirDT <- function(T,D) {
    y <- 0.
    icode <- 0
    res <- .Fortran('phiRDTTD', as.double(T), as.double(D), as.double(y), as.integer(icode))
#    out <- list(Temperature=T, Density=D, phirDT=res[[3]], ErrorCode=res[[4]])
    options(digits=9)
    if (res[[4]] != 0) { 
      error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
      print(error)
    }
 #   class(out) <- "IAPWS95"
    print(res[[3]])
  }
  