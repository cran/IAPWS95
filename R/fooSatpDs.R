#' Saturation Pressure, Function of Entropy
#'
#' @description The function \code{pSats(s)} returns the saturation pressure [MPa], 
#'     pSat, for given s [kJ kg-1 K-1]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param s Entropy [ kJ kg-1 K-1 ]
#' 
#' @return The saturation pressure: pSat [ MPa ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' s <- 2.10865845
#' pSats(s)
#' 
#' @export
#' 
pSats <- function(s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('pSats', as.double(s), as.double(y), as.integer(icode))
#  out <- list(Entropy=s, pSat=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 

#' Saturation Pressure, Function of Density
#'
#' @description The function \code{pSatD(D)} returns the saturation pressure [MPa], 
#'     pSat, for given D [ kg m-3 ]: it may have two different values!
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m-3]
#' 
#' @return The first saturation pressure: pSat_1 [ MPa ]
#' @return The second saturation pressure: pSat_2 [ MPa ]
#' @return An Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 890.341250
#' pSatD(D)
#' 
#' D <- 999.887406
#' pSatD(D)
#' 
#' @export
#' 
pSatD <- function(D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('pSatD', as.double(D), as.double(y), as.double(y), as.integer(icode))
  out <- list(pSa_1t=res[[2]], pSat_2=res[[3]])
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(out)
} 

#' Saturation Temperature, Function of Entropy
#'
#' @description The function \code{TSats(s)} returns the temperature [K], 
#'     TSat, for given s [kJ kg-1 K-1]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param s Entropy [kJ kg-1 K-1]
#' 
#' @return The Saturation Temperature: Tsat [ K ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' s <- 2.10865845
#' TSats(s)
#' 
#' @export
#' 
TSats <- function(s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('TSats', as.double(s), as.double(y), as.integer(icode))
#  out <- list(Entropy=s, TSat=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 

#' Saturation Temperature, Function of Density
#'
#' @description The function \code{TsatD(D)} returns the temperature [K], 
#'     TSat, for given D [ kg m-3 ]: it may have two different values!
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
#' @return The first saturation Temperature: TSat_1 [ K ]
#' @return The second saturation pressure: TSat_2 [ K ]
#' @return An Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 890.341250
#' TSatD(D)
#' 
#' D <- 999.887406
#' TSatD(D)
#' 
#' @export
#' 
TSatD <- function(D) {
  y <- 0.
  icode <- 0
  res <- .Fortran('TSatD', as.double(D), as.double(y), as.double(y), as.integer(icode))
  out <- list(TSat_1=res[[2]], TSat_2=res[[3]])
  options(digits=5)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(out)
} 

#' Saturation Temperature, Function of pressure
#'
#' @description The function \code{TSatp(p)} returns the temperature [K], 
#'     TSat, for given p [ MPa ]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' 
#' @return The Saturation Temperature: Tsat [ K ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 0.932203564
#' TSatp(p)
#' 
#' @export
#' 
TSatp <- function(p) {
  y <- 0.
  icode <- 0
  res <- .Fortran('TSatp', as.double(p), as.double(y), as.integer(icode))
#  out <- list(Pressure=p, TSat=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 

#' Saturated Liquid Density, Funtion of Pressure
#'
#' @description The function \code{Dfp(p)} returns the saturated liquid density [kg m-3], 
#'     Df, for given p [ MPa ]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' 
#' @return The saturated liquid density: Df [kg m-3] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 0.932203564
#' Dfp(p)
#' 
#' @export
#' 
Dfp <- function(p) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Dfp', as.double(p), as.double(y), as.integer(icode))
#  out <- list(Pressure=p, Df=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 

#' Saturated Gas Density, Funtion of Pressure
#'
#' @description The function \code{Dgp(p)} returns the saturated gas density [kg m-3], 
#'     Dg, for given p [ MPa ]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' 
#' @return The saturated gas density: Dg [kg m-3] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 0.932203564
#' Dgp(p)
#' 
#' @export
#' 
Dgp <- function(p) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Dgp', as.double(p), as.double(y), as.integer(icode))
#  out <- list(Pressure=p, Dg=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 

#' Saturated Liquid Density, Function of Entropy
#'
#' @description The function \code{Dfs(s)} returns the saturated liquid density [kg m-3], 
#'     Df, for given s [kJ kg-1 K-1]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param s Entropy [kJ kg-1 K-1]
#' 
#' @return The saturated Liquid density: Df [kg m-3] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' s <- 2.10865845
#' Dfs(s)
#' 
#' @export
#' 
Dfs <- function(s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Dfs', as.double(s), as.double(y), as.integer(icode))
#  out <- list(Entropy=s, Df=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 

#' Saturated Gas Density, Function of Entropy
#'
#' @description The function \code{Dgs(s)} returns the saturated gas density [kg m-3], 
#'     Dg, for given s [kJ kg-1 K-1]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param s Entropy [kJ kg-1 K-1]
#' 
#' @return The saturated Gas density: Dg [kg m-3] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' s <- 6.60921221
#' Dgs(s)
#' 
#' s <- 2.10865845
#' Dgs(s)
#' 
#' @export
#' 
Dgs <- function(s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Dgs', as.double(s), as.double(y), as.integer(icode))
#  out <- list(Entropy=s, Dg=res[[2]], ErrorCode=res[[3]])
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
#  class(out) <- "IAPWS95"
  print(res[[2]])
} 
