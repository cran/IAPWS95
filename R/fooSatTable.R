#' Table of Saturation Densities, Enthalpies and Entropies, Function of Temperature
#'
#' @description The function \code{satTabT(T1, T2, dT)} returns a table of 
#'     threee saturation properties for two phases: Density [kg/m3], 
#'     Enthalpy [kJ kg-1] and Entropy [kJ kg K-1] for a Temperature interval, T1:T2 [K]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T1 Initial Temperature [K]
#' @param T2 Final Temperature [K]
#' @param dT Temperature increment [K]
#' 
#' @return A table of saturation D, h and s, function of T
#' 
#' @examples
#' T1 <- 275.
#' T2 <- 450.
#' dT <- 5.
#' satTabT(T1, T2, dT)
#' 
#' T1 <- 300.
#' T2 <- 500.
#' dT <- 10.
#' TabT <- satTabT(T1, T2, dT)
#' 
#' @export
#' 
satTabT <- function(T1, T2, dT) {
  Tv <- seq(from=T1,  to=T2, by=dT)
  n  <- length(Tv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y[,3] <- y[,1]
  y[,4] <- y[,1]
  y[,5] <- y[,1]
  y[,6] <- y[,1]
  y[,7] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('satDhsofT', as.integer(n), Tv, y)
#  out <- list(Temperature=T, SatTab=as.data.frame(res[[2]]), ErrorCode=res[[3]])
  colnames(res[[3]]) <- c("T", "Df", "Dg", "hf", "hg", "sf", "sg")
  out <- res[[3]]
#  options(digits=9)
#  if (res[[3]] != 0) { 
#    out[[4]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
#  }
#  class(out) <- "IAPWS95"
  print(out)
} 

#' Table of Saturation Densities, Enthalpies and Entropies, Function of Pressure
#'
#' @description The function \code{satTabp(p1, p2, dp)} returns a table of 
#'     threee saturation properties for two phases: Density [kg/m3], 
#'     Enthalpy [kJ kg-1] and Entropy [kJ kg K-1] for a Pressure interval, p1:p2 [MPa]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p1 Initial Pressure [MPa]
#' @param p2 Final Pressure [MPa]
#' @param dp Pressure increment [MPa]
#' 
#' @return A table of saturation D, h and s, function of p
#' 
#' @examples
#' p1 <- 1.0
#' p2 <- 10.
#' dp <- 0.5
#' satTabp(p1, p2, dp)
#' 
#' p1 <- 0.1
#' p2 <- 10.
#' dp <- 0.5
#' Tabp <- satTabp(p1, p2, dp)
#' 
#' @export
#' 
satTabp <- function(p1, p2, dp) {
  pv <- seq(from=p1,  to=p2, by=dp)
  n  <- length(pv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y[,3] <- y[,1]
  y[,4] <- y[,1]
  y[,5] <- y[,1]
  y[,6] <- y[,1]
  y[,7] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('satDhsofp', as.integer(n), pv, y)
  colnames(res[[3]]) <- c("p", "Df", "Dg", "hf", "hg", "sf", "sg")
  out <- res[[3]]
  #  options(digits=9)
  #  if (res[[3]] != 0) { 
  #    out[[4]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
  #  }
  #  class(out) <- "IAPWS95"
  print(out)
} 

#' Table of Saturation Volumes, Enthalpies and Entropies, Function of of Temperature
#'
#' @description The function \code{satTabvT(T1, T2, dT)} returns a table of 
#'     threee saturation properties for two phases: Specific Volume [ m3 kg-1 ], 
#'     Enthalpy [kJ kg-1] and Entropy [kJ kg K-1] for a Temperature interval, T1:T2 [K]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T1 Initial Temperature [K]
#' @param T2 Final Temperature [K]
#' @param dT Temperature increment [K]
#' 
#' @return A table of saturation v, h and s, function of T
#' 
#' @examples
#' T1 <- 275.
#' T2 <- 450.
#' dT <- 5.
#' satTabvT(T1, T2, dT)
#' 
#' T1 <- 300.
#' T2 <- 500.
#' dT <- 10.
#' TabT <- satTabvT(T1, T2, dT)
#' 
#' @export
#' 
satTabvT <- function(T1, T2, dT) {
  Tv <- seq(from=T1,  to=T2, by=dT)
  n  <- length(Tv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y[,3] <- y[,1]
  y[,4] <- y[,1]
  y[,5] <- y[,1]
  y[,6] <- y[,1]
  y[,7] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('satvhsofT', as.integer(n), Tv, y)
  #  out <- list(Temperature=T, SatTab=as.data.frame(res[[2]]), ErrorCode=res[[3]])
  colnames(res[[3]]) <- c("T", "vf", "vg", "hf", "hg", "sf", "sg")
  out <- res[[3]]
  #  options(digits=9)
  #  if (res[[3]] != 0) { 
  #    out[[4]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
  #  }
  #  class(out) <- "IAPWS95"
  print(out)
}

#' Table of Saturation Volumes, Enthalpies and Entropies, Function of Pressure
#'
#' @description The function \code{satTabvp(p1, p2, dp)} returns a table of 
#'     threee saturation properties for two phases: Specific Volume [ m3 kg-1 ], 
#'     Enthalpy [kJ kg-1] and Entropy [kJ kg K-1] for a Pressure interval, p1:p2 [MPa]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p1 Initial Pressure [MPa]
#' @param p2 Final Pressure [MPa]
#' @param dp Pressure increment [MPa]
#' 
#' @return A table of saturation v, h and s, function of p
#' 
#' @examples
#' p1 <- 1.0
#' p2 <- 10.
#' dp <- 0.5
#' satTabvp(p1, p2, dp)
#' 
#' p1 <- 0.1
#' p2 <- 10.
#' dp <- 0.5
#' Tabp <- satTabvp(p1, p2, dp)
#' 
#' @export
#' 
satTabvp <- function(p1, p2, dp) {
  pv <- seq(from=p1,  to=p2, by=dp)
  n  <- length(pv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y[,3] <- y[,1]
  y[,4] <- y[,1]
  y[,5] <- y[,1]
  y[,6] <- y[,1]
  y[,7] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('satvhsofp', as.integer(n), pv, y)
  colnames(res[[3]]) <- c("p", "vf", "vg", "hf", "hg", "sf", "sg")
  out <- res[[3]]
  #  options(digits=9)
  #  if (res[[3]] != 0) { 
  #    out[[4]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
  #  }
  #  class(out) <- "IAPWS95"
  print(out)
} 

#' Table of Saturation Pressures, Function of Temperature
#'
#' @description The function \code{satTabpT(T1, T2, dT)} returns a table of 
#'     saturation pressures [MPa] for a Temperature interval, T1:T2 [K]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T1 Initial Temperature [K]
#' @param T2 Final Temperature [K]
#' @param dT Temperature increment [K]
#' 
#' @return A table of saturation pressures, function of T
#' 
#' @examples
#' T1 <- 275.
#' T2 <- 450.
#' dT <- 5.
#' satTabpT(T1, T2, dT)
#' 
#' T1 <- 300.
#' T2 <- 500.
#' dT <- 10.
#' TabT <- satTabpT(T1, T2, dT)
#' 
#' @export
#' 
satTabpT <- function(T1, T2, dT) {
  Tv <- seq(from=T1,  to=T2, by=dT)
  n  <- length(Tv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('pSatTab', as.integer(n), Tv, y)
  #  out <- list(Temperature=T, SatTab=as.data.frame(res[[2]]), ErrorCode=res[[3]])
  colnames(res[[3]]) <- c("T", "pSat")
  out <- res[[3]]
  #  options(digits=9)
  #  if (res[[3]] != 0) { 
  #    out[[4]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
  #  }
  #  class(out) <- "IAPWS95"
  print(out)
}

#' Table of Saturation Temperatures, Function of Pressure
#'
#' @description The function \code{satTabTp(p1, p2, dp)} returns a table of 
#'     Saturation Temperatures [K] for a Pressure interval, p1:p2 [MPa]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p1 Initial Pressure [MPa]
#' @param p2 Final Pressure [MPa]
#' @param dp Pressure increment [MPa]
#' 
#' @return A Table of Saturation Temperatures, function of p
#' 
#' @examples
#' p1 <- 1.0
#' p2 <- 10.
#' dp <- 0.5
#' satTabTp(p1, p2, dp)
#' 
#' p1 <- 0.1
#' p2 <- 10.
#' dp <- 0.5
#' Tabp <- satTabTp(p1, p2, dp)
#' 
#' @export
#' 
satTabTp <- function(p1, p2, dp) {
  pv <- seq(from=p1,  to=p2, by=dp)
  n  <- length(pv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('TSatTab', as.integer(n), pv, y)
  #  out <- list(Temperature=T, SatTab=as.data.frame(res[[2]]), ErrorCode=res[[3]])
  colnames(res[[3]]) <- c("p", "TSat")
  out <- res[[3]]
  #  options(digits=9)
  #  if (res[[3]] != 0) { 
  #    out[[4]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
  #  }
  #  class(out) <- "IAPWS95"
  print(out)
}

#' Table of Saturation Liquid Phase Enthalpies, Function of Temperature
#'
#' @description The function \code{satTabhT(T1, T2, dT)} returns a table of 
#'     saturation liquid enthalpies [kJ kg-1 K-1] for a Temperature interval, T1:T2 [K]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T1 Initial Temperature [K]
#' @param T2 Final Temperature [K]
#' @param dT Temperature increment [K]
#' 
#' @return A table of saturation fluid enthalpies, function of T
#' 
#' @examples
#' T1 <- 275.
#' T2 <- 450.
#' dT <- 5.
#' satTabhT(T1, T2, dT)
#' 
#' T1 <- 300.
#' T2 <- 500.
#' dT <- 10.
#' TabT <- satTabhT(T1, T2, dT)
#' 
#' @export
#' 
satTabhT <- function(T1, T2, dT) {
  Tv <- seq(from=T1,  to=T2, by=dT)
  n  <- length(Tv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('hfTTab', as.integer(n), Tv, y)
  #  out <- list(Temperature=T, SatTab=as.data.frame(res[[2]]), ErrorCode=res[[3]])
  colnames(res[[3]]) <- c("T", "hf")
  out <- res[[3]]
  #  options(digits=9)
  #  if (res[[3]] != 0) { 
  #    out[[4]] <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
  #  }
  #  class(out) <- "IAPWS95"
  print(out)
}
