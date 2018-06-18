#' Table of Densities, Function of Temperature for Fixed Pressure
#'
#' @description The function \code{DTpcteTab(T1, T2, dT, p)} returns a table of 
#'     densities [kg m-3] for a fixed p [MPa] within a range of T [K]: T1:T2 [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T1 first Temperature value[ K ]
#' @param T2 final Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param dT Temperature increment [ K ]
#' 
#' @return A table of Densities for fixed p and a T Interval: T1:T2.
#' 
#' @examples
#' T1 <- 275.
#' T2 <- 450.
#' dT <- 5.
#' p <- 5.
#' TabD <- DTpcteTab(T1, T2, dT, p)
#' TabD
#' 
#' T1 <- 300.
#' T2 <- 500.
#' dT <- 10.
#' p <- 10.
#' TabD <- DTpcteTab(T1, T2, dT, p)
#' TabD
#' 
#' @export
#' 
DTpcteTab <- function(T1, T2, dT, p) {
  Tv <- seq(from=T1,  to=T2, by=dT)
  n  <- length(Tv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('DTpcteTab', as.integer(n), p, Tv, y)
  
  DTab <- as.data.frame(res[[4]])
  DTab$p <- rep(p,nrow(DTab))
  DTab <- DTab[,c(3,1,2)]
  colnames(DTab) <- c("p [MPa]", "T [K]", "D [kg m-3]")
  return(DTab)
}

#' Table of Enthalpies, Function of Temperature and Fixed Pressure
#'
#' @description The function \code{hTpcteTab(T1, T2, dT, p)} returns a table of 
#'     enthalpies [kJ kg-1] for a fixed p [MPa] within a range of T [K]: T1:T2 [K]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T1 first Temperature value [ K ]
#' @param T2 final Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param dT Temperature increment [ K ]
#' 
#' @return A table of Enthalpies for fixed p and a T Interval: T1:T2.
#' 
#' @examples
#' T1 <- 275.
#' T2 <- 450.
#' dT <- 5.
#' p <- 5.
#'Tabh <-  hTpcteTab(T1, T2, dT, p)
#'Tabh
#' 
#' T1 <- 300.
#' T2 <- 500.
#' dT <- 10.
#' p <- 10.
#' Tabh <- hTpcteTab(T1, T2, dT, p)
#' Tabh
#' 
#' @export
#' 
hTpcteTab <- function(T1, T2, dT, p) {
  Tv <- seq(from=T1,  to=T2, by=dT)
  n  <- length(Tv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('hTpcteTab', as.integer(n), p, Tv, y)
  
  hTab <- as.data.frame(res[[4]])
  hTab$p <- rep(p,nrow(hTab))
  hTab <- hTab[,c(3,1,2)]
  colnames(hTab) <- c("p [MPa]", "T [K]", "h [kJ kg-1]")
  return(hTab)
}

#' Table of Entropies, Function of Temperature for a Fixed Pressure
#'
#' @description The function \code{sTpcteTab(T1, T2, dT, p)} returns a table of 
#'     entropies [kJ kg-1 K-1] for a fixed p [MPa] within a range of T [K]: T1:T2 [K]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T1 first Temperature value [ K ]
#' @param T2 final Temperature [ K ]
#' @param p Pressure [ MPa ]
#' @param dT Temperature increment [ K ]
#' 
#' @return A table of Entropies for fixed p and a T Interval: T1:T2.
#' 
#' @examples
#' T1 <- 275.
#' T2 <- 450.
#' dT <- 5.
#' p <- 5.
#' Tabs <- sTpcteTab(T1, T2, dT, p)
#' Tabs
#' 
#' T1 <- 300.
#' T2 <- 500.
#' dT <- 10.
#' p <- 10.
#' Tabs <- sTpcteTab(T1, T2, dT, p)
#' Tabs
#' 
#' @export
#' 
sTpcteTab <- function(T1, T2, dT, p) {
  Tv <- seq(from=T1,  to=T2, by=dT)
  n  <- length(Tv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('sTpcteTab', as.integer(n), p, Tv, y)
  
  sTab <- as.data.frame(res[[4]])
  sTab$p <- rep(p,nrow(sTab))
  sTab <- sTab[,c(3,1,2)]
  colnames(sTab) <- c("p [MPa]", "T [K]", "s [kJ kg-1 m K-1]")
  return(sTab)
}

#' Table of Densities, Function of Pressure for a Fixed Temperature
#'
#' This function provides a table of the densities [kg m-3] for a given T [K] within a range of p [MPa]
#'
#' @description The function \code{DpTcteTab(p1, p2, dp, T)} returns a table of 
#'     Densities [kg m-3] for a fixed T [K] within a range of p [MPa]: p1:p2 [MPa]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p1 first pressure value [ MPa ]
#' @param p2 final pressure [ MPa ]
#' @param dp Pressure increment [ MPa ]
#' @param T Temperature [ K ]
#' 
#' @return A table of Densities for fixed T and a p Interval: p1:p2.
#' 
#' @examples
#' p1 <- 1.0
#' p2 <- 10.
#' dp <- 1.
#' T <- 500.
#' TabD <- DpTcteTab(p1, p2, dp, T)
#' TabD
#' 
#' p1 <- 10.
#' p2 <- 100.
#' dp <- 10.
#' T <- 450.
#' TabD <- DpTcteTab(p1, p2, dp, T)
#' TabD
#' 
#' @export
#' 
DpTcteTab <- function(p1, p2, dp, T) {
  pv <- seq(from=p1,  to=p2, by=dp)
  n  <- length(pv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('DpTcteTab', as.integer(n), T, pv, y)
  
  DTab <- as.data.frame(res[[4]])
  DTab$T<- rep(T,nrow(DTab))
  DTab <- DTab[,c(3,1,2)]
  colnames(DTab) <- c("T [K]", "p [K]", "D [kg m-3]")
  return(DTab)
}

#' Table of Enthalpies, Function of Pressure for Fixed Temperature
#'
#' @description The function \code{hpTcteTab(p1, p2, dp, T)} returns a table of 
#'     Enthalpies [kJ kg-1] for a fixed T [K] within a range of p [MPa]: p1:p2 [MPa]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p1 first pressure value [ MPa ]
#' @param p2 final pressure [ MPa ]
#' @param dp Pressure increment [ MPa ]
#' @param T Temperature [ K ]
#' 
#' @return A table of Enthalpies for fixed T and a p Interval: p1:p2.
#' 
#' @examples
#' p1 <- 1.0
#' p2 <- 10.
#' dp <- 1.
#' T <- 500.
#' Tabh <- hpTcteTab(p1, p2, dp, T)
#' Tabh
#' 
#' p1 <- 10.
#' p2 <- 100.
#' dp <- 10.
#' T <- 450.
#' Tabh <- hpTcteTab(p1, p2, dp, T)
#' Tabh
#' 
#' @export
#' 
hpTcteTab <- function(p1, p2, dp, T) {
  pv <- seq(from=p1,  to=p2, by=dp)
  n  <- length(pv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('hpTcteTab', as.integer(n), T, pv, y)
  
  hTab <- as.data.frame(res[[4]])
  hTab$T<- rep(T,nrow(hTab))
  hTab <- hTab[,c(3,1,2)]
  colnames(hTab) <- c("T [K]", "p [K]", "h [kJ kg-1]")
  return(hTab)
}

#' Table of Entropies, Function of Pressure for Fixed Temperature
#'
#' @description The function \code{spTcteTab(p1, p2, dp, T)} returns a table of 
#'     Entropies [kJ kg-1 K-1] for a fixed T [K] within a range of p [MPa]: p1:p2 [MPa]
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p1 "initial"first pressure value [ MPa ]
#' @param p2 final pressure [ MPa ]
#' @param dp Pressure increment [ MPa ]
#' @param T Temperature [ K ]
#' 
#' @return A table of Entropies for fixed T and a p Interval: p1:p2.
#' 
#' @examples
#' p1 <- 1.0
#' p2 <- 10.
#' dp <- 1.
#' T <- 500.
#' Tabs <- spTcteTab(p1, p2, dp, T)
#' Tabs
#' 
#' p1 <- 10.
#' p2 <- 100.
#' dp <- 10.
#' T <- 450.
#' Tabs <- spTcteTab(p1, p2, dp, T)
#' Tabs
#' 
#' @export
#' 
spTcteTab <- function(p1, p2, dp, T) {
  pv <- seq(from=p1,  to=p2, by=dp)
  n  <- length(pv)
  y  <- seq(from=1., to=n, by=1.)
  y  <- as.data.frame(y)
  y[,2] <- y[,1]
  y <- as.matrix(y)
  icode <- 0
  res <- .Fortran('spTcteTab', as.integer(n), T, pv, y)
  
  sTab <- as.data.frame(res[[4]])
  sTab$T<- rep(T,nrow(sTab))
  sTab <- sTab[,c(3,1,2)]
  colnames(sTab) <- c("T [K]", "p [K]", "s [kJ kg-1 K-1]")
  return(sTab)
}
