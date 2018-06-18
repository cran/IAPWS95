#' Water Critical Pressure
#'
#' @description This function \code{pCrit()} returns the water critical pressure [MPa].
#' 
#' @return The Water Critical Pressure: pc [MPa]
#' 
#' @examples
#' pc <- pCrit()
#' pc
#' 
#' @export
#' 
  pCrit <- function() {
  pCrit <- as.double(22.064)
  return(pCrit)
  }

#' Water Critical Temperature
#'
#'  @description The function \code{TCrit()} returns the water critical temperature [K].
#' 
#' @return The Water Critical Temperature: Tc [K]
#' 
#' @examples
#' Tc <- TCrit()
#' Tc
#' 
#' @export
#' 
TCrit <- function() {
  TCrit <- as.double(647.096)
  return(TCrit)
}

#' Water Critical Density
#'
#' @description The function \code{DCrit()} returns the water density at the critical point [kg m-3].
#'
#' @return The Water Critical Density: Dc [kg m-3]
#' 
#' @examples
#' DC <- DCrit()
#' DC
#' 
#' @export
#' 
DCrit <- function() {
  DCrit <- as.double(322.00)
  return(DCrit)
}

#' Water Critical Enthalpy
#'
#'  @description The function \code{hCrit()} returns the water enthalpy at the critical point [kJ kg-1].
#'
#' @return The Water Critical Enthalpy: hc [ kJ kg-1 ]
#' 
#' @examples
#' hC <- hCrit()
#' hC
#' 
#' @export
#' 
hCrit <- function() {
  hCrit <- as.double(2084.25625591)
  return(hCrit)
}

#' Water Critical Entropy
#'
#' @description The function \code{sCrit()} returns the entropy at the critical point [kJ k-1 K-1 ].
#'
#' @return The Water Critical Entropy: sc [ kJ kg-1 K-1 ]
#' 
#' @examples
#' sC <- sCrit()
#' sC
#' 
#' @export
#' 
sCrit <- function() {
  sCrit <- as.double(4.4069618924)
  return(sCrit)
}

#' Water Temperature at Triple Point
#'
#' @description The function \code{TTr()} returns the Water Temperature at Triple Point [K]
#' 
#' @return The Triple Point Temperature: TTr [ K ]
#' 
#' @examples
#' Ttrip <- TTr()
#' Ttrip
#' 
#' @export
#' 
TTr <- function() {
  TTr <- as.double(273.16)
  return(TTr)
}

#' Water Pressure at Triple Point
#'
#' @description The function \code{pTr()} returns the Water Pressure at Triple Point [MPa].
#' 
#' @return The Triple Point Pressure: pTr [ MPa ]
#' 
#' @examples
#' pTrip <- pTr()
#' pTrip
#' 
#' @export
#' 
pTr <- function() {
  pTr <- as.double(0.6116547711e-03)
  return(pTr)
}

#' Liquid Water Density at Triple Point
#'
#' @description The function \code{DfTr()} returns the Water Liquid Density at Triple Point.
#'
#' @return Triple Point Liquid Density: DfTr [ kg m-3 ]
#' 
#' @examples
#' DfTrip <- DfTr()
#' DfTrip
#' 
#' @export
#' 
DfTr <- function() {
  DfTr <- as.double(0.999792520186e+03)
  return(DfTr)
}

#' Water Gas Density at Triple Point
#'
#' @description The function \code{DgTr()} returns the Water Gas Density at Triple Point.
#'
#' @return Triple Gas Density: DgTr [ kg m-3 ]
#'
#' @examples
#' DgTrip <- DgTr()
#' DgTrip
#' 
#' @export
#' 
DgTr <- function() {
  DgTr <- as.double(0.485457572553e-02)
  return(DgTr)
}

#' Liquid Water Entropy at Triple Point
#'
#' @description The function \code{sfTr()} returns the Water Liquid Entropy at Triple Point.
#'
#' @return Triple Point Liquid Entropy: sfTr [ kJ kg-1 K-1]
#' 
#' @examples
#' sfTrip <- sfTr()
#' sfTrip
#' 
#' @export
#' 
sfTr <- function() {
  sfTr <- as.double(0.0)
  return(sfTr)
}

#' Water Gas Entropy at Triple Point
#'
#' @description The function \code{sgTr()} returns the Water Gas Entropy at Triple Point.
#'
#' @return Triple Point Gas Entropy: sgTr [ kJ kg-1 K-1]
#' 
#' @examples
#' sgTrip <- sgTr()
#' sgTrip
#' 
#' @export
#' 
sgTr <- function() {
  sgTr <- as.double(9.1554934093)
  return(sgTr)
}

#' Water Specific Gas Constant
#'
#' @description The function \code{Rwater()} returns the Water Specific Gas Constant.
#'
#' @return Water Specific Gas Constant: R [ K-1 ]
#' 
#' @examples
#' Rw <- Rwater()
#' Rw
#' 
#' @export
#' 
Rwater <- function() {
  Rwater <- as.double(0.461518050)
  return(Rwater)
}
