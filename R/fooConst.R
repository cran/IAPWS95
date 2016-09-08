#' Water Critical Pressure
#'
#' @description This function \code{pCrit()} returns the water critical pressure [MPa]
#' 
#' @return The Water Critical Pressure: pc [MPa]
#' 
#' @examples
#' pCrit()
#' 
#' @export
#' 
  pCrit <- function() {
  pCrit <- as.double(22.064)
#  class(pCrit) <- "IAPWS95"
  print(pCrit)
  }

#' Water Critical Temperature
#'
#'  @description The function \code{TCrit()} returns the water critical temperature [K]
#'
#' @return The Water Critical Temperature: Tc [K]
#' 
#' @examples
#' TCrit()
#' 
#' @export
#' 
TCrit <- function() {
  TCrit <- as.double(647.096)
  #  class(TCrit) <- "IAPWS95"
  print(TCrit)
}

#' Water Critical Density
#'
#' @description The function \code{DCrit()} returns the water density at the critical point [kg m-3]
#'
#' @return The Water Critical Density: Dc [kg m-3]
#' 
#' @examples
#' DCrit()
#' 
#' @export
#' 
DCrit <- function() {
  DCrit <- as.double(322.00)
  #  class(DCrit) <- "IAPWS95"
  print(DCrit)
}

#' Water Critical Enthalpy
#'
#'  @description The function \code{hCrit()} returns the water enthalpy at the critical point [kJ kg-1]
#'
#' @return The Water Critical Enthalpy: hc [ kJ kg-1 ]
#' 
#' @examples
#' hCrit()
#' 
#' @export
#' 
hCrit <- function() {
  hCrit <- as.double(2084.25625591)
  #  class(hCrit) <- "IAPWS95"
  print(hCrit)
}

#' Water Critical Entropy
#'
#' @description The function \code{sCrit()} returns the entropy at the critical point [kJ k-1 K-1 ]
#'
#' @return The Water Critical Entropy: sc [ kJ kg-1 K-1 ]
#' 
#' @examples
#' sCrit()
#' 
#' @export
#' 
sCrit <- function() {
  sCrit <- as.double(4.4069618924)
  #  class(sCrit) <- "IAPWS95"
  print(sCrit)
}

#' Water Temperature at Triple Point
#'
#' @description The function \code{TTr()} returns the Water Temperature at Triple Point [K]
#'
#' @return The Triple Point Temperature: TTr [ K ]
#' 
#' @examples
#' TTr()
#' 
#' @export
#' 
TTr <- function() {
  TTr <- as.double(273.16)
  #  class(TTr) <- "IAPWS95"
  print(TTr)
}

#' Water Pressure at Triple Point
#'
#' @description The function \code{pTr()} returns the Water Pressure at Triple Point [MPa]
#'
#' @return The Triple Point Pressure: pTr [ MPa ]
#' 
#' @examples
#' pTr()
#' 
#' @export
#' 
pTr <- function() {
  pTr <- as.double(0.6116547711e-03)
  #  class(pTr) <- "IAPWS95"
  print(pTr)
}

#' Liquid Water Density at Triple Point
#'
#' @description The function \code{DfTr()} returns the Water Liquid Density at Triple Point
#'
#' @return Triple Point Liquid Density: DfTr [ kg m-3 ]
#' 
#' @examples
#' DfTr()
#' 
#' @export
#' 
DfTr <- function() {
  DfTr <- as.double(0.999792520186e+03)
  #  class(DfTr) <- "IAPWS95"
  print(DfTr)
}

#' Water Gas Density at Triple Point
#'
#' @description The function \code{DgTr()} returns the Water Gas Density at Triple Point
#'
#' @return Triple Gas Density: DgTr [ kg m-3 ]
#'
#' @examples
#' DgTr()
#' 
#' @export
#' 
DgTr <- function() {
  DgTr <- as.double(0.485457572553e-02)
  #  class(DgTr) <- "IAPWS95"
  print(DgTr)
}

#' Liquid Water Entropy at Triple Point
#'
#' @description The function \code{sfTr()} returns the Water Liquid Entropy at Triple Point
#'
#' @return Triple Point Liquid Entropy: sfTr [ kJ kg-1 K-1]
#' 
#' @examples
#' sfTr()
#' 
#' @export
#' 
sfTr <- function() {
  sfTr <- as.double(0.0)
  #  class(sfTr) <- "IAPWS95"
  print(sfTr)
}

#' Water Gas Entropy at Triple Point
#'
#' @description The function \code{sgTr()} returns the Water Gas Entropy at Triple Point
#'
#' @return Triple Point Gas Entropy: sgTr [ kJ kg-1 K-1]
#' 
#' @examples
#' sgTr()
#' 
#' @export
#' 
sgTr <- function() {
  sgTr <- as.double(9.1554934093)
  #  class(DgTr) <- "IAPWS95"
  print(sgTr)
}

#' Water Specific Gas Constant
#'
#' @description The function \code{Rwater()} returns the Water Specific Gas Constant
#'
#' @return Water Specific Gas Constant: R [ K-1 ]
#' 
#' @examples
#' Rwater()
#' 
#' @export
#' 
Rwater <- function() {
  Rwater <- as.double(0.461518050)
  #  class(Rwater) <- "IAPWS95"
  print(Rwater)
}
