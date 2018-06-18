#' Saturation Pressure, Function of Temperature
#'
#' @description The function \code{pSatT(T)} returns the saturation pressure [MPa], 
#'     pSat, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturation pressure: pSat [ MPa ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' p_Sat <- pSatT(T)
#' p_Sat
#' 
#' @export
#' 
pSatT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('pSatT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
}

#' Saturated Liquid Density, Function of Temperature
#'
#' @description The function \code{DfT(T)} returns the saturated liquid density [kg m-3], 
#'     Df, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated liquid density: Df [ kg m-3 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' Df <- DfT(T)
#' Df
#' 
#' @export
#' 
DfT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DfT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Saturated Gas Density, Function of Temperature
#'
#' @description The function \code{DgT(T)} returns the saturated gas density [kg m-3], 
#'     Dg, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated gas density: Dg [ kg m-3 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' Dg <- DgT(T)
#' Dg
#' 
#' @export
#' 
DgT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DgT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Saturated Liquid Enthalpy, Function of Temperature
#'
#' @description The function \code{hfT(T)} returns the saturated liquid enthalpy [kJ kg-1], 
#'     hf, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated liquid enthalpy: hf [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' hf <- hfT(T)
#' hf
#' 
#' @export
#' 
hfT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('hfT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Saturated Gas Enthalpy, Function of Temperature
#'
#' @description The function \code{hgT(T)} returns the saturated gas enthalpy [kJ kg-1], 
#'     hg, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated gas enthalpy: hg [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' hg <- hgT(T)
#' hg
#' 
#' @export
#' 
hgT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('hgT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Saturated Liquid Entropy, Function of Temperature
#'
#' @description The function \code{sfT(T)} returns the saturated liquid entropy [kJ kg-1 K-1], 
#'     sf, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated liquid entropy: sf [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' sf <- sfT(T)
#' sf
#' 
#' @export
#' 
sfT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('sfT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Saturated Gas Entropy, Function of Temperature
#'
#' @description The function \code{sgT(T)} returns the saturated gas entropy [kJ kg-1 K-1], 
#'     sg, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated gas entropy: sg [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' sg <- sgT(T)
#' sg
#' 
#' @export
#' 
sgT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('sgT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
   error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
   print(error)
  }
  return(res[[2]])
} 

#' Saturated Liquid Specific Internal Energy, Function of Temperature
#'
#' @description The function \code{ufT(T).} returns the saturated liquid internal energy [kJ kg-1], 
#'     uf, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated liquid internal energy: uf [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' uf <- ufT(T)
#' uf
#' 
#' @export
#' 
ufT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('ufT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Saturated Gas Specific Internal Energy, Function of Temperature
#'
#' @description The function \code{ugT(T)} returns the saturated gas internal energy [kJ kg-1], 
#'     ug, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The saturated gas internal energy: ug [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' ug <- ugT(T)
#' ug
#' 
#' @export
#' 
ugT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('ugT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Specific Isochoric Heat Capacity of Fluid Phase, Function of Temperature
#'
#' @description The function \code{CvfT(T)} returns the Isochoric Heat Capacity 
#'     of Fluid Phase [kJ kg-1 K-1],  Cvf, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The Isochoric Heat Capacity of Fluid Phase: Cvf [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' Cvf <- CvfT(T)
#' Cvf
#' 
#' @export
#' 
CvfT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CvfT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Specific Isochoric Heat Capacity of Gas Phase, Function of Temperature
#'
#' @description The function \code{CvgT(T)} returns the Isochoric Heat Capacity 
#'     of Gas Phase [kJ kg-1 K-1],  Cvg, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The Isochoric Heat Capacity of GaS Phase: Cvg [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' Cvg <- CvgT(T)
#' Cvg
#' 
#' @export
#' 
CvgT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CvgT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Specific Isobaric Heat Capacity of Fluid Phase, Function of Temperature
#'
#' @description The function \code{CpfT(T)} returns the Isobaric Heat Capacity 
#'     of Fluid Phase [kJ kg-1 K-1],  Cpf, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The Isobaric Heat Capacity of Fluid Phase: Cpf [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' Cpf <- CpfT(T)
#' Cpf
#' 
#' @export
#' 
CpfT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CpfT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Specific Isobaric Heat Capacity of Gas Phase, Function of Temperature
#'
#' @description The function \code{CpgT(T)} returns the Isobaric Heat Capacity 
#'     of Gas Phase [kJ kg-1 K-1],  Cpg, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The Isobaric Heat Capacity of Gas Phase: Cpg [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' Cpg <- CpgT(T)
#' Cpg
#' 
#' @export
#' 
CpgT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CpgT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Speed of Sound of Fluid Phase, Function of Temperature
#'
#' @description The function \code{wfT(T)} returns the Speed 
#'     of Sound of Fluid Phase [m s-1],  wf, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The Speed of Sound of Fluid Phase: wf [ m s-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' wf <- wfT(T)
#' wf
#' 
#' @export
#' 
wfT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('wfT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
} 

#' Speed of Sound of Gas Phase, Function of Temperature
#'
#' @description The function \code{wgT(T)} returns the Speed 
#'     of Sound of Gas Phase [m s-1],  wg, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' 
#' @return The Speed of Sound of Gas Phase: wg [ m s-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 450.
#' wg <- wgT(T)
#' wg
#' 
#' @export
#' 
wgT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('wgT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
   error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
   print(error)
  }
  return(res[[2]])
} 
