#' Saturation Pressure, Function of Temperature
#'
#' @description The function \code{pSatT(T,digits=9)} returns the saturation pressure [MPa], 
#'     pSat, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturation pressure: pSat [ MPa ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' p_Sat <- pSatT(Temp)
#' p_Sat
#' 
#' @export
#' 
pSatT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('pSatT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits=digits))
}

#' Saturated Liquid Density, Function of Temperature
#'
#' @description The function \code{DfT(Temp,digits=9)} returns the saturated liquid density [kg m-3], 
#'     Df, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated liquid density: Df [ kg m-3 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' Df <- DfT(Temp)
#' Df
#' 
#' @export
#' 
DfT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DfT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Saturated Gas Density, Function of Temperature
#'
#' @description The function \code{DgT(Temp,digits=9)} returns the saturated gas density [kg m-3], 
#'     Dg, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated gas density: Dg [ kg m-3 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' Dg <- DgT(Temp)
#' Dg
#' 
#' @export
#' 
DgT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DgT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Saturated Liquid Enthalpy, Function of Temperature
#'
#' @description The function \code{hfT(Temp,digits=9)} returns the saturated liquid enthalpy [kJ kg-1], 
#'     hf, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated liquid enthalpy: hf [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' hf <- hfT(Temp)
#' hf
#' 
#' @export
#' 
hfT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('hfT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Saturated Gas Enthalpy, Function of Temperature
#'
#' @description The function \code{hgT(Temp,digits=9)} returns the saturated gas enthalpy [kJ kg-1], 
#'     hg, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated gas enthalpy: hg [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' hg <- hgT(Temp)
#' hg
#' 
#' @export
#' 
hgT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('hgT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Saturated Liquid Entropy, Function of Temperature
#'
#' @description The function \code{sfT(Temp,digits=9)} returns the saturated liquid entropy [kJ kg-1 K-1], 
#'     sf, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated liquid entropy: sf [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' sf <- sfT(Temp)
#' sf
#' 
#' @export
#' 
sfT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('sfT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Saturated Gas Entropy, Function of Temperature
#'
#' @description The function \code{sgT(Temp,digits=9)} returns the saturated gas entropy [kJ kg-1 K-1], 
#'     sg, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated gas entropy: sg [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' sg <- sgT(Temp)
#' sg
#' 
#' @export
#' 
sgT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('sgT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
   error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
   print(error)
  }
  return(round(res[[2]],digits))
} 

#' Saturated Liquid Specific Internal Energy, Function of Temperature
#'
#' @description The function \code{ufT(Temp,digits=0).} returns the saturated liquid internal energy [kJ kg-1], 
#'     uf, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated liquid internal energy: uf [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' uf <- ufT(Temp)
#' uf
#' 
#' @export
#' 
ufT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('ufT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Saturated Gas Specific Internal Energy, Function of Temperature
#'
#' @description The function \code{ugT(Temp,digits=9)} returns the saturated gas internal energy [kJ kg-1], 
#'     ug, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The saturated gas internal energy: ug [kJ kg-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' ug <- ugT(Temp)
#' ug
#' 
#' @export
#' 
ugT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('ugT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Specific Isochoric Heat Capacity of Fluid Phase, Function of Temperature
#'
#' @description The function \code{CvfT(Temp,digits=9)} returns the Isochoric Heat Capacity 
#'     of Fluid Phase [kJ kg-1 K-1],  Cvf, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The Isochoric Heat Capacity of Fluid Phase: Cvf [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' Cvf <- CvfT(Temp)
#' Cvf
#' 
#' @export
#' 
CvfT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CvfT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Specific Isochoric Heat Capacity of Gas Phase, Function of Temperature
#'
#' @description The function \code{CvgT(Temp,digits=9)} returns the Isochoric Heat Capacity 
#'     of Gas Phase [kJ kg-1 K-1],  Cvg, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The Isochoric Heat Capacity of GaS Phase: Cvg [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' Cvg <- CvgT(Temp)
#' Cvg
#' 
#' @export
#' 
CvgT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CvgT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Specific Isobaric Heat Capacity of Fluid Phase, Function of Temperature
#'
#' @description The function \code{CpfT(Temp,digits=9)} returns the Isobaric Heat Capacity 
#'     of Fluid Phase [kJ kg-1 K-1],  Cpf, for given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The Isobaric Heat Capacity of Fluid Phase: Cpf [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' Cpf <- CpfT(Temp)
#' Cpf
#' 
#' @export
#' 
CpfT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CpfT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Specific Isobaric Heat Capacity of Gas Phase, Function of Temperature
#'
#' @description The function \code{CpgT(Temp,digits=9)} returns the Isobaric Heat Capacity 
#'     of Gas Phase [kJ kg-1 K-1],  Cpg, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The Isobaric Heat Capacity of Gas Phase: Cpg [kJ kg-1 K-1] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' Cpg <- CpgT(Temp)
#' Cpg
#' 
#' @export
#' 
CpgT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CpgT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Speed of Sound of Fluid Phase, Function of Temperature
#'
#' @description The function \code{wfT(Temp,digits=9)} returns the Speed 
#'     of Sound of Fluid Phase [m s-1],  wf, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The Speed of Sound of Fluid Phase: wf [ m s-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' wf <- wfT(Temp)
#' wf
#' 
#' @export
#' 
wfT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('wfT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(round(res[[2]],digits))
} 

#' Speed of Sound of Gas Phase, Function of Temperature
#'
#' @description The function \code{wgT(Temp,digits=9)} returns the Speed 
#'     of Sound of Gas Phase [m s-1],  wg, for given Temp [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param Temp Temperature [ K ]
#' @param digits Digits of results (optional)
#' 
#' @return The Speed of Sound of Gas Phase: wg [ m s-1 ] and an Error
#'      Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' Temp <- 450.
#' wg <- wgT(Temp)
#' wg
#' 
#' @export
#' 
wgT <- function(Temp,digits=9) {
  y <- 0.
  icode <- 0
  res <- .Fortran('wgT', as.double(Temp), as.double(y), as.integer(icode))
  if (res[[3]] != 0) { 
   error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
   print(error)
  }
  return(round(res[[2]],digits))
} 
