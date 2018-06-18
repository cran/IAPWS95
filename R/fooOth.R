#' Temperature, Function of Density and Pressure
#'
#' @description The function \code{TDp(D,p)} returns the water temperature, T [ K ],
#'      for given D [kg/m3] and p [ MPa ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m3 ]
#' @param p Pressure [ MPa ]
#' 
#' @return The Temperature: T [ K ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 838.025
#' p <- 10.0003858
#' T_Dp <- TDp(D,p)
#' T_Dp
#' 
#' @export
#' 
  TDp <- function(D,p) {
    y <- 0.
  icode <- 0
  res <- .Fortran('TDp', as.double(D), as.double(p), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
     error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
     print(error)
  }
  return(res[[3]])
  }

#' Temperature, Function of Density and Entropy
#'
#' @description The function \code{TDs(D,s)} returns the water temperature, T [ K ],
#'      for given D [kg/m3] and s [ kJ kg-1 K-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m3 ]
#' @param s Entropy in [ kJ kg-1 K-1 ]
#' 
#' @return The Temperature: T [ K ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 838.025
#' s <- 2.56690919
#' T_Ds <- TDs(D,s)
#' T_Ds
#' 
#' @export
#' 
TDs <- function(D,s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('TDs', as.double(D), as.double(s), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Temperature, Function of Density and Enthalpy
#'
#' @description The function \code{TDh(D,h)} returns the water temperature, T [ K ],
#'      for given D [kg/m3] and h [ kJ kg-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param D Density [ kg m3 ]
#' @param h Enthaly in [ kJ kg-1 ]
#' 
#' @return The Temperature: T [ K ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' D <- 838.025
#' h <- 977.181624
#' T_Dh <- TDh(D,h)
#' T_Dh
#' 
#' @export
#' 
TDh <- function(D,h) {
  y <- 0.
  icode <- 0
  res <- .Fortran('TDh', as.double(D), as.double(h), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Density, Function of Temperature and Enthalpy
#'
#' @description The function \code{DTh(T,h)} returns the water density, D [ kg m-3 ],
#'      for given T [K] and h [ kJ kg-1 ] (it may have two solutions for Density).
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature in Kelvin
#' @param h Enthaly in [ kJ kg-1 ]
#' 
#' @return The Density 1: Density_1 [ kg m-3 ]
#' @return The Density 2: Density_2 [ kg m-3 ]
#' @return Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' h <- 977.181624
#' D_Th <- DTh(T,h)
#' D_Th
#' 
#' @export
#' 
DTh <- function(T,h) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DTh', as.double(T), as.double(h), as.double(y), as.double(y), as.integer(icode))
  out <- list(Density_1=res[[3]], Density_2=res[[4]])
  options(digits=9)
  if (res[[5]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[5]]),2])
    print(error)
  }
  return(out)
}

#' Temperature, Function of Pressure and Enthalpy
#' 
#' @description The function \code{Tph(p,h)} returns the water temperature, T [ K ],
#'      for given p [MPa] and h [ kJ k-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' @param h Enthalpy [ kJ kg-1 ]
#' 
#' @return The Temperature: T [ K ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 10.0003858
#' h <- 977.181624
#' T_ph <- Tph(p,h)
#' T_ph
#' 
#' @export
#' 
Tph <- function(p,h) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Tph', as.double(p), as.double(h), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Density, Function of Pressure and Enthalpy
#'
#' @description The function \code{Dph(p,h)} returns the water density, D [ kg m-3 ],
#'      for given p [MPa] and h [ kJ k-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' @param h Enthalpy [ kJ kg-1 ]
#' 
#' @return The Density: D [ kg m-3 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 10.0003858
#' h <- 977.181624
#' D_ph <- Dph(p,h)
#' D_ph
#' 
#' @export
#' 
Dph <- function(p,h) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Dph', as.double(p), as.double(h), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
  }
  return(res[[3]])
}

#' Entropy, Function of Pressure and Enthalpy
#'
#' @description The function \code{sph(p,h)} returns the water entropy, s [ kJ kg-1 K-1 ],
#'      for given p [MPa] and h [ kJ k-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' @param h Enthalpy [ kJ kg-1 ]
#' 
#' @return The Entropy: s [ kJ kg-1 K-1 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 10.0003858
#' h <- 977.181624
#' s_ph <- sph(p,h)
#' s_ph
#' 
#' @export
#' 
sph <- function(p,h) {
  y <- 0.
  icode <- 0
  res <- .Fortran('sph', as.double(p), as.double(h), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Density, Function of Temperature and Entropy
#'
#' @description The function \code{DTs(T,s)} returns the water density, D [ kg m-3 ],
#'      for given T [K] and s [ kJ k-1 K-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [ K ]
#' @param s Entropy [ kJ kg-1 K-1 ]
#' 
#' @return The Density: D [ kg m-3 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' s <- 2.56690919
#' D_Ts <- DTs(T,s)
#' D_Ts
#' 
#' @export
#' 
#' @export
#' 
DTs <- function(T,s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('DTs', as.double(T), as.double(s), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Temperature, Function of Enthalpy and Entropy
#'
#' @description The function \code{Ths(h,s)} returns the water Temperature, T [ K ],
#'      for given h [kJ k-1] and s [ kJ k-1 K-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param h Enthalpy [ kJ kg-1 ]
#' @param s Entropy [ kJ kg-1 K-1 ]
#' 
#' @return The Temperature: T [ K ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' h <- 977.181624
#' s <- 2.56690919
#' T_hs <- Ths(h,s)
#' T_hs
#' 
#' @export
#' 
Ths <- function(h,s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Ths', as.double(h), as.double(s), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Density, Function of Enthalpy and Entropy
#'
#' @description The function \code{Dhs(h,s)} returns the water density, D [ kg m-3 ],
#'      for given h [kJ k-1] and s [ kJ k-1 K-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param h Enthalpy [ kJ kg-1 ]
#' @param s Entropy [ kJ kg-1 K-1 ]
#' 
#' @return The Density: D [ kg m-3 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' h <- 977.181624
#' s <- 2.56690919
#' D_hs <- Dhs(h,s)
#' D_hs
#' 
#' @export
#' 
Dhs <- function(h,s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Dhs', as.double(h), as.double(s), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Temperature, Function of Pressure and Entropy
#'
#' @description The function \code{Tps(p,s)} returns the water temperature, T [ K ],
#'      for given p [MPa] and s [ kJ k-1 K-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' @param s Entropy [ kJ kg-1 K-1 ]
#' 
#' @return The Temperature: T [ K ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 10.0003858
#' s <- 2.56690919
#' T_ps <- Tps(p,s)
#' T_ps
#' 
#' @export
#' 
Tps <- function(p,s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Tps', as.double(p), as.double(s), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Density, Function of Pressure and Entropy
#'
#' @description The function \code{Dps(p,s)} returns the water density, D [ kg m-3 ],
#'      for given p [MPa] and s [ kJ k-1 K-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' @param s Entropy [ kJ kg-1 K-1 ]
#' 
#' @return The Density: D [ kg m-3 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 10.0003858
#' s <- 2.56690919
#' D_ps <- Dps(p,s)
#' D_ps
#' 
#' @export
#' 
Dps <- function(p,s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('Dps', as.double(p), as.double(s), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Enthalpy, Function of Pressure and Entropy
#'
#' @description The function \code{hps(p,s)} returns the water enthalpy, h [ kJ kg-1 ],
#'      for given p [MPa] and s [ kJ k-1 K-1 ].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param p Pressure [ MPa ]
#' @param s Entropy [ kJ kg-1 K-1 ]
#' 
#' @return The Enthalpy: h [ kJ kg-1 ] and an Error Message (if an error occur: \link{errorCodes})
#' 
#' @examples
#' p <- 10.0003858
#' s <- 2.56690919
#' h_ps <- hps(p,s)
#' h_ps
#' 
#' @export
#' 
hps <- function(p,s) {
  y <- 0.
  icode <- 0
  res <- .Fortran('hps', as.double(p), as.double(s), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[4]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[4]]),2])
    print(error)
  }
  return(res[[3]])
}

#' Melting Pressure, Function of Temperature
#'
#' @description The function \code{pMeltT(T)} returns the water melting pressure,
#'      pMelt [ MPa ], for a given T [K].
#'
#' @details This function calls a Fortran DLL that solves the equations given at the
#'      Revised Release on the Pressure along the Melting and Sublimation Curves of
#'      Ordinary Water Substance (September 2011), developed by the International 
#'      Association for the Properties of Water and Steam, 
#'      \url{http://www.iapws.org/relguide/MeltSub.html}. It is valid from the 
#'      Temperature of 256.164 [K] to the Temperature of 715 [K].
#'     
#' @param T Temperature [K]
#' 
#' @return The melting pressure: pMelt [ MPa ] for regions III, V , VI and VII
#' @return The melting pressure: pMeltIh [ MPa ] for region Ih
#' @return The sublimation pressure: pSubl [ MPa ], below triple point Temperature
#' @return Error message (if an error occur)
#' 
#' @examples
#' T <- 275.
#' p_Melt <- pMeltT(T)
#' p_Melt
#' 
#' @export
#' 
pMeltT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('pMeltT', as.double(T), as.double(y), as.double(y), as.double(y), as.integer(icode))
  out <- list(Temperature=T, pMelt=res[[2]], pMeltIh=res[[3]], pSubl=res[[4]])
  options(digits=9)
  if (res[[5]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[5]]),2])
    print(error)
  }
  return(out)
}

#' Second Virial Coefficient (B), Function of Temperature
#'
#' @description The function \code{BT(T)} returns the second virial coefficient,
#'      B [ m3 kg-1 ], for a given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [K]
#' 
#' @return The second virial coefficient: B [ m3 kg-1 ] and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' B_T <- BT(T)
#' B_T
#' 
#' @export
#' 
  BT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('BT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
}

#' Third Virial Coefficient (C), Function of Temperature
#'
#' @description The function \code{CT(T)} returns the third virial coefficient,
#'      C [ m3 kg-1 ]**2, for a given T [K].
#'
#' @details This function calls a Fortran DLL that solves the Helmholtz Energy Equation. 
#'     in accordance with the Revised Release on the IAPWS Formulation 1995 for the 
#'     Thermodynamic Properties of Ordinary Water Substance for General and Scientific
#'     Use (June 2014) developed by the International Association for the Properties of
#'     Water and Steam,  \url{http://www.iapws.org/relguide/IAPWS-95.html}. It is valid  
#'     from the triple point to the pressure of 1000 MPa and temperature of 1273.
#'     
#' @param T Temperature [K]
#' 
#' @return The second virial coefficient: C [ m3 kg-1 ]**2 and an Error Message 
#'     (if an error occur: \link{errorCodes})
#' 
#' @examples
#' T <- 500.
#' C_T <- CT(T)
#' C_T
#' 
#' @export
#' 
CT <- function(T) {
  y <- 0.
  icode <- 0
  res <- .Fortran('CT', as.double(T), as.double(y), as.integer(icode))
  options(digits=9)
  if (res[[3]] != 0) { 
    error <-  as.character(errorCodes[which(errorCodes[,1]==res[[3]]),2])
    print(error)
  }
  return(res[[2]])
}
#' Vapor pressure, Function of Temperature
#'
#' @description The function \code{Vp(T)} returns the vapor pressure,
#'      Vp [ kPa ], for a given T [K]. 
#'
#' @details This function solves the Wagner Equation (Wagner and Pruss (1993))
#'      which gives one of the best fits to experimental data. It expresses reduced
#'      vapor pressure as a function of reduced temperature. This equation, for water,
#'      is valid from the temperature of 273.16 K to the critical temperature (624.096 K).
#'     
#' @param T Temperature [K]
#' 
Vp <- function(T) {
 # pCrit <- as.double(22.064): Critical Pressure
 # TCrit <- as.double(647.096): Critical Temperature

  TL <- 273.16  # Lower temperature limit
  TH <- 647.096 # Upper temperature limit
  if ( (T < TL) | (T > TH) ) { 
    print("Temperature out of bounds")
  }
  
 Tc1 <- 0.001545365757 # 1./Tc
 Tr = T*Tc1 # T/Tc
 Pc <- 22064. # kPa
 
 a1 <- -7.85951783
 a2 <- 1.84408259
 a3 <- -11.7866497
 a4 <- 22.6807411 
 a5 <- -15.9618719
 a6 <- 1.80122502
 tal <- 1.-Tr
 
 lnPr <- (a1*tal + a2*(tal^1.5) + a3*(tal^3) + a4*(tal^3.5) + a5*(tal^4) + a6*(tal^7.5))/Tr
 
 Pr <- exp(lnPr)
 Vp <- Pc * Pr
 return(Vp)
 }