
<!-- README.md is generated from README.Rmd. Please edit that file -->
IAPWS95
=======

The goal of IAPWS95 is to provide thermodynamic and transport properties of Water for general and scientifc use.

IAPWS95 calculates thermodynamic and transport properties of water as function of different combinations of Temperature, Density, Pressure, Enthalphy and Entropy. It is dedicated to scientists and engineers who analyze thermal and hydraulic experimental data or are involved with projects and equipment development, like turbines or nuclear reactors. This Vignette shows too few examples, but each function is documented with examples. The name IAPWS95 comes from the "IAPWS Formulation 1995 for the Thermodynamic Properties of Ordinary Water Substance for General and Scientific Use", a formulation developed by "The International Association for the Properties of Water and Steam", revised in 2014. <http://www.iapws.org> IAPWS-95 formulation is based on the fundamental equation of Helmholtz free energy as a function of temperature and density, f = f(T, ρ). Other thermodynamic properties are obtained by differentiation and algebraic manipulation without the use of any other information. IAPWS-95 defines accurately the thermodynamic properties of the fluid phases of ordinary water substance, with complete thermodynamic consistency among these properties, over a wide range of states (pressures up to 1000 MPa and temperatures from the melting and sublimation temperatures to 1273 K). In this package the lower temperature limit is the triple point temperature, 273.16 K. The definition of properties includes those on the liquid–vapor equilibrium line. The transport properties programmed were based on different IAPWS Releases: "Release on the IAPWS Formulation 2011 for the Thermal Conductivity of Ordinary Water Substance"; "Release on the IAPWS Formulation 2008 for the Viscosity of Ordinary Water Substance"; "Revised Release on Surface Tension of Ordinary Water Substance" (2014). The melting pressure was based on the "Revised Release on the Pressure along the Melting and Sublimation Curves of Ordinary Water Substance" (2011). ...

Installation
------------

You can install IAPWS95 from github with:

``` r
# install.packages("devtools")
devtools::install_github("IAPWS95")
```

Example
-------

This is a basic example which shows you how to obtain the water pressure as Function of Temperature and Pressure:

``` r
## basic example code: Pressure as Function of Temperature and Density
T <- 500.
D <- 838.025
IAPWS95::pTD(T,D)
#> [1] 10.0003858
```
