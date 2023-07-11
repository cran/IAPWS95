---
title: "NEWS"
author: "Shawn Way"
date: "11Jul2023"
output: html_document
---

# IAPSW 1.2.2

Updated DFLOAT to DBLE in Fortran code to meet current compiler requirements.



# IAPWS95 1.2.1

Updated Data location to make it consistent with standard package design.

Removed  R_forceSymbols(dll, TRUE) requirement to allow the Linux versions to
compile correctly.


# IAPWS95 1.2.0

## Fixed Type Mismatch

Fixed type mismatch issues with functions DtH and phi0dt, reported by CRAN

## Added digits option to most functions

The number of digits was hard coded in the functions. This was changed to
be an option with a upper limit of 9 decimal places and a default of 9.

## Changed Maintainer



# IAPWS95 1.1.0

## Second Issue

This is the second issue of this package, with a new function to  compute the water vapor pressure according to the  Wagner and Pru&#223; equation (1993).
Every function of the R Code was revised to eliminate the automatic echo; now the return should be assigned to a local variable.

