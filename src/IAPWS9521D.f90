! BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB
! BD                           B L O C K D A T A    W A T E R                          DB
! BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDB

! ---------------------------------------------------------------------------------------------
      BLOCK DATA WATER
! ---------------------------------------------------------------------------------------------
      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      DOUBLEPRECISION TNORAUX(3),YNORAUX(3),G(20,3),TPOTAUX(20,3)
      INTEGER ISL(3),ISN(3),ISR(3),IMAXAUX(3),ISU
     REAL(KIND=8) ::  Lij, Lk
     REAL(KIND=8) ::  Hij, Hi
     REAL(KIND=8) ::  Lambdao, qD1, Nicr, Gamacr, Csi0, Gamao0, TRbar, xMi, qC1, qD1v

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / EQUI / BNULL,BZ,B1,B2,B(18),TPOTI(18),NPOLI,NPEI,NI
      COMMON / EQUR / A(60),TPOT(60),DPOT(60),GAMMA(60),ETA(60),      &
                      EPSI(60),BETA(60),ALPHA(60),                  &
                      N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NGBS,NNA,NASSO
      COMMON / EQUAUX / TNORAUX,YNORAUX,G,TPOTAUX,ISL,ISN,ISR,IMAXAUX
      COMMON / HSDIAGRAM / HHMAX,SHMAX,HSMAX,SSMAX,HSMIN,SSMIN,TSMIN,TSMAX,IHS

      COMMON / CND   /  Lij(5,6), Lk(5)
      COMMON / VISC  /  Hij(6,7), Hi(4)
      COMMON / CND2 /  Lambdao, qD1, Nicr, Gamacr, Csi0, Gamao0, TRbar, xMi, qC1, qD1v

     COMMON / ChsMax  / ChsMax1(4), ChsMax2(7)
     COMMON / ChsMin  / ChsMin1(2), ChsMin2(4)
     COMMON / ChsCrit / Chspcrit(4), ChsTcrit(7)
     COMMON / Chsx    / Chsx0(3), Chsx1(7)
     COMMON / shLim   / sMax1, sMax2, sMin1, sMin2, shpcr1, shpcr2,      &
                   shTcr1, sTcri2, shx11, shx12, shx01, shx02

     COMMON / LIMITES / Tmini, TMAXI, PMAXI
      COMMON / TSAT / at(3), bt(3), ct(3)


!   WATER SPECIFIC DATA

      DATA R /0.461518050D+00/, TC /0.647096000D+03 /, PC /0.220640000D+02/,      &
         DC /0.322000000D+03/, SC/ 4.4069618924D+0/, HC/2084.25625591D+0/       &     ! 4.4069618924
         TTR /0.273160000D+03 /, PTR /0.6116547711D-03/,                      &
         DLTR /0.999792520186D+03/, DVTR /0.485457572553D-02/,               &
         SLTR/0.0D+0/, SVTR/9.1554934093D+0/                              

!   EQUATION FOR THE IDEAL PART OF FREE ENERGY

      DATA NPOLI / 0 /, NPEI / 5  /, NI / 5 /

      DATA BZ / 0.000000000000000D+00/, B1 / 0.66832105275932D+01/, B2 / -.83204464837497D+01/, &
          BNULL / 0.300632000D+01/ 

      DATA B(1) / 0.124360000D-01/, B(2) / 0.973150000D+00/, B(3) / 0.127950000D+01/,         &
          B(4) / 0.969560000D+00/, B(5) / 0.248730000D+00/

      DATA TPOTI(1)/ 0.128728967D+01/, TPOTI(2)/ 0.353734222D+01/, TPOTI(3)/ 0.774073708D+01/, &
         TPOTI(4)/ 0.924437796D+01/, TPOTI(5)/ 0.275075105D+02/ 

! EQUATION FOR RESIDUAL PART OF FREE ENERGY   EQUATION

      DATA N / 56/, NPOL / 7/, NE1 / 15/, NE2 / 20/, NE3 / 4/, NE4 / 1/,  &
         NE5 / 0/, NE6 / 4/, NGBS / 3/, NNA / 2/

      DATA A( 1) / 0.12533547935523D-01/, DPOT( 1) / 1.D0/, TPOT( 1) /-0.5D0/
      DATA A( 2) / 0.78957634722828D+01/, DPOT( 2) / 1.D0/, TPOT( 2) / 0.875D0/ 
      DATA A( 3) / -.87803203303561D+01/, DPOT( 3) / 1.D0/, TPOT( 3) / 1.D0/ 
      DATA A( 4) / 0.31802509345418D+00/, DPOT( 4) / 2.D0/, TPOT( 4) / 0.5D0/ 
      DATA A( 5) / -.26145533859358D+00/, DPOT( 5) / 2.D0/, TPOT( 5) / 0.75D0/ 
      DATA A( 6) / -.78199751687981D-02/, DPOT( 6) / 3.D0/, TPOT( 6) / 0.375D0/ 
      DATA A( 7) / 0.88089493102134D-02/, DPOT( 7) / 4.D0/, TPOT( 7) / 1.D0/ 
      DATA A( 8) / -.66856572307965D+00/, DPOT( 8) / 1.D0/, TPOT( 8) / 4.D0/ 
      DATA A( 9) / 0.20433810950965D+00/, DPOT( 9) / 1.D0/, TPOT( 9) / 6.D0/ 
      DATA A(10) / -.66212605039687D-04/, DPOT(10) / 1.D0/, TPOT(10) /12.D0/ 
      DATA A(11) / -.19232721156002D+00/, DPOT(11) / 2.D0/, TPOT(11) / 1.D0/
      DATA A(12) / -.25709043003438D+00/, DPOT(12) / 2.D0/, TPOT(12) / 5.D0/ 
      DATA A(13) / 0.16074868486251D+00/, DPOT(13) / 3.D0/, TPOT(13) / 4.D0/
      DATA A(14) / -.40092828925807D-01/, DPOT(14) / 4.D0/, TPOT(14) / 2.D0/ 
      DATA A(15) / 0.39343422603254D-06/, DPOT(15) / 4.D0/, TPOT(15) /13.D0/ 
      DATA A(16) / -.75941377088144D-05/, DPOT(16) / 5.D0/, TPOT(16) / 9.D0/ 
      DATA A(17) / 0.56250979351888D-03/, DPOT(17) / 7.D0/, TPOT(17) / 3.D0/ 
      DATA A(18) / -.15608652257135D-04/, DPOT(18) / 9.D0/, TPOT(18) / 4.D0/ 
      DATA A(19) / 0.11537996422951D-08/, DPOT(19) /10.D0/, TPOT(19) /11.D0/ 
      DATA A(20) / 0.36582165144204D-06/, DPOT(20) /11.D0/, TPOT(20) / 4.D0/ 
      DATA A(21) / -.13251180074668D-11/, DPOT(21) /13.D0/, TPOT(21) /13.D0/ 
      DATA A(22) / -.62639586912554D-09/, DPOT(22) /15.D0/, TPOT(22) / 1.D0/ 
      DATA A(23) / -.10793600908932D+00/, DPOT(23) / 1.D0/, TPOT(23) / 7.D0/ 
      DATA A(24) / 0.17611491008752D-01/, DPOT(24) / 2.D0/, TPOT(24) / 1.D0/ 
      DATA A(25) / 0.22132295167546D+00/, DPOT(25) / 2.D0/, TPOT(25) / 9.D0/ 
      DATA A(26) / -.40247669763528D+00/, DPOT(26) / 2.D0/, TPOT(26) /10.D0/
      DATA A(27) / 0.58083399985759D+00/, DPOT(27) / 3.D0/, TPOT(27) /10.D0/ 
      DATA A(28) / 0.49969146990806D-02/, DPOT(28) / 4.D0/, TPOT(28) / 3.D0/ 
      DATA A(29) / -.31358700712549D-01/, DPOT(29) / 4.D0/, TPOT(29) / 7.D0/ 
      DATA A(30) / -.74315929710341D+00/, DPOT(30) / 4.D0/, TPOT(30) /10.D0/ 
      DATA A(31) / 0.47807329915480D+00/, DPOT(31) / 5.D0/, TPOT(31) /10.D0/ 
      DATA A(32) / 0.20527940895948D-01/, DPOT(32) / 6.D0/, TPOT(32) / 6.D0/ 
      DATA A(33) / -.13636435110343D+00/, DPOT(33) / 6.D0/, TPOT(33) /10.D0/
      DATA A(34) / 0.14180634400617D-01/, DPOT(34) / 7.D0/, TPOT(34) /10.D0/ 
      DATA A(35) / 0.83326504880713D-02/, DPOT(35) / 9.D0/, TPOT(35) / 1.D0/ 
      DATA A(36) / -.29052336009585D-01/, DPOT(36) / 9.D0/, TPOT(36) / 2.D0/
      DATA A(37) / 0.38615085574206D-01/, DPOT(37) / 9.D0/, TPOT(37) / 3.D0/ 
      DATA A(38) / -.20393486513704D-01/, DPOT(38) / 9.D0/, TPOT(38) / 4.D0/ 
      DATA A(39) / -.16554050063734D-02/, DPOT(39) / 9.D0/, TPOT(39) / 8.D0/ 
      DATA A(40) / 0.19955571979541D-02/, DPOT(40) /10.D0/, TPOT(40) / 6.D0/ 
      DATA A(41) / 0.15870308324157D-03/, DPOT(41) /10.D0/, TPOT(41) / 9.D0/ 
      DATA A(42) / -.16388568342530D-04/, DPOT(42) /12.D0/, TPOT(42) / 8.D0/ 
      DATA A(43) / 0.43613615723811D-01/, DPOT(43) / 3.D0/, TPOT(43) /16.D0/ 
      DATA A(44) / 0.34994005463765D-01/, DPOT(44) / 4.D0/, TPOT(44) /22.D0/ 
      DATA A(45) / -.76788197844621D-01/, DPOT(45) / 4.D0/, TPOT(45) /23.D0/ 
      DATA A(46) / 0.22446277332006D-01/, DPOT(46) / 5.D0/, TPOT(46) /23.D0/ 
      DATA A(47) / -.62689710414685D-04/, DPOT(47) /14.D0/, TPOT(47) /10.D0/ 
      DATA A(48) / -.55711118565645D-09/, DPOT(48) / 3.D0/, TPOT(48) /50.D0/
      DATA A(49) / -.19905718354408D+00/, DPOT(49) / 6.D0/, TPOT(49) /44.D0/ 
      DATA A(50) / 0.31777497330738D+00/, DPOT(50) / 6.D0/, TPOT(50) /46.D0/ 
      DATA A(51) / -.11841182425981D+00/, DPOT(51) / 6.D0/, TPOT(51) /50.D0/ 
      DATA A(52) / -.31306260323435D+02/, DPOT(52) / 3.D0/, TPOT(52) / 0.D0/ 
      DATA A(53) / 0.31546140237781D+02/, DPOT(53) / 3.D0/, TPOT(53) / 1.D0/ 
      DATA A(54) / -.25213154341695D+04/, DPOT(54) / 3.D0/, TPOT(54) / 4.D0/ 
      DATA A(55) / -.14874640856724D+00/, DPOT(55) /28.D0/, TPOT(55) /700.D0/ 
      DATA A(56) / 0.31806110878444D+00/, DPOT(56) /32.D0/, TPOT(56) /800.D0/

!  PARAMETERS OF GIBBS AND NA TERMS

      DATA EPSI (52) / 0.100000000D+01/, ETA (52) / 0.200000000D+02/, &
         GAMMA(52) / 0.121000000D+01/, BETA(52) / 0.150000000D+03/    
      DATA EPSI (53) / 0.100000000D+01/, ETA (53) / 0.200000000D+02/, &
         GAMMA(53) / 0.121000000D+01/, BETA(53) / 0.150000000D+03/    
      DATA EPSI (54) / 0.100000000D+01/, ETA (54) / 0.200000000D+02/, &
         GAMMA(54) / 0.125000000D+01/, BETA(54) / 0.250000000D+03/    
     DATA EPSI (55) / 0.850000000D+00/, ETA (55) / 0.320000000D+00/, &
         GAMMA(55) / 0.200000000D+00/, BETA(55) / 0.300000000D+00/    
      DATA EPSI (56) / 0.950000000D+00/, ETA (56) / 0.320000000D+00/, &    
         GAMMA(56) / 0.200000000D+00/, BETA(56) / 0.300000000D+00/ 
            
      DATA ALPHA(55) / 0.350000000D+01/, ALPHA(56) / 0.350000000D+01/

! HANGING ON IN DEW LINE h/s CHART
! -1 = not yet verified
! 0 = NOT ABOUT HANGING
! 1 = LOW HANGING ON
! 2 = STRONG ON HANGING

!   Parameters for Functions of Entalphy and Entropy
!      TMINEQ  = TTR
!      TMAXEQ  = 1273.D0
!      PMAXEQ  = 1000.D0
!      IHS = 0
!      HHMAX = 2803.17476477413D+0      Satura??o x = 1.0    2084,09046 ou 2803.17476477413D+0  ????
!      SHMAX = 6.17576993539012D+0      Satura??o x = 1.0
!      TMAXCAL = 4000.D0
!      PMAXCAL = 1000.D0
!      HSMAX   = 2500,91519 ????
!      SSMAX   = 9,15549341 ??????
!      HSMIN   = 0.000611781667 ????
!      COMMON / HSDIAGRAM / HHMAX,SHMAX,HSMAX,SSMAX,HSMIN,SSMIN,TSMIN,TSMAX,IHS


!      DATA IHS / 0/, HHMAX / 2803.17476477413D+0/, SHMAX / 6.17576993539012D+0/
      DATA IHS / 0/, HHMAX / 2084.25625591D+0/, SHMAX / 4.4069618924D+0/      ! HC/2084.25625591D+0/ SC/ 4.4069618924D+0/
!     DATA HSMAX/2500.91519D+0/, SSMAX/9.15549341D+0/, HSMIN/0.000611781667D+0/      ! ????????
     DATA HSMAX/4642.47D+0/, SSMAX/12.3321289D+0/, HSMIN/1500.D+0/      ! ????????
!     DATA TSMAX /508.39D+0/, TSMIN / 273.16D+0/                           ! ??????????
     DATA TSMAX /1273.D+0/, TSMIN / 273.16D+0/                           ! ??????????

!     COMMON / LIMITES / Tmin, TMAXI, PMAXI
     DATA Tmini /273.16d+0/, TMAXI / 1273.D0/, PMAXI / 1000.d0/               ! ??????????

!     shMax1 --> 0 <= s <= 5,20477498
     DATA sMax1 / 5.20477498d+0/, sMax2 / 12.3321289d+0/
!     hMax1 = ChsMax1(1) + ChsMax1(2)*s + ChsMax1(3)*s*s + ChsMax1(4)*s*s*s         
     DATA ChsMax1(1)/ 8.70883214d+02 /, ChsMax1(2)/ 3.20383415d+02 /,    &
         ChsMax1(3)/ 2.05120474d+01 /, ChsMax1(4)/ 8.76150147d+00 /

!     shMax2 --> 5,20477498 <= s <= 12,3321289
!     hMax2 = ChsMax2(1) + ChsMax2(2)*s + ChsMax2(3)*s*s + ChsMax2(4)*s*s*s + ChsMax2(5)*s*s*s*s + ChsMax2(6)*s*s*s*s*s + ChsMax2(7)*s*s*s*s*s*s                  
     DATA ChsMax2(1)/3.29137039d+05/, ChsMax2(2)/-2.32007271d+05/, ChsMax2(3)/6.76063859d+04/,      &
           ChsMax2(4)/-1.03128809d+04/, ChsMax2(5)/8.70683828d+02/, ChsMax2(6)/-3.86446246d+01/   &
           ChsMax2(7)/7.05444273d-01/

!     shMin1 --> 0 <= s <= 9.15549341     [ sv(TTR) ]
     DATA sMin1 / 9.15549341d+0/, sMin2 / 12.3321289d+0/
!     hmin1 = ChsMin1(1) + ChsMin1(2)*s   
     DATA ChsMin1(1)/0.00061178167d0/, ChsMin1(2)/273.16d0/

!     shMin2 --> 9.15549341 <= s <= 12,3321289
!     hmin2 = ChsMin2(1) + ChsMin2(2)*s + ChsMin2(3)*s*s + ChsMin2(4)*s*s*s         
     DATA ChsMin2(1)/-1.027446712d+04/, ChsMin2(2)/4.179071360d+03/,   &
         ChsMin2(3)/-4.863683634d+02/,ChsMin2(4)/ 1.991397003d+01/

!     hpcrit --> 4,40670568 <= s <= 7,44523869
     DATA shpcr1 / 4.40670568d+0/, shpcr2 / 7.44523869d+0/
!     hpcrit = Chspcrit(1) + Chspcrit(2)*s + Chspcrit(3)*s*s + Chspcrit(4)*s*s*s         
     DATA Chspcrit(1)/-4.62822479d+03/, Chspcrit(2)/3.00656198d+03/,   &
         Chspcrit(3)/-4.80229426d+02/,Chspcrit(4)/ 3.25690647d+01/

!     hTcrit --> 2,8365 <= s <= 10,8
     DATA shTcr1 / 2.8365d+0/, sTcri2 / 10.8d+0/
!     hTcrit = ChsTcrit(1) + ChsTcrit(2)*s + ChsTcrit(3)*s*s + ChsTcrit(4)*s*s*s + 
!            ChsTcrit(5)*s*s*s*s + ChsTcrit(6)*s*s*s*s*s + ChsTcrit(7)*s*s*s*s*s*s                  
     DATA ChsTcrit(1)/4.27662214d+04/, ChsTcrit(2)/-3.83753885d+04/, ChsTcrit(3)/1.39777524d+04/   &
           ChsTcrit(4)/-2.55865395d+03/, ChsTcrit(5)/2.53458248d+02/, ChsTcrit(6)/-1.30269703d+01/   &
           ChsTcrit(7)/2.73204193d-01/

!     hx1 --> 4,40670568 <= s <= 9,15549341   
     DATA shx11 / 4.40670568d+0/, shx12 / 9.15549341d+0/       
!     hx1 = Chsx1(1) + Chsx1(2)*s + Chsx1(3)*s*s + Chsx1(4)*s*s*s + 
!            Chsx1(5)*s*s*s*s + Chsx1(6)*s*s*s*s*s + Chsx1(7)*s*s*s*s*s*s                  
     DATA Chsx1(1)/2.32927898d+05/, Chsx1(2)/-2.15033902d+05/, Chsx1(3)/8.11581082d+04/   &
           Chsx1(4)/-1.59357287d+04/, Chsx1(5)/1.72488433d+03/, Chsx1(6)/-9.79387310d+01/   &
           Chsx1(7)/2.28518624d+00/

!     hx0 --> 0 <= s <= 4,40670568   
     DATA shx01 / 0.d+0/, shx02 / 4.40670568d+0/       
!     hx0 = Chsx0(1) + Chsx0(2)*s + Chsx0(3)*s*s         
     DATA Chsx0(1)/4.81739266d+00/, Chsx0(2)/2.44767322d+02/, Chsx0(3)/5.17640328d+01/


!   PARAMETERS OF AUXILIARY EQUATIONS

! Parametros para estimativa da pressao de saturacao em funcao de T    
      DATA ISL(1)/ 3/, ISR(1)/ 1/, ISN(1)/ 1/, IMAXAUX(1)/ 6/

     DATA TPOTAUX(1,1)/ 0.100000000D+01/, G(1,1)/ -.785951783D+01/            
      DATA TPOTAUX(2,1)/ 0.150000000D+01/, G(2,1)/ 0.184408259D+01/          
      DATA TPOTAUX(3,1)/ 0.300000000D+01/, G(3,1)/ -.117866497D+02/          
      DATA TPOTAUX(4,1)/ 0.350000000D+01/, G(4,1)/ 0.226807411D+02/          
      DATA TPOTAUX(5,1)/ 0.400000000D+01/, G(5,1)/ -.159618719D+02/          
      DATA TPOTAUX(6,1)/ 0.750000000D+01/, G(6,1)/ 0.180122502D+01/
               
      DATA TNORAUX(1)/ 0.647096000D+03/, YNORAUX(1)/ 0.220640000D+02/       ! TC, PC

! Parametros para estimativa da densidade do l?quido saturado em funcao de T
    
      DATA ISL(2) / 4/, ISR(2) / 1/, ISN(2) / 1/, IMAXAUX(2) / 6/

      DATA TPOTAUX(1,2)/ 0.333333333D+00/, G(1,2)/ 0.199274064D+01/ 
      DATA TPOTAUX(2,2)/ 0.666666667D+00/, G(2,2)/ 0.109965342D+01/ 
      DATA TPOTAUX(3,2)/ 0.166666667D+01/, G(3,2)/ -.510839303D+00/ 
      DATA TPOTAUX(4,2)/ 0.533333333D+01/, G(4,2)/ -.175493479D+01/ 
      DATA TPOTAUX(5,2)/ 0.143333333D+02/, G(5,2)/ -.455170352D+02/ 
      DATA TPOTAUX(6,2)/ 0.366666667D+02/, G(6,2)/-.674694450D+06/
      
     DATA TNORAUX(2)/ 0.647096000D+03/, YNORAUX(2)/ 0.322000000D+03/   ! TC, DC

! Parametros para estimativa da densidade do vapor saturado em funcao de T 
   
      DATA ISL(3) / 1/, ISR(3) / 1/, ISN(3) / 1/, IMAXAUX(3) / 6/

      DATA TPOTAUX(1,3)/ 0.333333333D+00/, G(1,3)/ -.203150240D+01/
      DATA TPOTAUX(2,3)/ 0.666666670D+00/, G(2,3)/ -.268302940D+01/
      DATA TPOTAUX(3,3)/ 0.133333333D+01/, G(3,3)/ -.538626492D+01/
      DATA TPOTAUX(4,3)/ 0.300000000D+01/, G(4,3)/ -.172991605D+02/
      DATA TPOTAUX(5,3)/ 0.616666667D+01/, G(5,3)/-.447586581D+02/
      DATA TPOTAUX(6,3)/ 0.118333333D+02/, G(6,3)/ -.639201063D+02/

!    Parameters to estimate Saturation Temperature of Density

     data at(1)/-0.850711593d-2/, at(2)/-0.748762525d-2/
     data bt(1)/ 4.714698301d+0/, bt(2)/4.149644199d+0/
     data ct(1)/ 346.6959713d+0/, ct(2)/424.9918046d+0/

      DATA TNORAUX(3)/ 0.647096000D+03/, YNORAUX(3)/ 0.322000000D+03/      ! TC, DC 

!    Parameters to calculate Viscosity and Thermal Conductivity

     data Lk/ 2.443221d-3, 1.323095d-2, 6.770357d-3, -3.454586d-3, 4.096266d-4 /

     data Lij(1,1)/ 1.60397357d0   /, Lij(2,1)/ 2.33771842d0   /,                     &
          Lij(3,1)/ 2.19650529d0   /, Lij(4,1)/-1.21051378d0   /, Lij(5,1)/-2.7203370d0  /
     data Lij(1,2)/-0.646013523d0  /, Lij(2,2)/-2.78843778d0   /,                     &
          Lij(3,2)/-4.54580785d0   /, Lij(4,2)/ 1.60812989d0   /, Lij(5,2)/ 4.57586331d0 /
     data Lij(1,3)/ 0.111443906d0  /, Lij(2,3)/ 1.53616167d0   /,                     &
          Lij(3,3)/ 3.55777244d0   /, Lij(4,3)/-0.621178141d0  /, Lij(5,3)/-3.18369245d0 /
     data Lij(1,4)/ 0.102997357d0  /, Lij(2,4)/-0.463045512d0  /,                     &
          Lij(3,4)/-1.40944978d0   /, Lij(4,4)/ 0.0716373224d0 /, Lij(5,4)/ 1.1168348d0  /
     data Lij(1,5)/-0.0504123634d0 /, Lij(2,5)/0.0832827019d0  /,                     &
          Lij(3,5)/ 0.275418278d0  /, Lij(4,5)/ 0.0d0          /, Lij(5,5)/-0.19268305d0 /
     data Lij(1,6)/ 0.00609859258d0/, Lij(2,6)/-0.00719201245d0/,                     &
          Lij(3,6)/-0.0205938816d0 /, Lij(4,6)/ 0.0d0          /, Lij(5,6)/ 0.012913842d0/

     data Hi/ 1.67752d0, 2.20462d0, 0.6366564d0, -0.241605d0/

     data Hij(1,1)/ 5.20094d-1 /, Hij(2,1)/ 8.50895d-2 /, Hij(3,1)/-1.08374d0  /,   &
          Hij(4,1)/-2.89555d-1 /, Hij(5,1)/ 0.0d0      / ,Hij(6,1)/ 0.0d0      /
     data Hij(1,2)/ 2.22531d-1 /, Hij(2,2)/ 9.99115d-1 /, Hij(3,2)/ 1.88797d0  /,   &
          Hij(4,2)/ 1.26613d0  /, Hij(5,2)/ 0.0d0      /, Hij(6,2)/ 1.20573d-1 /
     data Hij(1,3)/-2.81378d-1 /, Hij(2,3)/-9.06851d-1 /, Hij(3,3)/-7.72479d-1 /,   &
          Hij(4,3)/-4.89837d-1 /, Hij(5,3)/-2.5704d-1  /, Hij(6,3)/ 0.0d0      /
     data Hij(1,4)/ 1.61913d-1 /, Hij(2,4)/ 2.57399d-1 /, Hij(3,4)/ 0.0d0      /,   &
          Hij(4,4)/ 0.0d0      /, Hij(5,4)/ 0.0d0      /, Hij(6,4)/ 0.0d0      /
     data Hij(1,5)/-3.25372d-2 /, Hij(2,5)/ 0.0d0      /, Hij(3,5)/ 0.0d0      /,   &
          Hij(4,5)/ 6.98452d-2 /, Hij(5,5)/ 0.0d0      /, Hij(6,5)/ 0.0d0      /
     data Hij(1,6)/ 0.0d0      /, Hij(2,6)/ 0.0d0      /, Hij(3,6)/ 0.0d0      /,   &
          Hij(4,6)/ 0.0d0      /, Hij(5,6)/ 8.72102d-3 /, Hij(6,6)/ 0.0d0      /
     data Hij(1,7)/ 0.0d0      /, Hij(2,7)/ 0.0d0      /, Hij(3,7)/ 0.0d0      /,   &
          Hij(4,7)/-4.35673d-3 /, Hij(5,7)/ 0.0d0      /, Hij(6,7)/-5.93264d-4 /

!    Critical-region constants (conductivity)
!      CNDRF   --> [ W K-1 m-1 ]
!      VISRF   --> [ Pa s ]
     data CNDRF/1.0D-3/, VISRF/1.0D-06/
     data Lambdao/177.8514d0/, qD1/0.40d0/, Nicr/0.630d0/, Gamacr/1.239d0/,         &
          Csi0/0.13d0/, Gamao0/0.06d0/, TRbar/1.5d0/

!    Critical-region constants (viscosity)
     data xMi/ 0.068d0/, qC1/ 1.9d0/, qD1v/ 1.1d0/
     
     
     END BLOCK DATA WATER

! BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBD

!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
!F                     OUTPUT FUNCTIONS                                       F
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
! -----------------------------------------------------
      REAL(KIND=8) function pcrit(pc)
! -----------------------------------------------------
!      Critical Pressure [ Mpa ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  pc

      pc = 0.220640000D+02
      pcrit = pc
      return
      end
! -----------------------------------------------------
      REAL(KIND=8) function Tcrit(Tc)
! -----------------------------------------------------
!      Critical Temperature  [ K ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  tc

      Tc = 0.647096000D+03 
      Tcrit = Tc
      return
      end
! -----------------------------------------------------
      REAL(KIND=8) function Dcrit(Dc)
! -----------------------------------------------------
!      Critical Density [ kg / m**3 ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  Dc

      Dc = 0.322000000D+03
      Dcrit = Dc
      return
      end
! -----------------------------------------------------
      REAL(KIND=8) function scrit(sc)
! -----------------------------------------------------
!      Critical Entropy [ kJ / (kg K) ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  sc

      sc = 4.40696189237D+0
      scrit = sc
      return
      end
! -----------------------------------------------------
      REAL(KIND=8) function hcrit(hc)
! -----------------------------------------------------
!      Critical Entalphy [ kJ / kg ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  hc

      hc = 2084.25625591D+0
      hcrit = hc
      return
      end
! -----------------------------------------------------      
      REAL(KIND=8) function tripT(Ttrip)
! -----------------------------------------------------
!      Temperature at the Triple Point [ K ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  Ttrip

      Ttrip = 0.273160000D+03 
      tripT = Ttrip
      return
      end
! -----------------------------------------------------      
      REAL(KIND=8) function tripp(ptrip)
! -----------------------------------------------------
!      Pressure at the Triple Point [ K ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  ptrip

      ptrip = 0.6116547711D-03
      tripp = ptrip
      return
      end
! -----------------------------------------------------      
       REAL(KIND=8) function tripDf(Dfr)
! -----------------------------------------------------
!      Fluid Density at Triple Point [ kg / m**3 ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  Dfr

      Dfr = 0.999792520186D+03
      tripDf = Dfr
      return
      end
! -----------------------------------------------------      
        REAL(KIND=8) function tripDg(Dgr)
! -----------------------------------------------------
!      Gas Density at the Triple Point [ kg / m**3 ]
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  Dgr
      Dgr = 0.485457572553D-02
      tripDg = Dgr
      return
      end
! -----------------------------------------------------      
        REAL(KIND=8) function critPt(R, Tc, pc, Dc, hc, sc)
! -----------------------------------------------------
!      R and T, p and Density at Critical Point
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  R, Tc, pc, Dc, hc, sc
        R    = 0.461518050D+00
        Tc   = 0.647096000D+03 
        pc   = 0.220640000D+02
        Dc   = 0.322000000D+03
      hc   = 2084.25625591D+0
      sc   = 4.40696189237D+0
      critPt = -999.d0
      return
      end
! -----------------------------------------------------      
        REAL(KIND=8) function tripPt(Ttr, ptr, Dftr, Dgtr)
! -----------------------------------------------------
!      Temp., pressure, fluid and gas densities at triple point
! -----------------------------------------------------
      implicit none
      REAL(KIND=8) ::  Ttr, ptr, Dftr, Dgtr
        Ttr  = 0.273160000D+03 
        ptr  = 0.6116547711D-03      ! Rev
        Dftr = 0.999792520186D+03
        Dgtr = 0.485457572553D-02
      tripPt = -999.d0
      return
      end

! -----------------------------------------------------------------------------      
      subroutine pSatTab(n, T, pSat)
! -----------------------------------------------------------------------------      
! Table of pSat function of T
! -----------------------------------------------------------------------------      
      integer n, i, icode
      REAL(KIND=8) ::  T(n), pSat(n,2), pSatT, p

      do 100 i = 1, n
          pSat(i,1) = T(i)                                        
          pSat(i,2) = pSatT( T(i), p, icode )                                        
  100 continue

      end
! -----------------------------------------------------------------------------      
      subroutine TSatTab(n, p, Tsat)
! -----------------------------------------------------------------------------      
! Table of TSat function of p
! -----------------------------------------------------------------------------      
      integer n, i, icode
      REAL(KIND=8) ::  p(n), TSat(n,2), TSatp, T

      do 100 i = 1, n
          TSat(i,1) = p(i)                                        
          TSat(i,2) = TSatp( p(i), T, icode )                                        
  100 continue

      end
! -----------------------------------------------------------------------------      
      subroutine hfTTab(n, T, hf)
! -----------------------------------------------------------------------------      
! Table of hf function of T
! -----------------------------------------------------------------------------      
      integer n, i, icode
      REAL(KIND=8) ::  T(n), hf(n,2), hfT, h

      do 100 i = 1, n
          hf(i,1) = T(i)                                        
          hf(i,2) = hfT( T(i), h, icode )                                        
  100 continue

      end
! -----------------------------------------------------------------------------      
      subroutine satDhsofT(n, T, satDhs)
! -----------------------------------------------------------------------------      
! Table of Sat Properties with Density function of T
! -----------------------------------------------------------------------------      
      integer n, i, icode
      REAL(KIND=8) ::  T(n), satDhs(n,7), DfT, DgT, hfT, hgT, sfT, sgT, y

      do 100 i = 1, n
          satDhs(i,1) = T(i)                                        
          satDhs(i,2) = DfT( T(i), y, icode )                                        
          satDhs(i,3) = DgT( T(i), y, icode )                                        
          satDhs(i,4) = hfT( T(i), y, icode )                                        
          satDhs(i,5) = hgT( T(i), y, icode )                                        
          satDhs(i,6) = sfT( T(i), y, icode )                                        
          satDhs(i,7) = sgT( T(i), y, icode )                                        
  100 continue

      end
! -----------------------------------------------------------------------------      
      subroutine satvhsofT(n, T, satvhs)
! -----------------------------------------------------------------------------      
! Table of Sat Properties with Specif volume function of T
! -----------------------------------------------------------------------------      
      integer n, i, icode
      REAL(KIND=8) ::  T(n), satvhs(n,7), DfT, DgT, hfT, hgT, sfT, sgT, y, um=1.0d0

      do 100 i = 1, n
          satvhs(i,1) = T(i)                                        
          satvhs(i,2) = um/DfT( T(i), y, icode )                                        
          satvhs(i,3) = um/DgT( T(i), y, icode )                                        
          satvhs(i,4) = hfT( T(i), y, icode )                                        
          satvhs(i,5) = hgT( T(i), y, icode )                                        
          satvhs(i,6) = sfT( T(i), y, icode )                                        
          satvhs(i,7) = sgT( T(i), y, icode )                                        
  100 continue

      end
! -----------------------------------------------------------------------------      
      subroutine satDhsofp(n, p, satDhs)
! -----------------------------------------------------------------------------      
! Table of Sat Properties with Density function of p
! -----------------------------------------------------------------------------      
      integer n, i, icode
      REAL(KIND=8) ::  p(n), T, Tsat, Tsatp, satDhs(n,7), DfT, DgT, &
                  hfT, hgT, sfT, sgT, y

      do 100 i = 1, n
          T = Tsatp( p(i), Tsat, icode )
        satDhs(i,1) = p(i)                                        
          satDhs(i,2) = DfT( T, y, icode )                                        
          satDhs(i,3) = DgT( T, y, icode )                                        
          satDhs(i,4) = hfT( T, y, icode )                                        
          satDhs(i,5) = hgT( T, y, icode )                                        
          satDhs(i,6) = sfT( T, y, icode )                                        
          satDhs(i,7) = sgT( T, y, icode )                                        
  100 continue

      end
! -----------------------------------------------------------------------------      
      subroutine satvhsofp(n, p, satvhs)
! -----------------------------------------------------------------------------      
! Table of Sat Properties with Specif volume function of p
! -----------------------------------------------------------------------------      
      integer n, i, icode
      REAL(KIND=8) ::  p(n), T, Tsat, Tsatp, satvhs(n,7), DfT, DgT, &
                  hfT, hgT, sfT, sgT, y, um = 1.0d0

      do 100 i = 1, n
          T = Tsatp( p(i), Tsat, icode )
        satvhs(i,1) = p(i)                                        
          satvhs(i,2) = um/DfT( T, y, icode )                                        
          satvhs(i,3) = um/DgT( T, y, icode )                                        
          satvhs(i,4) = hfT( T, y, icode )                                        
          satvhs(i,5) = hgT( T, y, icode )                                        
          satvhs(i,6) = sfT( T, y, icode )                                        
          satvhs(i,7) = sgT( T, y, icode )                                        
  100 continue

      end

! -----------------------------------------------------------------------------      
      subroutine vTpcteTab(nT, p, T, y)
! -----------------------------------------------------------------------------      
! Table of v function of T, p
! -----------------------------------------------------------------------------      
      integer nT, i, icode
      REAL(KIND=8) ::  p, T(nT), y(nT,2), vTp, v

      do 100 i = 1, nT
          y(i,1) = T(i)                                        
          y(i,2) = vTp( T(i), p, v, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine vpTcteTab(np, T, p, y)
! -----------------------------------------------------------------------------      
! Table of v function of T, p
! -----------------------------------------------------------------------------      
      integer np, i, icode
      REAL(KIND=8) ::  p(np), T, y(np,2), vTp, v

      do 100 i = 1, np
          y(i,1) = p(i)                                        
          y(i,2) = vTp( T, p(i), v, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine DTpcteTab(nT, p, T, y)
! -----------------------------------------------------------------------------      
! Table of D function of T, p
! -----------------------------------------------------------------------------      
      integer nT, i, icode
      REAL(KIND=8) ::  p, T(nT), y(nT,2), DTp, D

      do 100 i = 1, nT
          y(i,1) = T(i)                                        
          y(i,2) = DTp( T(i), p, D, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine DpTcteTab(np, T, p, y)
! -----------------------------------------------------------------------------      
! Table of D function of T, p
! -----------------------------------------------------------------------------      
      integer np, i, icode
      REAL(KIND=8) ::  p(np), T, y(np,2), DTp, D

      do 100 i = 1, np
          y(i,1) = p(i)                                        
          y(i,2) = DTp( T, p(i), D, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine hTpcteTab(nT, p, T, y)
! -----------------------------------------------------------------------------      
! Table of h function of T, p
! -----------------------------------------------------------------------------      
      integer nT, i, icode
      REAL(KIND=8) ::  p, T(nT), y(nT,2), hTp, h

      do 100 i = 1, nT
          y(i,1) = T(i)                                        
          y(i,2) = hTp( T(i), p, h, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine hpTcteTab(np, T, p, y)
! -----------------------------------------------------------------------------      
! Table of h function of T, p
! -----------------------------------------------------------------------------      
      integer np, i, icode
      REAL(KIND=8) ::  p(np), T, y(np,2), hTp, h

      do 100 i = 1, np
          y(i,1) = p(i)                                        
          y(i,2) = hTp( T, p(i), h, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine sTpcteTab(nT, p, T, y)
! -----------------------------------------------------------------------------      
! Table of s function of T, p
! -----------------------------------------------------------------------------      
      integer nT, i, icode
      REAL(KIND=8) ::  p, T(nT), y(nT,2), sTp, s

      do 100 i = 1, nT
          y(i,1) = T(i)                                        
          y(i,2) = sTp( T(i), p, s, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine spTcteTab(np, T, p, y)
! -----------------------------------------------------------------------------      
! Table of s function of T, p
! -----------------------------------------------------------------------------      
      integer np, i, icode
      REAL(KIND=8) ::  p(np), T, y(np,2), sTp, s

      do 100 i = 1, np
          y(i,1) = p(i)                                        
          y(i,2) = sTp( T, p(i), s, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine uTpcteTab(nT, p, T, y)
! -----------------------------------------------------------------------------      
! Table of u function of T, p
! -----------------------------------------------------------------------------      
      integer nT, i, icode
      REAL(KIND=8) ::  p, T(nT), y(nT,2), uTp, u

      do 100 i = 1, nT
          y(i,1) = T(i)                                        
          y(i,2) = uTp( T(i), p, u, icode )                                       
  100 continue
      end

! -----------------------------------------------------------------------------      
      subroutine upTcteTab(np, T, p, y)
! -----------------------------------------------------------------------------      
! Table of u function of T, p
! -----------------------------------------------------------------------------      
      integer np, i, icode
      REAL(KIND=8) ::  p(np), T, y(np,2), uTp, u

      do 100 i = 1, np
          y(i,1) = p(i)                                        
          y(i,2) = uTp( T, p(i), u, icode )                                       
  100 continue
      end

!----------------------------------------------------------------------------------------
     REAL(KIND=8) function CNDTD( T, D, Cdty, icode )
!----------------------------------------------------------------------------------------
!
!  THERMAL CONDUCTIVITY FUNCTION OF T AND D
!
!  INPUT:     T            TEMPERATURE [K]
!             D            DENSITY [KG / M ** 3]
! 
!  OUTPUT:    CNDTD/Cdty   [ W m-1 K-1 ]
!           icode (error code)
!
! -----------------------------------------------------------------------------      
     Implicit REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CND  /  Lij(5,6), Lk(5)
      COMMON / CND2 /  Lambdao, qD1, Nicr, Gamacr, Csi0, Gamao0, TRbar, xMi, qC1, qD1v

     REAL(KIND=8) ::  Lambdao, qD1, Nicr, Gamacr, Csi0, Gamao0, TRbar, xMi, qC1, qD1v
     REAL(KIND=8) ::  Lij, Lk, num, numT
     REAL(KIND=8) ::  pLim(5), Tlim(5)
     REAL(KIND=8) ::  TbarE(5), TbarI(5), rhobar1(6), um = 1.0d+0
     data Pi/3.1415926535d+0/
     data pLim/100.d0, 250.d0, 687.d0, 785.d0, 1000.d0/
     data TLim/1173.15d0, 874.d0, 573.d0, 403.d0, 348.d0/

      icode = 0
     p     = pTD( T,  D, p, icode )
     if ( (p .lt. 0.d0) .or. (p .gt. pLim(5)) ) icode = -1002    !   Outside pressure limit
     if ( (T .lt. TTR ) .or. (T .gt. TLim(1)) ) icode = -1001    !   Outside Temperature limit
     if (icode .eq. 0) then
        if (p .gt. pLim(4)) then
          if (T .gt. Tlim(5)) icode = -1212      !   Outside Pressure-Temperature Range
       else if (p .gt. pLim(3)) then
          if (T .gt. Tlim(4)) icode = -1212
       else if (p .gt. pLim(2)) then
          if (T .gt. Tlim(3)) icode = -1212
       else                              !         if (p .gt. pLim(1)) then
          if (T .gt. Tlim(2)) icode = -1212
       end if
     end if
     CNDTD = DFLOAT(icode)
     if (icode .eq. 0) then
     Tbar  = T / TC
     Dbar  = D / DC

     Visc  = VISCTD( T, D, Vscty, icode )

     Visbar= Visc / VISRF

     TbarE(1) = um
     TbarE(2) = Tbar
     TbarE(3) = Tbar*Tbar
     TbarE(4) = Tbar*TbarE(3)
     TbarE(5) = Tbar*TbarE(4)
     
     den   = 0.d+0
     do k = 1, 5
        den = den + Lk(k)/ TbarE(k)
     end do
     CND0 = DSQRT(Tbar) / den

     TbarI(1) = um
     TbarI(2) = ( um/Tbar - um)
     TbarI(3) = TbarI(2) * TbarI(2)
     TbarI(4) = TbarI(3) * TbarI(2)
     TbarI(5) = TbarI(4) * TbarI(2)

     rhobar1(1) = um
     rhobar1(2) = ( Dbar - um )
     rhobar1(3) = rhobar1(2) * rhobar1(2)
     rhobar1(4) = rhobar1(3) * rhobar1(2)
     rhobar1(5) = rhobar1(4) * rhobar1(2)
     rhobar1(6) = rhobar1(5) * rhobar1(2)

     numT = 0.d+0
     do i = 1, 5
        sum = 0.d+0
       do j = 1, 6
          sum = sum + Lij(i,j)*rhobar1(j)
       end do
       numT = numT + TbarI(i)*sum
     end do
     CND1 = DEXP( Dbar*numT )

     pcDc  = PC / DC
     TcR   = TRbar*TC
     zetaT = pcDc/dpdDTD( T, D, dpdD, icode )
     zetaTR= pcDc/dpdDTD( TcR, D, dpdD, icode )
     DQui  = Dbar*( zetaT - zetaTr*TRbar/Tbar )
     Csi  = 0.0d0
     if (DQui .gt. 0.d0) then
        Csi   = Csi0 * (DQui/Gamao0)**(Nicr/Gamacr)
     end if
     y     = Csi/qD1
     Z     = 0.d+0 
     if ( y .ge. 1.2d-07 ) then
        Cp    = CpTD(T, D, Cp, icode)
        Cpbar = Cp/R
        capa1 = CvTD(T, D, Cv, icode) / Cp
        A     = ( um - capa1 )*DATAN(y) + capa1*y 
        B     = ( um - DEXP( -um/( um/y + y*y/(3.d+0*Dbar*Dbar) ) ) )
        Z     = 2.d+0*( A - B )/(Pi*y)
     end if
     CND2  = UM
     if (Z .gt. 0.d0) then
        CND2  = Lambdao*Dbar*Cpbar*Tbar*Z/Visbar
     end if

!     CDbar = CDTY / CNDRF
     CNDTD = CNDRF*(CND0*CND1 + CND2)
     Cdty  = CNDTD
     
     end if
!     Cdty  = CNDTD
     
     return
     end      

!----------------------------------------------------------------------------------------
     REAL(KIND=8) function VISCTD( T, D, Vscty, icode )
!----------------------------------------------------------------------------------------
!
!  DYNAMIC VISCOSITY FUNCTION OF T AND D
!
!  INPUT:     T            TEMPERATURE [K]
!             D            DENSITY [KG / M ** 3]
! 
!  OUTPUT:    VISCTD/Vscty   [ Pa s ]
!           icode (error code)
!
! -----------------------------------------------------------------------------      
     Implicit REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CND  /  Lij(5,6), Lk(5)
      COMMON / VISC /  Hij(6,7), Hi(4)
      COMMON / CND2 /  Lambdao, qD1, Nicr, Gamacr, Csi0, Gamao0, TRbar, xMi, qC1, qD1v

     REAL(KIND=8) ::  Lambdao, qD1, Nicr, Gamacr, Csi0, Gamao0, TRbar, xMi, qC1, qD1v
     REAL(KIND=8) ::  Lij, Lk, Lw, num
     REAL(KIND=8) ::  TbarE(5), TbarI(6), rhobar1(7), um=1.0d+0
     REAL(KIND=8) ::  shij(6,7), stb1(6)
     data Pi/3.1415926535d+0/
     data CsiL/0.3817016416d0/

     REAL(KIND=8) ::  pLim(4), Tlim(4)
     data pLim/300.d0, 350.d0, 500.d0, 1000.d0/
     data TLim/1173.15d0, 873.15d0, 433.15d0, 373.15d0/

      icode = 0
     p     = pTD( T,  D, p, icode )
     if ( (p .lt. 0.d0) .or. (p .gt. pLim(4)) ) icode = -1002    !   Outside pressure limit
     if ( (T .lt. TTR ) .or. (T .gt. TLim(1)) ) icode = -1001    !   Outside Temperature limit
     if (icode .eq. 0) then
        if (p .gt. pLim(3)) then
          if (T .gt. Tlim(4)) icode = -1212      !   Outside Pressure-Temperature Range
       else if (p .gt. pLim(2)) then
          if (T .gt. Tlim(3)) icode = -1212
       else if (p .gt. pLim(1)) then
          if (T .gt. Tlim(2)) icode = -1212
       else                              
          if (T .gt. Tlim(1)) icode = -1212
       end if
     end if
     VISCTD = DFLOAT(icode)
     if (icode .eq. 0) then

      Tbar  = T / TC
!     pbar  = p / pc
     Dbar  = D / DC

     TbarE(1) = um
     TbarE(2) = Tbar
     TbarE(3) = Tbar*Tbar
     TbarE(4) = Tbar*TbarE(3)
     
     den   = 0.d+0
     do i = 1, 4
        den = den + Hi(i)/ TbarE(i)
     end do
     VISC0 = 1.0d+2 * DSQRT(Tbar) / den

     TbarI(1) = um
     TbarI(2) = ( um/Tbar - um)
     TbarI(3) = TbarI(2) * TbarI(2)
     TbarI(4) = TbarI(3) * TbarI(2)
     TbarI(5) = TbarI(4) * TbarI(2)
     TbarI(6) = TbarI(5) * TbarI(2)

     rhobar1(1) = um
     rhobar1(2) = ( Dbar - um )
     rhobar1(3) = rhobar1(2) * rhobar1(2)
     rhobar1(4) = rhobar1(3) * rhobar1(2)
     rhobar1(5) = rhobar1(4) * rhobar1(2)
     rhobar1(6) = rhobar1(5) * rhobar1(2)
     rhobar1(7) = rhobar1(6) * rhobar1(2)

     sumT = 0.d+0
     do i = 1, 6
        sum = 0.d+0
       do j = 1, 7
          sum = sum + Hij(i,j)*rhobar1(j)
         shij(i,j) = Hij(i,j)*rhobar1(j)
       end do
       sumT = sumT + TbarI(i)*sum
       stb1(i) = TbarI(i)*sum
     end do

     sum = 0.d0
     do i = 1, 6
        sum = sum + stb1(i)
     end do

     VISC1 = DEXP( Dbar*sumT )

     pcDc  = PC / DC
     TcR   = TRbar*TC
     zetaT = pcDc/dpdDTD( T, D, dpdD, icode )
     zetaTR= pcDc/dpdDTD( TcR, D, dpdD, icode )
     DQui  = Dbar*( zetaT - zetaTr*TRbar/Tbar )
     Csi  = 0.0d0
     if (DQui .gt. 0.d0) then
        Csi   = Csi0 * (DQui/Gamao0)**(Nicr/Gamacr)
     end if
     y     = Csi/qD1v
     PsiD  = DACOS(um/DSQRT(um + y*y))
     y2    = Csi/qC1
     w     = DSQRT(DABS((y2 - um)/(y2 + um)))*DTAN(0.5d0*PsiD)
     if ( y2 .gt. um ) then
        Lw   = DLOG( (um + w)/(um - w) )
     else
        Lw   = 2.d0*DATAN(DABS(w))
     end if

     if (Csi .gt. CsiL) then
        YG  = DSIN(3.0d0*PsiD)/12.d0 - 0.25d0*DSIN(2.0d0*PsiD)/y2 +      &
            DSIN(PsiD)*(um/(y2*y2) - 1.25d0) -                      &
             (um/(y2*y2*y2))*( PsiD*(um-1.5d0*y2*y2) -               &
             Lw*DABS(y2*y2 - um)**1.5d0 )
     else
        YG  = 0.2d0*y2*(y**5)*( um - y2 + y2*y2 - (765.d0/504.d0)*y*y )
     end if
     VISC2 = um
     if (YG .gt. 0.d0) then
        VISC2 = DEXP( xMi*YG )
     end if

     VISCTD = VISRF*(VISC0*VISC1*VISC2)
     end if
     Vscty  = VISCTD
     
     return
     end      

!----------------------------------------------------------------------------------------
     REAL(KIND=8) function KVISCTD( T, D, KVscty, icode )
!----------------------------------------------------------------------------------------
!
!  KINEMATIC VISCOSITY FUNCTION OF T AND D
!
!  INPUT:     T            TEMPERATURE [K]
!             D            DENSITY [KG / M ** 3]
! 
!  OUTPUT:    VISCTD/Vscty   [ Pa s ]
!           icode (error code)
!
! -----------------------------------------------------------------------------      
     Implicit REAL(KIND=8) (a-h, o-z)
     REAL(KIND=8) ::  KVscty
     integer icode

     KVISCTD = VISCTD( T, D, Vscty, icode )/D
     KVscty  = KVISCTD
     return
     end

!----------------------------------------------------------------------------------------
     REAL(KIND=8) function PrandtTD( T, D, Prandt, icode )
!----------------------------------------------------------------------------------------
!
!  PRANDT NUMBER FUNCTION OF T AND D
!
!  INPUT:     T            TEMPERATURE [K]
!             D            DENSITY [KG / M ** 3]
! 
!  OUTPUT:    PrandtTD/Prandt   [ - ]
!           icode (error code)
!
! -----------------------------------------------------------------------------      
     Implicit REAL(KIND=8) (a-h, o-z)
     integer icode

     Cp    = 1.0d3*CpTD(T, D, Cp, icode)
     if (icode .eq. 0) then
        Visc  = VISCTD( T, D, Vscty, icode )
     end if
     if (icode .eq. 0) then
        Cndt  = CNDTD( T, D, Cdty, icode )
     end if
     if (icode .eq. 0) then
        PrandtTD = Visc*Cp/Cndt
     else
        PrandtTD = DFLOAT(icode)
     end if
     Prandt= PrandtTD

     return
     end

!----------------------------------------------------------------------------------------
     REAL(KIND=8) function SigmaT( T, Sigma, icode )
!----------------------------------------------------------------------------------------
!
!  SURFACE TENSION FUNCTION OF T
!
!  INPUT:     T            TEMPERATURE [K]
!             D            DENSITY [KG / M ** 3]
! 
!  OUTPUT:    SigmaT/Sigma   [ mN/m ]
!           icode (error code)
!
! -----------------------------------------------------------------------------      
     Implicit none
     integer icode
     REAL(KIND=8) ::  BG=235.8d0, bp=-0.625d0, mi=1.256d0, um=1.0d0
     REAL(KIND=8) ::  T, Sigma, Tal
      REAL(KIND=8) :: R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

     if ( (T .ge. TTR) .and. (T .le. TC) ) then
        Tal    = um - T / TC
        SigmaT = BG*(Tal**mi)*(um + bp*Tal)
        icode  = 0
     else
        icode  = -1001
       SigmaT = DFLOAT(icode)
     end if
     
     Sigma  = SigmaT

     return
     end



! -----------------------------------------------------------------------------      
! HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
! H           PARTS OF THE DIMENSIONLESS Helmholtz FREE ENERGY                 H
! HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHI0TD(T, D, fhiztd, icode )
! -----------------------------------------------------------------------------      
!
!  IDEL-GAS PART OF THE DIMENSIONLESS Helmholtz FREE ENERGY FOR GIVEN T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
! 
!  OUTPUT:    PHI0 f/R T
!           icode (error code)
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (A-H,O-Z)
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     integer icode

      icode = 0

      IF ( T .LT. TTR ) THEN
          PHI0TD = -1001.D0
         icode = -1001
        goto 1000
     endif
     IF ( D .LE. 0.D0 ) THEN
          PHI0TD = -1003.D0
         icode = -1003
          goto 1000
     ENDIF
     if ( T .eq. TC ) then
         T = T - 1.d-13
     end if

      PHI0TD = Phio(T, D)

 1000 continue

     fhiztd = PHI0TD

      RETURN
      END
! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHI0DD(D, fhizd, icode)
! -----------------------------------------------------------------------------      
!
!  FIRST DERIVATIVE OF THE IDEAL GAS PART OF THE DIMENSIONLESS   Helmholtz
!   FREE ENERGY WITH RESPECT TO DENSITY
!
!  INPUT:     D         DENSITY [KG / M ** 3]
! 
!  OUTPUT:    PHI0DD
!           icode (error code)
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      icode = 0

     IF ( D .LE. 0.D0 ) THEN
          PHI0DD = -1003.D0
         icode = -1003
          goto 1000
      ENDIF

      PHI0DD = 322.0d+0/D

 1000 continue

     fhizd = PHI0DD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHI0DDD(D, fhizdd, icode)
! -----------------------------------------------------------------------------      
!
!  SECOND DERIVATIVE OF THE IDEAL GAS PART OF THE DIMENSIONLESS   Helmholtz 
!   FREE ENERGY FUNCTION OF DENSITY
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PHI0DDD, icode
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      icode = 0
     IF ( D .LE. 0.D0 ) THEN
          PHI0DDD = -1003.D0
         icode = -1003
          goto 1000
      ENDIF

      PHI0DDD = -103684.d+0/(D*D)      ! -1/delta**2 = -1 / (D/DC)**2

 1000 continue

     fhizdd = PHI0DDD

      RETURN
      END
! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHI0TTD(T, D, fhizttd, icode)
! -----------------------------------------------------------------------------      
!
!  FUNCTION FOR THE CALCULATION OF THE FIRST DERIVATIVE OF THE IDEAL GAS PART 
!  OF THE DIMENSIONLESS   Helmholtz FREE ENERGY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PHI0 f/R T
!           icode (error code)
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ( T .LT. TTR ) THEN
          PHI0TTD = -1001.D0
         icode = -1001
     else IF ( D .LE. 0.D0 ) THEN
          PHI0TTD = -1003.D0
         icode = -1003
     else
       icode = 0
      if ( T .eq. TC ) then                
         T = T - 1.d-13
      end if
       PHI0TTD = PhioT(T, D)
     endif

     fhizttd = PHI0TTD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHI0TTTD(T, D, fhiztttd, icode)
! -----------------------------------------------------------------------------      
!
!  SECOND DERIVATIVE OF THE IDEAL GAS PART OF THE DIMENSIONLESS
!   Helmholtz FREE ENERGY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PHI0 f/R T
!           icode (error code)
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ( T .LT. TTR ) THEN
          PHI0TTTD = -1001.D0
         icode = -1001
     elseif ( D .LE. 0.D0 ) THEN
            PHI0TTTD = -1003.D0
           icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        PHI0TTTD = PhioTT(T, D)

     end if

     fhiztttd = PHI0TTTD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHI0DT(fhizdt, icode)
! -----------------------------------------------------------------------------      
!
!  DERIVATIVE OF THE IDEAL GAS PART OF THE DIMENSIONLESS
!   Helmholtz FREE ENERGY
!
!  INPUT:     NONE
! 
!  OUTPUT:    PHI0DT
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      icode = 0
     PHI0DT = 0.d+00
     fhizdt = PHI0DT

      RETURN
      END


! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHIRTD( T, D, fhirtd, icode)
! -----------------------------------------------------------------------------      
!
!  RESIDUAL-GAS PART OF THE DIMENSIONLESS
!   Helmholtz FREE ENERGY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PHIR f/R T
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     integer icode


      IF ( T .LT. TTR ) THEN
          PHIRTD = -1001.D0
         icode = -1001
     elseif ( D .LE. 0.D0 ) THEN
          PHIRTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        PHIRTD = Phir(T, D)
     end if

     fhirtd = PHIRTD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHIRDTD( T, D, fhird, icode)
! -----------------------------------------------------------------------------      
!
!   FIRST DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY 
!   WITH RESPECT TO REDUCED DENSITY AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PHIRD
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     integer icode

      IF ( T .LT. TTR ) then
          PHIRDTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          PHIRDTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        PHIRDTD = PhirD(T, D)
     end if

     fhird = PHIRDTD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHIRDDTD( T, D, fhirdd, icode)
! -----------------------------------------------------------------------------      
!
!  SECOND DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY 
!   WITH RESPECT TO REDUCED DENSITY AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PHIRDD
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     integer icode


      if ( T .LT. TTR ) then
          PHIRDDTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          PHIRDDTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        PHIRDDTD = PhirDD(T, D)
     end if

     fhirdd = PHIRDDTD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHIRTTD( T, D, fhirt, icode)
! -----------------------------------------------------------------------------      
!
!  FIRST DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY 
! WITH RESPECT TO THE TEMPERATURE AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PHIRT
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          PHIRTTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          PHIRTTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        PHIRTTD = PhirT(T, D)
     end if

     fhirt = PHIRTTD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHIRTTTD( T, D, fhirtt, icode)
! -----------------------------------------------------------------------------      
!
!  SECOND DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY
!   WITH RESPECT TO THE REDUCED TEMPERATURE AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhirTT
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     integer icode

      if ( T .LT. TTR ) then
          PHIRTTTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          PHIRTTTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        PHIRTTTD = PhirTT(T, D)
     end if

     fhirtt = PHIRTTTD

      RETURN
      END

! -----------------------------------------------------------------------------      
      REAL(KIND=8) function PHIRDTTD( T, D, fhirdt, icode)
! -----------------------------------------------------------------------------      
!
!   SECOND DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY 
!   WITH RESPECT TO THE REDUCED DENSITY AND TEMPERATURE AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
! 
!  OUTPUT:    PHIRDT
!
! -----------------------------------------------------------------------------      

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     integer icode

      if ( T .LT. TTR ) then
          PHIRDTTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          PHIRDTTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        PHIRDTTD = PhirDT(T, D)
     end if

     fhirdt = PHIRDTTD

      RETURN
      END
! HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
  
! -----------------------------------------------------------------------------
      REAL(KIND=8) function pMeltT(T, pMelt, pMeltIh, pSubl, icode)
! -----------------------------------------------------------------------------
!
!  MELTING PRESSURE   AS FUNCTION OF T (256.164K < T < 715K)
!
!  INPUT:     T         TEMPERATURE [K]
! 
!  OUTPUT:    pMelt     PRESSURE OF MELTING [ MPa ]
!           icode (codigo de erro)
!
! -----------------------------------------------------------------------------

      IMPLICIT NONE
     REAL(KIND=8) ::  T, pSubl, pMelt, pMeltIh, um=1.0d0, pi, teta
     REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     REAL(KIND=8) ::  Ts0=50.d0, Tah=273.16d0,T0=273.16d0, T3=251.165d0, T4=256.164,      &
                      T5=273.31D0, T6=355.D0, T7=715.D0
     REAL(KIND=8) ::  Ta3=251.165d0,Ta5=256.164d0, Ta6=273.31d0, Ta7=355.d0
     REAL(KIND=8) ::  pah=611.657d-6, pa3=208.566d0, pa5=350.1d0, pa6=632.4d0, pa7=2216.d0
     REAL(KIND=8) ::  ah(3), bh(3)
     data ah/0.119539337d+7, 0.808183159d+5, 0.333826860d+4/
     data bh/0.300000d+1, 0.257500d+2, 0.1037250d+3/
     REAL(KIND=8) ::  as(3), bs(3), tlnpi
     data as/-0.212144006d+2, 0.273203819d+2, -0.610598130d+1/
     data bs/0.333333333d-2, 0.120666667d+1, 0.170333333d+1/
     REAL(KIND=8) ::  a31=-0.299948d0, a51=- 1.18721d0, a61=-1.07476d0
     REAL(KIND=8) ::  a71=0.173683d+1, a72=-0.544606d-1, a73=0.806106d-7
     REAL(KIND=8) ::  e61=4.6d0

     integer :: e31=60, e51=8, e71=-1, e72=5, e73=22                                           
     integer icode, i

      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

     pMeltIh = -3001.D0
     pMeltT  = -3001.D0
     pSubl   = -3001.D0
      IF ( T .LT. Ts0 ) THEN
        icode = -3001
        goto 1000
     ENDIF

!   SUBLIMATION PRESSURE  ( 50. < T < 273.16 )
     if ( (T .ge. Ts0) .and. (T .le. T0) ) then
        teta = T / T0
        tlnpi = 0.d0
        do i = 1,3
         tlnpi = tlnpi + as(i)*teta**bs(i)
        enddo
        pSubl = pah*DEXP( tlnpi/teta )
!        icode = -44444
        icode = 0
     endif
!   REGION Ih   ( 251.165 < T < 273.16 )
     if ( (T .ge. T3) .and. (T .le. T0) ) then
        teta = T / Tah
        pi = um
        do i = 1,3
         pi = pi + ah(i)*( um - teta**bh(i) )
        enddo
        pMeltIh = pah*pi
     endif
!   REGION III    ( 251.165 < T < 256.164 )
      if ( (T .gt. T3) .and. (T .le. T4) ) then
        teta = T / Ta3
        pMeltT   = pa3*( um + a31*(um - teta**e31) )
!   REGION V   ( 251.165 < T < 273.31 )
      else if  ( (T .gt. T3) .and. (T .le. T5) ) then
        teta = T / Ta5
        pMeltT   = pa5*( um + a51*(um - teta**e51) )
!   REGION VI   ( 273.31 < t < 355. )
      else if ( (T .gt. T5) .and. (T .le. T6) ) then
         teta = T / Ta6
         pMeltT   = pa6*( um + a61*(um - teta**e61) )
!   REGION VII     ( 355. < T < 715. )
      else if ( (T .gt. T6) .and. (T .le. T7) ) then
         teta = T / Ta7
         pMeltT = pa7*DEXP( a71*(um - um/teta) + a72*(um - teta**e72) + a73*(um - teta**e73) )
       else if ( T .GT. T7 ) then     !   ( T > 715. )
!   Over temp. limit of equations  T > 715. K
          pMeltT = 20617.8128d0
        icode = -3002
      else
        goto 1000
     ENDIF

1000  continue

     pMelt = pMeltT

     return
     end


! -----------------------------------------------------------------------------
      REAL(KIND=8) function pTD(T, D, p, icode)
! -----------------------------------------------------------------------------
!
!  PRESSURE   AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
! 
!  OUTPUT:    pTD/p     PRESSURE [MPA]
!           icode (codigo de erro)
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          pTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          pTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if
        XTP = 2.0D+00
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5d+0) THEN
          pTD = calcp(T,D)
          IF (pTD .LE. 0.0D+0) THEN
              pTD = -1002.D+0
             icode = -1002
          ENDIF
        ELSE
          pTD = PTP
        ENDIF
     end if
     p = pTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function hTD(T, D, h, icode)
! -----------------------------------------------------------------------------
! 
!  SPECIFIC ENTHALPY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    hTD/h     SPECIFIC ENTHALPY [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          hTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          hTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
          hTD = calch(T,D)
        ELSE
          HL = calch(T,DLTP)
          HV = calch(T,DVTP)
          hTD = HL + XTP * (HV - HL)
        ENDIF
     end if
      h = hTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function sTD( T, D, s, icode )
! -----------------------------------------------------------------------------
!  
!  SPECIFIC ENTROPY   FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    sTD      SPECIFIC ENTROPY [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      integer icode

      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          sTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          sTD = -1003.D0
         icode = -1003
     else
        icode = 0
       if ( T .eq. TC ) then
         T = T - 1.d-13
       end if

        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
          sTD = calcs(T,D)
        ELSE
          SL = calcs(T,DLTP)
          SV = calcs(T,DVTP)
          sTD = SL + XTP * (SV - SL)
        ENDIF
     end if
     s = sTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function fTD(T, D, f, icode)
! -----------------------------------------------------------------------------
!
!  HELMHOLTZ ENERGY AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    fTD       HELMHOLTZ ENERGY [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      integer icode

      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          fTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          fTD = -1003.D0
         icode = -1003
     else
        icode = 0

        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
          fTD = calcf(T,D)
        ELSE
          FL = calcf(T,DLTP)
          FV = calcf(T,DVTP)
          fTD = FL + XTP * (FV - FL)
        ENDIF
     end if
      f = fTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function fTp( T, p, f, icode)
! -----------------------------------------------------------------------------
!
!  SPECIFIC HELMHOLTZ ENERGY AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    fTp       SPECIFIC HELMHOLTZ ENERGY [KJ / KG]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          fTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          fTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,P,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
        fTp = calcf(T,DBER)
        ELSE
          fTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     f = fTp

      RETURN
      END



! -----------------------------------------------------------------------------
      REAL(KIND=8) function uTD( T, D, u, icode )
! -----------------------------------------------------------------------------
!
!  INTERNAL ENERGY AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M**3]
!
!  OUTPUT:    uTD       INTERNAL ENERGY [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          uTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          uTD = -1003.D0
         icode = -1003
     else
        icode = 0
        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
          uTD = calcu(T,D)
        ELSE
          UL = calcu(T,DLTP)
          UV = calcu(T,DVTP)
          uTD = UL + XTP * (UV - UL)
        ENDIF
     end if
     u = uTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CvTD( T, D, Cv, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOCHORIC HEAT CAPACITY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    Cv        SPECIFIC HEAT CAPACITY [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          CvTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          CvTD = -1003.D0
         icode = -1003
     else
        icode = 0
        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
          CvTD = calcCv(T,D)
        ELSE
          CvTD = -1004.D0
         icode = -1004
        ENDIF
     end if
     Cv = cvTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CpTD( T, D, Cp, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOBARIC HEAT CAPACITY AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    CpTD      SPECIFIC ISOBARIC HEAT CAPACITY [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /   R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          CpTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          CpTD = -1003.D0
         icode = -1003
     else
        icode = 0
        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
          CpTD = calcCp(T,D)
        ELSE
          CpTD = -1004.D0
         icode = -1004
        ENDIF
     end if
     Cp = CpTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function wTD( T, D, w, icode )
! -----------------------------------------------------------------------------
!
!  SPEED OF SOUND FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    w         SPEED OF SOUND [M / S]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          wTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          wTD = -1003.D0
         icode = -1003
     else
        icode = 0
        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
          wTD = calcw(T,D)
        ELSE
          wTD = -1004.D0
         icode = -1004
        ENDIF
     end if
     w = wTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function vTp( T, p, v, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC VOLUME AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    vTp      SPECIFIC VOLUME [ M**3 / kg]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          vTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          vTp = -1002.D0
         icode = -1002
     else
        icode = 0
       vTp = 1.0d0 / DTp( T, p, D, icode )
     end if
     v = vTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DTp( T, p, D, icode )
! -----------------------------------------------------------------------------
!
!  DENSITY AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    DTpP      DENSITY [KG / M**3]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          DTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          DTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,P,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          DTp = DBER
        ELSE
          DTp   = -1013.D0
         icode = -1013
        ENDIF
     end if
     D = DTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function hTp( T, p, h, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTHALPY AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    hTp       SPECIFIC ENTHALPY [KJ / KG]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          hTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          hTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,P,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          hTp = calch(T,DBER)
        ELSE
          hTp   = -1013.D0
         icode = -1013
        ENDIF
     end if
     h = hTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function sTp( T, p, s, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTROPY AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    sTp       SPECIFIC ENTROPY [KJ / (KG * K)]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          sTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          sTp = -1002.D0
         icode = -1002
     else if ( (T .EQ. TC) .AND. (p .EQ. PC) ) then
          sTp = SC
         icode = 0
     else
       icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          sTp = calcs(T,DBER)
        ELSE
          sTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     s = sTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function uTp( T, p, u, icode)
! -----------------------------------------------------------------------------
!
!  SPECIFIC INTERNAL ENERGY AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    uTp       SPECIFIC INTERNAL ENERGY [KJ / KG]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          uTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          uTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          uTP = calcu(T,DBER)
        ELSE
          uTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     u = uTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CvTP( T, p, Cv, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOCHORIC HEAT CAPACITY AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             p         PRESSURE [MPA]
!
!  OUTPUT:    CvTp      SPECIFIC ISOCHORIC HEAT CAPACITY [KJ / (KG * K)]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     integer icode

      if ( T .LT. TTR ) then
          CvTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          CvTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          CvTp = calcCv(T,DBER)
        ELSE
          CvTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     Cv = CvTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CpTP( T, p, Cp, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOBARIC HEAT CAPACITY AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             p         PRESSURE [MPA]
!
!  OUTPUT:    CpTp      SPECIFIC ISOBARIC HEAT CAPACITY  [KJ / (KG * K)]
!
!   (DO NOT TEST ICE REGIONS)
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          CpTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          CpTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          CpTp = calcCp(T,DBER)
        ELSE
          CpTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     Cp = CpTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function wTp( T, p, w, icode )
! -----------------------------------------------------------------------------
!
!  SPEED OF SOUND AS FUNCTION OF T AND P
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    wTp       SPEED OF SOUND [M / S]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          wTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          wTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          wTp = calcw(T,DBER)
        ELSE
          wTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     w = wTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DTs( T, s, D, icode)
! -----------------------------------------------------------------------------
!
!  DENSITYAS FUNCTION OF T AND s
!
!  INPUT:     T         TEMPERATURE [K]
!             S         SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    DTs       DENSITY [KG / M**3]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          DTs = -1001.D0
         icode = -1001
     else if ( s .LE. 0.D0 ) then
          DTs = -1005.D0
         icode = -1005
     else
        icode = 0
        CALL TSITER(T,S,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          DTs = DBER
        ELSE
          DTs = -1013.D0
         icode = -1013
        ENDIF
     end if
     D = DTs

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DTh( T, h, D, Dupp, icode)
! -----------------------------------------------------------------------------
!
!  DENSITY AS FUNCTION OF T AND H
!
!  In some cases two different densities can be assigned to a set of input values of
!  temperature and enthalpy (liquid or two-phase). It would be necessary to use DTh/D 
!  (for the usualcalculation) and Dupp (for the region above the Joule-Thomson inversion curve).
!  
!  INPUT:     T            TEMPERATURE [K]
!             H            SPECIFIC ENTHALPY [KJ / KG]
!
!  OUTPUT:    DTh/Dupp      DENSITY [KG / M**3]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          DTh = -1001.D0
          Dupp = -1001.D0
         icode = -1001
     else if ( h .LE. -11.31D0 ) then        ! ********* 
          DTh = -1006.D0
          Dupp = -1006.D0
         icode = -1006
     else
        icode = 0
       DBER = 0.0D+0
        CALL THITER(T,h,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          DTh = DBER
        ELSE
          DTh = -1013.D0
         icode = -1013
        ENDIF
       DBER = 0.0D+0
        CALL THITERZ(T,h,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          Dupp = DBER
        ELSE
          Dupp = -1013.D0
         icode = -1013
        ENDIF
     end if
     D = DTh

      RETURN
      END


! -----------------------------------------------------------------------------
      REAL(KIND=8) function TDp( D, p, T, icode)
! -----------------------------------------------------------------------------
!
!  TEMPERATURE   AS FUNCTION OF D AND P
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    TDp       TEMPERATURE [K]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     if ( D .LE. 0.D0 ) then
          TDp   = -1003.D0
         icode = -1003
     else if ( p .LE. 0.D0 ) then
          TDp   = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL PDITER(p,D,TBER,1.D-9)
        IF (TBER .GT. 0.D0) THEN
          TDp = TBER
        ELSE
          TDp = -1011.D0
         icode = -1011
        ENDIF
     end if
     T = TDp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function TDs( D, s, T, icode)
! -----------------------------------------------------------------------------
!
!  TEMPERATURE as function OF D AND s
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!             S         SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    TDs       TEMPERATURE [K]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     if ( D .LE. 0.D0 ) then
          TDs   = -1003.D0
         icode = -1003
     else if ( s .LE. 0.D0 ) then        ! ********* 
          TDs   = -1005.D0
         icode = -1005
     else
        icode = 0
        CALL DSITER(D,S,TBER,1.D-9)
        IF (TBER .GT. 0.D0) THEN
          TDs = TBER
        ELSE
          TDs = -1011.D0
         icode = -1011
        ENDIF
     end if
     T = TDs

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function TDh( D, h, T, icode)
! -----------------------------------------------------------------------------
!
!  TEMPERATURE AS FUNCTION OF D AND h 
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!             H         SPECIFIC ENTHALPY [KJ / KG]
!
!  OUTPUT:    TDh       TEMPERATURE [K]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     if ( D .LE. 0.D0 ) then
          TDh   = -1003.D0
         icode = -1003
     else if ( h .LE. -11.31D0 ) THEN        ! ********* 
          TDh  = -1006.D0
         icode = -1006
     else
        icode = 0
        CALL DHITER(D,h,TBER,1.D-9)
        IF (TBER .GT. 0.D0) THEN
          TDh = TBER
        ELSE
          TDh   = -1011.D0
         icode = -1011
        ENDIF
     end if
     T = TDh

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Tps( p, s, T, icode)
! -----------------------------------------------------------------------------
!
!  TEMPERATURE AS FUNCTION OF p AND s
!
!  INPUT:     p         PRESSURE [MPA]
!             s         SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    Tps       TEMPERATURE [K]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     if ( p .LE. 0.D0 ) then
          Tps   = -1002.D0
         icode = -1002
      else if ( s .LE. 0.D0 ) THEN        ! ********* 
          Tps   = -1005.D0
         icode = -1005
     else
        icode = 0
        CALL PSITER(p,s,TBER,D,1.D-9)
        IF (TBER .GT. 0.D0) THEN
          Tps = TBER
        ELSE
          Tps = -1011.D0
         icode = -1011
        ENDIF
     end if
     T = Tps

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Dps( p, s, D, icode)
! -----------------------------------------------------------------------------
!
!  DENSITY AS FUNCTION OF p AND s
!
!  INPUT:     p         PRESSURE [MPA]
!             s         SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    Dps       DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     if ( p .LE. 0.D0 ) then
          Dps   = -1002.D0
         icode = -1002
      else if ( s .LE. 0.D0 ) then        ! ********* 
          Dps   = -1005.D0
         icode = -1005
     else
        icode = 0
        CALL PSITER(p,s,T,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          Dps = DBER
        ELSE
          Dps = -1013.D0
         icode = -1013
        ENDIF
     end if
     D = Dps

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function hps( p, s, h, icode)
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTHALPY AS FUNCTIONOF p AND s
!
!  INPUT:     p         PRESSURE [MPA]
!             s         SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    hps       SPECIFIC ENTHALPY [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

     if ( p .LE. 0.D0 ) then
          hps   = -1002.D0
         icode = -1002
      else if ( s .LE. 0.D0 ) then        ! ********* 
          hps   = -1005.D0
         icode = -1005
     else
        icode = 0
        CALL PSITER(p,s,TBER,DBER,1.D-9)
        IF ((TBER .GT. 0.D0) .AND. (DBER .GT. 0.D0)) THEN
          XTP = 2.D0
          IF (TBER .LE. TC) THEN
              CALL QUALY(TBER,DBER,XTP,DVTP,DLTP,PTP)
          ENDIF
          IF (XTP .GT. 1.5D0) THEN
              hps = calch(TBER,DBER)
          ELSE
              HL = calch(TBER,DLTP)
              HV = calch(TBER,DVTP)
              hps = HL + XTP * (HV - HL)
          ENDIF
        ELSE
          hps = -1004.D0
         icode = -1004
        ENDIF
     end if
     h = hps

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Tph( p, h, T, icode )
! -----------------------------------------------------------------------------
!
!  TEMPERATURE   AS FUNCTION OF p AND h
!
!  INPUT:     p         PRESSURE [MPA]
!             h         SPECIFIC ENTHALPY [KJ / KG]
!
!  OUTPUT:    Tph       TEMPERATURE [K]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     if ( p .LE. 0.D0 ) then
          Tph   = -1002.D0
         icode = -1002
     else if ( h .LE. -11.31D0 ) then        ! ********* 
          Tph  = -1006.D0
         icode = -1006
     else
        icode = 0
        CALL PHITER(p,h,TBER,D,1.D-9)
        IF (TBER .GT. 0.D0) THEN
          Tph = TBER
        ELSE
          Tph = -1011.D0
         icode = -1011
        ENDIF
     end if
     T = Tph

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Dph( p, h, D, icode )
! -----------------------------------------------------------------------------
!
!  DENSITY AS FUNCTION OF P AND H
!
!  INPUT:     p         PRESSURE [MPA]
!             h         SPECIFIC ENTHALPY [KJ / KG]
!
!  OUTPUT:    Dph       DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     if ( p .LE. 0.D0 ) then
          Dph   = -1002.D0
         icode = -1002
     else if ( h .LE. -11.31D0 ) then        ! ********* 
          Dph  = -1006.D0
         icode = -1006
     else
        icode = 0
        CALL PHITER(p,h,T,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          Dph = DBER
        ELSE
          Dph = -1013.D0
         icode = -1013
        ENDIF
     end if
     D = Dph

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function sph( p, h, s, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTROPY AS FUNCTION OF p AND h
!
!  INPUT:     p         PRESSURE [MPA]
!             h         SPECIFIC ENTHALPY [KJ / KG]
!
!  OUTPUT:    sph       SPECIFIC ENTROPY [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

     if ( p .LE. 0.D0 ) then
          sph    = -1002.D0
        icode  = -1002
     else if ( h .LE. 0.D0 ) then        ! ********* 
          sph  = -1006.D0
         icode = -1006
     else
        icode = 0
        CALL PHITER(p,h,TBER,DBER,1.D-9)
        IF ((TBER .GT. 0.D0) .AND. (DBER .GT. 0.D0)) THEN
          XTP = 2.D0
          IF (TBER .LE. TC) THEN
              CALL QUALY(TBER,DBER,XTP,DVTP,DLTP,PTP)
          ENDIF
          IF (XTP .GT. 1.5D0) THEN
              sph = calcs(TBER,DBER)
          ELSE
              SL = calcs(TBER,DLTP) 
              SV = calcs(TBER,DVTP)
              sph = SL + XTP * (SV - SL)
          ENDIF
        ELSE
          sph = -1004.D0
         icode = -1004
        ENDIF
     end if
     s = sph

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Ths( h, s, T, icode)
! -----------------------------------------------------------------------------
!
!  TEMPERATURE AS FUNCTION OF h AND s
!
!  INPUT:     h        SPEC. ENTHALPY [KJ/Kg]
!           s        SPEC. ENTROPY [KJ/Kg K]
!
!  OUTPUT:    Ths      TEMPERATURE [K]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
      integer :: icode, ic, imax=200
      REAL(KIND=8) ::  EPS= 1.0D-9, meio= 0.5d+0

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     COMMON / ChsMax  / ChsMax1(4), ChsMax2(7)
     COMMON / ChsMin  / ChsMin1(2), ChsMin2(4)
     COMMON / ChsCrit / Chspcrit(4), ChsTcrit(7)
     COMMON / Chsx    / Chsx0(3), Chsx1(7)
     COMMON / shLim   / sMax1, sMax2, sMin1, sMin2, shpcr1, shpcr2,      &
                   shTcr1, sTcri2, shx11, shx12, shx01, shx02
     COMMON / LIMITES / Tmin, TMAXI, PMAXI

     call hsLimites( h, s, hMax, hMin, hx0, hx1, icode )
     T0  = TSats( s, Tsat, icode )
     hf0 = hfT( T0, hf, icode )

     if ( h .GT. 0.999d+0*hMax ) then        ! ********* 
!          Ths  = hMax
          Ths  = -9999.
        icode = -9999
      else if ( h .LT. 1.001d+0*hMin ) then        ! ********* 
!          Ths  = hMin
          Ths  = -1111.
        icode = -1111
!     else if ( (h - hx0)/hx0 .ge. -1.5d-02 ) then
     else if ( h .lt. hf0 ) then
!       T0  = TSats( s, Tsat, icode )

        DTR = DTs( TTR, s, D, icode)
        hTR = hTD( TTR, DTR, h2, icode )
       dTdh= (T0 - TTR)/(hf0 - hTR)
       T1  = TTR + dTdh*(h - hTR)

       i = 0
       LOOP4:DO
          DT1 = DTs( T1, s, D, icode )
           hT1 = hTD( T1, DT1, h2, icode )
          dTdh= (T1 - TTR)/(hT1 - hTR)
          T2  = TTR + dTdh*(h - hTR)
         DIF = DABS( T2 - T1 )/T2
         if ( DIF .LE. EPS ) EXIT LOOP4
         i = i + 1
         T1 = T2
         if ( i .GT. 80 ) EXIT LOOP4
       end do LOOP4
       Ths = T2

     else
        icode = 0
! Se h entre 2500.0031 - 2499.795 e s = 4.0
!      if ( (h .ge. 2499.795d0) .and. (h .le. 2500.0033d0) .and. (s .eq. 4.0d0) ) then
!         s = s + 2.0d-06
!      end if
!      if ( s .eq. 3.0d+0 ) then 
!         s = s + 0.00001d+0
!      end if

        CALL HSITER(h,s,TBER,D,1.D-9)
        IF (TBER .GT. TTR) THEN
          Ths = TBER
! Iteragir DTs com TDh at? convergir...
        i = 0
        T1 = Ths
!        D1 = DTh( T1, h, D, Dupp, icode)
!        if ( Dupp .gt. D1 ) then
!           D1 = 0.5d+0*(Dupp + D)
!        end if

        D1  = DTs( T1, s, D, icode )
        h_TD = hTD(T1, D1, h2, icode)
        if ( (DABS(h - h_TD)/h) .GT. 1.0d-6 ) then

          LOOP1:DO
             D1  = DTs( T1, s, D, icode )
            T2 = TDh( D1, h, T, icode )
            DIF = DABS( T1 - T2 )/T1
!            Tmed = meio*( T1 + T2 )
!            T1 = Tmed
            if ( DIF .LE. EPS ) EXIT LOOP1
            T1 = T2
            i = i + 1
            if ( i .GT. imax ) EXIT LOOP1
          end do LOOP1
          Ths = T1
        end if
        ELSE
        D1 = Dhs( h, s, D, icode)              ! T < TTR
        if (D1 .GT. 0.d+0) then
           Ths = TDh( D1, h, T, icode)
          T1 = Ths
          i = 0
          LOOP2:DO
             D1  = DTs( T1, s, D, icode )
            T2 = TDh( D1, h, T, icode )
            DIF = DABS( T1 - T2 )/T1
            if ( DIF .LE. EPS ) EXIT LOOP2
            T1 = T2
            i = i + 1
            if ( i .GT. imax ) EXIT LOOP2
          end do LOOP2
         Ths = T2

        else
           Difh = DABS(2084.3d+0 - h)/2084.3d+0
           Difs = DABS(4.407d+0 - s)/4.407d+0
          if ( (Difs .le. 0.04d+0) .and. (Difh .le. 0.07d+0) ) then
             T0 = TC
             i = 0
             LOOP3:DO
                D0  = DTs( T0, s, D, icode )
               T1 = TDh( D0, h, T, icode )
               DIF = DABS( T1 - T0 )/T1
               if ( DIF .LE. EPS ) EXIT LOOP3
               T0 = T1
               i = i + 1
               if ( i .GT. imax ) EXIT LOOP3
             end do LOOP3
            Ths = T0

          else 
               Ths = -1011.D0
              icode = -1011
          end if
        end if
        ENDIF
     end if
     T = Ths
      RETURN
      END


! -----------------------------------------------------------------------------
      REAL(KIND=8) function Dhs( h, s, D, icode )
! -----------------------------------------------------------------------------
!
!  DENSITY   AS FUNCTION OF h AND s
!
!  INPUT:     h         SPECIFIC ENTHALPY [KJ / KG]
!             s         SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    Dhs       DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     call hsLimites( h, s, hMax, hMin, hx0, hx1, icode )
     T0  = TSats( s, Tsat, icode )
     hf0 = hfT( T0, hf, icode )

     if ( h .GT. 0.999d+0*hMax ) then        ! ********* 
!          Dhs  = hMax
          Dhs  = -9999.
        icode = -9999
      else if ( h .LT. 1.01d+0*hMin ) then        ! ********* 
!          Dhs  = hMin
          Dhs  = -1111.
        icode = -1111
     else if ( h .lt. hf0 ) then
        DTR = DTs( TTR, s, D, icode)
        hTR = hTD( TTR, DTR, h2, icode )
       dTdh= (T0 - TTR)/(hf0 - hTR)
       T1  = TTR + dTdh*(h - hTR)
       i = 0
       LOOP4:DO
          DT1 = DTs( T1, s, D, icode )
           hT1 = hTD( T1, DT1, h2, icode )
          dTdh= (T1 - TTR)/(hT1 - hTR)
          T2  = TTR + dTdh*(h - hTR)
         DIF = DABS( T2 - T1 )/T2
         if ( DIF .LE. EPS ) EXIT LOOP4
         i = i + 1
         T1 = T2
         if ( i .GT. 80 ) EXIT LOOP4
       end do LOOP4
       Dhs = DTs( T2, s, D, icode )

     else
        icode = 0
        CALL HSITER(h,s,T,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          Dhs = DBER
        ELSE
          Dhs = -1013.D0
         icode = -1013
        ENDIF
     end if
     D = Dhs
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function pSatT( T, pSat, icode)
! -----------------------------------------------------------------------------
!
!  VAPOR PRESSURE AS FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    pSatT     VAPOR PRESSURE [MPA]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         pSatT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         pSatT = p
      ENDIF
     pSat = pSatT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DfT( T, Df, icode)
! -----------------------------------------------------------------------------
!
!  SATURATED LIQUID DENSITY AS FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    DfT       SATURATED LIQUID DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         DfT   = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         DfT = DL
      ENDIF
     Df = DfT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DgT( T, Dg, icode )
! -----------------------------------------------------------------------------
!
!  SATURATED VAPOR DENSITY AS FUNCTION OF T
!
!  OUTPUT:    DfT      SATURATED VAPOR DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         DgT   = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         DgT = DV
      ENDIF
     Dg = DgT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function hfT( T, hf, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTHALPY OF SATURATED LIQUID AS FUNCTION OF T
!  FOR GIVEN VALUES OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!
!  OUTPUT:    hfT       SPECIFIC ENTHALPY ON THE SATURATED 
!                       LIQUID LINE [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         hfT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DL .GT. 0.D0) THEN
             hfT = calch(T,DL)
         ELSE
             hfT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     hf = hfT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function hgT( T, hg, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTHALPY OF THE SATURATED VAPOR AS FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    hgT       SPECIFIC ENTHALPY ON THE SATURATED VAPOR  [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         hgT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DV .GT. 0.D0) THEN
             hgT = calch(T,DV)
         ELSE
             hgT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     hg = hgT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function sfT( T, sf, icode)
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTROPY OF SATURATED LIQUID AS FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    sfT       SPECIFIC ENTROPY OF SATURATED LIQUID [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         sfT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DL .GT. 0.D0) THEN
             sfT = calcs(T,DL)
         ELSE
             sfT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     sf = sfT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function sgT( T, sg, icode)
! -----------------------------------------------------------------------------
!
!  SPECIFIC ENTROPY OF SATURATED VAPOR FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    sgT       SPECIFIC ENTROPY ON THE SATURATED 
!                       VAPOR LINE [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         sgT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DV .GT. 0.D0) THEN
             sgT = calcs(T,DV)
         ELSE
             sgT = -1103.D0
          icode = -1103
         ENDIF
      ENDIF
     sg = sgT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function ufT( T, uf, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC INTERNAL ENERGY OF SATURATED LIQUID FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    ufT       SPECIFIC INTERNAL ENERGY ON THE SATURATED 
!                       LIQUID LINE [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         ufT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DL .GT. 0.D0) THEN
             ufT = calcu(T,DL)
         ELSE
             ufT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     uf = ufT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function ugT( T, ug, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC INTERNAL ENERGY OF SATURATED VAPOR FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    ugT       SPECIFIC INTERNAL ENERGY OF SATURATED VAPOR [KJ / KG]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         ugT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DV .GT. 0.D0) THEN
             ugT = calcu(T,DV)
         ELSE
             ugT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     ug = ugT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CvfT( T, Cvf, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOCHORIC HEAT CAPACITY OF SATURATED LIQUID FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    CvfT      SPECIFIC ISOCHORIC HEAT CAPACITY ON 
!                       THE  SATURATED LIQUID LINE [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         CvfT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DL .GT. 0.D0) THEN
             CvfT = calcCv(T,DL)
         ELSE
             CvfT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     Cvf = CvfT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CvgT( T, Cvg, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOCHORIC HEAT CAPACITY OF SATURATED VAPOR FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    CvgT      SPECIFIC ISOCHORIC HEAT CAPACITY OF SATURATED VAPOR [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         CvgT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DV .GT. 0.D0) THEN
             CvgT = calcCv(T,DV)
         ELSE
             CvgT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     Cvg = CvgT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CpfT( T, Cpf, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOBARIC HEAT CAPACITY OF SATURATED LIQUID FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    CpfT      SPECIFIC ISOBARIC HEAT CAPACITY OF SATURATED LIQUID [KJ /(KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         CpfT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DL .GT. 0.D0) THEN
             CpfT = calcCp(T,DL)
         ELSE
             CpfT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     Cpf = CpfT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CpgT( T, Cpg, icode )
! -----------------------------------------------------------------------------
!
!  SPECIFIC ISOBARIC HEAT CAPACITY OF SATURATED VAPOR FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    cPGt      SPECIFIC ISOBARIC HEAT CAPACITY OF SATURATED VAPOR [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         CpgT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DV .GT. 0.D0) THEN
             CpgT = calcCp(T,DV)
         ELSE
             CpgT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     Cpg =   CpgT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function wfT( T, wf, icode)
! -----------------------------------------------------------------------------
!
!  SPEED OF SOUND OF SATURATED LIQUID FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    wfT       SPEED OF SOUND ON THE SATURATED LIQUID [M / S]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         wfT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,P,1.D-9)
         IF (DL .GT. 0.D0) THEN
             wfT = calcw(T,DL)
         ELSE
             wfT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     wf = wfT
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function wgT( T, wg, icode )
! -----------------------------------------------------------------------------
!
!  SPEED OF SOUND OF SATURATED VAPOR FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    wgT       SPEED OF SOUND OF SATURATED VAPOR [M / S]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((T .LT. TTR) .OR. (T .GT. TC)) THEN
         wgT = -1101.D0
        icode = -1101
      ELSE
         CALL TSATIT(T,DV,DL,p,1.D-9)
         IF (DV .GT. 0.D0) THEN
              wgT = calcw(T,DV)
         ELSE
             wgT = -1103.D0
            icode = -1103
         ENDIF
      ENDIF
     wg = wgT

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Tsatp( p, Tsat, icode )
! -----------------------------------------------------------------------------
!
!  SATURATION TEMPERATURE FUNCTION OF p
!
!  INPUT:     P         PRESSURE [MPA]
!
!  OUTPUT:    Tsatp     SATURATION TEMPERATURE [K]
!                         
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((p .LT. PTR) .OR. (p .GT. PC)) THEN
         Tsatp = -1102.D0
        icode = -1102
      ELSE
         CALL PSATIT(T,DV,DL,p,1.D-9)
         Tsatp = T
      ENDIF
     Tsat = Tsatp
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Dfp( p, Df, icode )
! -----------------------------------------------------------------------------
!
!  SATURATED LIQUID DENSITY FUNCTION OF p
!
!  INPUT:     p         PRESSURE [MPa]
!
!  OUTPUT:    Dfp       SATURATED LIQUID DENSITY [KG / M ** 3]
!                         
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((p .LT. PTR) .OR. (p .GT. PC)) THEN
         Dfp = -1102.D0
        icode = -1102
      ELSE
         CALL PSATIT(T,DV,DL,p,1.D-9)
         Dfp = DL
      ENDIF
     Df = Dfp
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Dgp( p, Dg, icode )
! -----------------------------------------------------------------------------
!
!  SATURATED VAPOR DENSITY FUNCTION OF p
!
!  INPUT:     P         PRESSURE [MPA]
!
!  OUTPUT:    Dgp       SATURATED VAPOR DENSITY [KG / M ** 3]
!                         
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ((p .LT. PTR) .OR. (p .GT. PC)) THEN
         Dgp = -1102.D0
        icode = -1102
      ELSE
         CALL PSATIT(T,DV,DL,p,1.D-9)
         Dgp = DV
      ENDIF
     Dg = Dgp
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function TsatD( D, Tsat1, Tsat2, icode )
! -----------------------------------------------------------------------------
!
!  SATURATION TEMPERATURE FUNCTION OF D
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    TsatD     SATURATION TEMPERATURE [K]
!                         
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / TSAT / at(3), bt(3), ct(3)

      icode = 0
      IF ( D .LT. DVTR ) THEN   ! DLTR /0.999792520186D+03/, DVTR /0.485457572553D-02/   
         TsatD = -1113.D0
       Tsat1 = TsatD
       Tsat2 = TsatD
        icode = -1113
     else if ( D .eq. DLTR ) then
        TsatD = TTR
       Tsat1 = TsatD
       Tsat2 = TsatD
       icode = 0
     else if ( D .gt. DLTR ) then
       delta1 = bt(1)*bt(1) - 4.d0*at(1)*( ct(1)-D )
      if ( delta1 .lt. 0.d0 ) then
        delta1 = 0.d0
      end if
       delta2 = bt(2)*bt(2) - 4.d0*at(2)*( ct(2)-D )
      if ( delta2 .lt. 0.d0 ) then
        delta2 = 0.d0
      end if
       Tsat1 = 0.5d0*( -bt(1) + DSQRT(delta1) ) / at(1)
       Tsat2 = 0.5d0*( -bt(2) - DSQRT(delta2) ) / at(2)
      TsatD = Tsat2
      icode = 0
      ELSEIF (D .LE. DC) THEN            ! DC = 322.
         CALL DVSATIT(T,D,DL,p,1.D-9)
       TsatD = T
       Tsat1 = T
       Tsat2 = T
      ELSE
         CALL DLSATIT(T,DV,D,p,1.D-9)
       TsatD = T
       Tsat1 = T
       Tsat2 = T
      ENDIF
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function pSatD( D, pSat1, pSat2, icode )
! -----------------------------------------------------------------------------
!
!  VAPOR PRESSURE FUNCTION OF D
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    pSatD     VAPOR PRESSURE [MPA]
!                        
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      IF ( D .LT. DVTR ) THEN   ! DLTR /0.999792520186D+03/, DVTR /0.485457572553D-02/   
         pSatD = -1113.D0
       pSat1 = pSatD
       pSat2 = pSatD
        icode = -1113
     else if ( D .eq. DLTR ) then
       pSatD = PTR
      pSat1 = pSatD
      pSat2 = pSatD
      icode = 0
     else if ( D .gt. DLTR ) then
      T = TsatD( D, Tsat1, Tsat2, icode )
      pSat1 = pSatT(Tsat1, p1, icode )              !   pTD(T, D, p, icode)
      pSat2 = pSatT(Tsat2, p2, icode )
      pSatD = pSat1
      icode = 0
      ELSEIF (D .LE. DC) THEN
         CALL DVSATIT(T,D,DL,pSatD,1.D-9)
       pSat1 = pSatD
       pSat2 = pSatD
      ELSE
         CALL DLSATIT(T,DV,D,pSatD,1.D-9)
       pSat1 = pSatD
       pSat2 = pSatD
      ENDIF
      RETURN
      END

! -----------------------------------------------------------------------------
      DOUBLEPRECISION FUNCTION Dfs( s, Df, icode)
! -----------------------------------------------------------------------------
!
!  SATURATED LIQUID DENSITY FUNCTION Of s
!
!  INPUT:     s       SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    Dfs     SAT LIQUID DENSITY  [KG / M ** 3]
!                        
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

     CALL TSATIT(TTR,DV,DL,p,1.D-06)
     SLTR = calcs(TTR,DL)
!      SC = calcs(TC,DC)         ! ******************************************
     icode = 0
      IF ((s .LT. SC) .AND. (s .GT. SLTR)) THEN
          CALL SLSATIT(s,T,DV,Dfs,p,1.D-06)
     ELSE
         Dfs   = -1105.D+0
        icode = -1105
     ENDIF
     Df = Dfs

      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function Dgs( s, Dg, icode )
! -----------------------------------------------------------------------------
!
!  SATURATED VAPOUR DENSITY FUNCTION Of s
!
!  INPUT:     s       SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    Dgs     SAT VAPOUR DENSITY  [KG / M ** 3]
!                        
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      EPS = 1.D-13

      CALL TSATIT(TTR,DV,DL,p,EPS)

      STTR = calcs(TTR,DV)
!      SC = calcs(TC,DC)           ! *********************************
     icode = 0
      IF ((s .GT. STTR) .OR. (s .LT. SC)) THEN
       Dgs   = -1105.D+00
      icode = -1105
     else
      CALL SVSATITTR(s,T1,DV1,DL1,P1,EPS)
     CALL SVSATITCRIT(s,T2,DV2,DL2,P2,EPS)
      DVDIFF = DABS((DV2 - DV1)/DV2)

      IF (DVDIFF .LT. 1.D-6) THEN
         Dgs = DV2
     ELSE
         Dgs = -1115.D+0
          icode = -1115
     ENDIF
   end if

!1000 continue
     Dg = Dgs
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function pSats( s, pSat, icode )
! -----------------------------------------------------------------------------
!
!  SATURATED VAPOUR PRESSURE FUNCTION Of s
!
!  INPUT:     s        SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    pSats    SATURATED VAPOUR PRESSURE  [Mpa]
!                        
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      EPS = 1.D-06

      CALL TSATIT(TTR,DV,DL,PSAT,EPS)
     SLTR = calcs(TTR,DL)
     SVTR = calcs(TTR,DV)
!      SC = calcs(TC,DC)                !   4,406961904         ************************

      IF ((s .LT. SC) .AND. (s .GT. SLTR))THEN        ! SLTR -5.24205917e-14
          CALL SLSATIT(S,T1,DV1,DL1,P1,EPS)
       pSats = P1
     ELSEIF ((S .GE. SC) .AND. (S .LT. SVTR)) THEN
       CALL SVSATITTR(S,T2,DV2,DL2,P2,EPS)
       CALL SVSATITCRIT(S,T3,DV3,DL3,P3,EPS)
       PDIFF = DABS(P2 - P3)
       IF (PDIFF .LT. 1.D-7) THEN
           pSats = P2
       ELSE
           pSats = -1112.D+0
            icode = -1112
       ENDIF
     ELSE
       pSats = -1555.D+0
      icode = -1555
     ENDIF
     pSat = pSats

      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function TSats( s, Tsat, icode )
! -----------------------------------------------------------------------------
!
!  SATURATED VAPOUR TEMPERATURE FUNCTION Of s
!
!  INPUT:     s        SPECIFIC ENTROPY [KJ / (KG * K)]
!
!  OUTPUT:    TSats    SATURATED VAPOUR TEMPERATURE  [k]
!                        
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0
      EPS = 1.D-06

      CALL TSATIT(TTR,DV,DL,PSAT,EPS)
     SLTR = calcs(TTR,DL)
     SVTR = calcs(TTR,DV)
!      SC = calcs(TC,DC)         ! ***********************************

      IF ((s .LT. SC) .AND. (s .GT. SLTR))THEN
          CALL SLSATIT(s,T1,DV1,DL1,P1,EPS)
         TSats = T1
     ELSEIF ((s .GE. SC) .AND. (s .LT. SVTR)) THEN
       CALL SVSATITTR(s,T2,DV2,DL2,P2,EPS)
       CALL SVSATITCRIT(s,T3,DV3,DL3,P3,EPS)
       TDIFF = DABS(T2 - T3)
       IF (TDIFF .LT. 1.D-7) THEN
           TSats = T2
       ELSE
           TSats = -1015.D+0
            icode = -1015
       ENDIF
     ELSE
       TSats = -1105.D+0
      icode = -1105
     ENDIF
     TSat = TSats

      RETURN
     END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function FugaTp( T, p, Fuga, icode )
! -----------------------------------------------------------------------------
!
!  FFUGACITY AS FUNCTION OF T AND p
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPA]
!
!  OUTPUT:    FugaTp    FUGACITY [MPA]
!                        
!   (DO NOT TEST FOR ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          FugaTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          FugaTp = -1002.D0
         icode = -1002
     else
        icode = 0

        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          FugaTp = calcFuga(T,DBER)
        ELSE
          FugaTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     Fuga = FugaTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function GibbsTp( T, p, Gibbs, icode)
! -----------------------------------------------------------------------------
!
!  SPECIFIC GIBBS ENERGY FUNCTION OF T AND p 
!
!  INPUT:     T           TEMPERATURE [K]
!             p           PRESSURE [MPA]
!
!  OUTPUT:    GibbsTp     SPECIFIC GIBBS ENERGY [KJ / KG]
!
!   (DO NOT TEST FOR ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          GibbsTp = -1001.D0
         icode = -1001
     else if ( p .LE. 0.D0 ) then
          GibbsTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
          GibbsTp = calcG(T,DBER)
        ELSE
          GibbsTp = -1013.D0
         icode = -1013
        ENDIF
     end if
     Gibbs = GibbsTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function JTcTD( T, D, JTc, icode )
! -----------------------------------------------------------------------------
!
!  JOULE-THOMSON COEFFICIENT FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    JTcTD     JOULE-THOMSON COEFFICIENT [K / MPA]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode
     REAL(KIND=8) ::  JTc

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          JTcTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          JTcTD = -1003.D0
         icode = -1003
     else
        icode = 0

        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
              JTcTD = calcJTc(T,D)
        ELSE
          JTcTD = -1004.D0
         icode = -1004
        ENDIF
     end if
     JTc = JTcTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function ThrcTD( T, D, Thrc, icode )
! -----------------------------------------------------------------------------
!
!  THROTTLING COEFFICIENT FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    ThrcTD    ISOTHERMAL THROTTELING COEFFICIENT [-]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          ThrcTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          ThrcTD = -1003.D0
         icode = -1003
     else
        icode = 0

        XTP = 2.D0
        IF (T .LE. TC) THEN
          CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
              ThrcTD = calcTHC(T,D)
        ELSE
          ThrcTD = -1004.D0
         icode = -1004
        ENDIF
     end if
     Thrc = ThrcTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function BT( T, B, icode)
! -----------------------------------------------------------------------------
!
!  SECOND VIRIAL COEFFICIENT FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    B         SECOND VIRIAL COEFFICIENT [M**3 / KG]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0

      IF (T .LT. TTR) THEN
        BT = -1001.D0
       icode = -1001
      else
        BT = calcB(T)
     end if

     B = BT

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function CT( T, C, icode )
! -----------------------------------------------------------------------------
!
!  THIRD VIRIAL COEFFICIENT
!
!  INPUT:     T       TEMPERATURE [K]
!
!  OUTPUT:    C       THIRD VIRIAL COEFFICIENT [(M ** 3 / KG) ** 2]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      icode = 0

      IF (T .LT. TTR) THEN
          CT = -1001.D0
         icode = -1001
      else
        CT = calcC(T)
     end if

     C = CT

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function BetasTD( T, D, Beta, icode )
! -----------------------------------------------------------------------------
!
!  ISENTROPIC TEMPERATURE-PRESSURE COEFFICIENT FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcBeta    ISENTROPIC TEMPERATURE-PRESSURE COEFFICIENT COEFFICIENT [K / MPa]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LE. TTR ) then
          BetasTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          BetasTD = -1003.D0
         icode = -1003
     else
        icode = 0
        BetasTD = calcBeta(T, D)
     end if
     Beta = BetasTD

      RETURN
      END

 ! -----------------------------------------------------------------------------
      REAL(KIND=8) function kapaTD( T, D, kapa, icode )
! -----------------------------------------------------------------------------
!
!  ISOTHERMAL COMPRESSIBILITY FACTOR FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    KAPA    ISOTHERMAL COMPRESSIBILITY FACTOR [1 / MPa]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode
     REAL(KIND=8) ::  kapa

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LE. TTR ) then
          kapaTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          kapaTD = -1003.D0
         icode = -1003
     else
        icode = 0
       kapaTD  = 1.d0/(D*dpdDTD(T, D, dpdD, icode))    ! * 1.d+03 ?????????????????????
     end if
     kapa = kapaTD

      RETURN
      END

 ! -----------------------------------------------------------------------------
      REAL(KIND=8) function ZTD( T, D, Z, icode )
! -----------------------------------------------------------------------------
!
!  COMPRESSIBILITY FACTOR FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    Z         COMPRESSIBILITY FACTOR [  ]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode
     REAL(KIND=8) ::  Z

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LE. TTR ) then
          ZTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          ZTD = -1003.D0
         icode = -1003
     else
        icode = 0
       ZTD  = 1.d3*pTD(T,D,p,icode)/(D*R*T)    
     end if
     Z = ZTD

      RETURN
      END
!FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
 
!DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
!D                        PROPERTIES DERIVATIVES                    D
!DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD

! -----------------------------------------------------------------------------
      REAL(KIND=8) function dpdTTD( T, D, dpdT, icode )
! -----------------------------------------------------------------------------
!
!  FIRST DERIVATIVE OF THE PRESSURE WITH RESPECT TO THE TEMPERATURE
!  FUNCTION OF T AND D
!
!  INPUT:     T      TEMPERATURE [K]
!             D      DENSITY [KG / M ** 3]
!
!  OUTPUT:    dpdT   DERIVATIVE OF PRESSURE WITH RESPECT TO TEMPERATURE [MPA / K]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          dpdTTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          dpdTTD = -1003.D0
         icode = -1003
     else
        icode = 0
         XTP = 2.D0
         IF (T .LE. TC) THEN
             CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
         ENDIF
         IF (XTP .GT. 1.5D0) THEN
             dpdTTD = calcdpdT(T,D)
         ELSE
             dpdTTD = -1004.D0
            icode = -1004
         ENDIF
      end if
     dpdT = dpdTTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function dpdDTD( T, D, dpdD, icode )
! -----------------------------------------------------------------------------
!
!  FIRST DERIVATIVE OF PRESSURE WITH RESPECT TO DENSITY
!  AS FUNCTION OF T AND D
!
!  INPUT:     T      TEMPERATURE [K]
!             D      DENSITY [KG / M ** 3]
!
!  OUTPUT:    dpdD   DERIVATIVE OF THE PRESSURE WITH RESPECT 
!                     TO THE DENSITY [(MPa * M**3) / Kg ]
!                        
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          dpdDTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          dpdDTD = -1003.D0
         icode = -1003
     else
        icode = 0
        XTP = 2.D0
        IF (T .LE. TC) THEN
            CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
            dpdDTD = calcdpdD(T,D)
        ELSE
            dpdDTD = -1004.D0
           icode = -1004
        ENDIF
      end if
     dpdD = dpdDTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function dDdTTD( T, D, dDdT, icode )
! -----------------------------------------------------------------------------
!
!  FIRST DERIVATIVE OF DENSITY WITH RESPECT TO TEMPERATURE
!  FUNCTION OF T AND D
!
!  INPUT:     T      TEMPERATURE [K]
!             D      DENSITY[Kg / M**3]
!
!  OUTPUT:    dDdT   DERIVATIVE OF DENSITY WITH RESPECT TO TEMPERATURE [Kg / (K * M**3)]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          dDdTTD = -1001.D0
         icode = -1001
     else if ( D .LE. 0.D0 ) then
          dDdTTD = -1003.D0
         icode = -1003
     else
        icode = 0
        XTP = 2.D0
        IF (T .LE. TC) THEN
            CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
        ENDIF
        IF (XTP .GT. 1.5D0) THEN
            dDdTTD = - calcdpdT(T,D) / calcdpdD(T,D)
        ELSE
            dDdTTD = -1004.D0
           icode = -1004
        ENDIF
      end if
     dDdT = dDdTTD

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function dpdDTp( T, p, dpdD, icode )
! -----------------------------------------------------------------------------
!
! FIRST DERIVATIVE OF PRESSURE WITH RESPECT TO DENSITY 
!  FUNCTION OF T AND p
!
!  INPUT:     T      TEMPERATURE [K]
!             P      PRESSURE [MPA]
!
!  OUTPUT:    dpdD   DERIVATIVE OF PRESSURE WITH
!                       RESPECT TO DENSITY [(MPA * M ** 3) / KG]
!
!   (DO NOT TEST FOR ICE REGIONS)
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          dpdDTp = -1001.D0
         icode = -1001
     else if (p .LE. 0.0D+0) then
          dpdDTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
            dpdDTp = calcdpdD(T,DBER)
        ELSE
            dpdDTp = -1004.D0
           icode = -1004
        ENDIF
     end if
     dpdD = dpdDTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function dDdTTp( T, p, dDdT, icode )
! -----------------------------------------------------------------------------
!
!  FIRST DERIVATIVE OF DENSITY WITH RESPECT TO TEMPERATURE 
!   FUNCTION OF T AND p
!
!  INPUT:     T      TEMPERATURE [K]
!             P      PRESSURE [MPA]
!
!  OUTPUT:    dDdT   DERIVATIVE OF DENSITY WITH RESPECT TO
!                      TEMPERATURE  [KG / (K * M**3)]
!
!   (DO NOT TEST FOR ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          dDdTTp = -1001.D0
         icode = -1001
     else if (p .LE. 0.0D+0) then
          dDdTTp = -1002.D0
         icode = -1002
     else
        icode = 0
        CALL TPITER(T,p,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
            dDdTTp = - calcdpdT(T,DBER) / calcdpdD(T,DBER)
        ELSE
            dDdTTp = -1004.D0
           icode = -1004
        ENDIF
     end if
     dDdT = dDdTTp

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function dpdTTp( T, p, dpdT, icode )
! -----------------------------------------------------------------------------
!
!  FFIRST DERIVATIVE OF PRESSURE WITH RESPECT TO TEMPERATURE 
!   FUNCTION OF T AND P
!
!  INPUT:     T      TEMPERATURE [K]
!             P      PRESSURE [MPA]
!
!  OUTPUT:    dpdT   DERIVATIVE OF PRESSURE WITH
!                     RESPECT TO TEMPERATURE  [MPA / K]
!
!   (DO NOT TEST FOR ICE REGIONS)
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      if ( T .LT. TTR ) then
          dpdTTp = -1001.D0
         icode = -1001
     else if (p .LE. 0.0D+0) then
          dpdTTp = -1002.D0
         icode = -1002
!       ELSEIF ((P .GE. PMELTING) .AND. (PMELTING .GT. 0.0D+0)) THEN
!           icode = -10006
     else
        icode = 0
        CALL TPITER(T,P,DBER,1.D-9)
        IF (DBER .GT. 0.D0) THEN
            dpdTTp = calcdpdT(T,DBER)
        ELSE
            dpdTTp = -1004.D0
           icode = -1004
        ENDIF
     end if
     dpdT = dpdTTp

      RETURN
      END
!DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD

!AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
!A                AUXILIARY CALCULATION FUNCTIONS                       A
!AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcB(T)
! -----------------------------------------------------------------------------
!
!  SECOND VIRIAL COEFFICIENT FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    calcB      SECOND VIRIAL COEFFICIENT [M ** 3 / KG]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,PhirD,T,D
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      D = 1.0D-10
      IF (T .LE. 0.D0) THEN
         calcB = -111.D0
      else
        calcB = PhirD(T,D) / DC
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcC(T)
! -----------------------------------------------------------------------------
!
!  THIRD VIRIAL COEFFICIENT FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    calcC      THIRD VIRIAL COEFFICIENT [(M ** 3 / KG) ** 2]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,PhirDD,T,D
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      D = 1.0D-10
      IF (T .LE. 0.D0) THEN
         calcC = -111.D0
      else
         calcC = PhirDD(T,D) / DC / DC
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcJTc(T,D)
! -----------------------------------------------------------------------------
!
!  JOULE-THOMSON COEFFICIENT FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcJTc    JOULE-THOMSON COEFFICIENT [K / MPa]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,TAU,DELTA,PhirD,PhirDD,PhioTT,PhirTT,PhirDT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

!      if ( T .LE. 0.D0 ) then
!          calcJTc = -1001.D0
!         icode = -1001
!     else if ( D .LE. 0.D0 ) then
!         calcJTc = -1003.D0
!         icode = -1003
!     else
!        icode = 0
        TAU = TC / T
        DELTA = D / DC
        calcJTc = ((- DELTA * PhirD(T,D) - DELTA * DELTA * PhirDD(T,D) -    &
                  DELTA * TAU * PhirDT(T,D)) / ((1.D0 + DELTA * PhirD(T,D)     &
                  - DELTA * TAU * PhirDT(T,D)) * (1.D0 + DELTA * PhirD(T,D)  &
                  - DELTA * TAU * PhirDT(T,D)) - TAU * TAU * (1.D0           &
                  + 2.D0 * DELTA * PhirD(T,D) + DELTA * DELTA * PhirDD(T,D)) &
                  * (PhioTT(T,D) + PhirTT(T,D)))) / (R * D) * 1.D3
!     end if

      RETURN
      END
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcBeta(T,D)
! -----------------------------------------------------------------------------
!
!  ISENTROPIC TEMPERATURE-PRESSURE COEFFICIENT FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcBeta    ISENTROPIC TEMPERATURE-PRESSURE COEFFICIENT COEFFICIENT [K / MPa]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,TAU,DELTA,PhirD,PhirDD,PhioTT,PhirTT,PhirDT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF, num, den1, den2
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      calcBeta = 0.D0

      TAU   = TC / T
      DELTA =  D / DC

     num     = 1.d0 + DELTA*( PhirD(T,D) - TAU * PhirDT(T,D) )
     den1    = num * num
     den2    = - TAU*TAU * ( PhioTT(T,D) + PhirTT(T,D) ) *                     &
               ( 1.D0 + 2.D0 * DELTA * PhirD(T,D) + DELTA * DELTA * PhirDD(T,D) )
     calcBeta= num/( D * R * 1.0d-3 * (den1 + den2) )

      RETURN
      END


!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcG(T,D)
! -----------------------------------------------------------------------------
!
!  SPECIFIC GIBBS ENERGY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcG      SPECIFIC GIBBS ENERGY [KJ / KG]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      DELTA = D / DC
      calcG = R * T * (1.D0 + DELTA * PhirD(T,D) + Phio(T,D) + Phir(T,D))

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcp(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE PRESSURE FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcp      PRESSURE [MPa]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,DELTA,PhirD
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF                     !rev

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcp = -111.D0
     else
        DELTA = D / DC
        calcp = D * R * T * (1.D0 + DELTA * PhirD(T,D)) * 1.D-3
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcs(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE SPECIFIC ENTROPY FUNCTION OF T AND D 
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcs      SPECIFIC ENTROPY [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,TAU,Phio,Phir,PhioT,PhirT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcs = -111.D0
      else
        TAU = TC / T
         calcs = R * (TAU * (PhioT(T,D) + PhirT(T,D)) - (Phio(T,D) + Phir(T,D)))
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calch(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE SPECIFIC ENTHALPY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calch      SPECIFIC ENTHALPY [KJ / KG]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE

      REAL(KIND=8) ::  T,D,TAU,DELTA,PhirD,PhioT,PhirT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calch = -111.D0
      else
        TAU = TC / T
        DELTA= D / DC
        calch = R*T*(1.D0 + DELTA * PhirD(T,D) + TAU*(PhioT(T,D) + PhirT(T,D)))
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcf(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE SPECIFIC HELMHOLTZ ENERGY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcf      SPECIFIC HELMHOLTZ ENERGY [KJ / KG]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,Phir,Phio
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcf = -111.D0
      else
        calcf = R * T * (Phir(T,D) + Phio(T,D))
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcu(T,D)
! -----------------------------------------------------------------------------
!
!
!  SPECIFIC INTERNAL ENERGY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcu      SPECIFIC INTERNAL ENERGY [KJ / KG]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,TAU,PhioT,PhirT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcu = -111.D0
      else
        TAU = TC / T
        calcu = R * T * TAU * (PhioT(T,D) + PhirT(T,D))
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcCv(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE SPECIFIC ISOCHORIC HEAT CAPACITY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcCv     SPECIFIC ISOCHORIC HEAT CAPACITY [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,TAU,PhioTT,PhirTT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcCv = -111.D0
      else
        TAU = TC / T
        calcCv = - R * TAU * TAU * (PhioTT(T,D) + PhirTT(T,D))
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcCp(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE SPECIFIC ISOBARIC HEAT CAPACITY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcCp     SPECIFIC ISOBARIC HEAT CAPACITY [KJ / (KG * K)]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,TAU,DELTA,PhirD,PhirDD,PhirDT,PhioTT,PhirTT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcCp = -111.D0
      else
        TAU = TC / T
        DELTA = D / DC
        calcCp = R * (- TAU * TAU * (PhioTT(T,D) + PhirTT(T,D))       &
                + (1.D0 + DELTA * PhirD(T,D) - DELTA * TAU *        &
                PhirDT(T,D)) * (1.D0 + DELTA * PhirD(T,D) - DELTA *    &
                TAU * PhirDT(T,D)) / (1.D0 + 2.D0 * DELTA *        &
                PhirD(T,D) + DELTA * DELTA * PhirDD(T,D)))
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcw(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE SPEED OF SOUND FUNCTION OF T AND D 
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcw      SPEED OF SOUND [M / S]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
   
      REAL(KIND=8) ::  T,D,TAU,DELTA,PhirD,PhirDD,PhioTT,PhirTT,PhirDT,WB2
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcw = -111.D0
      else
        TAU = TC / T
        DELTA = D / DC
        WB2 = 1.D0 + 2.D0 * DELTA * PhirD(T,D) + DELTA * DELTA * PhirDD(T,D)  &
              - (1.D0 + DELTA * PhirD(T,D) - DELTA * TAU * PhirDT(T,D))      &
              * (1.D0 + DELTA * PhirD(T,D) - DELTA * TAU * PhirDT(T,D))      &
              / (TAU * TAU * (PhioTT(T,D) + PhirTT(T,D)))
        IF(WB2 .GT. 0.D0) THEN
           calcw = DSQRT(WB2 * R * T * 1.D3)
        ELSE       
           calcw = -111.D0
        ENDIF
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcFuga(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE FUGACITY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcFuga   FUGACITY [MPa]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,DELTA,Phir,PhirD,ZB,PB,F
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcFuga = -111.D0
      else
        DELTA = D / DC
        ZB = (1.D0 + DELTA * PhirD(T,D))
        IF (ZB.LT.0.D0) THEN
           calcFuga = -111.D0
        else
        PB = ZB * D * R * T * 1.D-3

!  FUGACITY-COEFFICIENT 
          F = ZB - 1.D0 + Phir(T,D) - DLOG(ZB)

!  FUGACITY 
          calcFuga = PB * DEXP(F)
       end if
     end if

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcTHC(T,D)
! -----------------------------------------------------------------------------
!
!  ISOTHERMAL THROTTLING COEFFICIENT FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcTHC    ISOTHERMAL THROTTLING COEFFICIENT [-]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,TAU,DELTA,PhirD,PhirDD,PhirDT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcTHC = -111.D0
      else
        calcTHC = 0.D0
        TAU = TC / T
        DELTA = D / DC
        calcTHC = ( 1.D0 - (1.D0 + DELTA * PhirD(T,D)- DELTA *       &
                  TAU * PhirDT(T,D)) / (1.D0 + 2.D0 * DELTA *       &
                  PhirD(T,D) + DELTA * DELTA * PhirDD(T,D)) ) / D
     end if

      RETURN
      END


! -----------------------------------------------------------------------------
      REAL(KIND=8) function DLEQN(T)
! -----------------------------------------------------------------------------
!
! FUNCTION FOR CALCULATING saturated liquid density   function of T
!
!  INPUT:     T         TEMPERATUR [K]
!
!  OUTPUT:    DLEQN     DENSITY [KG / M ** 3] 
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TNORAUX(3),YNORAUX(3),G(20,3),TPOTAUX(20,3),B(20)
      REAL(KIND=8) ::  T,TAU,SR
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      INTEGER ISL(3),ISN(3),ISR(3),IMAXAUX(3),KEND,I,K
      COMMON / EQUAUX / TNORAUX,YNORAUX,G,TPOTAUX,ISL,ISN,ISR,IMAXAUX
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      DLEQN = 0.D0
      SR = 0.D0

! NORMALIZED TEMPERATURE CALCULATION        **********************************************

       TAU = 1.D0 - T / TC      ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      IF (TAU .LE. 0.D0) THEN
         IF ( T .EQ. TC ) THEN
            DLEQN = DC + 0.1D-13
            goto 1000
         ELSE
            DLEQN = -111.D0
            goto 1000
         ENDIF
      ENDIF

! RIGTH SIDE OF DENSITY EQUATION

      DO 100 I = 1,IMAXAUX(2)       ! i = 1, 6
         B(I) = TAU ** TPOTAUX(I,2)
100   CONTINUE

         B(IMAXAUX(2)+1) = 0.D0       ! b(7) = 0.0
         G(IMAXAUX(2)+1,2) = 1.D0    ! g(7) = 1.0

      KEND = IMAXAUX(2)+1      ! kend = 7
      DO 200 K = 1,KEND
      SR = SR + B(K) * G(K,2)   ! final: sr = sr + 0.0 * 1.0 ????
200   CONTINUE

!   CALCULATION OF saturated liquid density (DEPENDING ON THE STANDARDS
!   LEFT SIDE OF DENSITY EQUATION)

         DLEQN = (SR + 1.D0) * DC                  ! YNORAUX(2) = DC = 0.322000000D+03 

 1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DVEQN(T)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF VAPOUR DENSITY FROM A EQUATION BASED ON TAU
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    DVEQN     DENSITY [KG / M ** 3] 
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TNORAUX(3),YNORAUX(3),G(20,3),TPOTAUX(20,3),B(20)
      REAL(KIND=8) ::  T,TAU,SR
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      INTEGER ISL(3),ISN(3),ISR(3),IMAXAUX(3),KEND,I,K
      COMMON / EQUAUX / TNORAUX,YNORAUX,G,TPOTAUX,ISL,ISN,ISR,IMAXAUX
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      DVEQN = 0.D0
      SR = 0.D0

!  REFERENCE TEMPERATURE

         TAU = 1.D0 - T / TC

      IF (TAU .LE. 0.D0) THEN
         IF ((T .LE. TC) .AND. (T .GE. TC)) THEN
            DVEQN = DC - 0.1D-13
            goto 1000
         ELSE
            DVEQN = -111.D0
            goto 1000
         ENDIF
      ENDIF

! CALCULATION OF THE RIGHT SIDE OF TAU DENSITY EQUATION

      DO 100 I = 1,IMAXAUX(3)
      B(I) = TAU ** TPOTAUX(I,3)
100   CONTINUE

         B(IMAXAUX(3)+1) = 0.D0
         G(IMAXAUX(3)+1,3) = 1.D0

      KEND = IMAXAUX(3)+1
      DO 200 K = 1,KEND
      SR = SR + B(K) * G(K,3)
200   CONTINUE

! CALCULATION OF DENSITY TAU (DEPENDING ON THE REFERENCE
!  THE LEFT SIDE OF THE EQUATION TAU SEALING)

         DVEQN = DEXP(SR) * DC

1000 continue
      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function VPEQN(T)
! -----------------------------------------------------------------------------
!
! VAPOR PRESSURE CALC. FUNCTION OF T
!
!  INPUT:     T         TEMPERATURE [K]
!
!  OUTPUT:    VPEQN     VAPOR PRESSURE [MPa] 
!
! -----------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  TNORAUX(3),YNORAUX(3),G(20,3),TPOTAUX(20,3),B(20)
      REAL(KIND=8) ::  T,TAU,SR
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      INTEGER ISL(3),ISN(3),ISR(3),IMAXAUX(3),KEND,I,K
      COMMON / EQUAUX / TNORAUX,YNORAUX,G,TPOTAUX,ISL,ISN,ISR,IMAXAUX
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      VPEQN = 0.D0
      SR = 0.D0

!  TEMPERATURE REFERENCE

         TAU = 1.D0 - T / TC

      IF (TAU .LE. 0.D0) THEN
         IF ((T .LE. TC) .AND. (T .GE. TC)) THEN
            VPEQN = PC - 0.01D0
            goto 1000
         ELSE
            VPEQN = -111.D0
            goto 1000
         ENDIF
      ENDIF

! RIGTH SIDE OF VAPOR PRESSURE EQUATION

      DO 100 I = 1,IMAXAUX(1)
        B(I) = TAU ** TPOTAUX(I,1)
100   CONTINUE

         B(IMAXAUX(1)+1) = 0.D0
         G(IMAXAUX(1)+1,1) = 1.D0

      KEND = IMAXAUX(1)+1
      DO 200 K = 1,KEND
      SR = SR + B(K) * G(K,1)
200   CONTINUE

!  VAPOR PRESSURE (DEPENDING ON THE REFERENCE LEFT SIDE OF VAPOR PRESSURE EQUATION)

         VPEQN = DEXP(SR / T * TC) * PC

1000 continue

      RETURN
      END


! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcdpdT(T,D)
! -----------------------------------------------------------------------------
!
!  DERIVATIVE OF THE PRESSURE WITH RESPECT TO THE REDUCED DENSITY
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    calcdpdT   DERIVATIVE OF THE PRESSURE WITH 
!                       RESPECT TO THE REDUCED DENSITY [(MPa * M **3) / KG ]
!
! -----------------------------------------------------------------------------
!   
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,DELTA,TAU, PhirD,PhirDD, PhirDT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      calcdpdT = 0.D0

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcdpdT = -111.D0
         goto 1000
      ENDIF

      DELTA = D / DC
      TAU = TC / T

      calcdpdT = 1.D-3*D*R*(1.D0 + DELTA * PhirD(T,D) - DELTA * TAU * PhirDT(T,D))

1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function calcdpDD(T,D)
! -----------------------------------------------------------------------------
!
!  DERIVATIVE OF THE PRESSURE WITH RESPECT TO THE REDUCED DENSITY F(T, D)
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M**3]
!
!  OUTPUT:    DPDDCAL   DERIVATIVE OF THE PRESSURE WITH RESPECT TO THE 
!                       REDUCED DENSITY [(MPa * M**3) / KG ]
!
! -----------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,DELTA,PhirD,PhirDD
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      calcdpDD = 0.D0

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         calcdpDD = -111.D0
         goto 1000
      ENDIF

      DELTA = D / DC

      calcdpDD = 1.D-3*R*T*(1.D0 + 2.D0 * DELTA * PhirD(T,D) + DELTA * DELTA * PhirDD(T,D)) 

1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function ABLRES(D,T)
! -----------------------------------------------------------------------------
!
! EXTERNAL FUNCTION TO CALCULATE Dp/dT DURING ITERATIONS
!
!  INPUT:     D         DICHTE [KG / M ** 3]
!             T         TEMPERATUR [K]
!
!  OUTPUT:    ABLRES    CONNECTION TO DENSITY [(MPa * M **3) / KG ]
!
      IMPLICIT NONE
      REAL(KIND=8) ::  calcdpdD, D, T
      ABLRES = 0.0D+0

      ABLRES = calcdpdD(T,D)

      RETURN
      END

!AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


!PHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPH
!PH            IDEAL-GAS PART AND RESIDUAL PART OF HELMHOLTZ FREE ENERGY                PH
!PHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPH

! ---------------------------------------------------------------------------------------------
      REAL(KIND=8) function Phio(T,D)
! ---------------------------------------------------------------------------------------------
!
!  CALCULATION OF THE IDEAL GAS PART OF THE REDUCED HELMHOLTZ ENERGY FUNCTION OF T AN D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY  [KG / M ** 3]
!
!  OUTPUT:    Phio       IDEAL GAS PART OF THE REDUCED
!                       HELMHOLTZ ENERGY [-]
!
! ---------------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      COMMON / EQUI / BNULL,BZ,B1,B2,B(18),TPOTI(18),NPOLI,NPEI,NI
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  BNULL,BZ,B1,B2,B,TPOTI
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  DELTA,TAU
      INTEGER NI,NPOLI,NPEI,NSTART,I

      DELTA = D / DC
      TAU   = TC / T

      Phio = DLOG(DELTA) + B2 + B1 * TAU + BNULL * DLOG(TAU)  &
                        + BZ * TAU * DLOG(TAU)

      DO 10 I = 1,NPOLI
         Phio = Phio + B(I) * TAU ** TPOTI(I)
   10 CONTINUE

      IF(NPOLI .EQ. NI) goto 1000

      DO 20 I = NPOLI+1,NPOLI+NPEI
         Phio = Phio + B(I) * DLOG(1.D0 - DEXP(-TPOTI(I) * TAU))
   20 CONTINUE

      IF(NPOLI + NPEI .EQ. NI) goto 1000
      NSTART = NPOLI + NPEI

      DO 30 I = NSTART+1,NSTART+2
         Phio = Phio + B(I) * DLOG(DABS(DSINH(TPOTI(I) * TAU)))
   30 CONTINUE

      DO 40 I = NSTART+3,NI
         Phio = Phio - B(I) * DLOG(DABS(DCOSH(TPOTI(I)*TAU)))
   40 CONTINUE

  1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function PhioT(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE FIRST DERIVATIVE OF THE IDEAL GAS PART OF THE REDUCED 
!   HELMHOLTZ ENERGY WITH RESPECT TO THE REDUCED TEPERATURE FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhioT      FIRST DERIVATIVE OF THE REDUCED HELMHOLTZ ENERGY 
!                       WITH RESPECT TO THE REDUCED TEMPERATURE [-]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      COMMON / EQUI / BNULL,BZ,B1,B2,B(18),TPOTI(18),NPOLI,NPEI,NI
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  BNULL,BZ,B1,B2,B,TPOTI
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  TAU
      INTEGER NI,NPOLI,NPEI,NSTART,I

      TAU   = TC / T

      PhioT = B1 + BNULL / TAU + BZ * DLOG(TAU) + BZ

      DO 10 I = 1,NPOLI
         PhioT = PhioT + B(I) * TPOTI(I) * TAU ** (TPOTI(I) - 1.D0)
   10 CONTINUE

      IF(NPOLI .EQ. NI) goto 1000

      DO 20 I = NPOLI+1,NPOLI+NPEI
         PhioT = PhioT + B(I) * TPOTI(I) * (1.D0 / (1.D0 - DEXP(-TPOTI(I) * TAU)) - 1.D0) 
   20 CONTINUE

      IF(NPOLI+NPEI .EQ. NI) goto 1000
      NSTART = NPOLI + NPEI

      DO 30 I = NSTART+1,NSTART+2
         PhioT = PhioT + B(I) * TPOTI(I) / DTANH(TPOTI(I) * TAU)
   30 CONTINUE

      DO 40 I= NSTART+3,NI
         PhioT = PhioT - B(I) * TPOTI(I) * DTANH(TPOTI(I) * TAU)
   40 CONTINUE

  1000 continue

!      PhioT = PhioT * R / R * TC / TC

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function PhioTT(T,D)
! -----------------------------------------------------------------------------
!
!  CALCULATION OF THE SECOND DERIVATIVE OF THE IDEAL GAS PART OF THE REDUCED 
!  HELMHOLTZ ENERGY WITH RESPECT TO THE REDUCED TEMPERATURE FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhioTT     SECOND DERIVATIVE OF THE REDUCED HELMHOLTZ ENERGY 
!                        WITH RESPECT TO THE REDUCED TEMPERATURE [-]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      COMMON / EQUI / BNULL,BZ,B1,B2,B(18),TPOTI(18),NPOLI,NPEI,NI
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      REAL(KIND=8) ::  T,D, tpt
      REAL(KIND=8) ::  BNULL,BZ,B1,B2,B,TPOTI
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  TAU
      INTEGER NI,NPOLI,NPEI,NSTART,I

      TAU   = TC / T

      PhioTT = - BNULL / (TAU * TAU) + BZ / TAU

     DO 20 I = NPOLI+1,NPOLI+NPEI
         tpt = DEXP(-TPOTI(I) * TAU)
         PhioTT = PhioTT - B(I) * TPOTI(I)*TPOTI(I) * tpt / ( (1.D0 - tpt)*(1.D0 - tpt) ) 
   20 CONTINUE

      RETURN
      END

! --------------------------------------------------------------------------------------
      REAL(KIND=8) function Phir(T,D)
! --------------------------------------------------------------------------------------
!
!  CALCULATION OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    Phir       RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY [-]
!
! --------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  A,DPOT,TPOT,GAMMA,ETA,EPSI,BETA,ALPHA
      REAL(KIND=8) ::  DELTA,TAU
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  EX,GBSTERM,AI,BETI,BI,CI,DI,EI,FI
      REAL(KIND=8) ::  DELNA1,DELNA2,DELNA,PSINA,DELM1,TAUM1
     REAL(KIND=8) ::  delta2, delta3, delta4, delta5, delta6
     REAL(KIND=8) ::  ex1, ex2, ex3, ex4, ex5, ex6, X
      INTEGER N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NMBWR,NGBS,NNA,NASSO,I

      COMMON / EQUR / A(60),TPOT(60),DPOT(60),GAMMA(60),ETA(60),     &
                     EPSI(60),BETA(60),ALPHA(60),        &
                     N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NGBS,NNA,NASSO
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      Phir = 0.D0

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         Phir = -111.D0
         goto 1000
      ENDIF

      TAU   = TC / T
      DELTA = D / DC
     delta2 = DELTA*DELTA
     delta3 = delta2*DELTA
     delta4 = delta3*DELTA
     delta5 = delta4* DELTA
     delta6 = delta5*DELTA
      ex1 = DEXP(-DELTA)
      ex2 = DEXP(-delta2)
      ex3 = DEXP(-delta3)
      ex4 = DEXP(-delta4)
      ex5 = DEXP(-delta5)
      ex6 = DEXP(-delta6)

!  POLYNOMIAL TERMS

      IF(NPOL .GE. 1) THEN
         DO 10 I = 1,NPOL
            Phir = Phir + A(I) * TAU**TPOT(I) * DELTA**DPOT(I)
   10    CONTINUE
      ENDIF

      IF(NPOL .EQ. N) goto 1000

!  E1-TERMS

      IF (NE1 .GE. 1) THEN
           DO 20 I = NPOL+1,NPOL+NE1
              Phir = Phir + A(I) * TAU**TPOT(I) * DELTA**DPOT(I) * ex1
   20    CONTINUE
      ENDIF

      IF((NPOL+NE1) .EQ. N) goto 1000

!  E2-TERMS

      IF (NE2 .GE. 1) THEN
           DO 30 I = NPOL+NE1+1,NPOL+NE1+NE2
              Phir = Phir + A(I) * TAU**TPOT(I) * DELTA**DPOT(I) * ex2
   30    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2) .EQ. N) goto 1000

!  E3-TERMS

      IF (NE3 .GE. 1) THEN
           DO 40 I = NPOL+NE1+NE2+1,NPOL+NE1+NE2+NE3
              Phir = Phir + A(I) * TAU ** TPOT(I) * DELTA ** DPOT(I) * ex3
   40    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3) .EQ. N) goto 1000

!  E4-TERMS

      IF (NE4 .GE. 1) THEN
           DO 50 I = NPOL+NE1+NE2+NE3+1,NPOL+NE1+NE2+NE3+NE4
              Phir = Phir + A(I) * TAU**TPOT(I) * DELTA**DPOT(I) * ex4
   50    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4) .EQ. N) goto 1000

!  E5-TERMS

      IF (NE5 .GE. 1) THEN
           DO 60 I = NPOL+NE1+NE2+NE3+NE4+1,NPOL+NE1+NE2+NE3+NE4+NE5
              Phir = Phir + A(I) * TAU**TPOT(I) * DELTA**DPOT(I) * ex5
   60    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5) .EQ. N) goto 1000
!  E6-TERMS

      IF (NE6 .GE. 1) THEN
           DO 70 I = NPOL+NE1+NE2+NE3+NE4+NE5+1,               &
                  NPOL+NE1+NE2+NE3+NE4+NE5+NE6
              Phir = Phir + A(I) * TAU**TPOT(I) * DELTA**DPOT(I) * ex6
   70    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5+NE6) .EQ. N) goto 1000

      NMBWR = NPOL + NE1 + NE2 + NE3 + NE4 + NE5 + NE6

!  GBS-TERMS

      IF (NGBS .GE. 1) THEN
           DO 80 I = NMBWR+1,NMBWR+NGBS
              GBSTERM = ETA(I) * (DELTA - EPSI(I)) * (DELTA - EPSI(I))      &
                       + BETA(I) * (TAU - GAMMA(I)) * (TAU - GAMMA(I))
              Phir = Phir + A(I) * TAU**TPOT(I) * DELTA**DPOT(I) * DEXP(-GBSTERM)
   80    CONTINUE
      ENDIF

      IF((NMBWR+NGBS) .EQ. N) goto 1000

!  NA-TERMS

      IF (NNA .GE. 1) THEN
           DO 90 I = NMBWR+NGBS+1,NMBWR+NGBS+NNA
              AI = ALPHA(I)
              BETI = BETA(I)
              BI = EPSI(I)
              CI = ETA (I)
              DI = GAMMA(I)
              EI = DPOT(I)
              FI = TPOT(I)
              DELM1 = DELTA - 1.D0
           if(DELM1 .eq. 0.d0) then
              DELM1 = 1.0d-13
           end if

              TAUM1 = TAU - 1.D0
              DELNA1 = 1.D0 - TAU + CI * ((DELTA - 1.D0) *           &
                      (DELTA - 1.D0)) ** (1.D0 / (2.D0 * BETI))
              DELNA2 = DI * ((DELTA - 1.D0) * (DELTA - 1.D0)) ** AI
              DELNA = DELNA1 * DELNA1 + DELNA2
              PSINA = DEXP(- EI * (DELTA - 1.D0) * (DELTA - 1.D0)     &
                        - FI * (TAU - 1.D0) * (TAU - 1.D0))
           if ( DELNA .GT. 0.D+0 ) then
              Phir = Phir + A(I) * DELTA * DELNA**BI * PSINA
           end if

   90    CONTINUE
      ENDIF

      IF((NMBWR+NGBS+NNA) .EQ. N) goto 1000

!  ASSOCIATION-TERMS

      IF (NASSO .GE. 1) THEN
           DO 100 I = NMBWR+NGBS+NNA+1,NMBWR+NGBS+NNA+NASSO
              Phir = Phir + A(I) * DELTA ** DPOT(I) * DEXP(GAMMA(I) *      &
                         TPOT(I) * TAU - EPSI(I) - (BETA(I) * DELTA) ** ALPHA(I))
  100    CONTINUE
      ENDIF

  1000 continue

      RETURN
      END

! -------------------------------------------------------------------------------------------
      REAL(KIND=8) function PhirD(T,D)
! -------------------------------------------------------------------------------------------
!
!  CALCULATION OF THE FIRST DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY
!   WITH RESPECT TO THE REDUCED DENSITY FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhirD      FIRST DERIVATIVE OF THE REDUCED HELMHOLTZ ENERGY 
!                       WITH RESPECT TO THE REDUCED DENSITY [-]
!
! -------------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  A,DPOT,TPOT,GAMMA,ETA,EPSI,BETA,ALPHA
      REAL(KIND=8) ::  DELTA,TAU
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  EX,EXPTERM,GBS1TERM,GBS2TERM
      REAL(KIND=8) ::  AI,BETI,BI,CI,DI,EI,FI
      REAL(KIND=8) ::  DELNA1,DELNA2,DELNA,PSINA,DELM1,DDDD,DDBDD,DPSDD

!     REAL(KIND=8) ::  PhirD
     REAL(KIND=8) ::  delta2, delta3, delta4, delta5, delta6
     REAL(KIND=8) ::  delna11, delna12
      INTEGER N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NMBWR,NGBS,NNA,NASSO,I

      COMMON / EQUR / A(60),TPOT(60),DPOT(60),GAMMA(60),ETA(60),   &
                     EPSI(60),BETA(60),ALPHA(60),       &
                     N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NGBS,NNA,NASSO
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      PhirD = 0.D0

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         PhirD = -111.D0
         goto 1000
      ENDIF

      TAU   = TC / T
      DELTA = D / DC
     delta2 = DELTA*DELTA
     delta3 = DELTA*DELTA2
     delta4 = DELTA*DELTA3
     delta5 = DELTA*DELTA4
     delta6 = DELTA*DELTA5

!  POLYNOMIAL TERMS    (OK)

      IF(NPOL .GE. 1) THEN
         DO 10 I = 1,NPOL
            PhirD = PhirD + A(I)*( TAU**TPOT(I) ) * DPOT(I) * ( DELTA**(DPOT(I) - 1.D0) )      ! rev
   10    CONTINUE
      ENDIF

!  E1-TERMS      (OK)

      IF (NE1 .GE. 1) THEN
           EX = DEXP(-DELTA)
           DO 20 I = NPOL+1,NPOL+NE1
              EXPTERM = (DPOT(I) - DELTA)
              PhirD = PhirD + A(I)*EX*( TAU**TPOT(I) )*EXPTERM * ( DELTA**(DPOT(I)-1.D0) )   ! rev

   20    CONTINUE
      ENDIF

!  E2-TERMS             (OK)

      IF (NE2 .GE. 1) THEN
           EX = DEXP(-delta2)
           DO 30 I = NPOL+NE1+1, NPOL+NE1+NE2
              EXPTERM = (DPOT(I) - 2.D0*delta2)
              PhirD = PhirD + A(I)*EX*EXPTERM * ( DELTA**(DPOT(I) - 1.D0) )  * ( TAU**TPOT(I) )       ! rev
   30    CONTINUE
      ENDIF

!  E3-TERMS         (OK)

      IF (NE3 .GE. 1) THEN
           EX = DEXP(-delta3)
           DO 40 I = NPOL+NE1+NE2+1,NPOL+NE1+NE2+NE3
              EXPTERM = (DPOT(I) - 3.D0*delta3)
              PhirD = PhirD + A(I)*EX*EXPTERM * ( DELTA**(DPOT(I) - 1.D0) ) * ( TAU**TPOT(I) )    ! rev
   40    CONTINUE
      ENDIF

      IF (NE4 .GE. 1) THEN
           EX = DEXP(-delta4)
           DO 50 I = NPOL+NE1+NE2+NE3+1,NPOL+NE1+NE2+NE3+NE4
              EXPTERM = (DPOT(I) - 4.D0 * delta4)
              PhirD = PhirD + A(I)*EX*EXPTERM * ( DELTA**(DPOT(I) - 1.D0) ) * ( TAU**TPOT(I) )    ! rev
   50    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4) .EQ. N) goto 1000

!  E5-TERMS      (NE5 = 0) Posso dispensar este trecho... ****************

      IF (NE5 .GE. 1) THEN
           EX = DEXP(-DELTA ** 5.D0)
           DO 60 I = NPOL+NE1+NE2+NE3+NE4+1,NPOL+NE1+NE2+NE3+NE4+NE5
              EXPTERM = (DPOT(I) -5.D0 * DELTA ** 5.D0)
              PhirD = PhirD + A(I) * TAU ** TPOT(I)      &
                         * DELTA ** (DPOT(I) - 1.D0) * EXPTERM * EX
   60    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5) .EQ. N) goto 1000

!  E6-TERMS          (OK)

      IF (NE6 .GE. 1) THEN
           EX = DEXP(-delta6)
           DO 70 I = NPOL+NE1+NE2+NE3+NE4+NE5+1, NPOL+NE1+NE2+NE3+NE4+NE5+NE6
              EXPTERM = (DPOT(I) - 6.D0 * ( DELTA ** 6.D0 ))
              PhirD = PhirD + A(I)*EX*EXPTERM * ( DELTA**(DPOT(I) - 1.D0) ) * ( TAU**TPOT(I) )    ! rev
   70    CONTINUE
      ENDIF

      NMBWR = NPOL + NE1 + NE2 + NE3 + NE4 + NE5 + NE6

!  GBS-TERMS        (OK)

      IF (NGBS .GE. 1) THEN
           DO 80 I = NMBWR+1,NMBWR+NGBS
              GBS1TERM = ETA(I) * (DELTA - EPSI(I)) * (DELTA - EPSI(I))   + BETA(I)*(TAU - GAMMA(I))*(TAU - GAMMA(I))
              GBS2TERM = DPOT(I) / DELTA - 2.D0 * ETA(I) * (DELTA - EPSI(I))
              PhirD = PhirD + A(I) * (DELTA ** DPOT(I)) * (TAU ** TPOT(I)) * DEXP(-GBS1TERM) * GBS2TERM       !rev
   80    CONTINUE
      ENDIF

      IF (NNA .GE. 1) THEN
           DO 90 I = NMBWR+NGBS+1, NMBWR+NGBS+NNA
              AI = ALPHA(I)
              BETI = BETA(I)
              BI = EPSI(I)
              CI = ETA (I)
              DI = GAMMA(I)
              EI = DPOT(I)
              FI = TPOT(I)
              DELM1 = DELTA - 1.D0
           if(DELM1 .eq. 0.d0) then
              DELM1 = 1.0d-13
           end if

           delna11 = (1.D0 - TAU)
           delna12 = CI * (DELM1*DELM1)**( 1.D0/(2.D0 * BETI) )
           DELNA2 = DI * (DELM1 * DELM1) ** AI
           delna1 = delna11 + delna12
              DELNA = DELNA1 * DELNA1 + DELNA2
              PSINA = DEXP(- EI * DELM1*DELM1 - FI * (TAU - 1.D0)*(TAU - 1.D0))

!            DDDD = 2.D0 * DELNA1 * (CI/BETI) * DELM1**(1.D0/BETI - 1.D0) + 2.D0 * DI * AI * (DELM1 * DELM1)**(2D0*AI - 1.D0)

              DDDD = DELM1*(CI*DELNA1*2.D0/BETI * (DELM1 * DELM1)**(0.5D0/BETI - 1.D0) &
                + 2.D0 * DI * AI * (DELM1 * DELM1) ** (AI - 1.D0))

              DPSDD = - 2.D0 * EI * DELM1 * PSINA
           DDBDD = 0.d+0
           if ( DELNA .GT. 0.d+0 ) then
               DDBDD = BI * DELNA**(BI - 1.D0) *DDDD
                 PhirD = PhirD + A(I) * (DELNA**BI * (PSINA + DELTA * DPSDD)+ DDBDD * DELTA * PSINA)
           end if

   90    CONTINUE
      ENDIF

      IF((NMBWR+NGBS+NNA) .EQ. N) goto 1000

!  ASSOCIATION-TERMS

      IF (NASSO .GE. 1) THEN
           DO 100 I = NMBWR+NGBS+NNA+1,NMBWR+NGBS+NNA+NASSO
              PhirD = PhirD + A(I) * DELTA ** (DPOT(I) - 1.D0) * (DPOT(I)    &
                         - ALPHA(I) * (BETA(I) * DELTA)** ALPHA(I)) * DEXP(GAMMA(I) *             &
                         TPOT(I) * TAU - EPSI(I) - (BETA(I) * DELTA) ** ALPHA(I))
  100    CONTINUE
      ENDIF

  1000 continue

      RETURN
      END

! ----------------------------------------------------------------------------------------
      REAL(KIND=8) function PhirDD(T,D)
! ----------------------------------------------------------------------------------------
!
!  SECOND DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY
!    WITH RESPECT TO THE REDUCED DENSITY FUNCTION OF T AND D
!                                                                            
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhirDD     SECOND DERIVATIVE OF THE REDUCED HELMHOLTZ ENERGY 
!                       WITH RESPECT TO THE REDUCED DENSITY [-]
!
! ----------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  A,DPOT,TPOT,GAMMA,ETA,EPSI,BETA,ALPHA
      REAL(KIND=8) ::  DELTA,TAU
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
     REAL(KIND=8) ::  delta2, delta3, delta4, delta5, delta6
      REAL(KIND=8) ::  EX,EXPTERM,GBS1TERM,GBS2TERM,GBS3TERM
      REAL(KIND=8) ::  AI,BETI,BI,CI,DI,EI,FI
      REAL(KIND=8) ::  DELNA1,DELNA2,DELNA,PSINA,DELM1,DDDD,DDBDD,DPSDD
      REAL(KIND=8) ::  DDDD2,DDBDD2,DPSDD2
      INTEGER N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NMBWR,NGBS,NNA,NASSO,I

      COMMON / EQUR / A(60),TPOT(60),DPOT(60),GAMMA(60),ETA(60),   &
                     EPSI(60),BETA(60),ALPHA(60),      &
                     N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NGBS,NNA,NASSO
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         PhirDD = -111.D0
         goto 1000
      ENDIF

      TAU   = TC / T
      DELTA = D / DC
     delta2 = DELTA*DELTA
     delta3 = DELTA*DELTA2
     delta4 = DELTA*DELTA3
     delta5 = DELTA*DELTA4
     delta6 = DELTA*DELTA5

!  POLYNOMIAL TERMS

      PhirDD = 0.D0     
      IF(NPOL .GE. 1) THEN
         DO 10 I = 1,NPOL
            PhirDD = PhirDD + A(I) * TAU ** TPOT(I) * (DPOT(I) - 1.D0)   &
                            * DPOT(I) * DELTA ** (DPOT(I) - 2.D0)
   10    CONTINUE
      ENDIF

      IF(NPOL .EQ. N) goto 1000

!  E1-TERMS

      IF (NE1 .GE. 1) THEN
           EX = DEXP(-DELTA)
           DO 20 I = NPOL+1,NPOL+NE1
              EXPTERM = ((DPOT(I) - DELTA) * (DPOT(I) - 1.D0 - DELTA) - DELTA)
              PhirDD = PhirDD + A(I) * TAU ** TPOT(I)                &
                            * DELTA ** (DPOT(I) - 2.D0) * EXPTERM * EX
   20    CONTINUE
      ENDIF

      IF((NPOL+NE1) .EQ. N) goto 1000

!  E2-TERMS

      IF (NE2 .GE. 1) THEN
           EX = DEXP(-delta2)
           DO 30 I = NPOL+NE1+1,NPOL+NE1+NE2
              EXPTERM = ((DPOT(I) - 2.D0 * delta2) * (DPOT(I) - 1.D0 - 2.D0  &
                       * delta2) - 4.D0 * delta2)
              PhirDD = PhirDD + A(I) * TAU ** TPOT(I)                  &
                            * DELTA ** (DPOT(I) - 2.D0) * EXPTERM * EX     
   30    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2) .EQ. N) goto 1000

!  E3-TERMS

      IF (NE3 .GE. 1) THEN
           EX = DEXP(-delta3)
           DO 40 I = NPOL+NE1+NE2+1,NPOL+NE1+NE2+NE3
              EXPTERM = ((DPOT(I) - 3.D0 * delta3) *   &
                        (DPOT(I) - 1.D0 - 3.D0 * delta3) - 9.D0 * delta3)
              PhirDD = PhirDD + A(I) * TAU ** TPOT(I)                  &
                             * DELTA ** (DPOT(I) - 2.D0) * EXPTERM * EX
   40    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3) .EQ. N) goto 1000

!  E4-TERMS

      IF (NE4 .GE. 1) THEN
                  EX = DEXP(-delta4)
           DO 50 I = NPOL+NE1+NE2+NE3+1,NPOL+NE1+NE2+NE3+NE4
              EXPTERM = ((DPOT(I) - 4.D0 * delta4) *   &
                       (DPOT(I) - 1.D0 - 4.D0 * delta4) - 16.D0 * delta4)
              PhirDD = PhirDD + A(I) * TAU ** TPOT(I)                  &
                            * DELTA ** (DPOT(I) - 2.D0) * EXPTERM * EX
   50    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4) .EQ. N) goto 1000

!  E5-TERMS

      IF (NE5 .GE. 1) THEN
           EX = DEXP(-delta5)
           DO 60 I = NPOL+NE1+NE2+NE3+NE4+1,NPOL+NE1+NE2+NE3+NE4+NE5
              EXPTERM = ((DPOT(I) - 5.D0 * delta5) * (DPOT(I) - 1.D0 - 5.D0    &
                       * delta5) - 25.D0 * delta5)
              PhirDD = PhirDD + A(I) * TAU ** TPOT(I)                   &
                            * DELTA ** (DPOT(I) - 2.D0) * EXPTERM * EX
   60    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5) .EQ. N) goto 1000

!  E6-TERMS

      IF (NE6 .GE. 1) THEN
           EX = DEXP(-delta6)
           DO 70 I = NPOL+NE1+NE2+NE3+NE4+NE5+1,                     &
                   NPOL+NE1+NE2+NE3+NE4+NE5+NE6
              EXPTERM = ((DPOT(I) - 6.D0 * delta6) * (DPOT(I) - 1.D0 - 6.D0 &
                       * delta6) - 36.D0 * delta6)
              PhirDD = PhirDD + A(I) * TAU ** TPOT(I)                   &
                            * DELTA ** (DPOT(I) - 2.D0) * EXPTERM * EX
   70    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5+NE6) .EQ. N) goto 1000

      NMBWR = NPOL + NE1 + NE2 + NE3 + NE4 + NE5 + NE6

!  GBS-TERMS

      IF (NGBS .GE. 1) THEN
           DO 80 I = NMBWR+1,NMBWR+NGBS
              GBS1TERM = ETA(I) * (DELTA - EPSI(I)) * (DELTA - EPSI(I))      &
                         + BETA(I) * (TAU - GAMMA(I)) * (TAU - GAMMA(I))
              GBS2TERM = DPOT(I) / DELTA - 2.D0 * ETA(I)                &
                         * (DELTA - EPSI(I))
              GBS3TERM = GBS2TERM * GBS2TERM - DPOT(I) / (DELTA * DELTA)    &
                         - 2.D0 * ETA(I)
              PhirDD = PhirDD + A(I) * TAU ** TPOT(I) * DELTA ** DPOT(I) *   &
                              GBS3TERM * DEXP(-GBS1TERM)
   80    CONTINUE
      ENDIF

      IF((NMBWR+NGBS) .EQ. N) goto 1000

!  NA-TERMS

      IF (NNA .GE. 1) THEN
           DO 90 I = NMBWR+NGBS+1,NMBWR+NGBS+NNA
              AI = ALPHA(I)
              BETI = BETA(I)
              BI = EPSI(I)
              CI = ETA (I)
              DI = GAMMA(I)
              EI = DPOT(I)
              FI = TPOT(I)

              DELM1 = DELTA - 1.D0
           if(DELM1 .eq. 0.d0) then
              DELM1 = 1.0d-13
           end if

              DELNA1 = 1.D0 - TAU + CI * (DELM1 * DELM1) ** (1.D0 / (2.D0 * BETI))
              DELNA2 = DI * (DELM1 * DELM1) ** AI
              DELNA = DELNA1 * DELNA1 + DELNA2
              PSINA = DEXP(- EI * DELM1 * DELM1   - FI * (TAU - 1.D0) * (TAU - 1.D0))
              
           DDDD = DELM1 * (CI * DELNA1 * 2.D0 / BETI               &
                     * (DELM1 * DELM1) ** (0.5D0 / BETI - 1.D0)         &
                     + 2.D0 * DI * AI * (DELM1 * DELM1) ** (AI - 1.D0))
              DDDD2 = DDDD / DELM1 + DELM1 * DELM1 *                 &
                     (4.D0 * DI * AI * (AI - 1.D0) *                  &
                     (DELM1 * DELM1) ** (AI - 2.D0)                  &
                     + 2.D0 * CI * CI / (BETI * BETI) *               &
                     ((DELM1 * DELM1) ** (0.5D0 / BETI - 1.D0))           &
                     * ((DELM1 * DELM1) ** (0.5D0 / BETI - 1.D0))        &
                     + CI * DELNA1 * 4.D0 / BETI * (0.5D0 / BETI - 1.D0)  &
                     * (DELM1 * DELM1) ** (0.5D0 / BETI - 2.D0))
              DPSDD = - 2.D0 * EI * DELM1 * PSINA
              DPSDD2 =  (2.D0 * EI * DELM1 * DELM1 - 1.D0) * 2.D0 * EI * PSINA

              DDBDD = 0.d+0
           if ( DELNA .GT. 0.d+0 ) then 
            DDBDD = BI * DELNA ** (BI - 1.D0) * DDDD
                DDBDD2 = BI * (DELNA ** (BI - 1.D0) * DDDD2 +            &
                         (BI - 1.D0) * DELNA ** (BI - 2.D0) * DDDD * DDDD)
           PhirDD = PhirDD + A(I) * (DELNA ** BI * (2.D0 * DPSDD +      &
                                DELTA * DPSDD2) + 2.D0 * DDBDD * (PSINA   &
                              + DELTA * DPSDD) + DDBDD2 * DELTA *PSINA)
           end if

   90    CONTINUE
      ENDIF

      IF((NMBWR+NGBS+NNA) .EQ. N) goto 1000

!  ASSOCIATION-TERMS

      IF (NASSO .GE. 1) THEN
           DO 100 I = NMBWR+NGBS+NNA+1,NMBWR+NGBS+NNA+NASSO
              PhirDD = PhirDD + A(I) * DELTA ** (DPOT(I) - 2.D0) *         &
                  ((DPOT(I) - ALPHA(I) * (BETA(I) * DELTA)     &
               ** ALPHA(I)) * (DPOT(I) - 1.D0 - ALPHA(I) * (BETA(I)      &
               * DELTA) ** ALPHA(I)) - ALPHA(I) * ALPHA(I)    &                            
               * (BETA(I) * DELTA) ** ALPHA(I))              &
               * DEXP(GAMMA(I) *  TPOT(I) * TAU - EPSI(I) - (BETA(I) *      &
                          DELTA) ** ALPHA(I))
  100    CONTINUE
      ENDIF

  1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function PhirT(T,D)
! -----------------------------------------------------------------------------
!  
!  CALCULATION OF THE FIRST DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED 
!  HELMHOLTZ ENERGY WITH RESPECT TO THE REDUCED TEMPERATURE FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhirT      FIRST DERIVATIVE OF THE REDUCED HELMHOLTZ ENERGY 
!                       WITH RESPECT TO THE REDUCED TEMPERATURE [-]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      COMMON / EQUR / A(60),TPOT(60),DPOT(60),GAMMA(60),ETA(60), &
                     EPSI(60),BETA(60),ALPHA(60),    &
                    N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NGBS,NNA,NASSO
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  A,DPOT,TPOT,GAMMA,ETA,EPSI,BETA,ALPHA
      REAL(KIND=8) ::  DELTA,TAU
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  EX,GBS1TERM,GBS2TERM
      REAL(KIND=8) ::  AI,BETI,BI,CI,DI,EI,FI
      REAL(KIND=8) ::  DELNA1,DELNA2,DELNA,PSINA,DELM1,DDBDT,DPSDT,TAUM1
     REAL(KIND=8) ::  delta2, delta3, delta4, delta5, delta6
     REAL(KIND=8) ::  ex1, ex2, ex3, ex4, ex5, ex6
      INTEGER N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NMBWR,NGBS,NNA,NASSO,I

      PhirT = -0.D0
      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         PhirT = -111.D0
         goto 1000
      ENDIF

      TAU   = TC / T
      DELTA = D / DC
     delta2 = DELTA*DELTA
     delta3 = delta2*DELTA
     delta4 = delta3*DELTA
     delta5 = delta4* DELTA
     delta6 = delta5*DELTA
      ex1 = DEXP(-DELTA)
      ex2 = DEXP(-delta2)
      ex3 = DEXP(-delta3)
      ex4 = DEXP(-delta4)
      ex5 = DEXP(-delta5)
      ex6 = DEXP(-delta6)

!  POLYNOMIAL TERMS

      IF(NPOL .GE. 1) THEN
         DO 10 I = 1,NPOL
            PhirT = PhirT + A(I) * TPOT(I) * TAU**(TPOT(I) - 1.D0) * DELTA**DPOT(I)
   10    CONTINUE
      ENDIF

      IF(NPOL .EQ. N) goto 1000

!  E1-TERMS

      IF (NE1 .GE. 1) THEN                                    ! NE1 = 15
           DO 20 I = NPOL+1,NPOL+NE1                           ! I = 8, 22
              PhirT = PhirT + A(I) * TPOT(I) * (DELTA**DPOT(I)) * ex1 * (TAU**(TPOT(I) - 1.D0) )
   20    CONTINUE
      ENDIF

      IF((NPOL+NE1) .EQ. N) goto 1000

!  E2-TERMS

      IF (NE2 .GE. 1) THEN
           DO 30 I = NPOL+NE1+1,NPOL+NE1+NE2                     ! I = 23, 42
              PhirT = PhirT + A(I) * TPOT(I) * (DELTA**DPOT(I)) * ex2 * (TAU**(TPOT(I) - 1.D0) )
   30    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2) .EQ. N) goto 1000

!  E3-TERMS

      IF (NE3 .GE. 1) THEN
           DO 40 I = NPOL+NE1+NE2+1,NPOL+NE1+NE2+NE3               ! I = 43, 46
               PhirT = PhirT + A(I) * TPOT(I) * (DELTA**DPOT(I)) * ex3 * (TAU**(TPOT(I) - 1.D0) )
   40    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3) .EQ. N) goto 1000

!  E4-TERMS

      IF (NE4 .GE. 1) THEN
           DO 50 I = NPOL+NE1+NE2+NE3+1,NPOL+NE1+NE2+NE3+NE4         ! I = 47, 47
               PhirT = PhirT + A(I) * TPOT(I) * (DELTA**DPOT(I)) * ex4 * (TAU**(TPOT(I) - 1.D0) )
   50    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4) .EQ. N) goto 1000

!  E5-TERMS

      IF (NE5 .GE. 1) THEN                                    ! NE5 = 0
           DO 60 I = NPOL+NE1+NE2+NE3+NE4+1,NPOL+NE1+NE2+NE3+NE4+NE5    
              PhirT = PhirT + A(I) * TPOT(I) * (DELTA**DPOT(I)) * ex5 * (TAU**(TPOT(I) - 1.D0) )
   60    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5) .EQ. N) goto 1000

!  E6-TERMS

      NMBWR = NPOL + NE1 + NE2 + NE3 + NE4 + NE5 + NE6         ! 51
      IF (NE6 .GE. 1) THEN
           DO 70 I = NPOL+NE1+NE2+NE3+NE4+NE5+1,  NMBWR         ! I = 48, 51
              PhirT = PhirT + A(I) * TPOT(I) * (DELTA**DPOT(I)) * ex6 * (TAU**(TPOT(I) - 1.D0) )
   70    CONTINUE
      ENDIF

      IF(NMBWR .EQ. N) goto 1000

!  GBS-TERMS

      IF (NGBS .GE. 1) THEN
           DO 80 I = NMBWR+1,NMBWR+NGBS                           ! I = 52, 54
              GBS1TERM = ETA(I) * (DELTA - EPSI(I)) * (DELTA - EPSI(I))   &
                        + BETA(I) * (TAU - GAMMA(I)) * (TAU - GAMMA(I))
              GBS2TERM = (TPOT(I) / TAU) - 2.D0 * BETA(I) * (TAU - GAMMA(I))
              PhirT = PhirT + A(I) * (TAU**TPOT(I)) * (DELTA**DPOT(I)) *   &
                           GBS2TERM * DEXP(-GBS1TERM)
   80    CONTINUE
      ENDIF

      IF((NMBWR+NGBS) .EQ. N) goto 1000

!  NA-TERMS

      IF (NNA .GE. 1) THEN
           DO 90 I = NMBWR+NGBS+1, NMBWR+NGBS+NNA     ! I = 55, 56

              AI = ALPHA(I)
              BETI = BETA(I)
              BI = EPSI(I)
              CI = ETA (I)
              DI = GAMMA(I)
              EI = DPOT(I)
              FI = TPOT(I)

              DELM1 = DELTA - 1.D0
           if(DELM1 .eq. 0.d0) then
              DELM1 = 1.0d-13
           end if
              TAUM1 = TAU - 1.D0

              DELNA1 = 1.D0 - TAU + CI * (DELM1 * DELM1)**(1.D0 / (2.D0 * BETI))   ! TETA
              
           DELNA2 = DI * (DELM1 * DELM1) ** AI                           
              DELNA = DELNA1 * DELNA1 + DELNA2                              ! DELTA
              PSINA = DEXP(- EI * DELM1 * DELM1 - FI * TAUM1 * TAUM1)            ! PSI
              DPSDT = - 2.D0 * FI * TAUM1 * PSINA                           ! dPSI/dTAU

              DDBDT = 0.D+0
           if ( DELNA .GT. 0.d+0 ) then
                 DDBDT = -2.D0 * DELNA1 * BI * DELNA ** (BI - 1.D0)               !dDeltab/dTAU
              PhirT = PhirT + A(I) * DELTA * (DDBDT * PSINA + (DELNA ** BI) * DPSDT)
           end if

   90    CONTINUE
      ENDIF

  1000 continue

      RETURN
      END

! ---------------------------------------------------------------------------------------------
      REAL(KIND=8) function PhirTT(T,D)
! ---------------------------------------------------------------------------------------------
!
!  CALCULATION OF THE SECOND DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED 
!  HELMHOLTZ ENERGY WITH RESPECT TO THE REDUCED TEMPERATURE AS FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhirTT    SECOND DERIVATIVE OF THE REDUCED HELMHOLTZ ENERGY 
!                       WITH RESPECT TO THE REDUCED DENSITY [-]
!
! ---------------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      COMMON / EQUR / A(60),TPOT(60),DPOT(60),GAMMA(60),ETA(60),   &
                      EPSI(60),BETA(60),ALPHA(60),      &
                      N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NGBS,NNA,NASSO
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  A,DPOT,TPOT,GAMMA,ETA,EPSI,BETA,ALPHA
      REAL(KIND=8) ::  DELTA,TAU
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  EX,GBS1TERM,GBS2TERM,GBS3TERM
      REAL(KIND=8) ::  AI,BETI,BI,CI,DI,EI,FI
      REAL(KIND=8) ::  DELNA1,DELNA2,DELNA,PSINA,DELM1,DDBDT,DPSDT
      REAL(KIND=8) ::  DDBDTT,DPSDTT,TAUM1
     REAL(KIND=8) ::  delta2, delta3, delta4, delta5, delta6
      INTEGER N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NMBWR,NGBS,NNA,NASSO,I

      PhirTT = 0.D0
     IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         PhirTT = -111.D0
         goto 1000
      ENDIF

      DELTA = D / DC
      TAU   = TC / T
     delta2 = DELTA*DELTA
     delta3 = delta2*DELTA
     delta4 = delta3*DELTA
     delta5 = delta4* DELTA
     delta6 = delta5*DELTA

!  POLYNOMIAL TERMS

      IF(NPOL .GE. 1) THEN
         DO 10 I = 1,NPOL
            PhirTT = PhirTT + A(I) * DELTA ** DPOT(I) * (TPOT(I) - 1.D0)     &
                            * TPOT(I) * TAU ** (TPOT(I) - 2.D0)
   10    CONTINUE
      ENDIF

      IF(NPOL .EQ. N) goto 1000

!  E1-TERMS

      IF (NE1 .GE. 1) THEN
           EX = DEXP(-DELTA)
           DO 20 I = NPOL+1,NPOL+NE1
              PhirTT = PhirTT + A(I) * TPOT(I) * (TPOT(I) - 1.D0)          &
                      * TAU ** (TPOT(I) - 2.D0) * DELTA ** DPOT(I) * EX
   20    CONTINUE
      ENDIF

      IF((NPOL+NE1) .EQ. N) goto 1000

!  E2-TERMS

      IF (NE2 .GE. 1) THEN
           EX = DEXP(-delta2)
           DO 30 I = NPOL+NE1+1,NPOL+NE1+NE2
              PhirTT = PhirTT + A(I) * TPOT(I) * (TPOT(I) - 1.D0)       &
                      * TAU ** (TPOT(I) - 2.D0) * DELTA ** DPOT(I) * EX
   30    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2) .EQ. N) goto 1000
!  E3-TERMS

      IF (NE3 .GE. 1) THEN
           EX = DEXP(-delta3)
           DO 40 I = NPOL+NE1+NE2+1,NPOL+NE1+NE2+NE3
              PhirTT = PhirTT + A(I) * TPOT(I) * (TPOT(I) - 1.D0)       &
                      * TAU ** (TPOT(I) - 2.D0) * DELTA ** DPOT(I) * EX
   40    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3) .EQ. N) goto 1000

!  E4-TERMS

      IF (NE4 .GE. 1) THEN
           EX = DEXP(-delta4)
           DO 50 I = NPOL+NE1+NE2+NE3+1,NPOL+NE1+NE2+NE3+NE4
              PhirTT = PhirTT + A(I) * TPOT(I) * (TPOT(I) - 1.D0)       &
                      * TAU ** (TPOT(I) - 2.D0) * DELTA ** DPOT(I) * EX
   50    CONTINUE
      ENDIF
      IF((NPOL+NE1+NE2+NE3+NE4) .EQ. N) goto 1000

!  E5-TERMS

      IF (NE5 .GE. 1) THEN
           EX = DEXP(-delta5)
           DO 60 I = NPOL+NE1+NE2+NE3+NE4+1,NPOL+NE1+NE2+NE3+NE4+NE5
              PhirTT = PhirTT + A(I) * TPOT(I) * (TPOT(I) - 1.D0)        &
                      * TAU ** (TPOT(I) - 2.D0) * DELTA ** DPOT(I) * EX
   60    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5) .EQ. N) goto 1000

!  E6-TERMS

      IF (NE6 .GE. 1) THEN
           EX = DEXP(-delta6)
           DO 70 I = NPOL+NE1+NE2+NE3+NE4+NE5+1,     &
                     NPOL+NE1+NE2+NE3+NE4+NE5+NE6
              PhirTT = PhirTT + A(I) * TPOT(I) * (TPOT(I) - 1.D0)       &
                      * TAU ** (TPOT(I) - 2.D0) * DELTA ** DPOT(I) * EX
   70    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5+NE6) .EQ. N) goto 1000

      NMBWR = NPOL + NE1 + NE2 + NE3 + NE4 + NE5 + NE6

!  GBS-TERMS

      IF (NGBS .GE. 1) THEN
           DO 80 I = NMBWR+1,NMBWR+NGBS
              GBS1TERM = ETA(I) * (DELTA - EPSI(I)) * (DELTA - EPSI(I))      &
                         + BETA(I) * (TAU - GAMMA(I)) * (TAU - GAMMA(I))
              GBS2TERM = TPOT(I) / TAU - 2.D0 * BETA(I)                &
                         * (TAU - GAMMA(I))
              GBS3TERM = GBS2TERM * GBS2TERM - TPOT(I) / (TAU * TAU)       &
                         - 2.D0 * BETA(I)
              PhirTT = PhirTT + A(I) * TAU ** TPOT(I) * DELTA ** DPOT(I) *    &
                              GBS3TERM * DEXP(-GBS1TERM)
   80    CONTINUE
      ENDIF

      IF((NMBWR+NGBS) .EQ. N) goto 1000

!  NA-TERMS

      IF (NNA .GE. 1) THEN
           DO 90 I = NMBWR+NGBS+1,NMBWR+NGBS+NNA
              AI = ALPHA(I)
              BETI = BETA(I)
              BI = EPSI(I)
              CI = ETA (I)
              DI = GAMMA(I)
              EI = DPOT(I)
              FI = TPOT(I)

              DELM1 = DELTA - 1.D0
           if(DELM1 .eq. 0.d0) then
              DELM1 = 1.0d-13
           end if

              TAUM1 = TAU - 1.D0
              
              DELNA1 = 1.D0 - TAU + CI * (DELM1 *  DELM1) ** (1.D0 / (2.D0 * BETI))
              DELNA2 = DI * (DELM1 * DELM1) ** AI
              DELNA = DELNA1 * DELNA1 + DELNA2
              PSINA = DEXP(- EI * DELM1 * DELM1 - FI * TAUM1 * TAUM1)
              DPSDT = - 2.D0 * FI * (TAU - 1.D0) * PSINA
              DPSDTT =  (2.D0 * FI * TAUM1 * TAUM1 - 1.D0) * 2.D0 * FI * PSINA

           DDBDT = 0.d+00
           DDBDTT = 0.d+00
           if ( DELNA .GT. 0.d+0 ) then
                DDBDT = -2.D0 * DELNA1 * BI * DELNA ** (BI - 1.D0)
             DDBDTT = 2.D0 * BI * DELNA ** (BI - 1.D0) + 4.D0 *      &
                         DELNA1 * DELNA1 * BI * (BI - 1.D0) * DELNA ** (BI - 2.D0)
                PhirTT = PhirTT + A(I) * DELTA * (DDBDTT * PSINA             &
                                + 2.D0 * DDBDT * DPSDT + DELNA ** BI * DPSDTT)
           end if

   90    CONTINUE
      ENDIF

      IF((NMBWR+NGBS+NNA) .EQ. N) goto 1000

!  ASSOCIATION-TERMS

      IF (NASSO .GE. 1) THEN
           DO 100 I = NMBWR+NGBS+NNA+1,NMBWR+NGBS+NNA+NASSO
              PhirTT = PhirTT + A(I) * DELTA ** DPOT(I) * GAMMA(I)     &
                                  * GAMMA(I) * TPOT(I) * TPOT(I) *     &
                            DEXP(GAMMA(I) * TPOT(I) * TAU - EPSI(I) -&
                            (BETA(I) * DELTA) ** ALPHA(I))
  100    CONTINUE
      ENDIF

  1000 continue

      RETURN
      END

! ----------------------------------------------------------------------------------------
      REAL(KIND=8) function PhirDT(T,D)
! ----------------------------------------------------------------------------------------
!
!  CALCULATION OF THE DERIVATIVE OF THE RESIDUAL PART OF THE REDUCED HELMHOLTZ ENERGY WITH
!   RESPECT TO THE REDUCED   DENSITY AND TEMPERATURE   FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!  OUTPUT:    PhirDT     DERIVATIVE OF THE REDUCED HELMHOLTZ ENERGY WITH RESPECT 
!                       TO THE REDUCED DENSITY AND TEMPERATURE [-]
!
! ----------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      COMMON / EQUR / A(60),TPOT(60),DPOT(60),GAMMA(60),ETA(60),     &
                      EPSI(60),BETA(60),ALPHA(60),        &
                      N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NGBS,NNA,NASSO
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      REAL(KIND=8) ::  T,D
      REAL(KIND=8) ::  A,DPOT,TPOT,GAMMA,ETA,EPSI,BETA,ALPHA
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  DELTA,TAU
      REAL(KIND=8) ::  delta2, delta3, delta4, delta5, delta6
      REAL(KIND=8) ::  EX,EXPTERM,GBS1TERM,GBS2TERM,GBS3TERM
      REAL(KIND=8) ::  AI,BETI,BI,CI,DI,EI,FI
      REAL(KIND=8) ::  DELNA1,DELNA2,DELNA,PSINA,DELM1,DDDD,DDBDD,DPSDD
      REAL(KIND=8) ::  DDBDT,DDBDDT,DPSDT,DPSDDT,TAUM1
      INTEGER N,NPOL,NE1,NE2,NE3,NE4,NE5,NE6,NMBWR,NGBS,NNA,NASSO,I

      IF ((T .LE. 0.D0) .OR. (D .LE. 0.D0)) THEN
         PhirDT = -111.D0
         goto 1000
      ENDIF

      TAU   = TC / T
      DELTA = D / DC
     delta2 = DELTA*DELTA
     delta3 = DELTA*DELTA2
     delta4 = DELTA*DELTA3
     delta5 = DELTA*DELTA4
     delta6 = DELTA*DELTA5

!  POLYNOMIAL TERMS

      PhirDT = 0.D0
      IF(NPOL .GE. 1) THEN
         DO 10 I = 1,NPOL
            PhirDT = PhirDT + A(I) * TPOT(I) * TAU ** (TPOT(I) - 1.D0)   &
                          * DPOT(I) * DELTA ** (DPOT(I) - 1.D0)
   10    CONTINUE
      ENDIF

      IF(NPOL .EQ. N) goto 1000

!  E1-TERMS

      IF (NE1 .GE. 1) THEN
           EX = DEXP(-DELTA)
           DO 20 I = NPOL+1,NPOL+NE1
              EXPTERM = (DPOT(I) - DELTA)
              PhirDT = PhirDT + A(I) * TPOT(I) * TAU ** (TPOT(I) - 1.D0)      &
                            * DELTA ** (DPOT(I) - 1.D0) * EXPTERM * EX
   20    CONTINUE
      ENDIF

      IF((NPOL+NE1) .EQ. N) goto 1000

!  E2-TERMS

      IF (NE2 .GE. 1) THEN
           EX = DEXP(-delta2)
           DO 30 I = NPOL+NE1+1,NPOL+NE1+NE2
              EXPTERM = (DPOT(I) - 2.D0 * delta2)
              PhirDT = PhirDT + A(I) * TPOT(I) * TAU ** (TPOT(I) - 1.D0)      &
                            * DELTA ** (DPOT(I) - 1.D0) * EXPTERM * EX
   30    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2) .EQ. N) goto 1000

!  E3-TERMS

      IF (NE3 .GE. 1) THEN
           EX = DEXP(-delta3)
           DO 40 I = NPOL+NE1+NE2+1,NPOL+NE1+NE2+NE3
              EXPTERM = (DPOT(I) - 3.D0 * delta3)
              PhirDT = PhirDT + A(I) * TPOT(I) * TAU ** (TPOT(I) - 1.D0)      &
                            * DELTA ** (DPOT(I) - 1.D0) * EXPTERM * EX
   40    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3) .EQ. N) goto 1000

!  E4-TERMS

      IF (NE4 .GE. 1) THEN
           EX = DEXP(-delta4)
           DO 50 I = NPOL+NE1+NE2+NE3+1,NPOL+NE1+NE2+NE3+NE4
              EXPTERM = (DPOT(I) - 4.D0 * delta4)
              PhirDT = PhirDT + A(I) * TPOT(I) * TAU ** (TPOT(I) - 1.D0)      &
                            * DELTA ** (DPOT(I) - 1.D0) * EXPTERM * EX
   50    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4) .EQ. N) goto 1000

!  E5-TERMS

      IF (NE5 .GE. 1) THEN
           EX = DEXP(-delta5)
           DO 60 I = NPOL+NE1+NE2+NE3+NE4+1,NPOL+NE1+NE2+NE3+NE4+NE5     
              EXPTERM = (DPOT(I) - 5.D0 * delta5)     
              PhirDT = PhirDT + A(I) * TPOT(I) * TAU ** (TPOT(I) - 1.D0)      &
                            * DELTA ** (DPOT(I) - 1.D0) * EXPTERM * EX
   60    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5) .EQ. N) goto 1000

!  E6-TERMS

      IF (NE6 .GE. 1) THEN
           EX = DEXP(-delta6)
           DO 70 I = NPOL+NE1+NE2+NE3+NE4+NE5+1,      &
                   NPOL+NE1+NE2+NE3+NE4+NE5+NE6
              EXPTERM = (DPOT(I) - 6.D0 * delta6)
              PhirDT = PhirDT + A(I) * TPOT(I) * TAU ** (TPOT(I) - 1.D0)      &
                            * DELTA ** (DPOT(I) - 1.D0) * EXPTERM * EX
   70    CONTINUE
      ENDIF

      IF((NPOL+NE1+NE2+NE3+NE4+NE5+NE6) .EQ. N) goto 1000

      NMBWR = NPOL + NE1 + NE2 + NE3 + NE4 + NE5 + NE6

!  GBS-TERMS

      IF (NGBS .GE. 1) THEN
           DO 80 I = NMBWR+1,NMBWR+NGBS
              GBS1TERM = ETA(I) * (DELTA - EPSI(I)) * (DELTA - EPSI(I))      &
                         + BETA(I) * (TAU - GAMMA(I)) * (TAU - GAMMA(I))
              GBS2TERM = DPOT(I) / DELTA - 2.D0 * ETA(I)                &
                         * (DELTA - EPSI(I))
              GBS3TERM = TPOT(I) / TAU - 2.D0 * BETA(I)                &
                         * (TAU - GAMMA(I))
              PhirDT = PhirDT + A(I) * TAU ** TPOT(I) * DELTA ** DPOT(I) *   &
                              GBS2TERM * GBS3TERM * DEXP(-GBS1TERM)
   80    CONTINUE
      ENDIF

      IF((NMBWR+NGBS) .EQ. N) goto 1000

!  NA-TERMS

      IF (NNA .GE. 1) THEN
           DO 90 I = NMBWR+NGBS+1,NMBWR+NGBS+NNA

              AI = ALPHA(I)
              BETI = BETA(I)
              BI = EPSI(I)
              CI = ETA (I)
              DI = GAMMA(I)
              EI = DPOT(I)
              FI = TPOT(I)

              DELM1 = DELTA - 1.D0
           if(DELM1 .eq. 0.d0) then
              DELM1 = 1.0d-13
           end if

              TAUM1 = TAU - 1.D0
              DELNA1 = 1.D0 - TAU + CI * (DELM1*DELM1)**(1.D0/(2.D0*BETI))
              DELNA2 = DI * (DELM1*DELM1)**AI
              DELNA = DELNA1 * DELNA1 + DELNA2
              PSINA = DEXP(- EI * DELM1 * DELM1 - FI * TAUM1 * TAUM1)
              
              DDDD = DELM1 * (CI * DELNA1 * 2.D0 / BETI               &
                     * (DELM1 * DELM1) ** (0.5D0 / BETI - 1.D0)         &
                     + 2.D0 * DI * AI * (DELM1 * DELM1) ** (AI - 1.D0))
              DPSDD = - 2.D0 * EI * DELM1 * PSINA
              DPSDT = - 2.D0 * FI * (TAU - 1.D0) * PSINA
              DPSDDT = 4.D0 * EI * FI * DELM1 * TAUM1 * PSINA

           DDBDD = 0.D0
           DDBDT = 0.D0
           DDBDDT = 0.D0

           if ( DELNA .GT. 0.d+0 ) then
                 DDBDD = BI * DELNA ** (BI - 1.D0) * DDDD
             DDBDT = -2.D0 * DELNA1 * BI * DELNA ** (BI - 1.D0)
                 DDBDDT = - CI * BI * 2.D0 / BETI * DELNA ** (BI - 1.D0) *     &
                            DELM1 * (DELM1 * DELM1) ** (0.5D0 / BETI - 1.D0)  &
                          - 2.D0 * DELNA1 * BI * (BI - 1.D0) *               &
                            DELNA ** (BI - 2.D0) * DDDD
                 PhirDT = PhirDT + A(I) * (DELNA ** BI * (DPSDT + DELTA * DPSDDT)      &
                              + DELTA * DDBDD * DPSDT + DDBDT * (PSINA + DELTA * DPSDD) &
                              + DDBDDT * DELTA * PSINA)
           end if

   90    CONTINUE
      ENDIF

      IF((NMBWR+NGBS+NNA) .EQ. N) goto 1000

!  ASSOCIATION-TERMS

      IF (NASSO .GE. 1) THEN
           DO 100 I = NMBWR+NGBS+NNA+1,NMBWR+NGBS+NNA+NASSO
              PhirDT = PhirDT + A(I) * DELTA ** (DPOT(I) - 1.D0) *                   &
                              GAMMA(I) * TPOT(I) * (DPOT(I) - ALPHA(I)                &
                              * (BETA(I) * DELTA) ** ALPHA(I)) * DEXP(GAMMA(I)         &
                       *   TPOT(I) * TAU - EPSI(I) - (BETA(I) * DELTA) ** ALPHA(I))
  100    CONTINUE
      ENDIF

  1000 continue

      RETURN
      END
!PHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPHPH

! SATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSAT
! SAT            SATURATION AUXILIARY / ITERATIVE ROUTINES                    SAT
! SATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSAT

! -----------------------------------------------------------------------------
      SUBROUTINE TSATIT(T,DV,DL,P,EPS)
! -----------------------------------------------------------------------------
!
!  VAPOR PRESSURE, SATURATION TEMPERATURE AND DENSITY BY A
!  STATE EQUATION AT CONTROLLED TEMPERATURE.
! 
!  INPUT:     T         TEMPERATURE [K]
!  EPS:               CONVERGENCE TOLERANCE
! 
!   OUTPUT:
!
!              DV    VAPOR DENSITY [KG / M ** 3]
!              DL    LIQUID DENSITY [KG / M ** 3]
!              P     PRESSURE [MPa]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,DV,DL,P,EPS
      REAL(KIND=8) ::    R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  DVMAX,DLMIN,DITPRES,CRMXWRES,ABLRES
      REAL(KIND=8) ::  VPEQN,DLEQN,DVEQN,calcp,Phir,calcdpdD
      REAL(KIND=8) ::  TCE,DCE,PCE,PSOLD,DLOLD,DVOLD,PDVOLD,PMOLD
      REAL(KIND=8) ::  DVN,DLN,PDVN,PDLN,PMN,ALPADL,ALPADV,DLV,PLV
      REAL(KIND=8) ::  AAA,BBB,CCC,EEE,VZTEST,VZW,DELTAL,DELTAV
      REAL(KIND=8) ::  T1,P1,D1,ABLTEST,DVMA,DVMB,DLMA,DLMB,X,PA,PB
      REAL(KIND=8) ::  DVAT,DV1A,DV1B,PTEST1,PTEST2,DV1,DLAT,DL1A,DL1B,DL1
      REAL(KIND=8) ::  P2,DV2A,DV2B,DL2A,DL2B,DV2,DL2,F1,F2,RES,DVITOLD
      REAL(KIND=8) ::  P3,DV3A,DV3B,DL3A,DL3B,DV3,DL3,F3,DDV,DDL,DD,PTEST
      REAL(KIND=8) ::  DPS,EPSCR,DVA,DVB,DLA,DLB,TSOLD,VPITOLD,DLITOLD
      INTEGER I,IX,ISTART,ITCM,ISUBAKT,ISUBOLD,IEQUAL,ISTARTL
      INTEGER IPCE
      integer imax, imax2, imax3

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT
      COMMON / DEXTREM / DVMAX,DLMIN
      EXTERNAL DITPRES,CRMXWRES,ABLRES

      SAVE TSOLD,VPITOLD,DLITOLD,DVITOLD,ISUBOLD
      DATA TSOLD /1.D9/
      DATA VPITOLD / -1.D+0 /
      DATA DLITOLD / -1.D+0 /
      DATA DVITOLD / -1.D+0 /
      DATA ISUBOLD /0/
!      DATA ISUBAKT /999/
      
     imax = 200 !<------------ (original = 200)
     imax2 = 40 !<------------ (original = 40)
     imax3 = 80 !<------------ (original = 80)

     ISTARTL = 0
      ISTART = 0
      ITCM = 0

      IF ((DABS(T-TSOLD).LT.1.D-8) .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         P = VPITOLD
         DV = DVITOLD
         DL = DLITOLD
         goto 1000
      END IF

      DV = 0.D0
      DL = 0.D0
      P = 0.D0

! CRITICAL PROPERTIES

         TCE = TC             ! TCE = 647.096
         DCE = DC
         PCE = PC

! T > TC OR T < TTR => CALCULATION NOT POSSIBLE   ! TTR = 273.16 K

      IF ((T .GT. TCE ) .OR. (T .LT. TTR)) THEN
         DV = -111.D0
         DL = -111.D0
         P = -111.D0
         goto 1000
      ELSEIF(DABS(T - TCE) .LT. 0.5D0) THEN

! AT SUBCRITICAL TEMPERATURES GO TO THE CALCULATION METHODS   OF SPAN BOUND

         T1 = T
         GOTO 200
      ELSE

! BASED ON VON ZSCHUNKE ET AL. CALCULATION (WITH MODIFICATIONS)

! ESTIMATING START VALUES OF T, DL AND DV

         PSOLD = VPEQN(T)                      ! <+++++++++++++++++++++++++++++++++
         DLOLD = DLEQN(T)                      ! <+++++++++++++++++++++++++++++++++
         IF (PSOLD .LT. 0.05d0) THEN
            DVOLD = PSOLD / R / T * 1.D3
         ELSE
            DVOLD = DVEQN(T)
            IF (DVOLD .GT. DC) THEN
                  DVOLD = DC - 10.D+0
            ENDIF             
         ENDIF

!  EVALUATING (MAXWELL)
!   FIRST CALL WITH START VALUES:

         PDVOLD = calcp(T,DVOLD)
!         PMOLD =  1.D-3*R*T*( Phir(T,DLOLD) - Phir(T,DVOLD) + DLOG(DLOLD/DVOLD) ) / (1.D0/DVOLD - 1.D0/DLOLD)
         PMOLD = (Phir(T,DLOLD) - Phir(T,DVOLD) + DLOG(DLOLD / DVOLD)) /     &
                 (1.D0 / DVOLD - 1.D0 / DLOLD) * R * T * 1.D-3
         DVN = 0.9999D0 * DVOLD
         DLN = 1.0001D0 * DLOLD

!  ITERATION LOOP

!         DO 100 I =1,40
         DO 100 I =1,imax2

!   MAXWELL EVALUATION
         PDVN = calcp(T,DVN)      ! <+++++++++++++++++++++++++++++++++
         PDLN = calcp(T,DLN)

         IF ((DLN / DVN) .LT. 0.0D+00) THEN
             T1 = T
             GOTO 200
         ENDIF
!         PMN = 1.D-3*R*T*( Phir(T,DLN) - Phir(T,DVN) + DLOG(DLN/DVN) ) / (1.D0/DVN - 1.D0/DLN)
         PMN = (Phir(T,DLN) - Phir(T,DVN) + DLOG(DLN / DVN)) /         &
                 (1.D0 / DVN - 1.D0 / DLN) * R * T * 1.D-3

! CONVERGENCE TEST

         IF(DABS((PMN / PMOLD) - 1.D0) .LT. EPS) THEN
            IF(DABS((DLN / DLOLD) - 1.D0) .LT. EPS) THEN
               IF(DABS((DVN / DVOLD) - 1.D0) .LT. EPS) THEN
                  ABLTEST = calcdpdD(T,DLN)                 ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  IF (ABLTEST .LE. 0.D0) THEN
                     GOTO 200
                  ENDIF
                  ABLTEST = calcdpdD(T,DVN)           ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  IF (ABLTEST .LE. 0.D0) THEN
                     GOTO 200
                  ENDIF
                  DL = DLN
                  DV = DVN
                  P = PMN
                  goto 1000
               ENDIF
            ENDIF
         ENDIF

! INCREMENT EVALUATION

         ALPADL = - calcdpdD(T,DLN) * DLN * DLN
         IF (DABS(DVN - DVOLD) .LT. 1.D-12) THEN
             T1 = T
             GOTO 200
         ENDIF
         ALPADV = (PDVN - PDVOLD) / (1.D0 / DVN - 1.D0 / DVOLD)
         DLV = 1.D0 / DLN - 1.D0 / DVN
         PLV = PDLN - PDVN
         AAA = 0.5D0 * ALPADL * (ALPADL - ALPADV)
         BBB = ALPADL * (PLV - ALPADV * DLV)
         CCC = ALPADV * DLV * (PMN - PDLN) + 0.5D0 * PLV * PLV
         EEE = 0.25D0 * BBB / AAA * BBB / AAA - CCC / AAA

! SHIFT IN CASE OF NUMERICAL PROBLEMS

         IF (EEE .LE. 1.D-10) THEN
            T1 = T
            GOTO 200
         ENDIF

! SIGNAL TEST

         VZTEST = (ALPADL - ALPADV) / ALPADV
         IF (VZTEST .GE. 0.D0) THEN
            VZW = 1.D0
         ELSE
            VZW = -1.D0
         ENDIF

! NEW CORRECTION INCREMENT

         DELTAL = -0.5D0 * BBB / AAA + VZW * DSQRT(EEE)
         DELTAV = (PLV + ALPADL * DELTAL) / ALPADV
         PDVOLD = PDVN
         DVOLD = DVN
         DLOLD = DLN
         PMOLD = PMN
         DVN = 1.D0 / (1.D0 / DVOLD + DELTAV)
         DLN=  1.D0 / (1.D0 / DLOLD + DELTAL)

! END OF ITERATION LOOP

100      CONTINUE

      ENDIF

! CALCULATION OF SPAN (ALCANCE)

200   CONTINUE

! 1. CASE: NOT NEAR CRITICAL TEMPERATURE

      IF (T .LT. (TCE - 0.25D0)) THEN

! START VALUE FOR PRESSURE WITH THE VAPOR PRESSURE EQUATION

         DVMAX = DCE
         DLMIN = DCE
         P1 = VPEQN(T)            ! <+++++++++++++++++++++++++++++++++
      ELSE

! 2. CASE: CLOSE TO CRITICAL TEMPERATURE

!  DETERMINATION OF MAXIMUM AND MINIMUM DENSITY OF MAXWELL LOOPS

         IF ((TCE - T1) .LE. 1.D-6) THEN
            T1 = TCE - 1.D-6
            ITCM = 1
         ENDIF

         D1 = DCE * 0.99999999D0
         ABLTEST = calcdpdD(T1,D1)
         IF (ABLTEST .LE. 0.D0) THEN
            DVMA = D1
            DVMB = D1
210         CONTINUE
            DVMA = DVMA - 1.D0
            ABLTEST = calcdpdD(T1,DVMA)
            IF (ABLTEST .LE. 0.D0) THEN
               DVMB = DVMA
               GOTO 210
            ENDIF
            CALL ITPEGS(DVMA,DVMB,ABLRES,T1,1.D-9,X,IX)       ! <+++++++++++++++++++++++++++++++++
            DVMAX = X

            DLMA = D1
            DLMB = D1
220         CONTINUE
            DLMB = DLMB + 1.D0
            ABLTEST = calcdpdD(T1,DLMB)
            IF (ABLTEST .LE. 0.D0) THEN
               DLMA = DLMB
               GOTO 220
            ENDIF
            CALL ITPEGS(DLMA,DLMB,ABLRES,T1,1.D-9,X,IX)
            DLMIN = X
         ELSE
            D1 = DCE * 0.6D0
            DVMA = D1
            DVMB = D1
230         CONTINUE
            DVMB = DVMB + 1.D0
            ABLTEST = calcdpdD(T1,DVMB)
            IF (ABLTEST .GE. 0.D0) THEN
               DVMA = DVMB
               GOTO 230
            ENDIF
            CALL ITPEGS(DVMA,DVMB,ABLRES,T1,1.D-9,X,IX)
            DVMAX = X

            DLMA = DVMB
            DLMB = DVMB
240         CONTINUE
            DLMB = DLMB + 1.D0
            ABLTEST = calcdpdD(T1,DLMB)
            IF (ABLTEST .LE. 0.D0) THEN
               DLMA = DLMB
               GOTO 240
            ENDIF
            CALL ITPEGS(DLMA,DLMB,ABLRES,T1,1.D-9,X,IX)
            DLMIN = X

         ENDIF

         IF (T1 .GE. (TCE - 1.D-3)) THEN
            PA = calcp(T1,DLMIN)
            PB = calcp(T1,DVMAX)
            GOTO 800
         ENDIF    

! START VALUE FOR PRESSUR FROM THE FUNDAMENTAL EQ. D = DC

         P1 = calcp(T,DCE)

      ENDIF

! START VALUE FOR DENSITY (TAU EQ.)

      DVAT = DVEQN(T)
      DV1A = DVAT * 0.99D0
      DV1B = DVAT * 1.01D0

      IF (DV1B .GT. DVMAX) THEN
         DV1B = DVMAX
         IEQUAL = 1
      ELSE
         IEQUAL = 0
      ENDIF

300   CONTINUE

      PTEST = calcp(T,DV1B)
      IF (IEQUAL .EQ. 1) THEN
          P1 = PTEST * 0.99999D+0
          IEQUAL = 0
      ENDIF
      IF (PTEST .LT. P1) THEN
         PTEST = calcp(T,DVMAX)
         IF (PTEST .LT. 0.0D+0) THEN
               PTEST = P1 * 1.00001D+0     ! <+++++++++++++++++++++++++++++++++
         ENDIF
         IF (PTEST .LT. P1) THEN
            P1 = PTEST * 0.999D0
            GOTO 300
         ELSE
            DV1B = DVMAX
         ENDIF
      ENDIF

      IF (DV1A .GE. DV1B) THEN
         DV1A = DV1B * 0.95D0
      ENDIF

310   CONTINUE

! ITERATION OF DENSITY

      CALL ITPEG(DV1A,DV1B,DITPRES,T,P1,1.D-9,X,IX)        
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN           ! <*********************************************
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV1 = X
         ELSE
            P1 = P1 * 0.9995D0
            DV1A = DV1A * 0.96D0
            DV1B = DV1B * 0.99D0
            ISTART = ISTART + 1
            IF (ISTART .LE. imax) THEN           ! <********************************************* 200 -> 1000
               GOTO 310
            ELSE
               P = -111.D0           ! <*********************************************
               DL = -111.D0
               DV = -111.D0
               goto 1000
            ENDIF
         ENDIF
      ELSE 
         P1 = P1 * 0.9995D0
         DV1A = DVAT * 0.95D0
         ISTART = ISTART + 1
         IF (ISTART .LE. imax) THEN           ! <********************************************* 200 -> 1000
            GOTO 310
         ELSE
            P = -111.D0           ! <*********************************************
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ENDIF

! START VALUE FOR saturated liquid density

      IF (ISTARTL .EQ. 0) THEN
          DLAT = DLEQN(T)
          DL1A = DLAT * 0.99D0
          DL1B = DLAT * 1.01D0
          ISTARTL = 1
      ENDIF

      IF (DL1A .LT. DLMIN) THEN
         DL1A = DLMIN
         IEQUAL = 1
      ELSE
         IEQUAL = 0
      ENDIF

320   CONTINUE

      PTEST = calcp(T,DL1A)
      P1 = PTEST * 1.00001D+0
      IF (DL1A .GE. DL1B) THEN
         DL1B = DL1A * 1.05D0
      ENDIF

! ITERATION OF SAT LIQ DENSITY

      CALL ITPEG(DL1A,DL1B,DITPRES,T,P1,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL1 = X
         ELSE
            P1 = P1 * 1.0005D0
!            P1 = P1 * 1.00005D0
            DL1B = DL1B * 1.01D0
            ISTART = ISTART + 1
            IF (ISTART .LE. imax) THEN
               GOTO 310
            ELSE
               P = -111.D0
               DL = -111.D0
               DV = -111.D0
               goto 1000
            ENDIF
         ENDIF
      ELSE 
         P1 = P1 * 1.0005D0
         DL1B = DL1B * 1.02D0
         ISTART = ISTART + 1
         IF (ISTART .LE. imax) THEN
            GOTO 310
         ELSE
            P = -111.D0
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ENDIF

! NEW PRESSURE CALCULATION

      P2 = R * T * (Phir(T,DL1) - Phir(T,DV1) + DLOG(DL1 / DV1)) /    &
           (1.D0 / DV1 - 1.D0 / DL1) * 1.D-3

      IPCE = 0
      IF (P2 .LE. 0.D0) THEN
         P2 = VPEQN(TTR)
      ELSEIF (P2 .GT. PCE) THEN
         P2 = PCE - 1.D-12
      IPCE = 1
      ENDIF
      IF (P2 .GT. P1) THEN
         DV2A = DV1 * 0.99D+0
         DV2B = DV1 * 1.02D0
         DL2A = DL1 * 0.99D+0
         DL2B = DL1 * 1.01D0
      ELSE
         DV2A = DV1 * 0.98D0
         DV2B = DV1 * 1.00001D+0
         DL2A = DL1 * 0.99D0
         DL2B = DL1 * 1.00001D+0
      ENDIF

      IF (DL2A .LT. DLMIN) THEN
         DL2A = DLMIN
      ENDIF

      IF (DV2B .GT. DVMAX) THEN
         DV2B = DVMAX
      ENDIF

400   CONTINUE

! ITERATION FOR NEW DENSITY

      CALL ITPEG(DV2A,DV2B,DITPRES,T,P2,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV2 = X
         ELSE
            P2 = P2 * 0.999D0
            GOTO 400
         ENDIF
      ELSEIF (IPCE .EQ. 1) THEN
       P2 = P2 * 0.9D+0
      ELSE
          P2 = P2 * 0.999D0
          GOTO 400
      ENDIF

! ITERATION FOR NEW SAT LIQ DENSITY

      CALL ITPEG(DL2A,DL2B,DITPRES,T,P2,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL2 = X
         ELSE
            P2 = P2 * 1.001D0
!            P2 = P2 * 1.0001D0
            GOTO 400
         ENDIF
      ELSE 
         P2 = P2 * 1.001D0
         GOTO 400
      ENDIF

! EVALUATION OF MAXWELL CRITERIOM

      CALL MXWRES(P1,T,RES,DL1,DV1)
      F1 = RES
      CALL MXWRES(P2,T,RES,DL2,DV2)
      F2 = RES

      IF (DABS(F1) .LT. 1.D-15) THEN
         DV = DV1
         DL = DL1
         P = P1
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         DV = DV2
         DL = DL2
         P = P2
         goto 1000
      ENDIF

! ITERATION LOOP

!      DO 700 I = 1,80
      DO 700 I = 1,imax3
       IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            DV = DV2
            DL = DL2
            P = P2
            goto 1000
         ELSE
            DV = DV1
            DL = DL1
            P = P1
            goto 1000
         ENDIF
       ENDIF

! NEW PRESSURE CALCULATION

      P3 = P2 + (P1 - P2) * F2 / (F2 - F1)

      IF (P3 .LT. 0.0D+0) THEN
            P3 = PTR
      ENDIF

      IF (P3 .GT. P2) THEN
         DV3A = DV2 * 0.98999D+0
         DV3B = DV2 * (1.03D0 + (I - 1) * 0.001D+0)
         DL3A = DL2 * 0.98999D+0
         DL3B = DL2 * (1.01D0 + (I - 1) * 0.001D+0)
      ELSE
         DV3A = DV2 * (0.98D0 - (I - 1) * 0.001D+0)
         DV3B = DV2 * 1.00001D+0
         DL3A = DL2 * (0.99D0 - (I - 1) * 0.001D+0)
         DL3B = DL2 * 1.00001D+0
      ENDIF

      IF (DV3B .GT. DVMAX) THEN
         DV3B = DVMAX
      ENDIF

      IF (DL3A .LT. DLMIN) THEN
         DL3A = DLMIN
      ENDIF

500   CONTINUE

! ITERATION FOR NEW DENSITY

      CALL ITPEG(DV3A,DV3B,DITPRES,T,P3,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV3 = X
         ELSE
            P3 = P3 * 0.9995D0
            GOTO 500
         ENDIF
      ELSE 
         P3 = P3 * 0.9995D0
         GOTO 500
      ENDIF

! ITERATION FOR NEW SAT LIQ DENSITY

      CALL ITPEG(DL3A,DL3B,DITPRES,T,P3,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL3 = X
         ELSE
            P3 = P3 * 1.0005D0
            IF (P3 .GT. PCE) THEN
                  P3 = PCE * 0.99999D+0
              ENDIF
            GOTO 500
         ENDIF
      ELSE 
         P3 = P3 * 1.0005D0
         IF (P3 .GT. (1.05D+0 * PCE)) THEN
               P3 = PCE
           ENDIF
         GOTO 500
      ENDIF

      IF (DV3 .LT. 0.D0) THEN
         DV3 = DVTR
      ENDIF

! MAXWELL-KRITERION

      CALL MXWRES(P3,T,RES,DL3,DV3)
      F3 = RES
      IF (DABS(F3) .LT. 1.D-13) THEN
         DV = DV3
         DL = DL3
         P = P3
         goto 1000
      ENDIF

! CHECKING THE CONVERGENCE CRITERION

      DDL = DABS((DL3 - DL2) / DL3)
      DDV = DABS((DV3 - DV2) / DV3)
      DD = DDL * DDL + DDV * DDV

      IF(DD .GT. EPS * EPS) GOTO 600

      DPS = DABS((P3 - P2) / P3)

      IF(DPS .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F3)) THEN
            DV = DV2
            DL = DL2
            P = P2
            goto 1000
         ELSE
            DV = DV3
            DL = DL3
            P = P3
            goto 1000
         ENDIF
      ENDIF

600   CONTINUE

      DL1 = DL2
      DV1 = DV2
      P1 = P2
      F1 = F2
      DL2 = DL3
      DV2 = DV3
      P2 = P3
      F2 = F3

! END OF ITERATION LOOP

700   CONTINUE

      P = -111.D0
      DL = -111.D0
      DV = -111.D0
      goto 1000

! ITERATION FOR TEMPERATURE CLOSE TO TC: 1.D-3 > TC - T > 1.D-6

800   CONTINUE

      EPSCR = EPS * 1.D-6
      IF (EPSCR .LT. 1.D-15) THEN
         EPSCR = 1.D-15
      ENDIF

! VAPOR PRESSURE ITERATION

      CALL ITPEGS(PA,PB,CRMXWRES,T1,EPSCR,X,IX)

      IF ((X .LT. PA) .OR. (X .GT. PB)) THEN
         T1 = T1 * 0.99999999D0
         ITCM = 1
         GOTO 200
      ENDIF

      P1 = X
      DVB = DVMAX
      DVA = DVB * 0.95D0
810   CONTINUE
      PTEST = calcp(T1,DVA)
      IF (PTEST .GT. P1) THEN
         DVA = DVA * 0.98D0
         GOTO 810
      ENDIF

! DENSITY INTERATION

      CALL ITPEG(DVA,DVB,DITPRES,T1,P1,1.D-9,X,IX)
      IF ((IX .LE. 3) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         IF (ABLTEST .LE. 0.D0) THEN
            X = X - 5.D-8
            ABLTEST = calcdpdD(T1,X)
         ENDIF
         PTEST = DABS(calcp(T1,X) - P1)
         IF ((ABLTEST .GT. 0.D0) .AND. (PTEST .LT. EPS)) THEN
            DV1 = X
         ELSE
            P = -111.D0
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ELSE 
         P = -111.D0
         DL = -111.D0
         DV = -111.D0
         goto 1000
      ENDIF

      DLA = DLMIN
      DLB = DLA * 1.02D0
820   CONTINUE
      PTEST = calcp(T1,DLB)
      IF (PTEST .LT. P1) THEN
         DLB = DLB * 1.02D0
         GOTO 820
      ENDIF

! SAT LIQ DENSITY ITERATION

      CALL ITPEG(DLA,DLB,DITPRES,T1,P1,1.D-9,X,IX)
      IF ((IX .LE. 3) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         IF (ABLTEST .LE. 0.D0) THEN
            X = X + 5.D-8
            ABLTEST = calcdpdD(T1,X)
         ENDIF
         PTEST = DABS(calcp(T1,X) - P1)
         IF ((ABLTEST .GT. 0.D0) .AND. (PTEST .LT. EPS)) THEN
            DL1 = X
         ELSE
            P = -111.D0
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ELSE 
         P = -111.D0
         DL = -111.D0
         DV = -111.D0
         goto 1000
      ENDIF

      IF (ITCM .EQ. 0) THEN
         P = P1
         DV = DV1
         DL = DL1
         goto 1000
      ELSE
         X = (T - T1) / (TCE - T1)
         P = P1 + X * (PCE - P1)
         DV = DV1 + X * (DCE - DV1)
         DL = DL1 + X * (DCE - DL1)
         goto 1000
      ENDIF

1000 continue

      TSOLD = T
      VPITOLD = P
      DVITOLD = DV
      DLITOLD = DL
      ISUBOLD = ISUBAKT

      RETURN
      END

 ! -----------------------------------------------------------------------------
      SUBROUTINE PSATIT(T,DV,DL,P,EPS)
! -----------------------------------------------------------------------------
!
!  ROUTINE FOR CALCULATING OF SAT. TEMPERATURE, VAPOR AND LIQUID DENSITY
!  FROM A STATE EQUATION, FUNCTION OF P
!
!   INPUT:     P         PRESSURE [MPa]
!              EPS       ITERATION TOLERANCE
!
!   OUTPUT:
!
!              T     SATURATION TEMPERATURE [K]
!              DV    VAPOR DENSITY [KG / M ** 3]
!              DL    LIQUID DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  DITPRES,MXWPRES,DLEQN,DVEQN,VPEQN,TVPIT,calcdpdD
      REAL(KIND=8) ::  Phir,TCE,DCE,PCE,T1,TG,DVG,DLG,PG,DVAT,DV1A,DV1B
      REAL(KIND=8) ::  X,ABLTEST,DV1,DLAT,DL1A,DL1B,DL1,T2,DV2A,DV2B
      REAL(KIND=8) ::  DV2,DL2A,DL2B,DL2,F1,F2,T3,DV3A,DV3B,DV3,DL3A
      REAL(KIND=8) ::  DL3B,DL3,F3,DDL,DDV,DD,DTS,TMH,DVMH,DLMH,PMH,TOH
      REAL(KIND=8) ::  DVOH,DLOH,POH,TS1,TS2,TOH2,DVOH2,DLOH2,POH2,TUH
      REAL(KIND=8) ::  DVUH,DLUH,PUH,XT,TH,DVH,DLH,PH,RES,PSOLD,TSOLD
      REAL(KIND=8) ::  DVOLD,DLOLD,PDIFF
      INTEGER I,IX,ISUBOLD,ISUBAKT

      EXTERNAL DITPRES,MXWPRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TSOLD,PSOLD,DLOLD,DVOLD,ISUBOLD
      DATA TSOLD / -1.D+0 /
      DATA PSOLD / -1.D+0 /
        DATA DLOLD / -1.D+0 /
        DATA DVOLD / -1.D+0 /
      DATA ISUBOLD / 0 /

      pdiff = p-psold
      IF ((DABS(P-PSOLD).LT.1.D-15) .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TSOLD
         DV = DVOLD
         DL = DLOLD
         goto 1000
      END IF

      T = 0.D0
      DL = 0.D0
      DV = 0.D0

      IF ((DLTR .LT. 1.D0) .OR. (DVTR .LT. 1.D-9)     &
           .OR. (PTR .LT. 1.D-12)) THEN
         DLTR = DLEQN(TTR)
         DVTR = DVEQN(TTR)
         PTR = VPEQN(TTR)
      END IF

! CRITICAL POINT

      IF( PC .GT. 1.D0 ) THEN
         TCE = TC
         DCE = DC
         PCE = PC
      ELSE
         TCE = TC
         DCE = DC
         PCE = PC
      END IF

!  IF P>PC OR P<PTR => CALCULATION NOT POSSIBLE

      IF ((P .GT. PCE) .OR. (P .LT. PTR)) THEN
         P = -111.D0
         DL = -111.D0
         T = -111.D0
         goto 1000
      ENDIF

!  START VALUE FOR SATURATION TEMPERATURE FROM THE VAPOR PRESSURE EQUATION

      T1 = TVPIT(P)

      IF (T1 .LT. TTR) THEN
         T1 = TTR
      ENDIF

!  AN OUTER ITERATION AT SUBCRITICAL PRESSURE WITH THE COMPUTATION OF GIVEN TEMPERATURES

      IF (P .GE. (0.95D0 * PCE)) THEN
         TG = TCE - 1.D-15
         CALL TSATIT(TG,DVG,DLG,PG,EPS)
         IF (P .GE. PG) THEN
            X = (P - PG) / (PCE - PG)
            T = TG + X * (TCE - TG)
            DV = DVG + X * (DCE - DVG)
            DL = DLG - X * (DLG - DCE)
            goto 1000
         ELSE
            GOTO 600
         ENDIF
      ENDIF

! DETERMINATION OF A START VALUE FOR VAPOR DENSITY

      DVAT = DVEQN(T1)
      DV1A = DVAT * 0.99D0
      DV1B = DVAT * 1.01D0

! ITERATION OF VAPOR DENSITY

      CALL ITPEG(DV1A,DV1B,DITPRES,T1,P,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV1 = X
         ELSE
            GOTO 600
         ENDIF
      ELSE 
         GOTO 600
      ENDIF

! START VALUE FOR LIQUID DENSITY

      DLAT = DLEQN(T1)
      DL1A = DLAT * 0.99D0
      DL1B = DLAT * 1.01D0

! ITERATION OF LIQUID DENSITY

      CALL ITPEG(DL1A,DL1B,DITPRES,T1,P,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL1 = X
         ELSE
            GOTO 600
         ENDIF
      ELSE 
         GOTO 600
      ENDIF

! NEW TEMPERATURE CALCULATION

      T2 = P / R * (1.D0 / DV1 - 1.D0 / DL1) /      &
           (Phir(T1,DL1) - Phir(T1,DV1) + DLOG(DL1 / DV1)) * 1.D3

      IF (T2 .LE. TTR) THEN
         T2 = TTR + 1.D-6
      ELSEIF (T2 .GT. TCE) THEN
         T2 = TCE - 1.D-6
      ENDIF

      IF (T2 .GT. T1) THEN
         DV2A = DV1 * 0.98D0
         DV2B = DV1
         DL2A = DL1 * 0.99D0
         DL2B = DL1
      ELSE
         DV2A = DV1
         DV2B = DV1 * 1.01D0
         DL2A = DL1
         DL2B = DL1 * 1.01D0
      ENDIF

! ITERATION OF VAPOR DENSITY

      CALL ITPEG(DV2A,DV2B,DITPRES,T2,P,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T2,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV2 = X
         ELSE
            GOTO 600
         ENDIF
      ELSE 
         GOTO 600
      ENDIF

! ITERATION OF THE NEW LIQUID DENSITY

      CALL ITPEG(DL2A,DL2B,DITPRES,T2,P,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T2,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL2 = X
         ELSE
            GOTO 600
         ENDIF
      ELSE 
         GOTO 600
      ENDIF

! EVALUATION OF MAXWELL CRITERIUM

      CALL MXWRES(P,T1,RES,DL1,DV1)
      F1 = RES
      CALL MXWRES(P,T2,RES,DL2,DV1)
      F2 = RES

      IF ((F1 .LT. 0.D0) .AND. (F2 .LT. 0.D0)) THEN
         GOTO 600
      ELSEIF ((F1 .GT. 0.D0) .AND. (F2 .GT. 0.D0)) THEN
         GOTO 600
      ENDIF

      IF (DABS(F1) .LT. 1.D-15) THEN
         T = T1
         DL = DL1
         DV = DV1
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         T = T2
         DL = DL2
         DV = DV2
         goto 1000
      ENDIF

! ITERATION LOOP

      DO 500 I = 1,40

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            T = T2
            DL = DL2
            DV = DV2
            goto 1000
         ELSE
            T = T1
            DL = DL1
            DV = DV1
            goto 1000
         ENDIF
      ENDIF

! CALCULATING A NEW TEMPERATURE

      T3 = T2 + (T1 - T2) * F2 / (F2 - F1)

      IF (T3 .LE. TTR) THEN
         T3 = TTR
         DV3A = DVTR * 0.99D0
         DV3B = DVTR * 1.01D0
         DL3A = DLTR * 0.99D0
         DL3B = DLTR * 1.01D0
         GOTO 300
      ENDIF

      IF (T3 .GT. T2) THEN
         DV3A = DV2 * 0.98D0
         DV3B = DV2
         DL3A = DL2 * 0.99D0
         DL3B = DL2
      ELSE
         DV3A = DV2
         DV3B = DV2 * 1.01D0
         DL3A = DL2
         DL3B = DL2 * 1.01D0
      ENDIF

300   CONTINUE

! ITERATION OF NEW VAPOR DENSITY

      CALL ITPEG(DV3A,DV3B,DITPRES,T3,P,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T3,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV3 = X
         ELSE
            GOTO 600
         ENDIF
      ELSE 
         GOTO 600
      ENDIF

! ITERATION OF NEW LIQUID DENSITY

      CALL ITPEG(DL3A,DL3B,DITPRES,T3,P,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T3,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL3 = X
         ELSE
            GOTO 600
         ENDIF
      ELSE 
         GOTO 600
      ENDIF

! MAXWELL CRITERIUM

      CALL MXWRES(P,T3,RES,DL3,DV3)
      F3 = RES

      IF (DABS(F3) .LT. 1.D-12) THEN
         T = T3
         DL = DL3
         DV = DV3
         goto 1000
      ENDIF

! TEST OF CONVERGENCE CRITERIUM

      DDL = DABS((DL3 - DL2) / DL3)
      DDV = DABS((DV3 - DV2) / DV3)
      DD = DDL * DDL + DDV * DDV

      IF(DD .GT. EPS * EPS) GOTO 400

      DTS = DABS((T3 - T2) / T3)

      IF(DTS .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F3)) THEN
            T = T2
            DL = DL2
            DV = DV2
            goto 1000
         ELSE
            T = T3
            DL = DL3
            DV = DV3
            goto 1000
         ENDIF
      ENDIF

400   CONTINUE

      DL1 = DL2
      T1 = T2
      DV1 = DV2
      F1 = F2

      DL2 = DL3
      T2 = T3
      DV2 = DV3
      F2 = F3

! END OF ITERATION LOOP

500   CONTINUE

!  OUTER ITERATION IN THE CALCULATION METHODS OF TEMPERATURE

600   CONTINUE

! START VALUES OF TEMPERATURE

      TMH = 0.5D0 * (TTR + TCE)
      CALL TSATITZ(TMH,DVMH,DLMH,PMH,1.D-6)
      IF (P .GE. PMH) THEN
         TOH = TTR + 0.9D0 * (TCE - TTR)
         CALL TSATITZ(TOH,DVOH,DLOH,POH,1.D-6)
         IF (P .LE. POH) THEN
            TS1 = TMH
            TS2 = TOH
         ELSE
            TOH2 = TCE - 1.0001D-3
            CALL TSATITZ(TOH2,DVOH2,DLOH2,POH2,1.D-6)
            IF (P .LE. POH2) THEN
               TS1 = TOH
               TS2 = TOH2
            ELSE
               TS1 = TOH2
               TS2 = TCE - 1.0001D-6
            ENDIF
         ENDIF
      ELSE
         TUH = TTR + 0.05D0 * (TCE - TTR)
         CALL TSATITZ(TUH,DVUH,DLUH,PUH,1.D-6)
         IF (P .GE. PUH) THEN
            TS1 = TUH
            TS2 = TMH
         ELSE
            TS1 = TTR + 1.D-6
            TS2 = TUH
         ENDIF
      ENDIF

! ITERATION OF TEMPERATURE

      CALL ITPEGS2(TS1,TS2,MXWPRES,P,EPS,XT,IX)

      IF (IX .EQ. 0) THEN
         TH = XT
         CALL TSATITZ(TH,DVH,DLH,PH,EPS)
         T = TH
         DL = DLH
         DV = DVH
      ELSEIF (IX .LE. 3) THEN
         TH = XT
         CALL TSATITZ(TH,DVH,DLH,PH,EPS)
         IF (DABS(PH - P) .LT. EPS) THEN
            T = TH
            DL = DLH
            DV = DVH
         ELSE
            T = -111.D0
            DV = -111.D0
            DL = -111.D0
         ENDIF
      ELSE
         T = -111.D0
         DV = -111.D0
         DL = -111.D0
      ENDIF

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL

      RETURN
      END

!--------------------------------------------------------------------------------------------
      SUBROUTINE TSATITZ(T,DV,DL,P,EPS)
!--------------------------------------------------------------------------------------------
!
!   VAPOR PRESSURE, VAPOR AND LIQUID DENSITY OF A STATE EQUATION AT CONTROLLED TEMPERATURE. 
!    (WITHOUT THE REQUEST OLD TEMPERATURE - MEETS ELSE TSATIT)
!
!   INPUT:     T         TEMPERATURE [K]
!              EPS       CONVERGENCE TOLERANCE
!
!   OUTPUT:
!
!              DV    VAPOR DENSITY [KG / M ** 3]
!              DL    SAT. LIQ. DENSITY [KG / M ** 3]
!              P     VAPOR PRESSURE [MPa]
!
!--------------------------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  DVMAX,DLMIN,DITPRES,CRMXWRES,ABLRES
      REAL(KIND=8) ::  VPEQN,DLEQN,DVEQN,calcp,Phir,calcdpdD,PDEV
      REAL(KIND=8) ::  TCE,DCE,PCE,PSOLD,DLOLD,DVOLD,PDVOLD,PMOLD
      REAL(KIND=8) ::  DVN,DLN,PDVN,PDLN,PMN,ALPADL,ALPADV,DLV,PLV
      REAL(KIND=8) ::  AAA,BBB,CCC,EEE,VZTEST,VZW,DELTAL,DELTAV
      REAL(KIND=8) ::  T1,P1,D1,ABLTEST,DVMA,DVMB,DLMA,DLMB,X,PA,PB
      REAL(KIND=8) ::  DVAT,DV1A,DV1B,PTEST,DV1,DLAT,DL1A,DL1B,DL1
      REAL(KIND=8) ::  P2,DV2A,DV2B,DL2A,DL2B,DV2,DL2,F1,F2,RES
      REAL(KIND=8) ::  P3,DV3A,DV3B,DL3A,DL3B,DV3,DL3,F3,DDV,DDL,DD
      REAL(KIND=8) ::  DPS,EPSCR,DVA,DVB,DLA,DLB,X1,X2,P2OLD
      COMMON / DEXTREM / DVMAX,DLMIN

      INTEGER I,IX,ISTART,ITCM,IEQUAL,ISTARTL
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      EXTERNAL DITPRES,CRMXWRES,ABLRES

      DV = 0.D0
      DL = 0.D0
      P = 0.D0

      ISTART = 0
      ISTARTL = 0
      ITCM = 0

!   CRITICAL POINT

         TCE = TC
         DCE = DC
         PCE = PC

!   T>TC OR T<TTR => CALCULUS NOT POSSIBLE

      IF ((T .GT. TCE ) .OR. (T .LT. TTR)) THEN
         DV = -111.D0
         DL = -111.D0
         P = -111.D0
         goto 1000
      ELSEIF(DABS(T - TCE) .LT. 0.5D0) THEN

! AT SUBCRITICAL TEMPERATURES THE CALCULATION USES SPAN BOUND METHODS

         T1 = T
         GOTO 200
      ELSE

! COMPUTATION BY Zschunke ET AL. (WITH MODIFICATIONS)

!    START VALUES OF P,DL,DV

         PSOLD = VPEQN(T)
         DLOLD = DLEQN(T)
         IF (PSOLD .LT. 0.05d0) THEN
            DVOLD = PSOLD / R / T * 1.D3
         ELSE
            DVOLD = DVEQN(T)
            IF (DVOLD .GT. DC) THEN
                  DVOLD = DC - 10.D+0
            ENDIF             
         ENDIF

!   MAXWELL   CRITERION

! FIRST CALL WITH STAR VALUES

         PDVOLD = calcp(T,DVOLD)
         PMOLD = (Phir(T,DLOLD) - Phir(T,DVOLD) + DLOG(DLOLD / DVOLD)) /     &
                 (1.D0 / DVOLD - 1.D0 / DLOLD) * R * T * 1.D-3

         DVN = 0.9999D0 * DVOLD
         DLN = 1.0001D0 * DLOLD

!  ITERATION LOOP

         DO 100 I =1,40

!     MAXWELL EVALUATION

         PDVN = calcp(T,DVN)
         PDLN = calcp(T,DLN)
         IF ((DLN / DVN) .LT. 0.0D+00) THEN
             T1 = T
             GOTO 200
         ENDIF
         PMN = (Phir(T,DLN) - Phir(T,DVN) + DLOG(DLN / DVN)) /   &
                 (1.D0 / DVN - 1.D0 / DLN) * R * T * 1.D-3

!   CHECKING CONVERGENCE CRITERION

         IF(DABS((PMN / PMOLD) - 1.D0) .LT. EPS) THEN
            IF(DABS((DLN / DLOLD) - 1.D0) .LT. EPS) THEN
               IF(DABS((DVN / DVOLD) - 1.D0) .LT. EPS) THEN
                  ABLTEST = calcdpdD(T,DLN)
                  IF (ABLTEST .LE. 0.D0) THEN
                     GOTO 200
                  ENDIF
                  ABLTEST = calcdpdD(T,DVN)
                  IF (ABLTEST .LE. 0.D0) THEN
                     GOTO 200
                  ENDIF
                  DL = DLN
                  DV = DVN
                  P = PMN
                  goto 1000
               ENDIF
            ENDIF
         ENDIF

!   CORRECTION CALCULATION

         ALPADL = - calcdpdD(T,DLN) * DLN * DLN
         IF (DABS(DVN-DVOLD) .LT. 1.D-12) THEN
             T1 = T
             GOTO 200
         ENDIF
         ALPADV = (PDVN - PDVOLD) / (1.D0 / DVN - 1.D0 / DVOLD)

         DLV = 1.D0 / DLN - 1.D0 / DVN
         PLV = PDLN - PDVN
         AAA = 0.5D0 * ALPADL * (ALPADL - ALPADV)
         BBB = ALPADL * (PLV - ALPADV * DLV)
         CCC = ALPADV * DLV * (PMN - PDLN) + 0.5D0 * PLV * PLV
         EEE = 0.25D0 * BBB / AAA * BBB / AAA - CCC / AAA

!   SHIFT IF NUMERICAL PROBLEMS WITH SPAN METHOD

         IF (EEE .LE. 1.D-10) THEN
            T1 = T
            GOTO 200
         ENDIF

! TEST OF SIGN

         VZTEST = (ALPADL - ALPADV) / ALPADV
         IF (VZTEST .GE. 0.D0) THEN
            VZW = 1.D0
         ELSE
            VZW = -1.D0
         ENDIF

!   NEW CORRECTION VALUE

         DELTAL = -0.5D0 * BBB / AAA + VZW * DSQRT(EEE)
         DELTAV = (PLV + ALPADL * DELTAL) / ALPADV
         PDVOLD = PDVN
         DVOLD = DVN
         DLOLD = DLN
         PMOLD = PMN
         DVN = 1.D0 / (1.D0 / DVOLD + DELTAV)
         DLN=  1.D0 / (1.D0 / DLOLD + DELTAL)

!   END OF ITERATION LOOP

100      CONTINUE

      ENDIF

!   SPAN COMPUTATION

200   CONTINUE

! 1. CASE: FAR FROM TEMPERATURE 

      IF (T .LT. (TCE - 0.25D0)) THEN

!   STAR VALUE OF PRESSURE WITH VAPOR PRESSURE EQUATION

         DVMAX = DCE
         DLMIN = DCE

         P1 = VPEQN(T)

      ELSE

! 2. CASE: NEAR CRITICAL TEMPERATURE 

!  MAXIMUM AND MINIMUM DENSITY OF MAXWELL LOOP

         IF ((TCE - T1) .LE. 1.D-6) THEN
            T1 = TCE - 1.D-6
            ITCM = 1
         ENDIF

         D1 = DCE * 0.99999999D0
         ABLTEST = calcdpdD(T1,D1)
         IF (ABLTEST .LE. 0.D0) THEN
            DVMA = D1
            DVMB = D1
210         CONTINUE
            DVMA = DVMA - 1.D0
            ABLTEST = calcdpdD(T1,DVMA)
            IF (ABLTEST .LE. 0.D0) THEN
               DVMB = DVMA
               GOTO 210
            ENDIF
            CALL ITPEGS(DVMA,DVMB,ABLRES,T1,1.D-9,X,IX)
            DVMAX = X

            DLMA = D1
            DLMB = D1
220         CONTINUE
            DLMB = DLMB + 1.D0
            ABLTEST = calcdpdD(T1,DLMB)
            IF (ABLTEST .LE. 0.D0) THEN
               DLMA = DLMB
               GOTO 220
            ENDIF
            CALL ITPEGS(DLMA,DLMB,ABLRES,T1,1.D-9,X,IX)
            DLMIN = X
         ELSE
            D1 = DCE * 0.6D0
            DVMA = D1
            DVMB = D1
230         CONTINUE
            DVMB = DVMB + 1.D0
            ABLTEST = calcdpdD(T1,DVMB)
            IF (ABLTEST .GE. 0.D0) THEN
               DVMA = DVMB
               GOTO 230
            ENDIF
            CALL ITPEGS(DVMA,DVMB,ABLRES,T1,1.D-9,X,IX)
            DVMAX = X

            DLMA = DVMB
            DLMB = DVMB
240         CONTINUE
            DLMB = DLMB + 1.D0
            ABLTEST = calcdpdD(T1,DLMB)
            IF (ABLTEST .LE. 0.D0) THEN
               DLMA = DLMB
               GOTO 240
            ENDIF
            CALL ITPEGS(DLMA,DLMB,ABLRES,T1,1.D-9,X,IX)
            DLMIN = X

         ENDIF

         IF (T1 .GE. (TCE - 1.1D-3)) THEN
            PA = calcp(T1,DLMIN)
            PB = calcp(T1,DVMAX)
            GOTO 800
         ENDIF    

!  DSTART VALUE FOR VAPOR PRESSURE FROM THE FUNDAMENTAL EQ. WITH D = Dc

         P1 = calcp(T,DCE)

      ENDIF

!   START VALUE FOR VAPOR DENSITY

      DVAT = DVEQN(T)
      DV1A = DVAT * 0.99D0
      DV1B = DVAT * 1.01D0

      IF (DV1B .GT. DVMAX) THEN
         DV1B = DVMAX
         IEQUAL = 1
      ELSE
         IEQUAL = 0
      ENDIF

300   CONTINUE

      PTEST = calcp(T,DV1B)
      IF (IEQUAL .EQ. 1) THEN
          P1 = PTEST * 0.99999D+0
          IEQUAL = 0
      ENDIF
      IF (PTEST .LT. P1) THEN
         PTEST = calcp(T,DVMAX)

         IF (PTEST .LT. 0.0D+0) THEN
               PTEST = P1 * 1.00001D+0
         ENDIF
         IF (PTEST .LT. P1) THEN
            P1 = PTEST * 0.99999D0
            GOTO 300
         ELSE
            DV1B = DVMAX
         ENDIF
      ENDIF

      IF (DV1A .GE. DV1B) THEN
         DV1A = DV1B * 0.95D0
      ENDIF

310   CONTINUE

!    ITERATION OF VAPOR DENSITY

      CALL ITPEG(DV1A,DV1B,DITPRES,T,P1,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV1 = X
         ELSE
            P1 = P1 * 0.9995D0
            DV1A = DV1A * 0.96D0
            DV1B = DV1B * 0.99D0
            ISTART = ISTART + 1
            IF (ISTART .LE. 200) THEN
               GOTO 310
            ELSE
               P = -111.D0
               DL = -111.D0
               DV = -111.D0
               goto 1000
            ENDIF
         ENDIF
      ELSE 
         P1 = P1 * 0.9995D0
         DV1A = DVAT * 0.95D0
         ISTART = ISTART + 1
         IF (ISTART .LE. 200) THEN
            GOTO 310
         ELSE
            P = -111.D0
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ENDIF

!   START VALUE FOR SAT. LIQUID

      IF (ISTARTL .EQ. 0) THEN
          DLAT = DLEQN(T)
          DL1A = DLAT * 0.99D0
          DL1B = DLAT * 1.01D0
          ISTARTL = 1
      ENDIF

      IF (DL1A .LT. DLMIN) THEN
         DL1A = DLMIN
         IEQUAL = 1
      ELSE
         IEQUAL = 0
      ENDIF

320   CONTINUE

      PTEST = calcp(T,DL1A)
      PDEV = PTEST / P1
      IF ((ABS(PDEV) .GT. 10.D+0) .OR. (IEQUAL .EQ. 1)) THEN
            IF (PTEST .GT. 0.0D+0) THEN
                P1 = PTEST * 1.00001D+0
              ELSE
                  P1 = PTEST * 0.99999D+0
              ENDIF
            IEQUAL = 0
      ENDIF
      IF (PTEST .GT. P1) THEN
         PTEST = calcp(T,DLMIN)
         IF (PTEST .GT. P1) THEN
            P1 = P1 * 1.00001D0
            GOTO 320
         ELSE
            DL1A = DLMIN
         ENDIF
      ENDIF

      IF (DL1A .GE. DL1B) THEN
         DL1B = DL1A * 1.05D0
      ENDIF

!    ITERATION OF SAT. LIQUID DENSITY

      CALL ITPEG(DL1A,DL1B,DITPRES,T,P1,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL1 = X
         ELSE
            P1 = P1 * 1.0005D0
            DL1B = DL1B * 1.01D0
            ISTART = ISTART + 1
            IF (ISTART .LE. 200) THEN
               GOTO 310
            ELSE
               P = -111.D0
               DL = -111.D0
               DV = -111.D0
               goto 1000
            ENDIF
         ENDIF
      ELSE 
         P1 = P1 * 1.0005D0
         DL1B = DL1B * 1.02D0
         ISTART = ISTART + 1
         IF (ISTART .LE. 200) THEN
            GOTO 310
         ELSE
            P = -111.D0
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ENDIF

!   CALCULATION OF NEW PRESSURE

      P2 = R * T * (Phir(T,DL1) - Phir(T,DV1) + DLOG(DL1 / DV1)) /     &
           (1.D0 / DV1 - 1.D0 / DL1) * 1.D-3

      IF (P2 .LE. 0.D0) THEN
         P2 = VPEQN(TTR)
      ELSEIF (P2 .GT. PCE) THEN
         P2 = PCE - 1.D-12
      ENDIF

      IF (P2 .GT. P1) THEN
         DV2A = DV1 * 0.99D+0
         DV2B = DV1 * 1.02D0
         DL2A = DL1 * 0.99D+0
         DL2B = DL1 * 1.01D0
      ELSE
         DV2A = DV1 * 0.98D0
         DV2B = DV1 * 1.00001D+0
         DL2A = DL1 * 0.99D0
         DL2B = DL1 * 1.00001D+0
      ENDIF

      IF (DL2A .LT. DLMIN) THEN
         DL2A = DLMIN
      ENDIF

      IF (DV2B .GT. DVMAX) THEN
         DV2B = DVMAX
      ENDIF

400   CONTINUE

!    ITERATION OF NEW VAPOR DENSITY

      CALL ITPEG(DV2A,DV2B,DITPRES,T,P2,1.D-9,X1,IX)
      IF ((IX .EQ. 0) .AND. (X1 .LT. DCE)) THEN
         ABLTEST = calcdpdD(T,X1)
         IF (ABLTEST .GT. 0.D0) THEN
            DV2 = X1
         ELSE
            P2 = P2 * 0.999D0
            GOTO 400
         ENDIF
      ELSE 
         P2 = P2 * 0.999D0
         GOTO 400
      ENDIF

!    ITERATION OF NEW SAT. LIQUID DENSITY

      CALL ITPEG(DL2A,DL2B,DITPRES,T,P2,1.D-9,X2,IX)
      IF ((IX .EQ. 0) .AND. (X2 .GT. DCE)) THEN
         ABLTEST = calcdpdD(T,X2)
         IF (ABLTEST .GT. 0.D0) THEN
            DL2 = X2
         ELSE
            P2OLD = P2
            P2 = P2 * 1.001D0
            GOTO 400
         ENDIF
      ELSE 
         P2 = P2 * 1.1D0
         GOTO 400
      ENDIF

!   EVALUATION OF MAXWELL-CRITERION

      CALL MXWRES(P1,T,RES,DL1,DV1)
      F1 = RES
      CALL MXWRES(P2,T,RES,DL2,DV2)
      F2 = RES

      IF (DABS(F1) .LT. 1.D-15) THEN
         DV = DV1
         DL = DL1
         P = P1
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         DV = DV2
         DL = DL2
         P = P2
         goto 1000
      ENDIF

!    ITERATIONS   LOOP

      DO 700 I = 1,80

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            DV = DV2
            DL = DL2
            P = P2
            goto 1000
         ELSE
            DV = DV1
            DL = DL1
            P = P1
            goto 1000
         ENDIF
      ENDIF

!   CALCULAITON OF NEW PRESSURE

      P3 = P2 + (P1 - P2) * F2 / (F2 - F1)
      IF (P3 .LT. 0.0D+0) THEN
            P3 = PTR
      ENDIF
      IF (P3 .GT. P2) THEN
         DV3A = DV2 * 0.98999D+0
         DV3B = DV2 * (1.03D0 + dfloat(I - 1) * 0.001D+0)
!         DV3B = DV2 * 1.02D0
         DL3A = DL2 * 0.98999D+0
         DL3B = DL2 * (1.02D0 + dfloat(I - 1) * 0.001D+0)
      ELSE
         DV3A = DV2 * (0.98D0 - dfloat(I - 1) * 0.001D+0)
         DV3B = DV2 * 1.00001D+0
         DL3A = DL2 * (0.99D0 - dfloat(I - 1) * 0.001D+0)
         DL3B = DL2 * 1.00001D+0
      ENDIF

      IF (DV3B .GT. DVMAX) THEN
         DV3B = DVMAX
      ENDIF

      IF (DL3A .LT. DLMIN) THEN
         DL3A = DLMIN
      ENDIF

500   CONTINUE

!    ITERATION OF NEW VAPOR DENSITY

      CALL ITPEG(DV3A,DV3B,DITPRES,T,P3,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV3 = X
         ELSE
            P3 = P3 * 0.9995D0
            GOTO 500
         ENDIF
      ELSE 
         P3 = P3 * 0.9995D0
         GOTO 500
      ENDIF

!    ITERATION OF NEW SAT. LIQ. DENSITY

      CALL ITPEG(DL3A,DL3B,DITPRES,T,P3,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL3 = X
         ELSE
            P3 = P3 * 1.0005D0
            GOTO 500
         ENDIF
      ELSE 
         P3 = P3 * 1.0005D0
           IF (P3 .GT. (1.1D+0 * PCE)) THEN
               P3 = PCE
           ENDIF
         GOTO 500
      ENDIF

      IF (DV3 .LT. 0.D0) THEN
         DV3 = DVTR
      ENDIF

!   Evaluation of MAXWELL criterion

      CALL MXWRES(P3,T,RES,DL3,DV3)
      F3 = RES

      IF (DABS(F3) .LT. 1.D-13) THEN
         DV = DV3
         DL = DL3
         P = P3
         goto 1000
      ENDIF

! VERIFYING CONVERGENCE CRITERION

      DDL = DABS((DL3 - DL2) / DL3)
      DDV = DABS((DV3 - DV2) / DV3)
      DD = DDL * DDL + DDV * DDV

      IF(DD .GT. EPS * EPS) GOTO 600

      DPS = DABS((P3 - P2) / P3)

      IF(DPS .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F3)) THEN
            DV = DV2
            DL = DL2
            P = P2
            goto 1000
         ELSE
            DV = DV3
            DL = DL3
            P = P3
            goto 1000
         ENDIF
      ENDIF

600   CONTINUE

      DL1 = DL2
      DV1 = DV2
      P1 = P2
      F1 = F2

      DL2 = DL3
      DV2 = DV3
      P2 = P3
      F2 = F3

!   END OF ITERATION LOOP

700   CONTINUE

      P = -111.D0
      DL = -111.D0
      DV = -111.D0
      goto 1000

!   ITERATION OF TEMPERATURE 1.1D-3 > TC - T > 1.D-6

800   CONTINUE

      EPSCR = EPS * 1.D-6
      IF (EPSCR .LT. 1.D-15) THEN
         EPSCR = 1.D-15
      ENDIF

!    ITERATION OF VAPOR PRESSURE

      CALL ITPEGS(PA,PB,CRMXWRES,T1,EPSCR,X,IX)

      IF ((X .LT. PA) .OR. (X .GT. PB)) THEN
         T1 = T1 * 0.99999999D0
         ITCM = 1
         GOTO 200
      ENDIF

      P1 = X

      DVB = DVMAX
      DVA = DVB * 0.95D0
810   CONTINUE
      PTEST = calcp(T1,DVA)
      IF (PTEST .GT. P1) THEN
         DVA = DVA * 0.98D0
         GOTO 810
      ENDIF

!    ITERATION OF VAPOR DENSITY

      CALL ITPEG(DVA,DVB,DITPRES,T1,P1,1.D-9,X,IX)
      IF ((IX .LE. 3) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         IF (ABLTEST .LE. 0.D0) THEN
            X = X - 5.D-8
            ABLTEST = calcdpdD(T1,X)
         ENDIF
         PTEST = DABS(calcp(T1,X) - P1)
         IF ((ABLTEST .GT. 0.D0) .AND. (PTEST .LT. EPS)) THEN
            DV1 = X
         ELSE
            P = -111.D0
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ELSE 
         P = -111.D0
         DL = -111.D0
         DV = -111.D0
         goto 1000
      ENDIF

      DLA = DLMIN
      DLB = DLA * 1.02D0
820   CONTINUE
      PTEST = calcp(T1,DLB)
      IF (PTEST .LT. P1) THEN
         DLB = DLB * 1.02D0
         GOTO 820
      ENDIF

!  ITERATION OF SAT. LIQUID DENSITY

      CALL ITPEG(DLA,DLB,DITPRES,T1,P1,1.D-9,X,IX)
      IF ((IX .LE. 3) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         IF (ABLTEST .LE. 0.D0) THEN
            X = X + 5.D-8
            ABLTEST = calcdpdD(T1,X)
         ENDIF
         PTEST = DABS(calcp(T1,X) - P1)
         IF ((ABLTEST .GT. 0.D0) .AND. (PTEST .LT. EPS)) THEN
            DL1 = X
         ELSE
            P = -111.D0
            DL = -111.D0
            DV = -111.D0
            goto 1000
         ENDIF
      ELSE 
         P = -111.D0
         DL = -111.D0
         DV = -111.D0
         goto 1000
      ENDIF

      IF (ITCM .EQ. 0) THEN
         P = P1
         DV = DV1
         DL = DL1
         goto 1000
      ELSE
         X = (T - T1) / (TCE - T1)
         P = P1 + X * (PCE - P1)
         DV = DV1 + X * (DCE - DV1)
         DL = DL1 + X * (DCE - DL1)
         goto 1000
      ENDIF

1000 continue

      RETURN
      END

!---------------------------------------------------------------------
      SUBROUTINE DLSATIT(T,DV,DL,P,EPS)
!---------------------------------------------------------------------
      IMPLICIT NONE
      REAL(KIND=8) ::  T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  DITPRES,TIPDRES,MXWDLRES,VPEQN,DVEQN,DLEQN
      REAL(KIND=8) ::  TDLIT,calcp,calcdpdD,TCE,PCE,DCE,T1,TG,DVG,DLG,PG
      REAL(KIND=8) ::  X,P1,DVAT,DV1A,DV1B,DV1M,ABLTEST,DV1,RES,T2
      REAL(KIND=8) ::  P2,DV2A,DV2B,DV2,T2A,T2B,F1,F2,DV3A,DV3B,DV3
      REAL(KIND=8) ::  T3,P3,F3,G,DTS,DPS,DDV,TS1,TS2,TH,DVH,DLH,PH
      REAL(KIND=8) ::  DL1,DL2,DL3,XT,TSOLD,PSOLD,DLOLD,DVOLD
      INTEGER I,IX
      INTEGER ISUBAKT,ISUBOLD

      EXTERNAL DITPRES,TIPDRES,MXWDLRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TSOLD,PSOLD,DLOLD,DVOLD
      SAVE ISUBOLD
      DATA TSOLD / -1.D+0 /
        DATA PSOLD / -1.D+0 /
        DATA DLOLD / -1.D+0 /
      DATA DVOLD / -1.D+0 /
      DATA ISUBOLD /0/

      IF ((DABS(DL-DLOLD).LT.1.D-8) .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TSOLD
         P = PSOLD
         DV = DVOLD
         goto 1000
      END IF

      P = 0.D0
      DV = 0.D0
!      IF ((DLTR .LT. 1.D0) .OR. (DVTR .LT. 1.D-12)          &
!           .OR. (PTR .LT. 1.D-14)) THEN
!         DLTR = DLEQN(TTR)
!         DVTR = DVEQN(TTR)
!         PTR = VPEQN(TTR)
!      END IF

!   CRITICAL POINT

         TCE = TC
         DCE = DC
         PCE = PC

!    IF DL<DC OR DL>DLTR => CALCULATION NOT POSSIBLE

!      IF ((DL .LT. DCE) .OR. (DL .GT. DLTR)) THEN      ! Este teste limita Tsatod a T >= 281.15K
      IF ((DL .LT. DCE)) THEN      ! revisao
         P = -11127.D0
         DV = -11127.D0
         T = -11127.D0
!         T = DLTR
         goto 1000
      ENDIF

!   TEMPERATURE START VALUE: LIQ. TEMP. OF DL

      T1 = TDLIT(DL)
      IF (T1 .LT. TTR) THEN
         T1 = TTR
      ENDIF

! AT SUBCRITICAL DENSITY, AN OUTER ITERATION WITH COMPUTATION OF GIVEN TEMP.

      IF (DL .LE. (1.2D0 * DCE)) THEN
         TG = TCE - 1.D-6
         CALL TSATIT(TG,DVG,DLG,PG,EPS)
         IF (DL .LE. DLG) THEN
            X = (DL - DCE) / (DLG - DCE)
            T = TCE - X * (TCE - TG)
            P = PCE - X * (PCE - PG)
            DV = DCE - X * (DCE - DVG)
            goto 1000
         ELSE
            GOTO 400
         ENDIF
      ENDIF

100   CONTINUE

!   START VALUE OF PRESSURE

      P1 = calcp(T1,DL)

      IF (P1 .LT. 0.D0) THEN
         T1 = T1 * 1.025D0
         GOTO 100 
      ENDIF

!   start values for vapor density

      DVAT = DVEQN(T1)
      DV1A = DVAT * 0.98D0
      DV1B = DVAT * 1.02D0

! ITERATION OF VAPOR DENSITY
      CALL ITPEG(DV1A,DV1B,DITPRES,T1,P1,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         DV1M = (DV1A + DV1B) * 0.5D0
         IF ((ABLTEST .GT. 0.D0) .AND. (X .LT. (0.9D+0 * DCE))       &
             .AND. (T1 .LE. (0.95D0 * TCE))                      &
             .AND. (X .LE. (1.D1 * DV1M))) THEN
            DV1 = X
         ELSEIF ((ABLTEST .GT. 0.D0) .AND. (X .LT. DCE) .AND.       &
                 (T1 .GT. (0.95D0 * TCE))) THEN
            DV1 = X
         ELSE
            GOTO 400
         ENDIF
      ELSE 
         GOTO 400
      ENDIF

! EVALUATION OF MAXWELL CRITERION

      CALL MXWRES(P1,T1,RES,DL,DV1)
      F1 = RES

!  DETERMINATION OF NEW VALUES FOR TEMPERATURE, PRESSURE AND VAPOR DENSITY
!   DEPENDING ON THE LOCATION OF THE START VALUES

      IF (F1 .GT. 0.D0) THEN

! ESTIMATING A NEW TEMPERATURE

         T2 = T1 * 0.99D0
         IF (T2 .LT. TTR) THEN
            T2 = TTR
         ENDIF

200      CONTINUE

!   NEW VALUE OF PRESSURE

         P2 = calcp(T2,DL)

         IF (P2 .LE. 0.D0) THEN
            T2 = T2 * 1.001D0
              GOTO 200
         ENDIF 

         ABLTEST = calcdpdD(T2,DL)

         IF (ABLTEST .LE. 0.D0) THEN
210         CONTINUE            
            T2 = T2 * 1.005D0
            ABLTEST = calcdpdD(T2,DL)
            IF (ABLTEST .LE. 0.D0) THEN
               GOTO 210
            ELSE
               P2 = calcp(T2,DL)
            ENDIF
         ENDIF

220      CONTINUE

         DVAT = DVEQN(T2)
         DV2A = DVAT * 0.98D0
         DV2B = DVAT * 1.02D0

!  ITERATION FOR NEW VALUE OF VAPOR DENSITY

         CALL ITPEG(DV2A,DV2B,DITPRES,T2,P2,1.D-9,X,IX)
         IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
            ABLTEST = calcdpdD(T2,X)
            IF ((ABLTEST .GT. 0.D0) .AND. (X .LT. (0.9D+0 * DCE))     &
                   .AND. (T2 .LE. (0.95D0 * TCE))) THEN
               DV2 = X
            ELSEIF ((ABLTEST .GT. 0.D0) .AND. (X .LT. DCE)           &
                   .AND. (T2 .GT. (0.95D0 * TCE))) THEN
               DV2 = X
            ELSE
               GOTO 400
            ENDIF
         ELSE
            GOTO 400
         ENDIF

!   EVALUATION OF THE MAXWELL CRITERION

         CALL MXWRES(P2,T2,RES,DL,DV2)
         F2 = RES
         IF (F2 .GT. 0.D0) THEN
            P2 = P2 * 0.8D0
            T2A = T1 * 0.95D0
            T2B = T1

! ITERATION FOR NEW TEMPERATURE VALUE

            CALL ITPEG(T2A,T2B,TIPDRES,P2,DL,1.D-9,X,IX)
            IF (IX .EQ. 0) THEN
               T2 = X
            ELSE
               GOTO 400
            ENDIF
              GOTO 220
         ENDIF          
      ELSE

!   CALCULATING NEW VALUE OF PRESSURE

         P2 = P1 * 1.1D0

230      CONTINUE

         IF (P2 .GE. PCE) THEN
            P2 = PCE - 1.D-12
         ENDIF
         T2A = T1
         T2B = T1 * 1.05D0

! ITERATION FOR NEW TEMPERATURE VALUE

         CALL ITPEG(T2A,T2B,TIPDRES,P2,DL,1.D-9,X,IX)
         IF (IX .EQ. 0) THEN
            T2 = X
         ELSE
            GOTO 400
         ENDIF

         DV2A = DV1
         DV2B = DV1 * 1.05D0

!    ITERATION FOR NEW VALUE OF VAPOR DENSITY

         CALL ITPEG(DV2A,DV2B,DITPRES,T2,P2,1.D-9,X,IX)
         IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
            ABLTEST = calcdpdD(T2,X)
            IF (ABLTEST .GT. 0.D0) THEN
               DV2 = X
            ELSE
               GOTO 400
            ENDIF
         ELSE
            GOTO 400
         ENDIF

!   EVALUATION OF MAXWELL CRITERION

         CALL MXWRES(P2,T2,RES,DL,DV2)
         F2 = RES
         IF (F2 .LT. 0.D0) THEN
            P2 = P2 * 1.02D0
              GOTO 230
         ENDIF
      ENDIF

      IF (DABS(F1) .LT. 1.D-15) THEN
         T = T1
         DV = DV1
         P = P1
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         T = T2
         DV = DV2
         P = P2
         goto 1000
      ENDIF

!    ITERATION
      DO 300 I = 1,60

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            T = T2
            DV = DV2
            P = P2
            goto 1000
         ELSE
            T = T1
            DV = DV1
            P = P1
            goto 1000
         ENDIF
      ENDIF

!   NEW TEMPERATURE

      T3 = T2 - F2 / (F2 - F1) * (T2 - T1)

      P3 = calcp(T3,DL)
      DVAT = DVEQN(T3)
      DV3A = DVAT * 0.98D0
      DV3B = DVAT * 1.02D0

!    ITERATION FOR NEW VALUE OF VAPOR DENSITY

      CALL ITPEG(DV3A,DV3B,DITPRES,T3,P3,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .LT. DCE)) THEN
         ABLTEST = calcdpdD(T3,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DV3 = X
         ELSE
            GOTO 400
         ENDIF
      ELSE 
         GOTO 400
      ENDIF

!   EVALLUATION OF MAXWELL CRITERION

      CALL MXWRES(P3,T3,RES,DL,DV3)
      F3 = RES

      IF ((F2 * F3) .LT. 0.D0) THEN
         T1 = T2
         T2 = T3
         P1 = P2
         P2 = P3
         DV1 = DV2
         DV2 = DV3
         F1 = F2
         F2 = F3
      ELSEIF ((F2 * F3) .GT. 0.D0) THEN
         G = F2 / (F2 + F3)
         T2 = T3
         P2 = P3
         DV2 = DV3
         F1 = G * F1
         F2 = F3
      ENDIF

      IF (DABS(F3) .LT. 1.D-15) THEN
         T = T3
         DV = DV3
         P = P3
         goto 1000
      ENDIF

!   CHECKING THE CONVERGENCE CRITERION

      DDV = DABS((DV2 - DV1) / DV1)
      DPS = DABS((P2 - P1) / P2)
      DTS = DABS((T2 - T1) / T2)

      IF ((DTS .LT. EPS) .AND. (DPS .LT. EPS) .AND. (DDV .LT. EPS)) THEN
         IF (DABS(F1) .LT. DABS(F2)) THEN
            T = T1
            DV = DV1
            P = P1
            goto 1000
         ELSE
            T = T2
            DV = DV2
            P = P2
            goto 1000
         ENDIF
      ENDIF

300   CONTINUE

!  OUTER ITERATION ON THE CALCULATION METHODS OF TEMPERATURE

400   CONTINUE

!   START VALUE OF TEMPERATURE

      T1 = TCE * 0.95D0
      CALL TSATITZ(T1,DV,DL1,P,EPS)
      IF (DL .LE. DL1) THEN
         TS1 = T1
         TS2 = TCE - 1.001D-6
      ELSE
         T2 = TCE * 0.65D0
      IF (T2 .LT. TTR) THEN
          T2 = TTR
      ENDIF
         CALL TSATITZ(T2,DV,DL2,P,EPS)
         IF (DL .LE. DL2) THEN
            TS1 = T2
            TS2 = T1
         ELSE
            T3 = TTR * 1.2D0
            CALL TSATITZ(T3,DV,DL3,P,EPS)
            IF (DL .LE. DL3) THEN
               TS1 = T3
               TS2 = T2
            ELSE
               TS1 = TTR + 1.D-6
               TS2 = T3
            ENDIF
         ENDIF
      ENDIF

! ITERATION OF TEMPERATURE
      CALL ITPEGS2(TS1,TS2,MXWDLRES,DL,EPS,XT,IX)

      IF (IX .EQ. 0) THEN
         TH = XT
         CALL TSATITZ(TH,DVH,DLH,PH,EPS)
         T = TH
         DV = DVH
         P = PH
      ELSEIF (IX .LE. 3) THEN
         TH = XT
         CALL TSATITZ(TH,DVH,DLH,PH,EPS)
         IF (DABS(DLH - DL) .LT. (EPS * 1.D1)) THEN
            T = TH
            DV = DVH
            P = PH
         ELSE
            T = -111.D0
            P = -111.D0
            DV = -111.D0
         ENDIF
      ELSE
         T = -11128.D0
         P = -11128.D0
         DV = -11128.D0
      ENDIF

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL

      RETURN
      END


! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function TDLIT(D)
! ---------------------------------------------------------------------------------------
!
!  ITERATION DER TEMPERATUR FROM A SAT. DENSITY EQUATION
!
!  INPUT:     D         SAT. LIQ. DENSITY [KG / M ** 3] 
!
!  OUTPUT:    TDLIT     TEMPERATURE [K] 
!
! ---------------------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  D,DH,TH,DLEQN,DLHRES,X,T1,T2
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      INTEGER IX
      EXTERNAL DLHRES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

!      IF ((D .GT. DLTR) .OR. (D .LT. DC)) THEN       ! Este teste limita Tsatod a T >= 281.15K
      IF ((D .LT. DC)) THEN       ! *********************
         TDLIT = -111.D0
         goto 1000
      ENDIF

!   START VALUE

      TH = TTR + 0.5D0 * (TC - TTR)
      DH = DLEQN(TH)

      IF (D .GT. DH) THEN
         T1 = TTR
         T2 = TH
      ELSE
         T1 = TH
         T2 = TC
      ENDIF

!  ITERATION OF TEMPERATUR
      CALL ITPEGS(T1,T2,DLHRES,D,1.D-6,X,IX)

      IF (IX .LE. 3) THEN
         TDLIT = X
      ELSE
         TDLIT = -111.D0
      ENDIF

1000 continue

      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE DVSATIT(T,DV,DL,P,EPS)
!----------------------------------------------------------------------
      IMPLICIT NONE
      REAL(KIND=8) ::  T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  DITPRES,MXWDVRES,VPEQN,DVEQN,DLEQN,calcp,calcdpdD
      REAL(KIND=8) ::  Phir,TDVIT,TCE,DCE,PCE,T1,TG,DVG,DLG,PG,P1,DLAT
      REAL(KIND=8) ::  DL1A,DL1B,X,ABLTEST,DL1,T2,P2,DL2A,DL2B,DL2,RES
      REAL(KIND=8) ::  F1,F2,T3,P3,DL3A,DL3B,DL3,F3,DPS,DTS,DDL,TMH,DVMH
      REAL(KIND=8) ::  DLMH,PMH,TOH,DVOH,DLOH,POH,TOH2,DVOH2,DLOH2,POH2
      REAL(KIND=8) ::  TUH,DVUH,DLUH,PUH,TS1,TS2,XT,TH,DVH,DLH,PH
      REAL(KIND=8) ::  TSOLD,PSOLD,DVOLD,DLOLD
      INTEGER I,IX,ISUBAKT,ISUBOLD

      EXTERNAL DITPRES,MXWDVRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TSOLD,PSOLD,DLOLD,DVOLD,ISUBOLD
      DATA TSOLD / -1.D+0 /
        DATA PSOLD / -1.D+0 /
        DATA DLOLD / -1.D+0 /
      DATA DVOLD / -1.D+0 /
      DATA ISUBOLD /0/

      IF ((DABS(DV-DVOLD).LT.1.D-8) .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TSOLD
         P = PSOLD
         DL = DLOLD
         goto 1000
      END IF

      T = 0.D0
      P = 0.D0
      DL = 0.D0

      IF ((DLTR .LT. 1.D0) .OR. (DVTR .LT. 1.D-9)       &
           .OR. (PTR .LT. 1.D-12)) THEN
         DLTR = DLEQN(TTR)
         DVTR = DVEQN(TTR)
         PTR = VPEQN(TTR)
      END IF

!    CRITICAL POINT

         TCE = TC
         DCE = DC
         PCE = PC

!    IF D > DC OR D < DVTR => CALCULATION NOT POSSIBLE
      IF ((DV .GT. DCE) .OR. (DV .LT. DVTR)) THEN
         P = -111.D0
         T = -111.D0
         DL = -111.D0
         goto 1000
      ENDIF

!    A START VALUE FROM SAT. TEMPERATURE FUNCTION OF DV

      T1 = TDVIT(DV)

      IF (T1 .LT. TTR) THEN
         T1 = TTR
      ENDIF

!    AT SUBCRITICAL PRESSURE AN OUTER ITERATION
!     WITH THE COMPUTATION OF GIVEN TEMPERATURES

      IF (DV .GE. (0.8D0 * DCE)) THEN
         TG = TCE - 1.D-6
         CALL TSATIT(TG,DVG,DLG,PG,EPS)
         IF (DV .GE. DVG) THEN
            X = (DV - DVG) / (DCE - DVG)
            T = TG + X * (TCE - TG)
            P = PG + X * (PCE - PG)
            DL = DLG - X * (DLG - DCE)
            goto 1000
         ELSE
            GOTO 300
         ENDIF
      ENDIF

      P1 = calcp(T1,DV)

      IF (P1 .LT. PTR) THEN
         P1 = PTR
      ENDIF

!   START VALUE OF SAT. LIQ. DENSITY

      DLAT = DLEQN(T1)
      DL1A = DLAT * 0.99D0
      DL1B = DLAT * 1.01D0

! ITERATION SAT. LIQ. DENSITY

      CALL ITPEG(DL1A,DL1B,DITPRES,T1,P1,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T1,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL1 = X
         ELSE
            GOTO 300
         ENDIF
      ELSE 
         GOTO 300
      ENDIF

!   CALCULATING A NEW TEMPERATURE

      T2 = P1 / R * (1.D0 / DV - 1.D0 / DL1) /               &
           (Phir(T1,DL1) - Phir(T1,DV) + DLOG(DL1 / DV)) * 1.D3

      IF (T2 .LE. TTR) THEN
         T2 = TTR + 1.D-6
      ELSEIF (T2 .GT. TCE) THEN
         T2 = TCE - 1.D-6
      ENDIF

!   CALCULATING A NEW PRESSURE

      P2 = calcp(T2,DV)

      IF (P2 .GT. P1) THEN
         DL2A = DL1
         DL2B = DL1 * 1.01D0
      ELSE
         DL2A = DL1 * 0.99D0
         DL2B = DL1
      ENDIF

!   ITERATION OF NEW SAT. LIQ. DENSITY

      CALL ITPEG(DL2A,DL2B,DITPRES,T2,P2,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T2,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL2 = X
         ELSE
            GOTO 300
         ENDIF
      ELSE 
         GOTO 300
      ENDIF

!    EVALUATION OF MAXWELL CRITERION

      CALL MXWRES(P1,T1,RES,DL1,DV)
      F1 = RES
      CALL MXWRES(P2,T2,RES,DL2,DV)
      F2 = RES

      IF ((F1 .LT. 0.D0) .AND. (F2 .LT. 0.D0)) THEN
         GOTO 300
      ELSEIF ((F1 .GT. 0.D0) .AND. (F2 .GT. 0.D0)) THEN
         GOTO 300
      ENDIF

      IF (DABS(F1) .LT. 1.D-15) THEN
         T = T1
         P = P1
         DL = DL1
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         T = T2
         P = P2
         DL = DL2
         goto 1000
      ENDIF

      DO 200 I = 1,40

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            T = T2
            P = P2
            DL = DL2
            goto 1000
         ELSE
            T = T1
            P = P1
            DL = DL1
            goto 1000
         ENDIF
      ENDIF

! CALCULATING A NEW TEMPERATURE

      T3 = T2 + (T1 - T2) * F2 / (F2 - F1)

      IF (T3 .LE. TTR) THEN
         T3 = TTR
         P3 = PTR
         DL3A = DLTR * 0.99D0
         DL3B = DLTR * 1.01D0
         GOTO 100
      ENDIF

!    CALCULATING A NEW PRESSURE

      P3 = calcp(T3,DV)

      IF (P3 .GT. P2) THEN
         DL3A = DL2
         DL3B = DL2 * 1.01D0
      ELSE
         DL3A = DL2 * 0.99D0
         DL3B = DL2
      ENDIF

100   CONTINUE

! ITERATION OF NEW SAT. LIQ. DENSITY

      CALL ITPEG(DL3A,DL3B,DITPRES,T3,P3,1.D-9,X,IX)
      IF ((IX .EQ. 0) .AND. (X .GT. DCE)) THEN
         ABLTEST = calcdpdD(T3,X)
         IF (ABLTEST .GT. 0.D0) THEN
            DL3 = X
         ELSE
            GOTO 300
         ENDIF
      ELSE 
         GOTO 300
      ENDIF

! EVALUATION OF MAXWELL CRITERION

      CALL MXWRES(P3,T3,RES,DL3,DV)
      F3 = RES

      IF (DABS(F3) .LT. 1.D-15) THEN
         T = T3
         DL = DL3
         P = P3
         goto 1000
      ENDIF

!   CHECKING THE CONVERGENCE CRITERION

      DDL = DABS((DL3 - DL2) / DL3)
      DPS = DABS((P3 - P2) / P3)
      DTS = DABS((T3 - T2) / T3)

      IF((DTS .LT. EPS) .AND. (DPS .LT. EPS) .AND. (DDL .LT. EPS))THEN
         IF (DABS(F2) .LT. DABS(F3)) THEN
            T = T2
            P = P2
            DL = DL2
            goto 1000
         ELSE
            T = T3
            P = P3
            DL = DL3
            goto 1000
         ENDIF
      ENDIF

      DL1 = DL2
      T1 = T2
      P1 = P2
      F1 = F2

      DL2 = DL3
      T2 = T3
      P2 = P3
      F2 = F3

! END OF ITERATION LOOP

200   CONTINUE

! OUTER ITERATION OF THE CALCULATION METHODS OF TEMPERATURE

300   CONTINUE

!   START VALUE OF TEMPERATURE

      TMH = 0.5D0 * (TTR + TCE)
      CALL TSATITZ(TMH,DVMH,DLMH,PMH,1.D-6)
      IF (DV .GE. DVMH) THEN
         TOH = TTR + 0.9D0 * (TCE - TTR)
         CALL TSATITZ(TOH,DVOH,DLOH,POH,1.D-6)
         IF (DV .LE. DVOH) THEN
            TS1 = TMH
            TS2 = TOH
         ELSE
            TOH2 = TCE - 1.0001D-3
            CALL TSATITZ(TOH2,DVOH2,DLOH2,POH2,1.D-6)
            IF (DV .LE. DVOH2) THEN
               TS1 = TOH
               TS2 = TOH2
            ELSE
               TS1 = TOH2
               TS2 = TCE - 1.0001D-6
            ENDIF
         ENDIF
      ELSE
         TUH = TTR + 0.05D0 * (TCE - TTR)
         CALL TSATITZ(TUH,DVUH,DLUH,PUH,1.D-6)
         IF (DV .GE. DVUH) THEN
            TS1 = TUH
            TS2 = TMH
         ELSE
            TS1 = TTR + 1.D-6
            TS2 = TUH
         ENDIF
      ENDIF

! ITERATION OF TEMPERATURE

      CALL ITPEGS2(TS1,TS2,MXWDVRES,DV,EPS,XT,IX)

      IF (IX .EQ. 0) THEN
         TH = XT
         CALL TSATITZ(TH,DVH,DLH,PH,EPS)
         T = TH
         DL = DLH
         P = PH
      ELSEIF (IX .LE. 3) THEN
         TH = XT
         CALL TSATITZ(TH,DVH,DLH,PH,EPS)
         IF (DABS(DVH - DV) .LT. (EPS * 1.D1)) THEN
            T = TH
            DL = DLH
            P = PH
         ELSE
            T = -111.D0
            P = -111.D0
            DL = -111.D0
         ENDIF
      ELSE
         T = -111.D0
         P = -111.D0
         DL = -111.D0
      ENDIF

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL
      ISUBOLD = ISUBAKT

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE QUALY(T,D,XTP,DVTP,DLTP,PTP)
! -----------------------------------------------------------------------------
!
!   STEAM QUALITY
!
      IMPLICIT REAL(KIND=8) (a-h, o-z)

      XTP = 2.D0

      DLT = 1.5D0 * DLEQN(T)         ! 1.5 * 800 = 1200
      DVT = 0.5D0 * DVEQN(T)         ! 0.5 * 0.2 = 0.10

      IF ((D .GT. DVT) .AND. (D .LT. DLT)) THEN
!         CALL TSATIT(T,DV,DL,P,1.D-9)
         CALL TSATITZ(T,DV,DL,P,1.D-9)
         IF ((D .GT. DV) .AND. (D .LT. DL)) THEN
            PTP = P
            DVTP = DV
            DLTP = DL
            V = 1.D0 / D
            VV = 1.D0 / DV
            VL = 1.D0 / DL
            XTP = (V - VL) / (VV - VL)
         ENDIF
      ENDIF

      RETURN
      END


! -----------------------------------------------------------------------------
      REAL(KIND=8) function CRMXWRES(PM,TM)
! -----------------------------------------------------------------------------
!
! ROUTINE FOR DETERMINING THE DIFFERENCE BETWEEN FUGACITY ON BOILING
!  AND DEW LINE NEAR THE CRITICAL POINT
!
!  INPUT:     PM        PRESSURE [MPA]
!             TM        TEMPERATURE [K]
!
!  OUTPUT:    CRMXWRES  DIFFERENCE OF FUGACITY  ON SAT AND DEW LINE
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  DVMAX,DLMIN,DVM,TM,DLM,PM,calcp,PTEST,calcFuga
      REAL(KIND=8) ::  DVM1,DVM2,DLM1,DLM2,DITPRES,X,FUG1,FUG2
      INTEGER IX
      EXTERNAL DITPRES
      COMMON / DEXTREM / DVMAX,DLMIN

      CRMXWRES = 0.0D+0

! START VALUE FOR DENSITY

      X = 0.D0
      DVM2 = DVMAX
      DVM1 = DVM2 * 0.95D0
204   CONTINUE
      PTEST = calcp(TM,DVM1)
      IF (PTEST .GT. PM) THEN
         DVM1 = DVM1 * 0.98D0
         GOTO 204
      ENDIF

! ITERATION OF DENSITY

      CALL ITPEG(DVM1,DVM2,DITPRES,TM,PM,1.D-9,X,IX)
      DVM = X

! START VALUE FOR SAT. LIQ. DENSITY

      DLM1 = DLMIN
      DLM2 = DLM1 * 1.02D0
304   CONTINUE
      PTEST = calcp(TM,DLM2)
      IF (PTEST .LT. PM) THEN
         DLM2 = DLM2 * 1.02D0
         GOTO 304
      ENDIF

! ITERATION OF SAT. LIQ. DENSITY

      CALL ITPEG(DLM1,DLM2,DITPRES,TM,PM,1.D-9,X,IX)
      DLM = X

! FUGACITY ON BOILING AND DEW LINE

      FUG1 = calcFuga(TM,DLM)
      FUG2 = calcFuga(TM,DVM)

!  RESIDUAL FUG REFERRED TO MAXWELL CRITERIA

      CRMXWRES = FUG1 - FUG2

      RETURN
      END

            
! -----------------------------------------------------------------------------
      SUBROUTINE MXWRES(PM,TM,RES,DLM,DVM)
! -----------------------------------------------------------------------------
!
! DURATION OF THE PHASES EQUILIBRIA: CONDITIONS OF MAXWELL CRITERION FUNCT. OF T, P,
!  ITERATIONS ON THE PHASE BOUNDARY TO MINIMIZE RESIDUAL
!
!  INPUT:     DVM       TAUDICHTE [KG / M ** 3]
!             DLM       SIEDEDICHTE [KG / M ** 3]
!             PM        DRUCK [MPA]
!             TM        TEMPERATUR [K]
!
!  OUTPUT:    RES       DEVIATION OF MAXWELL CRITERIA
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  DVM,TM,RES,DLM,PM
      REAL(KIND=8) ::  Phir, R,TC,PC,DC,HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR,FL,FV, CNDRF, VISRF
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      FL = Phir(TM,DLM)
      FV = Phir(TM,DVM)

      RES = PM / R / TM * 1.D3 * (1.D0 / DVM - 1.D0 / DLM) -   &
           DLOG(DLM / DVM) - (FL - FV)

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function TVPIT(P)
! -----------------------------------------------------------------------------
!
!  VAPOR TEMPERATURE FROM A VAPOR PRESSURE EQUATION   FUNCTION OF P
!
!  INPUT:     P         VAPOR PRESSURE [MPa] 
!
!  OUTPUT:    TVPIT     TEMPERATUR [K] 
! -----------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  P,PH,TH,VPEQN,VPHRES,X,T1,T2
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  PCE,TCE
      INTEGER IX

      EXTERNAL VPHRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

         TCE = TC
         PCE = PC

      IF ((P .GT. PCE) .OR. (P .LT. PTR)) THEN
         TVPIT = -111.D0
         goto 1000
      ENDIF

! START INTERVAL T1, T2

      TH = TTR + 0.5D0 * (TCE - TTR)
      PH = VPEQN(TH)

      IF (P .LT. PH) THEN
         T1 = TTR
         T2 = TH
      ELSE
         T1 = TH
         T2 = TCE
      ENDIF

!  ITERATION OF TEMPERATURE

      CALL ITPEGS(T1,T2,VPHRES,P,1.D-6,X,IX)

      IF (IX .LE. 3) THEN
         TVPIT = X
      ELSE
         TVPIT = -111.D0
      ENDIF

1000 continue

      RETURN
      END


! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function MXWPRES(T,P)
! ---------------------------------------------------------------------------------------
!
!   DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VAPOR PRESSURE FROM TSATITZ
!
!  INPUT:     P         PRESSURE [MPa]
!             T         TEMPERATURE [K]
!
!  OUTPUT:    MXWPRES   DIFFERENCE OF PRESSURES [MPa]
! ---------------------------------------------------------------------------------------
      IMPLICIT NONE
      REAL(KIND=8) ::  DVM,T,DLM,PM,P

      CALL TSATITZ(T,DVM,DLM,PM,1.D-9)

      MXWPRES = P - PM        

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function TIDH2RES(TZ,D,H)
! ---------------------------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A   GIVEN AND A CALCULATED VALUE OF ENTHALPY IN TWO PHASE REGION
!  FUNCTION OF D AND h
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,D,H,DV,DL,V1,V2,V,X,HL,HV,HH,calch,P

      CALL TSATIT(TZ,DV,DL,P,1.D-6)
      V1 = 1.D0 / DL
      V2 = 1.D0 / DV
      V = 1.D0 / D
      X = (V - V1) / (V2 -V1)
      HL = calch(TZ,DL)
      HV = calch(TZ,DV)
      HH = HL + X * (HV - HL)

      TIDH2RES = HH - H

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function TDVIT(D)
! ---------------------------------------------------------------------------------------
!
! SAT. TEMPERATUREFUNCTION OF VAPOR DENSITY
!
!  INPUT:     D         VAPOR DENSITY [KG / M ** 3] 
!
!  OUTPUT:    TDVIT     TEMPERATURE [K] 
!
! ---------------------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  D,DH,TH,DVEQN,DVHRES,X,T1,T2
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      INTEGER IX
      EXTERNAL DVHRES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      IF ((D .LT. DVTR) .OR. (D .GT. DC)) THEN
         TDVIT = -111.D0
         goto 1000
      ENDIF

!   START INTERVAL

      TH = TTR + 0.5D0 * (TC - TTR)
      DH = DVEQN(TH)

      IF (D .LT. DH) THEN
         T1 = TTR
         T2 = TH
      ELSE
         T1 = TH
         T2 = TC
      ENDIF

!  ITERATION OF TEMPERATURE
      CALL ITPEGS(T1,T2,DVHRES,D,1.D-6,X,IX)

      IF (IX .LE. 3) THEN
         TDVIT = X
      ELSE
         TDVIT = -111.D0
      ENDIF

1000 continue

      RETURN
      END

!---------------------------------------------------------------------
      REAL(KIND=8) function TIDS2RES(TZ,D,S)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE FOR ENTROPY IN
!  TWO PHASE FIELD, FUNCTION T, D AND s
!
!---------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,D,S,DV,DL,V1,V2,V,X,SL,SV,SH,calcs,P, eps
     integer imax

      CALL TSATIT(TZ,DV,DL,P,1.D-6)
      V1 = 1.D0 / DL
      V2 = 1.D0 / DV
      V = 1.D0 / D
      X = (V - V1) / (V2 -V1)
      SL = calcs(TZ,DL)
      SV = calcs(TZ,DV)
      SH = SL + X * (SV - SL)

      TIDS2RES = SH - S

      RETURN
      END

!---------------------------------------------------------------------
      REAL(KIND=8) function DVHRES(T,D)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE FOR VAPOR DENSITY,
!  AT ITERATION OF TEMPERATURE FROM A VAPOR DENSITY
!
!---------------------------------------------------------------------

      IMPLICIT NONE
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      REAL(KIND=8) ::  T,D,DV,DVEQN
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      DV = DVEQN(T)
      IF ((DV .LT. 0.0D+0) .AND. (DABS(T-TC) .LT. 1.D-06)) THEN
          DV = DC
      ENDIF

      DVHRES = D - DV

      RETURN
      END

!---------------------------------------------------------------------
      REAL(KIND=8) function DLHRES(T,D)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE FOR SATURATED LIQUID
!   DENSITY AT ITERATION OF TEMPERATURE FROM A LIQUID DENSITY
!
!---------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  T,D,DL,DLEQN

      DL = DLEQN(T)
      DLHRES = D - DL

      RETURN
      END

!---------------------------------------------------------------------
      REAL(KIND=8) function MXWDLRES(T,DL)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A   GIVEN AND A CALCULATED VALUE FOR 
!   THE SATURATED LIQUID DENSITY
!
!  INPUT:     DL        SAT. LIQ. DENSITY [KG / M ** 3]
!             T         TEMPERATUR [K]
!
!  OUTPUT:    MXWDLRES  DIFFERENCE OF GIVEN AND CALCULATED 
!                  DENSITIES [KG / M ** 3]
!
!---------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  DVM,T,DLM,PM,DL , eps

! ITERATION OF PHASE LIMITS FOR GIVEN TEMPERATURE

      CALL TSATITZ(T,DVM,DLM,PM,1.D-9)

      MXWDLRES = DL - DLM           

      RETURN
      END

!---------------------------------------------------------------------
      REAL(KIND=8) function MXWDVRES(T,DV)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A   GIVEN AND A CALCULATED VALUE FOR 
!   THE VAPOR DENSITY
!
!  INPUT:     DL        SAT. LIQ. DENSITY [KG / M ** 3]
!             T         TEMPERATURE [K]
!
!  OUTPUT:    MXWDVRES  DIFFERENZCE OF GIVEN AND CALCULATED 
!                       VAPOR DENSITY [KG / M ** 3]
!
!---------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  DVM,T,DLM,PM,DV   , eps
      common / imax1 / imax
      integer imax


! ITERATION PHASE LIMIT FOR GIVEN TEMPERATURE

      CALL TSATITZ(T,DVM,DLM,PM,1.D-9)

      MXWDVRES = DV - DVM           

      RETURN
      END

! ---------------------------------------------------------------------------------------
      SUBROUTINE SVSATITCRIT(S,T,DV,DL,P,EPS)
! ---------------------------------------------------------------------------------------
!
!   VAPOR PRESSURE, SAT. LIQ. AND VAPOR FROM ONE STATE EQUATION
!   AT CONTROLLED ENTROPY ON DEW LINE FROM THE CRITICAL POINT DOWN TO
!   TRIPLE POINT DIRECTION, AFTER FIRST ZERO POINT
!
!   INPUT:     S         ENTROPY ON DEW LINE [KJ / (KG * K)]
!              EPS       CONVERGENCE CRITERION (TOLERANCE)
!
!   OUTPUT:    T     TEMPERATURE [K]
!              DV    SAT. VAPOR DENSITY [KG / M ** 3]
!              DL    SAT. LIQ. DENSITY [KG / M ** 3]
!              P     PRESSURE [MPa]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  S,T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  SCRIT
      REAL(KIND=8) ::  T1,T2,SVSATRES,X,TSOLD,PSOLD,DLOLD,DVOLD,SOLD,calcs
      REAL(KIND=8) ::  TSTART,SVSTART,DLSTART,PSTART,DVSTART,TOLD,SVOLD
      INTEGER IT

      EXTERNAL SVSATRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      SAVE TSOLD,PSOLD,DLOLD,DVOLD,SOLD
      DATA TSOLD / -1.D+0 /
      DATA PSOLD / -1.D+0 /
      DATA DLOLD / -1.D+0 /
      DATA DVOLD / -1.D+0 /
      DATA SOLD /1.D9/

      T = 0.0D0
      DV = 0.0D0 
      DL = 0.0D0
      P = 0.0D0

      IF (DABS(S-SOLD).LT.1.D-8) THEN
         T = TSOLD
         P = PSOLD
         DV = DVOLD
         DL = DLOLD
         goto 1000
      END IF

! START ON CRITICAL POINT

!      SCRIT = calcs(TC,DC)         ! *********************************
      SCRIT = SC
      TSTART = TC - 1.D-3
      CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
!      CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS, imax)
      SVSTART = calcs(TSTART,DVSTART)
      IF ((S .LE. SVSTART) .AND. (S .GE. SCRIT))THEN
         T1 = TC - 5.D-6
         T2 = TSTART
      ELSE
100      CONTINUE
         TOLD = TSTART
         SVOLD = SVSTART
         TSTART = TSTART * 0.995D0
           IF (TSTART .LT. TTR) THEN
               TSTART = TTR
           ENDIF
         CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
!         CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS, imax)
         SVSTART = calcs(TSTART,DVSTART)
         IF ((S .LE. SVSTART) .AND. (S .GE. SCRIT)) THEN
            T1 = TOLD
            T2 = TSTART
         ELSE
            GOTO 100
         ENDIF
        ENDIF

      CALL ITPEGS2(T1,T2,SVSATRES,S,EPS,X,IT)
      IF (IT .EQ. 0) THEN
         T = X
      ELSE
         T = -111.D0
      ENDIF

      CALL TSATITZ(T,DV,DL,P,EPS)

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL
      SOLD = S

      RETURN
      END

! ---------------------------------------------------------------------------------------
      SUBROUTINE SVSATITMID(S,TIN,TIN2,T,DV,DL,P,EPS)
! ---------------------------------------------------------------------------------------
!
!   VAPOR PRESSURE, SAT. LIQUID AND VAPOR DENSITIES FROM ONE STATE EQUATION AT 
!    CONTROLLED ENTROPY ON DEW LINE
!   SEEKING A TEMPERATURE TIN UP DIRECTION TO SUPERCRITICAL POINT AFTER
!   FIRST ZERO POINT
!
!   INPUT:     S         ENTROPY AT DEW LINE [KJ / (KG * K)]
!              TIN       START TEMPERATURE OF ITERATION
!              TIN2      UPPER LIMIT OF ITERATION
!              EPS       CONVERGENCE CRITERION (TOLERANCE)
!
!   OUTPUT:    T     TEMPERATURE [K]
!              DV    VAPOR DENSITY [KG / M ** 3]
!              DL    LIQ. DENSITY [KG / M ** 3]
!              P     PRESSURE [MPa]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  S,T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,TINOLD,TIN2OLD
      REAL(KIND=8) ::  TIN,SDIFF1,SDIFF2,TIN2
      REAL(KIND=8) ::  T1,T2,SVSATRES,X,TSOLD,PSOLD,DLOLD,DVOLD,SOLD,calcs
      REAL(KIND=8) ::  TSTART,SVSTART,DLSTART,PSTART,DVSTART,TOLD,SVOLD
      INTEGER IT

      EXTERNAL SVSATRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      SAVE TSOLD,PSOLD,DLOLD,DVOLD,SOLD
      DATA TSOLD / -1.D+0 /
      DATA PSOLD / -1.D+0 /
      DATA DLOLD / -1.D+0 /
      DATA DVOLD / -1.D+0 /
      DATA SOLD /1.D9/
        DATA TINOLD / -1.D+0 /
        DATA TIN2OLD / -1.D+0 /

      T = 0.0D0
      DV = 0.0D0 
      DL = 0.0D0
      P = 0.0D0

      IF ((DABS(S-SOLD).LT.1.D-8) .AND. (DABS(TIN - TINOLD) .LT. 1.D-8)      &
         .AND. (DABS(TIN2 - TIN2OLD) .LT. 1.D-8)) THEN
         T = TSOLD
         P = PSOLD
         DV = DVOLD
         DL = DLOLD
         goto 1000
      END IF

!     START IN TIN

      TSTART = TIN + 1.D-06
      CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
      SVSTART = calcs(TSTART,DVSTART)
!      IF (S .GE. SVSTART) THEN
!          T1 = TTR + 5.D-6
!          T2 = TSTART
!      ELSE
200       CONTINUE
          TOLD = TSTART
          SVOLD = SVSTART
          TSTART = TSTART * 1.01D0
            IF (TSTART .GT. TC) THEN
                T1 = TIN2
                T2 = TOLD
            ELSE
              CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
              SVSTART = calcs(TSTART,DVSTART)
              SDIFF1 = S - SVOLD
              SDIFF2 = S - SVSTART
              IF ((SDIFF1 * SDIFF2) .LT. 0.0D+0) THEN
                  T1 = TSTART
                  T2 = TOLD
              ELSE
                  GOTO 200
              ENDIF
            ENDIF
!      ENDIF

      CALL ITPEGS2(T1,T2,SVSATRES,S,EPS,X,IT)
      IF (IT .EQ. 0) THEN
         T = X
      ELSE
         T = -111.D0
      ENDIF

      CALL TSATITZ(T,DV,DL,P,EPS)

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL
      SOLD = S
        TINOLD = TIN
        TIN2OLD = TIN2

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function SVSATRES(TZ,S)
! ---------------------------------------------------------------------------------------
!
!   DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE FOR ENTROPY ON SATURATION
!
!  INPUT:     s         ENTROPY AT SAT. LINE [KJ / (KG * K)]
!             TZ        TEMPERATURE [K]
!
!  OUTPUT:    SVSATRES  DIFFERENCE OF GIVEN AND CALCULATED 
!                  ENTROPY AT DEW LINE [KJ / (KG * K)]
!
! ---------------------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,S,calcs,DV,DL,P

! ITERATION PHASE LIMIT FOR GIVEN TEMPERATURE

      CALL TSATITZ(TZ,DV,DL,P,1.D-6)

      SVSATRES = calcs(TZ,DV) - S

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function TDIHS2RES(TZ,H,S)
! ---------------------------------------------------------------------------------------
!
!   DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE OF ENTHALPY IM TWO PHASE REGION
!    AT ITERATION OF TEMPERATURE AND DENSITY FOR VALUES OF h AND s
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,H,S,calch,calcs,SL,SV,HL,HV,XZ,DLZ,DVZ,HBER,PZ

      CALL TSATIT(TZ,DVZ,DLZ,PZ,1.D-9)
      SL = calcs(TZ,DLZ)
      SV = calcs(TZ,DVZ)
      HL = calch(TZ,DLZ)
      HV = calch(TZ,DVZ)
      XZ = (S - SL) / (SV - SL)
      HBER = HL + XZ * (HV - HL)
      TDIHS2RES = HBER - H

      RETURN
      END

! ---------------------------------------------------------------------------------------
      SUBROUTINE SVSATITTR(S,T,DV,DL,P,EPS)
! ---------------------------------------------------------------------------------------
!
!   VAPOR PRESSURE, SAT. LIQ. AND VAPOR DENSITIES FROM ONE STATE EQUATION 
!    AT CONTROLLED ENTROPY ON DEW LINE [ x = 1.0 ? ]
!
!   INPUT:     S         ENTROPY AT DEW LINE [KJ / (KG * K)]
!              EPS       CONVERGENCE CRITERION (TOLERANCE)
!
!   OUTPUT:    T     TEMPERATURE [K]
!              DV    VAPOR DENSITY [KG / M ** 3]
!              DL    SAT. LIQ. DENSITY [KG / M ** 3]
!              P     PRESSURE [MPa]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  S,T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  T1,T2,SVSATRES,X,TSOLD,PSOLD,DLOLD,DVOLD,SOLD,calcs
      REAL(KIND=8) ::  TSTART,SVSTART,DLSTART,PSTART,DVSTART,TOLD,SVOLD
      REAL(KIND=8) ::  HHMAX,SHMAX,HSMAX,SSMAX,HSMIN,SSMIN,TSMIN,TSMAX
      REAL(KIND=8) ::  TNEU,DVNEU,DLNEU,PNEU,SVNEU
      INTEGER IT,IHS
      EXTERNAL SVSATRES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / HSDIAGRAM / HHMAX,SHMAX,HSMAX,SSMAX,HSMIN,SSMIN,TSMIN,TSMAX,IHS

      SAVE TSOLD,PSOLD,DLOLD,DVOLD,SOLD
      DATA TSOLD / -1.D+0 /, PSOLD / -1.D+0 /, DLOLD / -1.D+0 /
      DATA DVOLD / -1.D+0 /, SOLD /1.D9/

      T = 0.0D0
      DV = 0.0D0 
      DL = 0.0D0
      P = 0.0D0

      IF (DABS(S-SOLD).LT.1.D-8) THEN
         T = TSOLD
         P = PSOLD
         DV = DVOLD
         DL = DLOLD
         goto 1000
      END IF

!     START AT TRIPLE POINT

      TSTART = TTR
      CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
      SVSTART = calcs(TSTART,DVSTART)
      IF (S .GE. SVSTART) THEN
          T1 = TTR + 5.D-6
          T2 = TSTART
      ELSE
          TOLD = TSTART
         TNEU = TSTART
200       CONTINUE
          TNEU = TNEU * 1.05D0
          IF (TNEU .GT. TC) THEN          
              TNEU = TC
          ENDIF
          CALL TSATITZ(TNEU,DVNEU,DLNEU,PNEU,EPS)
          SVNEU = calcs(TNEU,DVNEU)
          IF ((S .GE. SVNEU) .OR. (SVNEU .GT. SVOLD)) THEN
              IF (IHS .EQ. 0) THEN
               T1 = TOLD
               T2 = TNEU
           ELSE
                T1 = TOLD
               T2 = TSMIN
           ENDIF
          ELSE
              SVOLD = SVNEU
              TOLD = TNEU
              GOTO 200
          ENDIF
      ENDIF

      CALL ITPEGS2(T1,T2,SVSATRES,S,EPS,X,IT)
      IF (IT .EQ. 0) THEN
         T = X
      ELSE
         T = -111.D0
      ENDIF

      CALL TSATITZ(T,DV,DL,P,EPS)

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL
      SOLD = S

      RETURN
      END


! ---------------------------------------------------------------------------------------
      SUBROUTINE SLSATIT(S,T,DV,DL,P,EPS)
! ---------------------------------------------------------------------------------------
!
!   VAPOR PRESSURE, SAT. LIQUID AND VAPOR DENSITIES FROM A STATE EQUATION 
!    AT CONTROLLED SAT. ENTROPY
!
!   INPUT:     S         ENTROPY AT SAT. LIQ. LINE [KJ / (KG * K)]
!              EPS       CONVERGENCE CRITERION (TOLERANCE)
!
!   OUTPUT:    T     TEMPERATURE [K]
!              DV    VAPOR DENSITY [KG / M ** 3]
!              DL    SAT. LIQ. DENSITY [KG / M ** 3]
!              P     PRESSURE [MPa]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  S,T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  T1,T2,SLSATRES,X,TSOLD,PSOLD,DLOLD,DVOLD,SOLD
      INTEGER IT

      EXTERNAL SLSATRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      SAVE TSOLD,PSOLD,DLOLD,DVOLD,SOLD
      DATA TSOLD / -1.D+0 /, PSOLD / -1.D+0 /, DLOLD / -1.D+0 /
      DATA DVOLD / -1.D+0 /, SOLD /1.D9/

      IF (DABS(S-SOLD).LT.1.D-8) THEN
         T = TSOLD
         P = PSOLD
         DV = DVOLD
         DL = DLOLD
         goto 1000
      END IF

      T1 = TTR + 1.D-3
      T2 = TC - 1.D-3

      CALL ITPEGS2(T1,T2,SLSATRES,S,EPS,X,IT)
      IF (IT .EQ. 0) THEN
         T = X
      ELSE
         T = -111.D0
      ENDIF

      CALL TSATITZ(T,DV,DL,P,EPS)

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL
      SOLD = S

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function SLSATRES(TZ,S)
! ---------------------------------------------------------------------------------------
!
! DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE OF ENTROPY ON SATURATION
!
!  INPUT:     s         ENTROPY AT SAT. LIQ. LINE [KJ / (KG * K)]
!             TZ        TEMPERATURE [K]
!
!  OUTPUT:    SLSATRES  DIFFERENCE BETWEEN A GIVEN AND A CALCULATED
!                   VALUE OF ENTROPY ON SATURATION [KJ / (KG * K)]
!
! ---------------------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,S,calcs,DV,DL,P

! ITERATION FOR PHASE LIMIT FOR GIVEN TEMP.

      CALL TSATITZ(TZ,DV,DL,P,1.D-6)

      SLSATRES = calcs(TZ,DL) - S

      RETURN
      END

! ---------------------------------------------------------------------------------------
      SUBROUTINE SVSATIT(S,T,DV,DL,P,EPS)
! ---------------------------------------------------------------------------------------
!
!  VAPOR PRESSURE, SAT. LIQ. AND VAPOR DENSITIES FROM ONE STATE EQUATION 
!   AT CONTROLLED ENTROPY ON DEW LINE
!
!   INPUT:     S         ENTROPY AT SAT. VAPOR LINE [KJ / (KG * K)]
!              EPS       CONVERGENCE CRITERION (TOLERANCE)
!
!   OUTPUT:    T     TEMPERATURE [K]
!              DV    VAPOR DENSITY [KG / M ** 3]
!              DL    SAT. LIQ. DENSITY [KG / M ** 3]
!              P     PRESSURE [MPa]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  S,T,DV,DL,P,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  SCRIT
      REAL(KIND=8) ::  T1,T2,SVSATRES,X,TSOLD,PSOLD,DLOLD,DVOLD,SOLD,calcs
      REAL(KIND=8) ::  TSTART,SVSTART,DLSTART,PSTART,DVSTART,TOLD,SVOLD
      INTEGER IT
      EXTERNAL SVSATRES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      SAVE TSOLD,PSOLD,DLOLD,DVOLD,SOLD
      DATA TSOLD / -1.D+0 /, PSOLD / -1.D+0 /, DLOLD / -1.D+0 /
      DATA DVOLD / -1.D+0 /, SOLD /1.D9/

      T = 0.0D0
      DV = 0.0D0 
      DL = 0.0D0
      P = 0.0D0

      IF (DABS(S-SOLD).LT.1.D-8) THEN
         T = TSOLD
         P = PSOLD
         DV = DVOLD
         DL = DLOLD
         goto 1000
      END IF

! START AT CRITICAL POINT

!      SCRIT = calcs(TC,DC)      ! *************************************
      SCRIT = SC
      TSTART = TC - 1.D-3
      CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
      SVSTART = calcs(TSTART,DVSTART)
      IF ((S .LE. SVSTART) .AND. (S .GE. SCRIT))THEN
         T1 = TC - 5.D-6
         T2 = TSTART
      ELSE
100      CONTINUE
         TOLD = TSTART
         SVOLD = SVSTART
         TSTART = TSTART * 0.995D0
         CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
         SVSTART = calcs(TSTART,DVSTART)
         IF ((S .LE. SVSTART) .AND. (S .GE. SCRIT)) THEN
            T1 = TOLD
            T2 = TSTART
         ELSEIF (SVSTART .GT. SVOLD) THEN
            GOTO 100
         ELSE

! NEW START AT TRIPLE POINT

            TSTART = TTR + 1.D-3
            CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
            SVSTART = calcs(TSTART,DVSTART)
            IF (S .GE. SVSTART) THEN
              T1 = TTR + 5.D-6
              T2 = TSTART
            ELSE
200            CONTINUE
               TOLD = TSTART
               SVOLD = SVSTART
               TSTART = TSTART * 1.05D0
               CALL TSATITZ(TSTART,DVSTART,DLSTART,PSTART,EPS)
               SVSTART = calcs(TSTART,DVSTART)
               IF ((S .GE. SVSTART) .OR. (SVSTART .GT. SVOLD)) THEN
                  T1 = TSTART
                  T2 = TOLD
               ELSE
                  GOTO 200
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      CALL ITPEGS2(T1,T2,SVSATRES,S,EPS,X,IT)
      IF (IT .EQ. 0) THEN
         T = X
      ELSE
         T = -111.D0
      ENDIF

      CALL TSATITZ(T,DV,DL,P,EPS)

1000 continue

      TSOLD = T
      PSOLD = P
      DVOLD = DV
      DLOLD = DL
      SOLD = S

      RETURN
      END


! SATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSATSAT

!******************************************************************************
! -----------------------------------------------------------------------------
      SUBROUTINE hsLimites( h, s, hMax, hMin, hx0, hx1, icode )
! -----------------------------------------------------------------------------
!
!  ENTALPHY LIMITS FOR FUNCTIONS OF h AND s
!
!  INPUT:     h            SPEC. ENTHALPY [KJ/Kg]
!             s            SPEC. ENTROPY [KJ/Kg K]
!
!  OUTPUT:    hMax, hMin   ENTALPHIES LIMITS [kJ/kg]
!
! -----------------------------------------------------------------------------

      IMPLICIT REAL(KIND=8) (a-h, o-z)
     integer icode

     COMMON / ChsMax  / ChsMax1(4), ChsMax2(7)
     COMMON / ChsMin  / ChsMin1(2), ChsMin2(4)
     COMMON / ChsCrit / Chspcrit(4), ChsTcrit(7)
     COMMON / Chsx    / Chsx0(3), Chsx1(7)
     COMMON / shLim   / sMax1, sMax2, sMin1, sMin2, shpcr1, shpcr2,      &
                   shTcr1, sTcri2, shx11, shx12, shx01, shx02
     COMMON / LIMITES / Tmin, TMAXI, PMAXI

!   UPPER BOUNDARY
      if ( s .LE. sMax1 ) then
!        Pressure limit (hsMax1)
!        shMax1 --> 0 <= s <= 5,20477498
        hMax = ChsMax1(1) + ChsMax1(2)*s + ChsMax1(3)*s*s + ChsMax1(4)*s*s*s
       icode = 0 
     else if ( s .LE. sMax2 ) then
!        Temperature limit (hsMax2) [ T <- 1273.0 K
!        shMax2 --> 5,20477498 <= s <= 12,3321289
        hMax = ChsMax2(1) + ChsMax2(2)*s + ChsMax2(3)*s*s + ChsMax2(4)*s*s*s +      &
              ChsMax2(5)*s*s*s*s + ChsMax2(6)*s*s*s*s*s + ChsMax2(7)*s*s*s*s*s*s
       icode = 0
      else
         icode = -1005 
      end if                  
        
!   LOWER BOUNDARY
      if ( s .LE. sMin1 ) then
!        shMin1 --> 0 <= s <= 9.15549341     [ sv(TTR) ]  - Triple Point
        hmin = ChsMin1(1) + ChsMin1(2)*s
       icode = 0 
     else if ( s .LE. sMin2 ) then
!     shMin2 --> 9.15549341 <= s <= 12,3321289   - Sup.
        hmin = ChsMin2(1) + ChsMin2(2)*s + ChsMin2(3)*s*s + ChsMin2(4)*s*s*s
       icode = 0 
     else
        icode = -1005
     end if
     
!   SATURATION LINE
  
!     hx0 --> 0 <= s <= 4,40670568   
!     DATA shx01 / 0.d+0/, shx02 / 4.40670568d+0/
     if (s .le. shx02) then
        hx0 = Chsx0(1) + Chsx0(2)*s + Chsx0(3)*s*s
     else
        hx0 = 0.d+0
     end if         
              
!     hx1 --> 4,40670568 <= s <= 9,15549341   
!     DATA shx11 / 4.40670568d+0/, shx12 / 9.15549341d+0/
     if ( (s .ge. shx11) .and. (s .le. shx12) ) then
        hx1 = Chsx1(1) + Chsx1(2)*s + Chsx1(3)*s*s + Chsx1(4)*s*s*s +        &
            Chsx1(5)*s*s*s*s + Chsx1(6)*s*s*s*s*s + Chsx1(7)*s*s*s*s*s*s                  
     else
        hx1 = 0.d+0
     end if

      return
     end


!******************************************************************************


! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
! I                        ITERATIVE CALCULATIONS                                       I
! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
! -----------------------------------------------------------------------------
      SUBROUTINE TPITER(T,p,D,EPS)
! -----------------------------------------------------------------------------
!
!  ITERATION OF DENSITY   FUNCTION OF T AND D
!
!  INPUT:     T         TEMPERATURE [K]
!             P         PRESSURE [MPa]
!             EPS       TOLERANCE FOR ITERATION
!
!  OUTPUT:    D         DENSITY [KG / M**3]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,P,D,EPS
      REAL(KIND=8) ::  DL,DV,DEST,D1,D2,DFAK,PS,P1,P2,PDIFF1,PDIFF2
      REAL(KIND=8) ::  DSOAVE,calcp,VPEQN,DLEQN,DVEQN,DITPRES,X,PS1,PS2
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,PTEST
      REAL(KIND=8) ::  TCE,PCE,DCE,TOLD,POLD,DOLD
      REAL(KIND=8) ::  ABLTEST,calcdpdD,DLTEST,DVABLTEST
      REAL(KIND=8) ::  DLABLTEST,PABLTEST
      INTEGER IX,ISUBAKT,ISUBOLD
      EXTERNAL DITPRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,POLD,DOLD,ISUBOLD
      DATA DOLD / -1.D+0 /
      DATA TOLD / -1.D+0 /
      DATA POLD / 1.D9 /
      DATA ISUBOLD / 0 /

      D = 0.0D+0
      P1 = 0.0D+0
      P2 = 0.0D+0
      D1 = 0.0D+0
      D2 = 0.0D+0

! DETERMINATION OF CRITICAL POINT OF GL

         TCE = TC
         DCE = DC
         PCE = PC

!  DETERMINING A DENSITY ENCLOSING INTERVAL (D1 TO D2)

      IF (T .GT. TCE) THEN

! NEAR CRITICAL TEMPERATURES THE SOAVE SUBROUTINE DETERMINES A START VALUE

         D1 = DSOAVE(T,P,0)
         IF (D1 .LT. 0.D0) THEN
            D1 = P / R / T * 1.D3
         ENDIF
         P1 = calcp(T,D1)
         IF (P1 .LT. 0.D0) THEN
            D1 = DCE
            P1 = calcp(T,D1)
         ENDIF
         PDIFF1 = P - P1
         IF (P1 .LT. P) THEN
            IF (P .GE. PCE) THEN
               DFAK = 1.05D0
            ELSE
               DFAK = 1.1D0
            ENDIF
         ELSE
            IF (P .GE. PCE) THEN
               DFAK = 0.95D0
            ELSE
               DFAK = 0.9D0
            ENDIF
         ENDIF

100      CONTINUE         
         D2 = D1 * DFAK
         P2 = calcp(T,D2)
         IF (P2 .LT. 0.D0) THEN
            D2 = DCE
            P2 = calcp(T,DCE)
            IF (P2 .LT. P) THEN
               DFAK = 1.05D0
            ELSE
               DFAK = 0.95D0
            ENDIF
         ENDIF
         PDIFF2 = P - P2
         IF ((PDIFF1 * PDIFF2) .LE. 0.D0) GOTO 101
         D1 = D2
         P1 = P2
         PDIFF1 = PDIFF2
         GOTO 100

101      CONTINUE
      ELSE
         PS1 = VPEQN(T)
         IF ((P .GT. (0.9D0 * PS1)) .AND. (P .LT. (1.1D0 * PS1))   &
             .OR. (PS1 .LT. 0.08D0)) THEN
            CALL TSATIT(T,DV,DL,PS2,1.D-9)
            PS = PS2
         ELSE
            PS = PS1
         ENDIF

! UNDER CRITICAL TEMPERATURES AND P <0.5*PSAT THE START VALUE IS OBATAINED BY DSOAVE

         IF (P .LT. (0.5D0 * PS)) THEN
            DEST = DSOAVE(T,P,2)
            D1 = DEST * 0.95D0
            D2 = DEST * 1.05D0

! UNDER CRITICAL TEMPERATURES AND 0.5*PS TO p < PS (T) THE START VALUE IS
! FROM THE AUXILIARY EQUATION FOR VAPOR DENSITY DVEON

         ELSEIF (P .LT. PS) THEN
            D1 = DVEQN(T)
            P1 = calcp(T,D1)
            PDIFF1 = P - P1
            IF (P1 .LT. P) THEN
               DFAK = 1.02D0
            ELSE
               DFAK = 0.95D0
            ENDIF

200         CONTINUE  
            D2 = D1 * DFAK
            P2 = calcp(T,D2)
            PDIFF2 = P - P2
            IF ((PDIFF1 * PDIFF2) .LE. 0.D0) GOTO 201
            D1 = D2
            P1 = P2
            PDIFF1 = PDIFF2
            GOTO 200

201         CONTINUE

! UNDER CRITICAL TEMPERATURES AND P > Psat THE START VALUE IS FROM
! THE AUXILIARY EQUATION FOR saturated liquid density DLEON

         ELSEIF (P .GT. PS) THEN
            D1 = DLEQN(T)
            P1 = calcp(T,D1)
            PDIFF1 = P - P1
            IF ((P1 .LT. P) .AND. (P .LT. PCE)) THEN
               DFAK = 1.02D0
            ELSEIF (P1 .LT. P) THEN
               DFAK = 1.05D0
            ELSE
               DFAK = 0.98D0
            ENDIF
            CALL TSATIT(T,DVABLTEST,DLABLTEST,PABLTEST,1.D-9)
            DLTEST = DLABLTEST
300         CONTINUE  
            D2 = D1 * DFAK
            P2 = calcp(T,D2)

            ABLTEST = calcdpdD(T,D2)
            IF ((ABLTEST .LT. 0.0D+0) .AND. (DFAK .GT. 1.D+0) .AND.  &
         ((T .GT. TCE) .OR. ((D2 .GT. DLTEST) .AND. (DLTEST .GT. 0.0D+0)) &
         .OR. (D2 .GT. DLTR))) THEN
                D = -111.D+0
                goto 1000
            ENDIF

            PDIFF2 = P - P2
            IF ((PDIFF1 * PDIFF2) .LE. 0.D0) GOTO 301
            D1 = D2
            P1 = P2
            PDIFF1 = PDIFF2
            GOTO 300

301         CONTINUE

! ERROR CONDITION: POINT IS EXACTLY ON THE PHASE BOUNDARY P = BS (T)

         ELSE
            D = -111.D0
            goto 1000
         ENDIF
      ENDIF

!  ITERATION FOR DENSITY

      CALL ITPEG(D1,D2,DITPRES,T,P,EPS,X,IX)

      IF (IX .EQ. 0) THEN
         D = X
      ELSEIF (IX .LE. 3) THEN
         PTEST = calcp(T,X)
         IF (DABS(PTEST - P) .LT. (EPS * 1.D1)) THEN
            D = X
         ELSE
            D = -111.D0
         ENDIF
      ELSE
         D = -111.D0
      ENDIF

1000 continue

      TOLD = T
      POLD = P
      DOLD = D
      ISUBOLD = ISUBAKT

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DSOAVE(T,P,IREG)
! -----------------------------------------------------------------------------
!
! DENSITY FROM CUBIC STATE EQUATION   OF SOAVE (1972) FOR VALUES OF T AND P 
! (SEE ALSO   SPAN (2000): MULTIPARAMETER EQUATIONS OF STATE, p.48 -51)
!  
!  INPUT:     T         TEMPERATURR [K]
!             P         PRESSURE [MPa]
!             IREG      MANAGEMENT OF REGIONS UNDER CRITICAL TEMPERATURES 
!                       LIQUID:      IREG = 1
!                       VAPOUR:     IREG = 2
!
!  OUTPUT:    DSOAVE    DENSITY [KG / M**3]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,P
      REAL(KIND=8) ::  TAFRED,PAF,AF,EM,AK,BK,AG,BG,RK,QK,DG
      REAL(KIND=8) ::  UK,EINDRITT,Y1,Y2,Y3,TETA,PHI,PI,VPEQN,PS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,VALUE1
      INTEGER IREG,NREG

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      PARAMETER (PI = 3.14159265359D0)

! DETERMINING THE ACENTRIC FACTOR

      TAFRED = 0.7D0 * TC
      PAF = VPEQN(TAFRED)
      AF = -DLOG10(PAF / PC) - 1.D0

      EM = 0.48D0 * 1.574D+0 * AF - 0.176D0 * AF * AF
      AK = 0.42747D0 * R * R * TC * TC / PC * (1.D0 + EM *    &
           (1.D0 - DSQRT(T / TC))) * (1.D0 + EM * (1.D0 - DSQRT(T / TC)))
      BK = 0.08664D0 * R * TC / PC
      AG = AK * P / (R * T) / (R * T)
      BG = BK * P / (R * T)
      RK = (3.D0 * (AG - BG - BG * BG) - 1.D0) / 3.D0
      QK = - 2.D0 / 27.D0 + (AG - BG - BG * BG) / 3.D0 - AG * BG
      DG = (RK / 3.D0) * (RK / 3.D0) * (RK / 3.D0)           &
           + (QK / 2.D0) * (QK / 2.D0)

! SOLUTION FOR T> TC

      IF (DG .GE. 0.D0) THEN
         EINDRITT = 1.D0 / 3.D0
         UK = (DSQRT(DG) - 0.5D0 * QK) ** EINDRITT
         Y1 = UK - RK / UK / 3.D0
         DSOAVE = P / (R * T * (Y1 + EINDRITT)) * 1.D3 

! SELECTION OF THREE POSSIBLE SOLUTIONS FOR T<TCC (DEPENDING ON IREG)

      ELSE
         TETA = DSQRT( - RK * RK * RK / 27.D0)
         VALUE1 = - 0.5D0 * QK / TETA
           IF (DABS(VALUE1 - 1.D+0) .LT. 1.D-15) THEN
               PHI = 0
           ELSE
             PHI = DACOS(VALUE1)
           ENDIF
         EINDRITT = 1.D0 / 3.D0
         Y1 = 2.D0 * TETA ** EINDRITT * DCOS(PHI / 3.D0)
         Y2 = 2.D0 * TETA ** EINDRITT * DCOS((PHI + 2.D0 * PI) / 3.D0)
         Y3 = 2.D0 * TETA ** EINDRITT * DCOS((PHI + 4.D0 * PI) / 3.D0)

         IF ((IREG .EQ. 1) .OR. (IREG .EQ. 2)) THEN
            NREG = IREG
         ELSE

            PS = VPEQN(T)
            IF ((P .GT. PS) .AND. (PS .GT. 0.0D+0)) THEN
               NREG = 1
            ELSEIF ((P .LT. PS) .AND. (PS .GT. 0.0D+0)) THEN
               NREG = 2
            ELSE
              NREG = 0
            ENDIF

         ENDIF

         IF (NREG .EQ. 1) THEN
            IF ((Y1 .LT. Y2) .AND. (Y1 .LT. Y3)) THEN
               DSOAVE = P / (R * T * (Y1 + EINDRITT)) * 1.D3
            ELSEIF (Y2 .LT. Y3) THEN
               DSOAVE = P / (R * T * (Y2 + EINDRITT)) * 1.D3
            ELSE
               DSOAVE = P / (R * T * (Y3 + EINDRITT)) * 1.D3
            ENDIF
         ELSE
            IF ((Y3 .GT. Y2) .AND. (Y3 .GT. Y1)) THEN
               DSOAVE = P / (R * T * (Y3 + EINDRITT)) * 1.D3
            ELSEIF (Y2 .GT. Y1) THEN
               DSOAVE = P / (R * T * (Y2 + EINDRITT)) * 1.D3
            ELSE
               DSOAVE = P / (R * T * (Y1 + EINDRITT)) * 1.D3
            ENDIF
         ENDIF
      ENDIF

      END

! -----------------------------------------------------------------------------
      SUBROUTINE ITPEGS(XA,XB,RES,Y,EPS,X,IX)
! -----------------------------------------------------------------------------
!
! ITERATION OF X, FUNCTION OF RES(X, Y) = 0 AFTER PROCESS WITH STARTING INTERVAL XA, XB
!
!  INPUT:     XA,XA     VALUES OF START INTERVAL 
!             RES       FUNCTION WHOSE ZERO POINT IS SEARCH
!             Y         INDEPENDENT VARIABLE FUNCTION RES
!             EPS       EPS TOLERANCE FOR ITERATION
!
!  OUTPUT:    X         ITERATION RESULTA
!             IX        ERROR CODE
! -----------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  XA,XB,RES,Y,EPS,X
      REAL(KIND=8) ::  X1,X2,F1,F2,S12,X3,F3,G
      INTEGER IX,I
      EXTERNAL RES

      X1 = XB
      X2 = XA

50    CONTINUE
      IX = 0

      F1 = RES(X1,Y)
      F2 = RES(X2,Y)     

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 1
            goto 1000
         ELSE
            X = X1
            IX = 1
            goto 1000
         ENDIF

      ELSEIF (DABS(F1) .LT. 1.D-15) THEN
         X = X1
         IX = 2
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         X = X2
         IX = 2
         goto 1000
      ENDIF

!      DO 100 I = 1,40
      DO 100 I = 1,80

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 3
            goto 1000
         ELSE
            X = X1
            IX = 3
            goto 1000
         ENDIF
      ENDIF

      S12 = (F2 - F1) / (X2 - X1)
      X3 = X2 - F2 / S12
      F3 = RES(X3,Y)

      IF (DABS(F3) .LT. 1.D-15) THEN
         X = X3
         goto 1000
      ENDIF

      IF ((F2 * F3) .LT. 0.D0) THEN
         X1 = X2
         X2 = X3
         F1 = F2
         F2 = F3
      ELSEIF ((F2 * F3) .GT. 0.D0) THEN
         G = F2 / (F2 + F3)
         X2 = X3
         F1 = G * F1
         F2 = F3
      ENDIF

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
          IF (DABS(F2) .LT. DABS(F1)) THEN
              X = X2
              goto 1000
          ELSE
              X = X1
              goto 1000
          ENDIF
      ENDIF

100   CONTINUE

      IX = 4

1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE ITPEGZ(XA,ZA,XB,ZB,RES,Y1,Y2,EPS,X,Z,IX)
! -----------------------------------------------------------------------------
!
! ITERATION OF X, FUNCTION OF RES(X, Y1, Y2) = 0 
! AFTER PROCESS WITH STARTING INTERVAL XA, XB
!
!  INPUT:     XA,XB     VALUES OF START INTERVAL 
!             RES       FUNCTION WHOSE ZERO POINT IS SEARCH
!             Y1,Y2     INDEPENDENT VARIABLES FUNCTION RES
!             EPS       EPS TOLERANCE FOR ITERATION
!
!  OUTPUT:    X         ITERATION RESULTA
!             IX        ERROR CODE
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  XA,ZA,XB,ZB,RES,Y1,Y2,EPS,X,Z
      REAL(KIND=8) ::  X1,X2,F1,F2,S12,X3,F3,G,SZ,Z1,Z2,Z3,Z1IT,Z2IT,Z3IT
      INTEGER IX,I
      EXTERNAL RES

      X1 = XA
      X2 = XB

      Z1 = ZA
      Z2 = ZB

      IX = 0

      F1 = RES(X1,Z1,Z1IT,Y1,Y2)
      Z1 = Z1IT
      F2 = RES(X2,Z2,Z2IT,Y1,Y2)
      Z2 = Z2IT

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            Z = Z2
            IX = 1
            goto 1000
         ELSE
            X = X1
            Z = Z1
              IX = 1
            goto 1000
         ENDIF
      ELSEIF (DABS(F1) .LT. 1.D-15) THEN
         X = X1
         Z = Z1
         IX = 2
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         X = X2
         Z = Z2
         IX = 2
         goto 1000
      ENDIF

!      DO 100 I = 1,80
      DO 100 I = 1,200

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            Z = Z2
              IX = 3
            goto 1000
         ELSE
            X = X1
            Z = Z1
              IX = 3
            goto 1000
         ENDIF
      ENDIF

      S12 = (F2 - F1) / (X2 - X1)
      X3 = X2 - F2 / S12
      SZ = (F2 - F1) / (Z2 - Z1)
      Z3 = Z2 - F2 / SZ
      F3 = RES(X3,Z3,Z3IT,Y1,Y2)
      Z3 = Z3IT

      IF (DABS(F3) .LT. 1.D-15) THEN
         X = X3
         Z = Z3
         goto 1000
      ENDIF

      IF ((F2 * F3) .LT. 0.D0) THEN
         X1 = X2
         X2 = X3
         F1 = F2
         F2 = F3
         Z1 = Z2
         Z2 = Z3
      ELSEIF ((F2 * F3) .GT. 0.D0) THEN
         G = F2 / (F2 + F3)
         X2 = X3
         F1 = G * F1
         F2 = F3
         Z2 = Z3
      ENDIF

      IF (DABS((X2 - X1) / X2) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            Z = Z2
            goto 1000
         ELSE
            X = X1
            Z = Z1
            goto 1000
         ENDIF
      ENDIF

100   CONTINUE

      IX = 4

1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE ITPEGS2(XA,XB,RES,Y,EPS,X,IX)
! -----------------------------------------------------------------------------
!
! ITERATION OF X, FUNCTION OF RES(X, Y) = 0 AFTER PROCESS
!    WITH STARTING INTERVAL XA, XB
!
!  INPUT:     XA,XA     VALUES OF START INTERVAL 
!             RES       FUNCTION WHOSE ZERO POINT IS SEARCH
!             Y         INDEPENDENT VARIABLE FUNCTION RES
!             EPS       EPS TOLERANCE FOR ITERATION
!
!  OUTPUT:    X         ITERATION RESULTA
!             IX        ERROR CODE
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  XA,XB,RES,Y,EPS,X
      REAL(KIND=8) ::  X1,X2,F1,F2,S12,X3,F3,G
      REAL(KIND=8) ::  TC,PC,DC,HC, SC,R,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      INTEGER IX,I

      EXTERNAL RES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      X1 = XA
      X2 = XB
      IX = 0

      F1 = RES(X1,Y)
      F2 = RES(X2,Y)     

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 1
            goto 1000
         ELSE
            X = X1
            IX = 1
            goto 1000
         ENDIF
      ELSEIF (DABS(F1) .LT. 1.D-15) THEN
         X = X1
         IX = 2
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         X = X2
         IX = 2
         goto 1000
      ENDIF

!      DO 100 I = 1,40
      DO 100 I = 1,80

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 3
            goto 1000
         ELSE
            X = X1
            IX = 3
            goto 1000
         ENDIF
      ENDIF

      S12 = (F2 - F1) / (X2 - X1)
      X3 = X2 - F2 / S12

      IF (X3 .GT. TC) THEN
          X3 = TC - 1.D-12
      ENDIF

      IF (X3 .LT. TTR) THEN
          X3 = TTR
      ENDIF

      F3 = RES(X3,Y)

      IF ((F2 * F3) .LT. 0.D0) THEN
         X1 = X2
         X2 = X3
         F1 = F2
         F2 = F3
      ELSEIF ((F2 * F3) .GT. 0.D0) THEN
         G = F2 / (F2 + F3)
         X2 = X3
         F1 = G * F1
         F2 = F3
      ENDIF

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF ((DABS(F2) .LT. 1.D-01) .OR. (DABS(F3) .LT. 1.D-01)) THEN
             IF (DABS(F2) .LT. DABS(F1)) THEN
                 X = X2
                 goto 1000
             ELSE
                 X = X1
                 goto 1000
             ENDIF
      ELSE
          X = -111.D+0
          goto 1000
      ENDIF
      ENDIF

100   CONTINUE

      IX = 4

1000 continue

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE ITPEGB(XA,XB,RES,Y1,Y2,EPS,X,IX)
! -----------------------------------------------------------------------------
!
! ITERATION OF X, FUNCTION OF RES(X, Y1, Y2) = 0 
! AFTER PROCESS WITH STARTING INTERVAL XA, XB
!
!  INPUT:     XA,XB     VALUES OF START INTERVAL 
!             RES       FUNCTION WHOSE ZERO POINT IS SEARCH
!             Y1,Y2     INDEPENDENT VARIABLES FUNCTION RES
!             EPS       EPS TOLERANCE FOR ITERATION
!
!  OUTPUT:    X         ITERATION RESULTA
!             IX        ERROR CODE
! -----------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  XA,XB,RES,Y1,Y2,EPS,X
      REAL(KIND=8) ::  X1,X2,F1,F2,S12,X3,F3,G
      INTEGER IX,I
      EXTERNAL RES

      X1 = XA
      X2 = XB
      IX = 0

      F1 = RES(X1,Y1,Y2)
      F2 = RES(X2,Y1,Y2)     

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 1
            goto 1000
         ELSE
            X = X1
            IX = 1
            goto 1000
         ENDIF
      ELSEIF (DABS(F1) .LT. 1.D-15) THEN
         X = X1
         IX = 2
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-15) THEN
         X = X2
         IX = 2
         goto 1000
      ENDIF

!      DO 100 I = 1,40
      DO 100 I = 1,80

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 3
            goto 1000
         ELSE
            X = X1
            IX = 3
            goto 1000
         ENDIF
      ENDIF

      S12 = (F2 - F1) / (X2 - X1)
      X3 = X2 - F2 / S12
      F3 = RES(X3,Y1,Y2)

      IF (DABS(F3) .LT. 1.D-15) THEN
         X = X3
         goto 1000
      ENDIF

      IF ((F2 * F3) .LT. 0.D0) THEN
         X1 = X2
         X2 = X3
         F1 = F2
         F2 = F3
      ELSEIF ((F2 * F3) .GT. 0.D0) THEN
         G = F2 / (F2 + F3)
         X2 = X3
         F1 = G * F1
         F2 = F3
      ENDIF

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            goto 1000
         ELSE
            X = X1
            goto 1000
         ENDIF
      ENDIF

100   CONTINUE

      IX = 4

1000 continue

      RETURN
      END


! -----------------------------------------------------------------------------
      REAL(KIND=8) function DITPRES(D,T,P)
! -----------------------------------------------------------------------------
!
!  DIFFERENCE (RESIDUAL) BETWEEN GIVEN AND CALCULATED VAPOR DENSITY.
!   ITERATION OF DENSITY AS FUNCTION OF T AND D, IS THE START VALUE WANTED
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  D,T,P,calcp
      DITPRES = 0.0D+0

      DITPRES = P - calcp(T,D)

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE ITPEG(XA,XB,RES,Y1,Y2,EPS,X,IX)
! -----------------------------------------------------------------------------
!
! ITERATION OF X, FUNCTION OF RES(X, Y1, Y2) = 0 
! AFTER PROCESS WITH STARTING INTERVAL XA, XB
!
!  INPUT:     XA,XB     VALUES OF START INTERVAL 
!             RES       FUNCTION WHOSE ZERO POINT IS SEARCH
!             Y1,Y2     INDEPENDENT VARIABLES FUNCTION RES
!             EPS       EPS TOLERANCE FOR ITERATION
!
!  OUTPUT:    X         ITERATION RESULTA
!             IX        ERROR CODE
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  XA,XB,RES,Y1,Y2,EPS,X
      REAL(KIND=8) ::  X1,X2,F1,F2,S12,X3,F3,G
      INTEGER IX,I,REP
      EXTERNAL RES

      REP = 0

      X1 = XA
      X2 = XB

      IX = 0
      X = 0.0D+0

50    CONTINUE
      F1 = RES(X1,Y1,Y2)
      F2 = RES(X2,Y1,Y2)

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 1
            goto 1000
         ELSE
            X = X1
            IX = 1
            goto 1000
         ENDIF
      ELSEIF (DABS(F1) .LT. 1.D-16) THEN
         X = X1
         IX = 2
         goto 1000
      ELSEIF (DABS(F2) .LT. 1.D-16) THEN
         X = X2
         IX = 2
         goto 1000
      ENDIF

!      DO 100 I = 1,80
      DO 100 I = 1,160

      IF (DABS(F2 - F1) .LT. 1.D-15) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            IX = 3
            goto 1000
         ELSE
            X = X1
            IX = 3
            goto 1000
         ENDIF
      ENDIF

      S12 = (F2 - F1) / (X2 - X1)
      X3 = X2 - F2 / S12

      F3 = RES(X3,Y1,Y2)

      IF (DABS(F3) .LT. 1.D-15) THEN
         X = X3
         goto 1000
      ENDIF

      IF ((F2 * F3) .LT. 0.D0) THEN
         X1 = X2
         X2 = X3
         F1 = F2
         F2 = F3
      ELSEIF ((F2 * F3) .GT. 0.D0) THEN
         G = F2 / (F2 + F3)
         X2 = X3
         F1 = G * F1
         F2 = F3
      ENDIF

      IF ((DABS((X2 - X1) / X2)) .LT. EPS) THEN
         IF (DABS(F2) .LT. DABS(F1)) THEN
            X = X2
            goto 1000
         ELSE
            X = X1
            goto 1000
         ENDIF
      ENDIF

100   CONTINUE

      IX = 4

1000 continue

      RETURN
      END


! -----------------------------------------------------------------------------
      SUBROUTINE TSITER (T,S,D,EPS)
! -----------------------------------------------------------------------------
!
!  ITERATION OF DENSITY FUNCTION OF T AND s
!
!  INPUT:     T         TEMPERATURE [K]
!             S         ENTROPY [KJ / (KG * K)]
!             EPS       ITERATION CONVERGENCE CRITERION
!
!  OUTPUT:    D         DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,S,D,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  D1,S1,calcs,DFAK,SDIFF1,D2,S2,SDIFF2,DVEQN,DLEQN
      REAL(KIND=8) ::  DV,DL,SV,SL,DIT,DITSRES,V,X,P,TCE,DCE
      REAL(KIND=8) ::  SLTEST,SVTEST,TOLD,SOLD,DOLD
      INTEGER IX,ISUBAKT,ISUBOLD

      EXTERNAL DITSRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,SOLD,DOLD,ISUBOLD
      DATA DOLD / -1.D+0 /
      DATA TOLD / -1.D+0 /
      DATA SOLD / 1.D9 /
      DATA ISUBOLD / 0 /

      IF ((DABS(T-TOLD).LT.1.D-8) .AND. (DABS(S-SOLD).LT.1.D-8)    &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         D = DOLD
         goto 1000
      END IF

! CRITICAL POINT

         TCE = TC
         DCE = DC

! DENSITY ENCLOSING INTERVAL (D1 s1 - D2, s2)

      IF (T .GE. TCE) THEN

! CLOSE TO CRITICAL POINT USES CRITICAL DENSITY AS START DENSITY

         D1 = DCE
         S1 = calcs(T,D1)
         IF (S .LT. S1) THEN 
            DFAK = 1.2D0
         ELSE 
            DFAK = 0.9D0
         ENDIF
         SDIFF1 = S - S1

100      CONTINUE
         D2 = D1 * DFAK
         S2 = calcs(T,D2)
           SDIFF2 = S - S2
         IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 101
         S1 = S2
         SDIFF1 = SDIFF2
         D1 = D2
         GOTO 100

101      CONTINUE 

! CLOSE TO CRITICAL TEMPERATURES FIRST IS CHECKED WHICH IS
!  THE APPROPRIATE POINT 

      ELSEIF (T .GE. TTR) THEN

! FIRST REVIEW WITH AUXILIARY EQUATIONS FOR BOILING AND DENSITY

!C
         DV = DVEQN(T)
         SV = calcs(T,DV)
         DL = DLEQN(T)
         SL = calcs(T,DL)

! PHASE LIMIT FROM THE FUNDAMENTAL EQUATION FOR EXACT REVIEW OF PHASE LIMIT

         IF (SL .GT. 0.D0) THEN
            SLTEST = 0.9D0 * SL
         ELSE
            SLTEST = 1.1D0 * SL
         ENDIF

         IF (SV .GT. 0.D0) THEN
            SVTEST = 1.2D0 * SV
         ELSE
            SVTEST = 0.8D0 * SV
         ENDIF

         IF ((SLTEST .LT. S) .AND. (SVTEST .GT. S)) THEN
            CALL TSATIT(T,DV,DL,P, eps)
            SV = calcs(T,DV)
            SL = calcs(T,DL)
         ENDIF

!  IN TWO PHASE FIELD THE DENSITY IS CALCULATED DIRECTLY

         IF ((SV .GE. S) .AND. (SL .LE. S)) THEN
            X = (S - SL) / (SV - SL)
            V = 1.D0 / DL + X * (1.D0 / DV - 1.D0 / DL)
            D = 1.D0 / V
            goto 1000

! IN HOMOGENEOUS REGION THE SATURATED CONDITIONS ARE USED AS START VALULES

         ELSEIF (S .GT. SV) THEN
            DFAK = 0.9D0
            S1 = SV
            D1 = DV
         ELSEIF (S .LT. SL) THEN
            DFAK = 1.05D0
            S1 = SL
            D1 = DL
         END IF

         SDIFF1 = S - S1

200      CONTINUE
         D2 = D1 * DFAK
         S2 = calcs(T,D2)
         IF ((S2 .GT. S1) .AND. (DFAK .GT. 1.D+0)) GOTO 201
         SDIFF2 = S - S2
         IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 201
         S1 = S2
         SDIFF1 = SDIFF2
         D1 = D2
         GOTO 200

201      CONTINUE 

      ELSE
         D = -111.D0
         goto 1000

      ENDIF

! ITERATION OF DENSITY

      CALL ITPEG(D1,D2,DITSRES,T,S,EPS,DIT,IX)
      IF (IX .LE. 3) THEN
         D = DIT
      ELSE
         D = -111.D0
      ENDIF

1000 continue

      TOLD = T
      SOLD = S
      DOLD = D
      ISUBOLD = ISUBAKT

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DITSRES(DZ,T,S)
! -----------------------------------------------------------------------------
!
!  RESIDUAL OBTAINED BETWEEN s AND s AS FUNCTION OF T, D
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  DZ,T,S,calcs

      DITSRES = calcs(T,DZ) - S

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE THITER(T,H,D,EPS)
! -----------------------------------------------------------------------------
!
!  ITERATION OF DENSITY FUNCTION OF T AND h   (LOWER = NORMALLY)
!
!  INPUT:     T         TEMPERATURE [K]
!             h         ENTHALPY [KJ / (KG * K)]
!             EPS       ITERATION CONVERGENCE CRITERION
!
!  OUTPUT:    D         DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------
!

      IMPLICIT NONE
      REAL(KIND=8) ::  T,H,D,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  D1,H1,calch,DFAK,HDIFF1,D2,H2,HDIFF2,calcTHC
      REAL(KIND=8) ::  DV,DL,HV,HL,DIT,DITHRES,V,X,P,ABLTEST,THCRES
      REAL(KIND=8) ::  DTEST,HTEST,DEXTR,TCE,DCE,TOLD,HOLD,DOLD,DFAK1
      INTEGER IX,ISUBAKT,ISUBOLD,IREPEAT,IABLTEST
     integer imax
      REAL(KIND=8) ::  HDIFF1BEGIN,D1BEGIN

      EXTERNAL DITHRES,THCRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,HOLD,DOLD,ISUBOLD
      DATA DOLD / -1.D+0 /
      DATA TOLD / -1.D0 /
      DATA HOLD / 1.D+9 /
      DATA ISUBOLD / 0 /

      D = 0.0D+0
     imax = 200
      IABLTEST = 1

      IF ((DABS(T-TOLD).LT.1.D-8) .AND. (DABS(H-HOLD).LT.1.D-8)      &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         D = DOLD
         goto 1000
      END IF

! critical point

         TCE = TC
         DCE = DC

! DENSITY ENCLOSING INTERVAL

      D1 = 0.5D0*(DC + DLTR)

     IF (T .GE. TCE) THEN

! CLOSE TO CRITICAL POINT THE CRITICAL DENSITY IS USED AS START VALUR

         D1 = DCE

! CHECKING IF THE START VALUE IN THE RIGHT PART OF
!  EQ. (if dp/dh <0)

100      CONTINUE       
         ABLTEST = calcTHC(T,D1)
         IF (ABLTEST .GT. 0.D0) THEN
            D1 = D1 * 0.5D0
            GOTO 100
         ENDIF
         H1 = calch(T,D1)
         IF (H .LT. H1) THEN 
            DFAK = 1.1D0
         ELSE 
            DFAK = 0.9D0
         ENDIF
         HDIFF1 = H - H1
           D1BEGIN = D1
           HDIFF1BEGIN = HDIFF1
         IREPEAT = 0

200      CONTINUE
         D2 = D1 * DFAK
         ABLTEST = calcTHC(T,D2)

         IF ((ABLTEST .GT. 0.D0) .AND. (IABLTEST .EQ. 1))THEN
! IF NECESSARY, THE DENSITY OF ENTHALPY IS DETERMINED ITERATIVELY

            CALL ITPEGS(D1,D2,THCRES,T,1.D-6,DEXTR,IX)
            IF (IX .EQ. 4) THEN
               D = -111.D0
               goto 1000
            ENDIF
            HTEST = calch(T,DEXTR)

!  IF GIVEN ENTHALPIE IS LESS THAN THE MINIMUM VALUE, THE DENSITY CAN NOT BE DETERMNINED

            IF (HTEST .GT. H) THEN
               D = -111.D0
               goto 1000
            ELSE
               D2 = DEXTR
            ENDIF
         ENDIF
         H2 = calch(T,D2)
         HDIFF2 = H - H2

         IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 201

         H1 = H2
         HDIFF1 = HDIFF2
         D1 = D2
         IF (D1 .LT. 1.D-12) THEN
            D = -111.D0
            goto 1000
         ENDIF

         IREPEAT = IREPEAT + 1
           IF (IREPEAT .GT. imax) THEN
               IF (IABLTEST .EQ. 0) THEN
                   D = -111.D+0
                   goto 1000
               ENDIF
             DFAK = 1.1D+0
               D1 = D1BEGIN
               HDIFF1 = HDIFF1BEGIN
               IREPEAT = 0
               IABLTEST = 0
           ENDIF

         IF (IREPEAT .GT. 100) THEN
               IF (DFAK .GT. 1.0D+0) THEN
                   DFAK = 1.2D+0
               ELSE
                   DFAK = 0.8D+0
               ENDIF
           ENDIF

         GOTO 200

201      CONTINUE 

         IABLTEST = 1

! CLOSE TO CRITICAL CONDITIONS IS CHECKED IF THE START POINT IN TWO PHASE IS APPROPRIATED

      ELSEIF (T .GE. TTR) THEN
         CALL TSATIT(T,DV,DL,P,1.D-6)
         DTEST = DL + EPS
         ABLTEST = calcTHC(T,DTEST)
         HL = calch(T,DL)
         IF ((ABLTEST .GT. 0.D0) .OR. (H .GT. HL)) THEN
            HV = calch(T,DV)

! IF GIVEN ENTHALPY IS LESS THAN MINIMUM, THE CALC. IS NOT POSSIBLE

            IF (H .LT. HL) THEN
               D = -111.D0

! IN THE TWO PHASE REGION THE DENSITY IS CALCULATED DIRECTLY

            ELSEIF (H .LE. HV) THEN
               X = (H - HL) / (HV - HL)
               V = 1.D0 / DL + X * (1.D0 / DV - 1.D0 / DL)
               D = 1.D0 / V
               goto 1000

! IN GAS REGION, THE START VALUE IS VAPOR DENSITY

            ELSE
               H1 = HV
               D1 = DV
               HDIFF1 = H - H1
                 DFAK1 = 0.9D+0
300            CONTINUE
               D2 = D1 * DFAK1
               H2 = calch(T,D2)
               HDIFF2 = H - H2
               IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 301
               H1 = H2
               HDIFF1 = HDIFF2
               IF (D2 .LT. 1.D-15) THEN
                     D = -111.D+0
                     goto 1000
               ENDIF
               D1 = D2

               GOTO 300

301            CONTINUE
            ENDIF
         ELSE

! IN LIQUID REGION THE START VALUE IS THE SAT. LIQUID DENSITY

            D1 = DL
            H1 = HL
            HDIFF1 = H - H1

400         CONTINUE
            D2 = D1 * 1.05D0
            ABLTEST = calcTHC(T,D2)
            IF (ABLTEST .GT. 0.D0) THEN

! IF NECESSARY, THE DENSITY IS ITERATIVELY ESTIMATED BY THE MINIMUM ENTHALPY

               CALL ITPEGS(D1,D2,THCRES,T,1.D-6,DEXTR,IX)
               IF (IX .EQ. 4) THEN
                  D = -111.D0
                  goto 1000
               ENDIF
               HTEST = calch(T,DEXTR)

! IF THE GIVEN ENTHALPIE IS LESS THAN THE MINIMUM VALUE,
!  THE DENSITY CAN NOT BE DETERMINED

               IF (HTEST .GT. H) THEN
                  D = -111.D0
                  goto 1000
               ELSE
                  D2 = DEXTR
               ENDIF
            ENDIF
            H2 = calch(T,D2)
            HDIFF2 = H - H2

            IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 401

            H1 = H2
            HDIFF1 = HDIFF2
            D1 = D2
            GOTO 400

401         CONTINUE 

         ENDIF
      ELSE
         D = -111.D0
         goto 1000

      ENDIF

!  ITERATION OF DENSITY

      CALL ITPEG(D1,D2,DITHRES,T,H,EPS,DIT,IX)
      IF (IX .LE. 3) THEN
         D = DIT
      ELSE
         D = -111.D0
      ENDIF

1000 continue

      TOLD = T
      HOLD = H
      DOLD = D
      ISUBOLD = ISUBAKT

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function THCRES(DZ,T)
! -----------------------------------------------------------------------------
      IMPLICIT NONE
      REAL(KIND=8) ::  DZ,T,calcTHC

      THCRES = calcTHC(T,DZ)

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function DITHRES(DZ,T,H)
! -----------------------------------------------------------------------------
!  RESIDUAL: DIFFERENCE BETWEEN CALCULATED AND GIVEN ENTALPHY
! -----------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(KIND=8) ::  DZ,T,H,calch

      DITHRES = calch(T,DZ) - H

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE THITERZ(T,H,D,EPS)
! -----------------------------------------------------------------------------
!
! ITERATION OF DENSITY (UPPER = ADDITIONAL FALL) FUNCTION OF T AND h
!
!  INPUT:     T         TEMPERATURE [K]
!             H         ENTHALPY [KJ / KG]
!             EPS       ITERATION TOLERANCE
!
!  OUTPUT:    D         DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,H,D,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  D1,H1,calch,DFAK,HDIFF1,D2,H2,HDIFF2,calcTHC
      REAL(KIND=8) ::  DV,DL,HL,DIT,DITHRES,P,ABLTEST,TOLD,HOLD,DOLD
      REAL(KIND=8) ::  DTEST,DEXTR,HTEST,TCE,DCE,THCRES
      REAL(KIND=8) ::  ABLTEST2,calcdpdD,ABLTEST1
      INTEGER IX,ISUBAKT,ISUBOLD
      EXTERNAL DITHRES,THCRES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,HOLD,DOLD,ISUBOLD
      DATA DOLD / -1.D+0 /
      DATA TOLD / 1.D9 /
      DATA HOLD / 1.D9 /
      DATA ISUBOLD / 0 /

      D = 0.0D+0

      IF ((DABS(T-TOLD).LT.1.D-8) .AND. (DABS(H-HOLD).LT.1.D-8)      &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         D = DOLD
         goto 1000
      END IF

! CRITICAL POINT

         TCE = TC
         DCE = DC

!   DENSITY INTERVAL

      IF (T .GE. TCE) THEN

!   CRITICAL DENSITY AS START CLOSE TO CRITICAL POINT

         D1 = DCE

!   TESTING IF START VALUE IS IN THE RIGHT PART OF REGION (WITH dp/dh > 0)

100      CONTINUE       
         ABLTEST = calcTHC(T,D1)
         IF (ABLTEST .LT. 0.D0) THEN
            D1 = D1 * 2.D0
            GOTO 100
         ENDIF
         H1 = calch(T,D1)
         IF (H .LT. H1) THEN 
            DFAK = 0.9D0
         ELSE 
            DFAK = 1.1D0
         ENDIF
         HDIFF1 = H - H1

200      CONTINUE
         D2 = D1 * DFAK
         ABLTEST = calcTHC(T,D2)
         IF (ABLTEST .LT. 0.D0) THEN

! IF NECESSARY, THE DENSITY OF ENTHALPY MINIMUMS IS DETERMINED ITERATIVELY

            CALL ITPEGS(D1,D2,THCRES,T,1.D-6,DEXTR,IX)
            IF (IX .EQ. 4) THEN
               D = -111.D0
               goto 1000
            ENDIF
            HTEST = calch(T,DEXTR)

            IF (HTEST .GT. H) THEN
               D = -111.D0
               goto 1000
            ELSE
               D2 = DEXTR
            ENDIF
         ENDIF
         H2 = calch(T,D2)
         HDIFF2 = H - H2

         IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 201

         H1 = H2
         HDIFF1 = HDIFF2
         D1 = D2
         GOTO 200

201      CONTINUE 

! UNDER CRITICAL TEMPERATURES THE SAT. LIQ. DENSITY IS USED AS START VALUE

      ELSEIF (T .GE. TTR) THEN
         CALL TSATIT(T,DV,DL,P,1.D-6)
         DTEST = DL + EPS
         ABLTEST = calcTHC(T,DTEST)
         HL = calch(T,DL)
         IF (ABLTEST .GT. 0.D0) THEN

! 1 CASE: THE MINIMUM ENTHALPIE IS ON BOILING
!    GIVEN ENTHALPIE IS LESS THAN MINIMUM VALUE: DENSITY CAN NOT BE DETERMINED 

            IF (H .LT. HL) THEN
               D = -111.D0
            ELSE
               H1 = HL
               D1 = DL
               ABLTEST1 = calcTHC(T,D1) * calcdpdD(T,D1)

               HDIFF1 = H - H1
300            CONTINUE
               D2 = D1 * 1.05D0
               ABLTEST2 = calcTHC(T,D2) * calcdpdD(T,D2)
               H2 = calch(T,D2)
               HDIFF2 = H - H2
               IF ((HDIFF1 * HDIFF2) .LE. 0.D0) THEN
                  GOTO 301
               ELSEIF ((ABLTEST1 * ABLTEST2) .LE. 0.0D+0) THEN
                  GOTO 301
               ENDIF

               H1 = H2
               HDIFF1 = HDIFF2
               ABLTEST1 = ABLTEST2
               D1 = D2
               GOTO 300

301            CONTINUE
            ENDIF
         ELSE

! 2. CASE: THE ENTHALPY MINIMUM LIES IN HOMOGENEOUS LIQUID REGION

            D1 = DL * 1.05D0
400         CONTINUE
            ABLTEST = calcTHC(T,D1)
            IF (ABLTEST .LT. 0.D0) THEN
               D1 = D1 * 1.05D0
               GOTO 400
            ENDIF
            H1 = calch(T,D1)
            HDIFF1 = H - H1

            IF (H .LT. H1) THEN 
               DFAK = 0.99D0
            ELSE 
               DFAK = 1.05D0
            ENDIF

500         CONTINUE
            D2 = D1 * DFAK
            ABLTEST = calcTHC(T,D2)
            IF (ABLTEST .LT. 0.D0) THEN

! IF NECESSARY, THE ENTHALPY OF DENSITY   IS DETERMINED ITERATIVELY

               CALL ITPEGS(D1,D2,THCRES,T,1.D-6,DEXTR,IX)
               IF (IX .EQ. 4) THEN
                  D = -111.D0
                  goto 1000
               ENDIF
               HTEST = calch(T,DEXTR)

! GIVEN ENTHALPY IS LESS THAN THE MINIMUM

               IF (HTEST .GT. H) THEN
                  D = -111.D0
                  goto 1000
               ELSE
                  D2 = DEXTR
               ENDIF
            ENDIF
            H2 = calch(T,D2)
            HDIFF2 = H - H2

            IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 501

            H1 = H2
            HDIFF1 = HDIFF2
            D1 = D2
            GOTO 500

501         CONTINUE 

         ENDIF
      ELSE
         D = -111.D0
         goto 1000

      ENDIF

!  ITERATION OF DENSITY

      CALL ITPEG(D1,D2,DITHRES,T,H,EPS,DIT,IX)
      IF (IX .LE. 3) THEN
         D = DIT
      ELSE
         D = -111.D0
      ENDIF

1000 continue

      TOLD = T
      HOLD = H
      DOLD = D
      ISUBOLD = ISUBAKT

      RETURN
      END

! -----------------------------------------------------------------------------
      SUBROUTINE PDITER(P,D,T,EPS)
! -----------------------------------------------------------------------------
!
!  ITERATION OF TEMPERATURE   FUNCTION OF p AND D
!
!  INPUT:     P         PRESSURE [MPa]
!             D         DENSITY [KG / M ** 3]
!             EPS       ITERATION TOLERANCE
!
!  OUTPUT:    T         TEMPERATURE [K]
!
!  MAY ALSO   D         DENSITY [KG / M ** 3]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,P,D,EPS
      REAL(KIND=8) ::  DPDTTEST,TEST,T1,T2,TFAK,TS,P1,P2,PDIFF1,PDIFF2
      REAL(KIND=8) ::  TVDW,calcp,DLEQN,DVEQN,TIPDRES,X,DLH,DVH
      REAL(KIND=8) ::  TSR,DVR,DLR,PTEST,TCE,PCE,DCE
      REAL(KIND=8) ::  calcdpdT
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,TVPIT,DOLD,POLD,TOLD
      INTEGER IX,ISUBAKT,ISUBOLD

      EXTERNAL TIPDRES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,POLD,DOLD,ISUBOLD
      DATA TOLD / -1.D+0 /
      DATA DOLD / 1.D9 /
      DATA POLD / 1.D9 /
      DATA ISUBOLD / 0 /

      IF ((DABS(D-DOLD).LT.1.D-15) .AND. (DABS(P-POLD).LT.1.D-15)   &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TOLD
         goto 1000
      END IF

! CRITICAL POINT

         TCE = TC
         DCE = DC
         PCE = PC

! DETERMINING TEMPERATURE INTERVAL T1, T2

      IF (P .GT. PCE) THEN

! CLOSE TO THE CRITICAL PRESSURE THE START VALUE WILL BE
!  DETERMINED BY THE Van der Waals EQUATION

50       CONTINUE
         IF (D .LE. DCE) THEN
            T1 = TVDW(P,D)
         ELSE
            T1 = TCE
         ENDIF

         P1 = calcp(T1,D)
         PDIFF1 = P - P1
         IF (P1 .LT. P) THEN
            TFAK = 1.05D0
         ELSE
            TFAK = 0.95D0
         ENDIF

100      CONTINUE         

         T2 = T1 * TFAK
         P2 = calcp(T2,D)
         PDIFF2 = P - P2
           DPDTTEST = calcdpdT(T2,D)
           IF (DPDTTEST .LT. 0.0D+0) THEN
               D = D + 100.D+0
             GOTO 50
           ENDIF
         IF ((PDIFF1 * PDIFF2) .LE. 0.D0) GOTO 101
         T1 = T2
         P1 = P2
         PDIFF1 = PDIFF2
         GOTO 100

101      CONTINUE

      ELSE
         TS = TVPIT(P)
         DVH = DVEQN(TS)
         DLH = DLEQN(TS)

! IF THE POINT IS IN THE WET STEAM REGION OR NEAR PHASE LIMIT, 
!   THE SAT. AND VAPOR DENSITIES ARE ITERATED FROM FUNDAMENTAL EQUATION

         IF ((D .GT. (0.6D0 * DVH)) .AND. (D .LT. (1.3D0 * DLH))) THEN
            CALL PSATIT(TSR,DVR,DLR,P,1.D-6)
            TS = TSR
            DVH = DVR
            DLH = DLR
         ENDIF

!  UNDER CRITICAL PRESSURES AND D <0.5 * DV 
!   THE START VALUE IS DETERMINED BY EQUATION OF Van der Waals

         IF (D .LT. (0.5D0 * DVH)) THEN
            TEST = TVDW(P,D)
            T1 = TEST * 0.9D0
            T2 = TEST * 1.1D0

!   UNDER CRITICAL PRESSURES AND 0.5*DV < D < DV D
!    THE START VALUE OF D IS OBAITNED BY SAT. TEMP.

         ELSEIF (D .LE. DVH) THEN
            T1 = TS
            P1 = calcp(T1,D)
            PDIFF1 = P - P1
            IF (P1 .LT. P) THEN
               TFAK = 1.02D0
            ELSE
               TFAK = 0.95D0
            ENDIF

200         CONTINUE  

            T2 = T1 * TFAK
            P2 = calcp(T2,D)
            PDIFF2 = P - P2
            IF ((PDIFF1 * PDIFF2) .LE. 0.D0) GOTO 201
            T1 = T2
            P1 = P2
            PDIFF1 = PDIFF2
            GOTO 200

201         CONTINUE

!  UNDER CRITICAL PRESSURES AND D > DL THE START VALUE
!  USES THE SAT. TEMPERATURE

         ELSEIF (D .GE. DLH) THEN
            T1 = TS
            P1 = calcp(T1,D)
            PDIFF1 = P - P1
            IF (P1 .LT. P) THEN
               TFAK = 1.02D0
            ELSE
               TFAK = 0.98D0
            ENDIF

300         CONTINUE  

            T2 = T1 * TFAK
            P2 = calcp(T2,D)
            PDIFF2 = P - P2
            IF ((PDIFF1 * PDIFF2) .LE. 0.D0) GOTO 301
            T1 = T2
            P1 = P2
            PDIFF1 = PDIFF2
            GOTO 300

301         CONTINUE

! IN ND AREA THE SAT. TEMPERATURE WILL BE USED TO ITERATE

         ELSE
            T = TS
            goto 1000
         ENDIF
      ENDIF

!  ITERATION OF TEMPERATURE

      CALL ITPEG(T1,T2,TIPDRES,P,D,EPS,X,IX)

      IF (IX .EQ. 0) THEN
         T = X
      ELSEIF (IX .LE. 3) THEN
         PTEST = calcp(X,D)
         IF (DABS(PTEST - P) .LT. (EPS * 1.D1)) THEN
            T = X
         ELSE
            T = -111.D0
         ENDIF
      ELSE
         T = -111.D0
      ENDIF

1000 continue

      TOLD = T
      POLD = P
      DOLD = D
      ISUBOLD = ISUBAKT

      RETURN
      END

 ! -----------------------------------------------------------------------------
      REAL(KIND=8) function TIPDRES(T,P,D)
! -----------------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE OF PRESSURE.
!  DURING ITERATION OF DENSITY EVALUATION FUNCTION OF P AND D
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,P,D,calcp

      TIPDRES = P - calcp(T,D)

      RETURN
      END

! -----------------------------------------------------------------------------
      REAL(KIND=8) function TVDW(P,D)
! -----------------------------------------------------------------------------
!
!  START VALUE OF TEMPERATURE FROM van der Waals EQUATION
!
!  INPUT:     P         PRESSURE [MPa]
!             D         DENSITY [KG / M ** 3]
!             EPS       ITERATION TOLERANCE
!
!  OUTPUT:    T         TEMPERATURE [K]
!
! -----------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  P,D,  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF, A,B,V

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF

      A = 27.D0 / 64.D0 * R * R * TC * TC / PC
      B = R * TC / (8.D0 * PC) * 1.D-3
      V = 1.D0 / D

      TVDW = (P * 1.D6 + A / (V * V)) * (V - B) / R * 1.D-3

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function VPHRES(T,P)
! ---------------------------------------------------------------------------------------
!
!   DIFFERENCE BETWEEN A GIVEN AND A CALCULATED VAPOR PRESSURE FROM FPEQN
! ---------------------------------------------------------------------------------------
      IMPLICIT NONE
      REAL(KIND=8) ::  T,P,PS,VPEQN

      PS = VPEQN(T)
      VPHRES = P - PS

      RETURN
      END

! ---------------------------------------------------------------------------------------
      SUBROUTINE DSITER (D,S,T,EPS)
! ---------------------------------------------------------------------------------------
!
!   ITERATION OF TEMPERATURE FROM D AND s
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!             S         ENTROPY [KJ / (KG * K)]
!             EPS       CONVERGENCE CRITERION
!
!  OUTPUT:    T         TEMPERATURE [K]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,S,D,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC, TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  T1,S1, calcs,TFAK,SDIFF1,T2,S2,SDIFF2
      REAL(KIND=8) ::  DV,DL,SV,SL,TIT,TIDSRES,TIDS2RES,V,X,P
      REAL(KIND=8) ::  TSAT,SSAT,STR,V1,V2,TDLIT,TDVIT, SCD
      REAL(KIND=8) ::  TCE,DCE,STEST,TOLD,DOLD,SOLD,STEST1
      REAL(KIND=8) ::  SLTEST,SVTEST,XTP,PTP,DVTP,DLTP
      INTEGER IX,ISUBAKT,ISUBOLD,ITTR, icode

      EXTERNAL TIDSRES,TIDS2RES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT
      COMMON / CODE / icode

      SAVE TOLD,SOLD,DOLD,ISUBOLD
      DATA TOLD / -1.D+0 /
      DATA DOLD / -1.D+0 /
      DATA SOLD / 1.D+9 /
      DATA ISUBOLD / 0 /

      IF ((DABS(D-DOLD).LT.1.D-15) .AND. (DABS(S-SOLD).LT.1.D-15)  &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TOLD
         goto 1000
      END IF

!   CRITICAL POINT

         TCE = TC
         DCE = DC

! IF DENSITY GREATER THAN THE DENSITY OF LIQUID   PHASE AT TRIPLE POINT, 
!   THE START VALUE WILL BE THE CRITICAL TEMPERATURE

      IF (D .GE. DLTR) THEN
         T1 = TCE
         S1 = calcs(T1,D)
         IF (S .LT. S1) THEN 
            TFAK = 0.9D0
         ELSE 
            TFAK = 1.2D0
         ENDIF
         SDIFF1 = S - S1

!     TEMPERATURE INTERVAL T1, T2

100      CONTINUE
         T2 = T1 * TFAK
         S2 = calcs(T2,D)
         SDIFF2 = S - S2
         IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 101
         S1 = S2
         SDIFF1 = SDIFF2
         T1 = T2
         GOTO 100

101      CONTINUE 

!   ITERATION

         CALL ITPEG(T1,T2,TIDSRES,D,S,EPS,TIT,IX)
         IF (IX .LE. 3) THEN
            T = TIT
         ELSE
            T = -111.D0
         ENDIF

!  IF GIVEN DENSITY IS SMALLER THAN THE LIQUID DENSITY AND GREATER THAN VAPOR DENSITY
!  AT TRIPLE POINT, IS CHECKED IF IT IS IN THE TWO PHASE REGION

      ELSEIF (D .GE. DVTR) THEN

!   FIRST REVIEW WITH VAPOR AND SAT. LIQUID DENSITIES

         IF (D .GE. DCE) THEN 
            TSAT = TDLIT(D)
         ELSE
            TSAT = TDVIT(D)
         ENDIF

          IF (TSAT .LT. TTR) THEN
              TSAT = TTR
          ENDIF

         SSAT = calcs(TSAT,D)

!    CALCULATION OF PHASE LIMIT FROM THE FUNDAMENTAL EQUATION

         IF (SSAT .GT. 0.D0) THEN
            STEST = 1.2D0 * SSAT
         ELSE
            STEST = 0.8D0 * SSAT
         ENDIF

         IF (S .LT. STEST) THEN
            IF (D .GE. DCE) THEN 
               CALL DLSATIT(T,DV,D,P,1.D-6)
               TSAT = T
            ELSE
               CALL DVSATIT(T,D,DL,P,1.D-6)
               TSAT = T
            ENDIF
            SSAT = calcs(TSAT,D)
         ENDIF 

!   THE START VALUE   FOR THE HOMOGENEOUS REGION IS THE CRITICAL TEMPERATURE
!    (FOR s > s(TC, D)) OR THE SATURATION TEMPERATURE

         IF (S .GE. SSAT) THEN
            SCD = calcs(TCE,D)        ! **************************************************************
            IF (S .GT. SCD) THEN
               T1 = TCE
               S1 = SC
            ELSE
               T1 = TSAT
               S1 = SSAT
            ENDIF
            TFAK = 1.2D0
            SDIFF1 = S - S1

200         CONTINUE
!         TEMPERATURE INTERVAL

            T2 = T1 * TFAK
            S2 = calcs(T2,D)
            SDIFF2 = S - S2
            IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 201
            S1 = S2
            SDIFF1 = SDIFF2
            T1 = T2
            GOTO 200

201         CONTINUE 

!  ITERATION

            CALL ITPEG(T1,T2,TIDSRES,D,S,EPS,TIT,IX)
            IF (IX .LE. 3) THEN
               T = TIT
                 XTP = 2.D0
                 IF (T .LE. TC) THEN
                     CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
                 ENDIF
                 IF (XTP .GT. 1.5D0) THEN
                      STEST1 = calcs(T,D)
                 ELSE
                     SLTEST = calcs(T,DLTP)
                     SVTEST = calcs(T,DVTP)
                     STEST1 = SLTEST + XTP * (SVTEST - SLTEST)
                 ENDIF

                 IF (ABS(STEST1 - S) .GT. 1.D-6) THEN
                     T1 = T2
                     GOTO 200
                 ENDIF
            ELSE
               T = -111.D0
            ENDIF

         ELSE
            SLTR = calcs(TTR,DLTR)
            SVTR = calcs(TTR,DVTR)
            V1 = 1.D0 / DLTR
            V2 = 1.D0 / DVTR
            V = 1.D0 / D
            X = (V - V1) / (V2 -V1)
            STR = SLTR + X * (SVTR - SLTR)
            IF (S .LT. STR) THEN
               T = -111.D0
               goto 1000
            ENDIF

!   THE START VALUE IN TWO PHASE REGION IS THE SATURATION TEMPERATURE

            T1 = TSAT
            S1 = SSAT
            TFAK = 0.9D0
            SDIFF1 = S - S1

!   TEMPERATURE INTERVAL
!      ( IF T1 IS SO LOW THAT T2 FALLS UNDER TTR WILL CAUSE AN ENDLESS LOOP )

            ITTR = 0
300         CONTINUE
            T2 = T1 * TFAK

            IF (T2 .LT. TTR) THEN
              T2 = TTR
           ITTR = ITTR + 1
           IF (ITTR .GT. 20) THEN
               T = -111.D+0
              icode = -1028
              goto 1000
             ENDIF
            ENDIF

            CALL TSATIT(T2,DV,DL,P,1.D-6)
            V1 = 1.D0 / DL
            V2 = 1.D0 / DV
            V = 1.D0 / D
            X = (V - V1) / (V2 -V1)
            SL = calcs(T2,DL)
            SV = calcs(T2,DV)
            S2 = SL + X * (SV - SL)
            SDIFF2 = S - S2
            IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 301
            S1 = S2
            SDIFF1 = SDIFF2
            T1 = T2
            GOTO 300

301         CONTINUE 

!  ITERATION
            CALL ITPEGB(T1,T2,TIDS2RES,D,S,EPS,TIT,IX)
            IF (IX .LE. 3) THEN
               T = TIT
            ELSE
               T = -111.D0
            ENDIF

         ENDIF
      ELSE

!  IF GIVEN DENSITY IS LOWER THAN THE VAPOR DENSITY AT TRIPLE POINT,
!   THE START VALUE WILL BE THE TRIPLE POINT TEMPERATURE

         T1 = TTR
         S1 = calcs(T1,D)
         IF (S .LT. S1) THEN
            TFAK = 0.9D0
         ELSE
            TFAK = 1.1D0
         ENDIF
         SDIFF1 = S - S1

!   TEMPERATURE INTERVAL

400      CONTINUE
         T2 = T1 * TFAK
         S2 = calcs(T2,D)
         SDIFF2 = S - S2
         IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 401
         S1 = S2
         SDIFF1 = SDIFF2
         T1 = T2
         GOTO 400

401      CONTINUE 

!  ITERATION

         CALL ITPEG(T1,T2,TIDSRES,D,S,EPS,TIT,IX)
         IF (IX .LE. 3) THEN
            T = TIT
         ELSE
            T = -111.D0
         ENDIF
      ENDIF

1000 continue

      TOLD = T
      SOLD = S
      DOLD = D
      ISUBOLD = ISUBAKT

      RETURN
      END

! ---------------------------------------------------------------------------------------
      SUBROUTINE DHITER (D,H,T,EPS)
! ---------------------------------------------------------------------------------------
!
!  ITERATION OF TEMPERATUR FOR GIVEN D AND h
!
!  INPUT:     D         DENSITY [KG / M ** 3]
!             H         ENTHALPY [KJ / KG]
!             EPS       CONVERGENCE CRITERION
!
!  OUTPUT:    T         TEMPERATURE [K]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  T,H,D,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  T1,H1,calch,TFAK,HDIFF1,T2,H2,HDIFF2,HTEST
      REAL(KIND=8) ::  DV,DL,HV,HL,TIT,TIDHRES,TIDH2RES,V,X,P
      REAL(KIND=8) ::  TSAT,HSAT,HTR,V1,V2,TDLIT,TDVIT,HLTR,HVTR
      REAL(KIND=8) ::  TCE,DCE,TOLD,DOLD,HOLD,HTEST1
      REAL(KIND=8) ::  HLTEST,HVTEST,XTP,PTP,DLTP,DVTP
      INTEGER IX,ISUBAKT,ISUBOLD
      EXTERNAL TIDHRES,TIDH2RES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,HOLD,DOLD,ISUBOLD
      DATA TOLD / -1.D+0 /
      DATA DOLD / -1.D+0 /
      DATA HOLD / 1.D+9 /
      DATA ISUBOLD / 0 /

      IF ((DABS(D-DOLD).LT.1.D-8) .AND. (DABS(H-HOLD).LT.1.D-8)     &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TOLD
         goto 1000
      END IF

! CRITICAL POINT

         TCE = TC
         DCE = DC

!  IF IS GIVEN A DENSITY GREATER THAN SAT. LIQ. DENSITY AT TRIPLE POINT,
!   THE START VALUE IS THE CRITICAL TEMPERATURE

      IF (D .GT. DLTR) THEN
         T1 = TCE
         H1 = calch(T1,D)
         IF (H .LT. H1) THEN 
            TFAK = 0.9D0
         ELSE 
            TFAK = 1.2D0
         ENDIF
         HDIFF1 = H - H1

!   TEMPERATURE INTERVAL

100      CONTINUE
         T2 = T1 * TFAK
         H2 = calch(T2,D)
         HDIFF2 = H - H2
         IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 101
         H1 = H2
         HDIFF1 = HDIFF2
         T1 = T2
         GOTO 100

101      CONTINUE 

!  ITERATION

         CALL ITPEG(T1,T2,TIDHRES,D,H,EPS,TIT,IX)
         IF (IX .LE. 3) THEN
            T = TIT
         ELSE
            T = -111.D0
         ENDIF

!  IF THE GIVEN DENSITY IS LOWER THAN THE SAT. LIQ. DENSITY AND GREATER 
!   THAN THE VAPOR DENSITY AT TRIPLE POINT, IT IS CHECKED IF THE STATE IS IN TE TWO PHASE REGION

      ELSEIF (D .GE. DVTR) THEN

!    FIRST REVIEW WITH AUXILIARY EQUATIONS FOR SAT. LIQ. AND VAPOR DENSITIES

         IF (D .GE. DCE) THEN 
            TSAT = TDLIT(D)
         ELSE
            TSAT = TDVIT(D)
         ENDIF

         HSAT = calch(TSAT,D)

!   REVISION OF PHASE LIMIT FROM THE FUNDAMENTAL EQUATION

         IF (HSAT .GT. 0.D0) THEN
            HTEST = 1.2D0 * HSAT
         ELSE
            HTEST = 0.8D0 * HSAT
         ENDIF

         IF (H .LT. HTEST) THEN
            IF (D .GE. DCE) THEN 
               CALL DLSATIT(T,DV,D,P,1.D-6)
               TSAT = T
            ELSE
               CALL DVSATIT(T,D,DL,P,1.D-6)
               TSAT = T
            ENDIF
            HSAT = calch(TSAT,D)
         ENDIF 

! THE START VALUE IN THE HOMOGENEOUS REGION IS THE CRITICAL TEMPERATURE (FOR h > h(TC, D)) OR THE
!   SAT.TEMPERATURE

         IF (H .GE. HSAT) THEN
            HC = calch(TCE,D)
            IF (H .GT. HC) THEN
               T1 = TCE
               H1 = HC
            ELSE
               T1 = TSAT
               H1 = HSAT
            ENDIF
            TFAK = 1.2D0
            HDIFF1 = H - H1

200         CONTINUE
!   TEMPERATURE INTERVAL
            T2 = T1 * TFAK
            H2 = calch(T2,D)
            HDIFF2 = H - H2
            IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 201
            H1 = H2
            HDIFF1 = HDIFF2
            T1 = T2
            GOTO 200

201         CONTINUE 

!  ITERATION
            CALL ITPEG(T1,T2,TIDHRES,D,H,EPS,TIT,IX)
            IF (IX .LE. 3) THEN
               T = TIT
               XTP = 2.D0
               IF (T .LE. TC) THEN
                   CALL QUALY(T,D,XTP,DVTP,DLTP,PTP)
               ENDIF
               IF (XTP .GT. 1.5D0) THEN
                   HTEST1 = calch(T,D)
               ELSE
                   HLTEST = calch(T,DLTP)
                   HVTEST = calch(T,DVTP)
                   HTEST1 = HLTEST + XTP * (HVTEST - HLTEST)
               ENDIF

               IF (ABS(HTEST1 - H) .GT. 1.D-6) THEN
                   T1 = T2
                   GOTO 200
               ENDIF
            ELSE
               T = -111.D0
            ENDIF

         ELSE
            HLTR = calch(TTR,DLTR)
            HVTR = calch(TTR,DVTR)
            V1 = 1.D0 / DLTR
            V2 = 1.D0 / DVTR
            V = 1.D0 / D
            X = (V - V1) / (V2 -V1)
            HTR = HLTR + X * (HVTR - HLTR)
            IF (H .LT. HTR) THEN
               T = -111.D0
               goto 1000
            ENDIF

! SAT. TEMPERATUURE USED AS STAR VALUE IN TWO PHASE REGION

            T1 = TSAT
            H1 = HSAT
            TFAK = 0.9D0
            HDIFF1 = H - H1

! TEMPERATURE INTERVAL

300         CONTINUE
            T2 = T1 * TFAK
            IF (T2 .LT. TTR) THEN
                T2 = TTR
            ENDIF
            CALL TSATIT(T2,DV,DL,P,1.D-6)
            V1 = 1.D0 / DL
            V2 = 1.D0 / DV
            V = 1.D0 / D
            X = (V - V1) / (V2 -V1)
            HL = calch(T2,DL)
            HV = calch(T2,DV)
            H2 = HL + X * (HV - HL)
            HDIFF2 = H - H2
            IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 301
            H1 = H2
            HDIFF1 = HDIFF2
            T1 = T2
            GOTO 300

301         CONTINUE 

!  ITERATION
            CALL ITPEGB(T1,T2,TIDH2RES,D,H,EPS,TIT,IX)
            IF (IX .LE. 3) THEN
               T = TIT
            ELSE
               T = -111.D0
            ENDIF

         ENDIF
      ELSE

!  IF GIVEN DENSITY IS LOWER THAN THE VAPOR DENSITY AT TRIPLE POINT, THE
! START VALUE IS THE TRIPLE POINT TEMPERATURE

         T1 = TTR
         H1 = calch(T1,D)
         IF (H .LT. H1) THEN
            TFAK = 0.9D0
         ELSE
            TFAK = 1.1D0
         ENDIF
         HDIFF1 = H - H1

400      CONTINUE
!   TEMPERATURE INTERVAL
         T2 = T1 * TFAK
         H2 = calch(T2,D)
         HDIFF2 = H - H2
         IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 401
         H1 = H2
         HDIFF1 = HDIFF2
         T1 = T2
         GOTO 400

401      CONTINUE 

!  ITERATION
         CALL ITPEG(T1,T2,TIDHRES,D,H,EPS,TIT,IX)
         IF (IX .LE. 3) THEN
            T = TIT
         ELSE
            T = -111.D0
         ENDIF
      ENDIF

1000 continue

      TOLD = T
      HOLD = H
      DOLD = D
      ISUBOLD = ISUBAKT

      RETURN
      END


! ---------------------------------------------------------------------------------------
      SUBROUTINE PSITER (P,S,T,D,EPSS)
! ---------------------------------------------------------------------------------------
!
!  ITERATION OF TEMPERATURA AND DENSITY FUNCTION OF p AND s
!
!  INPUT:     P         PRESSURE [MPa]
!             S         ENTROPY [KJ / (KG * K)]
!             EPS       CONVERGENCE CRITERION
!
!  OUTPUT:    T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  P,S,T,D,EPSS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,DLEQN
      REAL(KIND=8) ::  D1,TH,T1,S1,calcs,TFAK,SDIFF1,T2,D2,S2,DH,SDIFF2
      REAL(KIND=8) ::  TS,DV,DL,SV,SL,DIT,TIT,TDIPSRES,V,X,TVPIT,DVEQN
      REAL(KIND=8) ::  calcCp,CPTEST,P1,CACHE
      REAL(KIND=8) ::  TCE,PCE,DCE,SLTEST,SVTEST,TOLD,DOLD,POLD,SOLD
      INTEGER IX,ISUBAKT,ISUBOLD
      EXTERNAL TDIPSRES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,POLD,DOLD,SOLD,ISUBOLD
      DATA TOLD / -1.D+0 /
      DATA DOLD / -1.D+0 /
      DATA SOLD / 1.D9 /
      DATA POLD / 1.D9 /
      DATA ISUBOLD / 0 /

      IF ((DABS(S-SOLD).LT.1.D-8) .AND. (DABS(P-POLD).LT.1.D-8)    &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TOLD
         D = DOLD
         goto 1000
      END IF

!   CRITICAL POINT

         TCE = TC
         DCE = DC
         PCE = PC

! TEMPERATURE AND DENSITY INTERVAL

      IF (P .GE. PCE) THEN

! CLOSE TO CRITICAL PRESSURE THE CRITICAL DENSITY WILL BE USED AS START VALUE

         IF (P .GT. (10.D+0 * PCE)) THEN
             D1 = 2.D+0 * DCE
      ELSE
          D1 = DCE
      ENDIF

!  ITERATION OF TEMPERATURE
         CALL PDITER(P,D1,TH,1.D-6)
         T1 = TH
         S1 = calcs(T1,D1)
         IF (S .GT. S1) THEN 
            TFAK = 1.2D0
            IF (P .LT. (1.2D0 * PCE)) THEN
               TFAK = 1.01D0
            ENDIF
         ELSE 
            TFAK = 0.9D0
            IF (P .LT. (1.2D0 * PCE)) THEN
               TFAK = 0.99D0
            ENDIF
         ENDIF
         SDIFF1 = S - S1

100      CONTINUE

         IF ((T1 .GT. (2.D0 * TC)) .AND. (TFAK .GT. 1.15D0)) THEN
            TFAK = 1.02D0
         ENDIF

         T2 = T1 * TFAK

         IF (T2 .LT. TTR) THEN
            T2 = TTR
            CALL TPITER(T2,P,DH,1.D-9)
            D2 = DH
            S2 = calcs(T2,D2)
            IF (S .LT. S2) THEN 
               T = -111.D0
               D = -111.D0
               goto 1000
            ELSE
               GOTO 101
            ENDIF
         ENDIF

         CALL TPITER(T2,P,DH,1.D-6)
         D2 = DH
         S2 = calcs(T2,D2)
         SDIFF2 = S - S2
         IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 101
         S1 = S2
         SDIFF1 = SDIFF2
         T1 = T2
         D1 = D2
         GOTO 100

101      CONTINUE 

! UNDER CRITICAL PRESSURE FIRST IS CHECKED IF THE STATE IS IN THE TWO PHASE REGION

      ELSEIF (P .GE. PTR) THEN

! FIRST REVIEW WITH AUXILIARY EQUATIONS FOR SAT. TEMP. AND SAT. DENSITIES

         TS = TVPIT(P)
         DV = DVEQN(TS)
         SV = calcs(TS,DV)
         DL = DLEQN(TS)
         SL = calcs(TS,DL)

! DETERMING THE PHASE LIMIT FROM THE FUNDAMENTAL EQUATION

         IF (SL .GT. 0.D0) THEN
            SLTEST = 0.9D0 * SL
         ELSE
            SLTEST = 1.1D0 * SL
         ENDIF

         IF (SV .GT. 0.D0) THEN
            SVTEST = 1.2D0 * SV
         ELSE
            SVTEST = 0.8D0 * SV
         ENDIF

         IF ((SLTEST .LT. S) .AND. (SVTEST .GT. S)) THEN
            CALL PSATIT(TS,DV,DL,P,1.D-9)
            SV = calcs(TS,DV)
            SL = calcs(TS,DL)
         ENDIF

! IN TWO PHASE REGION THE DENSITY IS CALCULATED DIRECTLY

         IF ((SL .LE. S) .AND. (SV .GE. S)) THEN
            T = TS
            X = (S - SL) / (SV - SL)
            V = 1.D0 / DL + X * (1.D0 / DV - 1.D0 / DL)
            D = 1.D0 / V
            goto 1000

! IN THE HOMOGENEOUS REGION THE START VALUES ARE THE SAT. TEMP. AND SAT. DENSITIES

         ELSEIF (S .GT. SV) THEN
            TFAK = 1.2D0
            S1 = SV
            D1 = DV
         P1 = P
         ELSEIF (S .LT. SL) THEN
            TFAK = 0.95D0
            S1 = SL
            D1 = DL
         END IF

         T1 = TS
         SDIFF1 = S - S1

200      CONTINUE

         IF ((T1 .GT. (2.D0 * TC)) .AND. (TFAK .GT. 1.D0)) THEN
            TFAK = 1.02D0
         ENDIF

         T2 = T1 * TFAK
!C
         IF (T2 .LT. TTR) THEN
            T2 = TTR
            CALL TPITER(T2,P,DH,1.D-9)
            D2 = DH
            S2 = calcs(T2,D2)
            IF (S .LT. S2) THEN 
               T = -111.D0
               D = -111.D0
               goto 1000
            ELSE
               GOTO 201
            ENDIF
         ENDIF

! ITERATION OD DENSITY

         CALL TPITER(T2,P,DH,1.D-6)
         D2 = DH
         S2 = calcs(T2,D2)
         CPTEST = calcCp(T2,D2)
         IF (CPTEST .LE. 0.D0) THEN
            T = -111.D0
            D = -111.D0
            goto 1000
         ENDIF
         SDIFF2 = S - S2

         IF ((SDIFF1 * SDIFF2) .LE. 0.0D+0) GOTO 201
         S1 = S2
         SDIFF1 = SDIFF2
         T1 = T2
         D1 = D2
         GOTO 200

201      CONTINUE 

      ELSE

! FOR PRESSURES UNDER TRIPLE POINT PRESS., THE START VALUES ARE
!  THE TRIPLE POIN TEMPERATURE AND THE CRITICAL TEMPERATURE

         T1 = TTR

! ITERATION OF DENSITY
         CALL TPITER(T1,P,DH,1.D-6)
         D1 = DH
         S1 = calcs(T1,D1)
         T2 = TCE

! ITERATION OF DENSITY

         CALL TPITER(T2,P,DH,1.D-6)
         D2 = DH
         S2 = calcs(T2,D2)
         IF (S .GT. S2) THEN
            TFAK = 1.2D0
            S1 = S2
            SDIFF1 = S - S1
        T1 = T2
            D1 = D2
300         CONTINUE
            T2 = T1 * TFAK

! ITERATION OF DENSITY
            CALL TPITER(T2,P,DH,1.D-6)
            D2 = DH
            S2 = calcs(T2,D2)
            SDIFF2 = S - S2
            IF ((SDIFF1 * SDIFF2) .LE. 0.D0) GOTO 301
            S1 = S2
            SDIFF1 = SDIFF2
            T1 = T2
            D1 = D2
            GOTO 300

301         CONTINUE
         ENDIF

      ENDIF

!  ITERATION OF TEMPERATURE AND DENSITY
      CALL ITPEGZ(T1,D1,T2,D2,TDIPSRES,P,S,EPSS,TIT,DIT,IX)
      IF (IX .LE. 3) THEN
         T = TIT
         D = DIT
      ELSE
         T = -111.D0
         D = -111.D0
      ENDIF

1000 continue

      TOLD = T
      SOLD = S
      DOLD = D
      POLD = P
      ISUBOLD = ISUBAKT

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function TIDHRES(TZ,D,H)
! ---------------------------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A GIVEN AND A CALCULATED ENTHALPY, FUNCTION OF D AND H
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,D,H,calch

      TIDHRES = calch(TZ,D) - H

      RETURN
      END


!---------------------------------------------------------------------
      REAL(KIND=8) function TIDSRES(TZ,D,S)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A   GIVEN AND A CALCULATED VALUE FOR ENTROPY.
!  FUNCTION OF D AND s
!
!---------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,D,S,calcs
!
      TIDSRES = calcs(TZ,D) - S

      RETURN
      END


!---------------------------------------------------------------------
      REAL(KIND=8) function TDIPSRES(TZ,DZ,DIT,P,S)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A   GIVEN AND A CALCULATED VALUE FOR ENTROPY.
!  AT ITERATION OF TEMPERATURE AND DENSITY FOR VALUES
!  OF P AND s
!
!---------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,DZ,P,S,D1,D2,DITPRES,XD,D,calcs,DIT
      INTEGER IXD
      EXTERNAL DITPRES

      D1 = DZ * 0.98D0
      D2 = DZ * 1.02D0

! ITERATION OF DENSITY

      CALL ITPEG(D1,D2,DITPRES,TZ,P,1.D-9,XD,IXD)
      D = XD
      TDIPSRES = calcs(TZ,D) - S
      DIT = D

      RETURN
      END

!---------------------------------------------------------------------
      SUBROUTINE PHITER (P,H,T,D,EPSH)
!---------------------------------------------------------------------
!
!  ITERATION OF TEMPERATURE AND DENSITY FUNCTION OF p AND h
!
!  INPUT:     P         PRESSURE [MPa]
!             H         ENTHALPY [KJ / KG]
!             EPS       CONVERGENCE TOLERANCE
!
!  OUTPUT:    T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!---------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  P,H,T,D,EPSH
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF,TVPIT
      REAL(KIND=8) ::  D1,TH,T1,H1,calch,TFAK,HDIFF1,T2,D2,H2,DH,HDIFF2
      REAL(KIND=8) ::  TS,DV,DL,HV,HL,DIT,TIT,TDIPHRES,V,X,DLEQN,DVEQN
      REAL(KIND=8) ::  TCE,PCE,DCE,HLTEST,HVTEST,TOLD,DOLD,POLD,HOLD
      INTEGER IX,ISUBAKT,ISUBOLD
      INTEGER ITLTTTR
      EXTERNAL TDIPHRES
      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT

      SAVE TOLD,POLD,DOLD,HOLD,ISUBOLD
      DATA TOLD / -1.D+0 /
        DATA DOLD / -1.D+0 /
      DATA HOLD /1.D9/
      DATA POLD /1.D9/
      DATA ISUBOLD /0/

      IF ((DABS(H-HOLD).LT.1.D-8) .AND. (DABS(P-POLD).LT.1.D-8)      &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TOLD
         D = DOLD
         goto 1000
      END IF

! CRITICAL POINT

         TCE = TC
         DCE = DC
         PCE = PC

!  TEMPERATURE INTERVAL

      IF (P .GE. PCE) THEN

!  CLOSE TO CRITICAL PRESSURE THE START VALUE IS THE CRITICAL DENSITY

         IF (P .GT. (10.D+0 * PCE)) THEN
          D1 = 2.D+0 * DCE
      ELSE
             D1 = DCE
         ENDIF

!  ITERATION OF TEMPERATURE

         CALL PDITER(P,D1,TH,1.D-6)
         T1 = TH
         H1 = calch(T1,D1)
         IF (H .GT. H1) THEN 
            TFAK = 1.2D0
            IF (P .LT. (1.2D0 * PCE)) THEN
                TFAK = 1.01D0
            ENDIF
         ELSE 
            TFAK = 0.9D0
            IF (P .LT. (1.2D0 * PCE)) THEN
                TFAK = 0.99D0
            ENDIF
         ENDIF
         HDIFF1 = H - H1

         ITLTTTR = 0
100      CONTINUE
         T2 = T1 * TFAK

! IF NO CONVERGENCE THE LOOP IS INTERRUPED

         IF (T2 .LT. TTR) THEN
            ITLTTTR = ITLTTTR + 1
            IF (ITLTTTR .GT. 40) THEN
                T = -111.D+0
                D = -111.D+0
                goto 1000
            ENDIF
         ENDIF
         CALL TPITER(T2,P,DH,1.D-6)

! CONVERGENCE TO NEGATIVE VALUES OF D REQUEST EXIT

         IF (DH .LT. 0.0D+0) THEN
             T = -111.D+0
             D = -111.D+0
             goto 1000
         ENDIF

         D2 = DH
         H2 = calch(T2,D2)
         HDIFF2 = H - H2
         IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 101
         H1 = H2
         HDIFF1 = HDIFF2
         T1 = T2
         D1 = D2
         GOTO 100

101      CONTINUE 

!  UNDER CRITICAL PRESSURES FIRST IS CHECKED IF THE STATE POINT IS IN TWO PHASE REGION

      ELSEIF (P .GE. PTR) THEN

!  FIRST REVIEW WITH AUXILIARY EQUATIONS FOR SAT. TEMP., LIQUID AND VAPOR DENSITIES

         TS = TVPIT(P)
         DV = DVEQN(TS)
         HV = calch(TS,DV)
         DL = DLEQN(TS)
         HL = calch(TS,DL)

!  CALCULATION OF PHASE LIMIT FROM THE FUNDAMENTAL EQUATION FOR   EXACT REVIEW OF PHASE LIMITS

         IF (HL .GT. 0.D0) THEN
            HLTEST = 0.9D0 * HL
         ELSE
            HLTEST = 1.1D0 * HL
         ENDIF

         IF (HV .GT. 0.D0) THEN
            HVTEST = 1.2D0 * HV
         ELSE
            HVTEST = 0.8D0 * HV
         ENDIF

         IF ((HLTEST .LT. H) .AND. (HVTEST .GT. H)) THEN
            CALL PSATIT(TS,DV,DL,P,1.D-9)
            HV = calch(TS,DV)
            HL = calch(TS,DL)
         ENDIF

! IN TWO PHASE FIELD THE DENSITY IS CALCULATED DIRECTLY

         IF ((HL .LE. H) .AND. (HV .GE. H)) THEN
            T = TS
            X = (H - HL) / (HV - HL)
            V = 1.D0 / DL + X * (1.D0 / DV - 1.D0 / DL)
            D = 1.D0 / V
            goto 1000

! IN HOMOGENEOUS REGION THE SAT. TEMP., LIQUID AND VAPOR DENSITIES, ARE USED AS START VALUES

         ELSEIF (H .GT. HV) THEN
            TFAK = 1.2D0
            H1 = HV
            D1 = DV
         ELSEIF (H .LT. HL) THEN
            TFAK = 0.95D0
            H1 = HL
            D1 = DL
         END IF

         T1 = TS
         HDIFF1 = H - H1

200      CONTINUE
         T2 = T1 * TFAK

! ITERATION OF DENSITY

         CALL TPITER(T2,P,DH,1.D-6)
         D2 = DH
         H2 = calch(T2,D2)
         HDIFF2 = H - H2
         IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 201
         H1 = H2
         HDIFF1 = HDIFF2
         T1 = T2
         D1 = D2
         GOTO 200

201      CONTINUE 

      ELSE

! FOR PRESSURES BELOW TRIPLE POINT THE TRIPLE POINT AND CRITICAL TEMP. DEFINES THE START INTERVAL 

         T1 = TTR

! ITERATION OF DENSITY
         CALL TPITER(T1,P,DH,1.D-6)
         D1 = DH
         H1 = calch(T1,D1)
         T2 = TCE

! ITERATION OF DENSITY
         CALL TPITER(T2,P,DH,1.D-6)
         D2 = DH
         H2 = calch(T2,D2)
         IF (H .GT. H2) THEN
            TFAK = 1.2D0
            H1 = H2
            HDIFF1 = H - H1
              T1 = T2
            D1 = D2
300         CONTINUE
            T2 = T1 * TFAK

! ITERATION OF DENSITY
            CALL TPITER(T2,P,DH,1.D-6)
            D2 = DH
            H2 = calch(T2,D2)
            HDIFF2 = H - H2
            IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 301
            H1 = H2
            HDIFF1 = HDIFF2
            T1 = T2
            D1 = D2
            GOTO 300

301         CONTINUE
         ENDIF

      ENDIF

!  ITERATION OF TEMPERATURE AND DENSITY

      CALL ITPEGZ(T1,D1,T2,D2,TDIPHRES,P,H,EPSH,TIT,DIT,IX)
      IF (IX .LE. 3) THEN
         T = TIT
         D = DIT
      ELSE
         T = -111.D0
         D = -111.D0
      ENDIF

1000 continue

      TOLD = T
      HOLD = H
      DOLD = D
      POLD = P
      ISUBOLD = ISUBAKT

      RETURN
      END

!---------------------------------------------------------------------
      REAL(KIND=8) function TDIPHRES(TZ,DZ,DIT,P,H)
!---------------------------------------------------------------------
!
!  DIFFERENCE BETWEEN A   GIVEN AND A CALCULATED VALUE FOR ENTHALPY
!  AT ITERATION OF TEMPERATURE AND DENSITY FOR VALUES
!  OF p AND h
!
!---------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,DZ,P,H,D1,D2,DITPRES,XD,D,calch,DIT
      INTEGER IXD
      EXTERNAL DITPRES

      D1 = DZ * 0.98D0
      D2 = DZ * 1.02D0

! ITERATION OF DENSITY

      CALL ITPEG(D1,D2,DITPRES,TZ,P,1.D-9,XD,IXD)
      D = XD
      TDIPHRES = calch(TZ,D) - H
      DIT = D

      RETURN
      END

!---------------------------------------------------------------------
      SUBROUTINE HSITER(H,S,T,D,EPS)
!---------------------------------------------------------------------
!
!  ITERATION OF TEMPERATURE AND DENSITY FUNCTION OF h AND s
!
!  INPUT:     H         ENTHALPY [KJ / KG]
!             S         ENTROPY [KJ / (KG * K)]
!             EPS       CONVERGENCE CRITERION/TOLERANCE
!
!  OUTPUT:    T         TEMPERATURE [K]
!             D         DENSITY [KG / M ** 3]
!
!---------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  H,T,S,D,EPS
      REAL(KIND=8) ::  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      REAL(KIND=8) ::  calcs,DV,DL,P,TS,HSAT,calch,T2TEST
      REAL(KIND=8) ::  D1,T1,D2,T2,DFAK,HDIFF1,HDIFF2,H1,H2,DSAT,TDIHS2RES
      REAL(KIND=8) ::  TH,TDIHSRES,TIT,DIT,TFAK,DV2,DL2,P2,HL,HV,HOLD
      REAL(KIND=8) ::  SL,SV,X2,VV,VL,V,X,TCE,DCE,TOLD,DOLD,SOLD,TSMAX,TSMIN
      REAL(KIND=8) ::  ABLTEST,calcdpdD,HSAT2,TS2,HSAT3,HSAT1,P3,DL3,DV3
      REAL(KIND=8) ::  TS3,P1,DL1,DV1,TS1,TS1OLD,HSAT4,DL4,DV4,P4,TS4
      REAL(KIND=8) ::  SDIFF1,SDIFF2,S1,S2,HDIFF3,HSAT5,DL5,DV5,P5,TS5
      REAL(KIND=8) ::  TS8,D8,DLV8,P8
      REAL(KIND=8) ::  HHMAX,SHMAX,HSMAX,SSMAX,HSMIN,SSMIN,DSTART,HSTART,TSTART
      INTEGER IX,ISUBAKT,ISUBOLD,ILAUF,IHS,IPHASE,IFAK
      INTEGER IDFAK, icode

      EXTERNAL TDIHSRES,TDIHS2RES

      COMMON / CRTR /  R,TC,PC,DC, HC, SC,TTR,PTR,DLTR,DVTR, SLTR, SVTR, CNDRF, VISRF
      COMMON / SUBIDENT / ISUBAKT
      COMMON / CODE / icode
      COMMON / HSDIAGRAM / HHMAX,SHMAX,HSMAX,SSMAX,HSMIN,SSMIN,TSMIN,TSMAX,IHS

      SAVE TOLD,HOLD,DOLD,SOLD,ISUBOLD
      DATA TOLD / -1.D+0 /
      DATA DOLD / -1.D+0 /
      DATA HOLD /1.D9/
      DATA SOLD /1.D9/
      DATA ISUBOLD /0/
      IFAK = 0

      IF ((DABS(S - SOLD) .LT. 1.D-8) .AND. (DABS(H - HOLD) .LT. 1.D-8)    &
          .AND. (ISUBAKT .EQ. ISUBOLD)) THEN
         T = TOLD
         D = DOLD
       return
      END IF

!   CRITICAL POINT

         TCE = TC
         DCE = DC

! START INTERVAL FOR TEMPERATURE AND DENSITY

!      SC = calcs(TCE,DCE)         ! ************************
!   HC = calch(TCE,DCE)

! CALCULATION OF ENTROPY OF GAS AND LIQUID PHASE AT THE TRIPLE POINT

!      CALL TSATIT(TTR,DV,DL,P,1.D-9)
!      IF ((DL .GT. 0.D0) .AND. (DV .GT. 0.0D+0)) THEN
!         SLTR = calcs(TTR,DL)
!         SVTR = calcs(TTR,DV)
!      ELSE
!         T = -111.D0
!         D = -111.D0
!         goto 1000
!      ENDIF


! IS GIVEN ENTROPY IS LARGER THAN ENTROPY OF GAS PHASE AT TRIPLE POINT,
!  THE START VALUE IS THE VAPOR DENSITY AT TRIPLE POINT

      IF (S .GT. SVTR) THEN     !   9.1554934093D+0
         D1 = DVTR             !   0.485457572553D-02

! ITERATION OF TEMPERATURE

50       CONTINUE         
         CALL DSITER(D1,S,TH,1.D-9)
         IF (TH .LT. 0.0D+0) THEN
          D1 = D1 * 1.05D+0            ! ERA *1.1
          GOTO 50
        ENDIF
         T1 = TH
         H1 = calch(T1,D1)
        S1 = calcs(T1,D1)
         IF (H1 .GT. H) THEN
            DFAK = 0.95D0             ! ERA 0.90
         ELSE
            DFAK = 1.05D0             ! ERA 1.1
         ENDIF
       HDIFF1 = H - H1
      SDIFF1 = S - S1

      HDIFF3 = 0.0D+0
      IDFAK = 0
100      CONTINUE
      D2 = D1 * DFAK

! ITERATION OF TEMPERATURE
         CALL DSITER(D2,S,TH,1.D-9)

      IF ((DABS(TH - (-111.D+0)) .LT. 1.D-15) .AND. (icode .EQ. -1028)) THEN
          IDFAK = IDFAK +1
         icode = 0
         IF (IDFAK .EQ. 1) THEN
            DFAK = 0.99D+0
         ELSE
             D1 = D2
          ENDIF
         GOTO 100
      ENDIF
         T2 = TH
         T2TEST = T2
         IF (T2 .LE. TTR) THEN
              ABLTEST = calcdpdD(T2,D2)
              IF (ABLTEST .LT. 0.0D+0) THEN
                T2 = TTR
              ENDIF
         ENDIF
         H2 = calch(T2,D2)
        S2 = calcs(T2,D2)
         IF ((T2TEST .LT. TTR) .AND. (H .LT. H2)) THEN
            T = -111.D0
            D = -111.D0
            goto 1000
         ENDIF
         HDIFF2 = H - H2
        SDIFF2 = S - S2
         IF ((((HDIFF1 * HDIFF2) .LE. 0.D0) .OR.                &
             ((DABS(HDIFF3) .GT. DABS(HDIFF1)) .AND.             &
             (DABS(HDIFF2) .GT. DABS(HDIFF1)))) .AND.             &
             (((SDIFF1 * SDIFF2) .LE. 0.D0) .OR.                &
              (DABS(SDIFF1) .LT. 1.D-10) .OR.                   &
              (DABS(SDIFF2) .LT. 1.D-10)))         GOTO 101
         H1 = H2
        S1 = S2
        SDIFF1 = SDIFF2
         HDIFF3 = HDIFF1
         HDIFF1 = HDIFF2
         D1 = D2
         T1 = T2
         GOTO 100

101      CONTINUE
         GOTO 998

!  IF GIVEN ENTROPY IS SMALLER THAN THE ENTROPY OF LIQUID PHASE AT THE TRIPLE POINT,
!   THE DENSITY OF LIQUID PHASE AT THE TRIPLE POINT IS USED AS START VALUE

      ELSEIF (S .LE. SLTR) THEN
         D1 = DLTR

! ITERATION OF TEMPERATURE
         CALL DSITER(D1,S,TH,1.D-9)
         T1 = TH
         IF (TH .LT. TTR) THEN
             T1 = TTR
         ENDIF
         H1 = calch(T1,D1)
         IF (H1 .GT. H) THEN
            DFAK = 0.99D0
         ELSE
            DFAK = 1.01D0
         ENDIF
         HDIFF1 = H - H1
200      CONTINUE
         D2 = D1 * DFAK

! ITERATION OF TEMPERATURE
         CALL DSITER(D2,S,TH,1.D-9)
         T2 = TH
         T2TEST = T2
         IF (T2 .LE. TTR) THEN
            T2 = TTR
         ENDIF
         H2 = calch(T2,D2)
!         IF ((T2TEST .LT. TTR) .AND. (H .LT. H2)) THEN
!            T = -111.D0
!            D = -111.D0
!            goto 1000
!         ENDIF
         HDIFF2 = H - H2
         IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 201
         H1 = H2
         HDIFF1 = HDIFF2
         D1 = D2
         T1 = T2
         GOTO 200

201      CONTINUE
         GOTO 998

!  IF GIVEN ENTROPY IS LARGER THAN LIQUID PHASE ENTROPY ( 0.0 )AND SMALLER THAN THE VAPOR ENTROPY OF GAS PHASE
!   AT TRIPLE POINT ( 9.15549341 ), THE LIMIT IS FROM FUNDAMENTAL EQUATION

      ELSE

!  DETERMINATION OF THE POINT: HOMOGENEOUS REGION (IPHASE = 1)
!   OR TWO PHASE FIELD (IPHASE = 0)

!  ITER 1

!      IHS = 0
!      HHMAX = 2803.17476477413D+0      Satura??o x = 1.0    MUDEI PARA 2084,09046 ????
!      SHMAX = 6.17576993539012D+0      Satura??o x = 1.0    MUDEI PARA 4.4069618924D+0
!      TMAXCAL = 4000.D0
!      PMAXCAL = 1000.D0
!      HSMAX,  ??? 2500,91519
!      SSMAX,  ????  9,15549341
!      HSMIN,  0.000611781667  ???????????


          IF (H .GE. HHMAX) THEN          !      HHMAX = 2803.17476477413D+0
           IPHASE = 1
              IF (S .LT. SHMAX) THEN   ! s < SHMAX 6.17576993539012D+0  ? < svtr ?????????? 5.20477?                     
               D8 = DLTR            ! pto 0
               TS8 = TTR
              ELSEIF ((H .LT. HSMIN) .OR. (IHS .EQ. 0)) THEN   ! s >= SHMAX   12,3321289 ?????????      HSMIN > HHMAX ??
                  IF (S .LE. SVTR) THEN                     ! svtr = 9,1555
                      CALL SVSATITTR(S,TS8,D8,DLV8,P8,EPS)
                 ELSE                                 ! s > svtr
                   TS8 = -111.D+0
                 ENDIF
              ELSEIF (H .GT. HSMAX) THEN                  !    HHMAX < 
                  IF (S .GT. SC) THEN                     ! 4,40670568
                      CALL SVSATITCRIT(S,TS8,D8,DLV8,P8,EPS)
                 ELSE
                      CALL SLSATIT(S,TS8,DLV8,D8,P8,EPS)
                 ENDIF
             ELSE
                  CALL SVSATITMID(S,TSMIN,TSMAX,TS8,D8,DLV8,P8,EPS)
             ENDIF

              IF (TS8 .LT. 0.0D+0) THEN
               DSTART = DLTR
               TSTART = TTR
               HSTART = calch(TTR,DLTR)
             ELSE
                DSTART = D8
               TSTART = TS8
               HSTART = calch(TS8,D8)
             ENDIF
!  ITER 2       
          ELSEIF ((((S .GT. SSMAX) .AND. (H .LT. HHMAX))                   &      ! h < HHMAX   
            .OR. ((H .LT. HSMIN) .AND. (S .GT. SSMIN) .AND. (S .LE. SSMAX)))  &
            .AND. (IHS .GT. 0)) THEN                                       
           CALL SVSATITTR(S,TS1,DV1,DL1,P1,EPS)
              HSAT1 = calch(TS1,DV1)
              DSTART = DV1
             TSTART = TS1
             HSTART = HSAT1
             IF (H .GT. HSAT1) THEN
               IPHASE = 1
             ELSE
               IPHASE = 0
             ENDIF
!  ITER 3
         ELSEIF (((S .LT. SC) .AND. (H .LT. HHMAX) .AND. (IHS .EQ. 1))      &
          .OR. ((S .LT. SSMIN) .AND. (H .LT. HHMAX) .AND. (IHS .EQ. 2))      &
          ) THEN                                              
              CALL SLSATIT(S,TS2,DV2,DL2,P2,EPS)
              HSAT2 = calch(TS2,DL2)
              DSTART = DL2
           TSTART = TS2
           HSTART = HSAT2
           IF (H .GT. HSAT2) THEN
               IPHASE = 1
           ELSE
               IPHASE = 0
           ENDIF
!   ITER 4
          ELSEIF                                         &
                 (((((S .GT. SC) .AND. (S .LT. SSMIN))       &
              .OR. ((S .GT. SSMIN) .AND. (S .LT. SSMAX) .AND. (H.GT.HSMAX)))  &
             .AND. ((IHS .EQ. 1) .AND. (H .LT. HHMAX)))           &
              .OR. (((IHS .EQ. 2) .AND. (H .LT. HHMAX))           & 
          .AND. ((S .GT. SC) .AND. (S .LT. SSMAX)            &
          .AND. (H.GT.HSMAX) .AND. (H .LT. HHMAX)))           &
           .OR. ((S .GT. SHMAX) .AND. (S .LT. SSMAX)           &
          .AND. (H .GT. HSMAX) .AND. (H .LT. HHMAX))) THEN      
              CALL SVSATITCRIT(S,TS3,DV3,DL3,P3,EPS)
              HSAT3 = calch(TS3,DV3)
              DSTART = DV3
              TSTART = TS3
              HSTART = HSAT3
              IF (H .LT. HSAT3) THEN
                 IPHASE = 0
              ELSEIF (H .GT. HSAT3) THEN
                  IPHASE = 1
              ENDIF
!   ITER 5   
          ELSEIF                                        &
                  ((H .LT. HSMAX) .AND. (S .GT. SSMIN)             &
          .AND. (S .LT. SSMAX) .AND. (H .GT. HSMIN)                &
            .AND. (IHS .GT. 0)                              &
             .OR. ((S .GT. SSMIN) .AND. (S .LT. SC)               &
         .AND. (H .GT. HSMAX) .AND. (H .LT. HHMAX)            &
         .AND. (IHS .EQ. 2))) THEN
              CALL SVSATITMID(S,TSMIN,TSMAX,TS4,DV4,DL4,P4,EPS)
              HSAT4 = calch(TS4,DV4)
             IF (IHS .EQ. 1) THEN
               IF (H .GT. HSAT4) THEN
                      CALL SVSATITCRIT(S,TS2,DV2,DL2,P2,EPS)
                   HSAT2 = calch(TS2,DV2)
                      IF (H .LT. HSAT2) THEN 
                          IPHASE = 0
                   ELSE
                       IPHASE = 1
                       DV4 = DV2
                       TS4 = TS2
                       HSAT4 = HSAT2
                   ENDIF
               ELSE
                   IPHASE = 1
                   IFAK = 1
               ENDIF
                  DSTART = DV4
                  TSTART = TS4
                  HSTART = HSAT4
           ELSEIF (IHS .EQ. 2) THEN
               IF (H .LT. HSAT4) THEN
                   IPHASE = 1
                   IFAK = 1
                      DSTART = DV4
                      TSTART = TS4
                   HSTART = HSAT4
               ELSE
                      IF (S .LT. SC) THEN
                          CALL SLSATIT(S,TS5,DV5,DL5,P5,EPS)
                      ELSE
                          CALL SVSATITCRIT(S,TS5,DV5,DL5,P5,EPS)
                   ENDIF
                      HSAT5 = calch(TS5,DL5)
                   IF (H .LT. HSAT5) THEN
                       IPHASE = 0
                       DSTART = DV4
                       TSTART = TS4
                       HSTART = HSAT4
                   ELSE
                       IPHASE = 1
                          DSTART = DL5
                       TSTART = TS5
                       HSTART = HSAT5
                   ENDIF
               ENDIF
           ENDIF
!   ITER 6
          ELSEIF                               &
            ((H .LT. HHMAX) .AND. (IHS .EQ. 0)) THEN
           IF (S .LT. SC) THEN
                  CALL SLSATIT(S,TS2,DV2,DL2,P2,EPS)
                  HSAT2 = calch(TS2,DL2)
                  DSTART = DL2
               HSTART = HSAT2
               TSTART = TS2
              ELSE
                  CALL SVSATIT(S,TS2,DV2,DL2,P2,EPS)
                  HSAT2 = calch(TS2,DV2)
                  DSTART = DV2
               HSTART = HSAT2
               TSTART = TS2
           ENDIF
              IF (H .LT. HSAT2) THEN
               IPHASE = 0
           ELSE
               IPHASE = 1
              ENDIF
          ENDIF

      ENDIF

      IF (IPHASE .EQ. 1) THEN

! THE POINT IS IN THE HOMOGENEOUS FIELD

!  FOR ITERATION IN THE HOMOGENEOUS REGION, THE START VALUES ARE
!   THE SATURATION TEMPERATURE AND THE SAT. LIQUID ENTHALPY OR THE DEW LINE

          DFAK = 1.2D0
          T1 = TSTART
          D1 = DSTART
          H1 = HSTART
          HDIFF1 = H - H1

         ILAUF = 0

300       CONTINUE
          IF (((ILAUF .GT. 5) .AND. (IFAK .EQ. 0))          &
            .OR. (IFAK .EQ. 2)) THEN
              D2 = D1 * DFAK
         ELSEIF ((ILAUF .LE. 5) .AND. (IFAK .EQ. 0)) THEN
           D2 = D1 * 1.01D+0
          ELSEIF (IFAK .EQ. 1) THEN
           D2 = D1 * 0.99D+0
         ENDIF
       ILAUF = ILAUF + 1

! ITERATION OF TEMPERATURE

          CALL DSITER(D2,S,TH,1.D-6)
          T2 = TH
          H2 = calch(T2,D2)
          HDIFF2 = H - H2
       IF ((DABS(HDIFF2) .GT. DABS(HDIFF1)) .AND.       &
             (DFAK .GT. 1.D+0)) THEN
           IF (ILAUF .GT. 30) THEN
               DFAK = 0.8D+0
               IFAK = 2
              ENDIF
          ENDIF
          IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 998
          H1 = H2
          HDIFF1 = HDIFF2
          D1 = D2
          T1 = T2
          GOTO 300

      ELSEIF (IPHASE .EQ. 0) THEN

!     THE POINT IS IN THE TWO PHASE REGION

          T1 = TSTART
          D1 = DSTART
          H1 = HSTART
          TFAK = 0.95D0
          HDIFF1 = H - H1
400       CONTINUE
          T2 = T1 * TFAK
          T2TEST = T2
          IF (T2 .LE. TTR) THEN
              T2 = TTR
          ENDIF
          CALL TSATIT(T2,DV2,DL2,P2,1.D-6)
!          CALL TSATIT(T2,DV2,DL2,P2,eps, imax)
          SL = calcs(T2,DL2)
          SV = calcs(T2,DV2)
          HL = calch(T2,DL2)
          HV = calch(T2,DV2)
          X2 = (S - SL) / (SV - SL)
          H2 = HL + X2 * (HV - HL)
          IF ((T2TEST .LT. TTR) .AND. (H .LT. H2)) THEN
              T = -111.D0
              D = -111.D0
              goto 1000
          ENDIF
          HDIFF2 = H - H2
          IF ((HDIFF1 * HDIFF2) .LE. 0.D0) GOTO 401
          H1 = H2
          HDIFF1 = HDIFF2
          D1 = D2
          T1 = T2
          GOTO 400

401       CONTINUE

!  ITERATION OF TEMPERATURE IN THE TWO PHASE REGION

          CALL ITPEG(T1,T2,TDIHS2RES,H,S,EPS,TIT,IX)
          IF (IX .LE. 3) THEN
              T = TIT
              CALL TSATIT(T,DV,DL,P, 1.d-9)
              IF ((DV .LT. 0.0D+0) .AND. (DL .LT. 0.0D+0)) THEN
                  T = -111.D+0
                  D = -111.D+0
                  goto 1000
              ENDIF
              VL = 1.D0 / DL
              VV = 1.D0 / DV
              SL = calcs(T,DL)
              SV = calcs(T,DV)
              X = (S - SL) / (SV -SL)
              V = VL + X * (VV - VL)
              D = 1.D0 / V
              IF (X .GT. 1.D0) THEN
                  T1 = T * 0.98D0
                  T2 = T * 1.02D0
                  D1 = D * 0.99D0
                  D2 = D * 1.01d0
              ELSE
                  goto 1000
              ENDIF
          ELSE
              T = -111.D0
              D = -111.D0
              goto 1000
          ENDIF

      ENDIF

!  ITERATION OF DENSITY AND TEMPERATURE IN HOMOGENEOUS REGION

998   CONTINUE
      CALL ITPEGZ(D1,T1,D2,T2,TDIHSRES,H,S,EPS,DIT,TIT,IX)
      IF (IX .LE. 3) THEN
          T = TIT
          D = DIT
      ELSE
          T = -111.D0
          D = -111.D0
      ENDIF

1000 continue

      TOLD = T
      SOLD = S
      DOLD = D
      HOLD = H
      ISUBOLD = ISUBAKT

      RETURN
      END

! ---------------------------------------------------------------------------------------
      REAL(KIND=8) function TDIHSRES(DZ,TZ,TIT,H,S)
! ---------------------------------------------------------------------------------------
!
!  RDIFFERENCE BETWEEN A GIVEN AND A CALCULATED VALUE FOR ENTHALPY
!  AT ITERATION OF TEMPERATURE AND DENSITY FUNCTION OF h AND s
!
! ---------------------------------------------------------------------------------------
!
      IMPLICIT NONE
      REAL(KIND=8) ::  TZ,DZ,H,S,T1,T2,TIDSRES,XT,T,calch,TIT
      INTEGER IXT
      EXTERNAL TIDSRES

      T1 = TZ * 0.98D0
      T2 = TZ * 1.02D0

! ITERATION OF TEMPERATURE

      CALL ITPEG(T1,T2,TIDSRES,DZ,S,1.D-9,XT,IXT)
      T = XT
      TDIHSRES = calch(T,DZ) - H
      TIT = T

      RETURN
      END

! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
