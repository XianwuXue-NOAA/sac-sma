      SUBROUTINE SAC1(DT,PXV,EP,TCI,ROIMP,SDRO,SSUR,SIF,BFS,BFP,TET,
     &                BFNCC,
!     SAC FROZEN GROUND VARIABLES
     &                IFRZE,TA,LWE,WE,ISC,AESC,
!     SAC PARAMETERS
     &                UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,
     &                REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE,
     &                SIDE,RSERV,
!     SAC State variables  ',
     &                UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC)

!----------------------------------------------------------------
!      RLM - added explicit variable/parameter list - 10/22

!      IMPLICIT NONE

!    ----- PARAMETERS -----
!    UZTWM    Maximum upper zone tension water
!    UZFWM    Maximum upper zone free water
!    LZTWM    Maximum lower zone tension water
!    LZFSM    Maximum lower zone free water, secondary (aka
!                 supplemental)
!    LZFPM    Maximum lower zone free water, primary
!    ADIMP    Additional "impervious" area due to saturation.  Also has
!                 been defined as the fraction of area that can produce
!                 direct runoff - this is the maximum value it can be
!    UZK      Upper zone recession coefficient
!    LZPK     Lower zone recession coefficient, primary
!    LZSK     Lower zone recession coefficient, secondary (supplemental)
!    ZPERC    Minimum percolation rate coefficient
!    REXP     Percolation equation exponent
!    PCTIM    Minimum percent impervious area.  this area is always
!                 impervious (e.g. roads)
!    PFREE    Percent percolating directly to lower zone free water
!    RIVA     Percent riparian area
!    SIDE     Portion of baseflow which does *NOT* go to the stream
!    RSERV    Percent of lower zone free water not transferable to the
!                 lower zone tension water
      REAL UZTWM,UZFWM,LZTWM,LZFSM,LZFPM
      REAL ADIMP,UZK,LZPK,LZSK
      REAL ZPERC,REXP,PCTIM,PFREE,RIVA,SIDE,RSERV

!    ----- FORCINGS -----
!    PXV      Input moisture (e.g. precip, precip+melt)
!    ET       Potential evapotranspiration
      REAL PXV,EP

!    ----- VARIABLES -----
!    DT       Computational time interval
!    IFRZE    Frozen ground module switch.  0 = No frozen ground module,
!                 1 = Use frozen ground module
!    EDMND    ET demand for the time interval
!    E1       ET from the upper zone tension water content (UZTWC)
!    RED      Residual ET demand
!    E2       ET from upper zone free water content (UZFWC)
!    UZRAT    Upper zone ratio used to transfer water from free to
!                 tension water store
!    E3       ET from the lower zone tension water content (LZTWC)
!    RATLZT   Ratio of the lower zone tension water content to the
!                 maximum tension water.  AKA: percent saturation of
!                 the lower zone tension water
!    DEL      Used for multiple calculations in the code:
!                 1. Amount of water moved from lower zone free water
!                    content to the tension water content
!                 2. Incremental interflow
!    E5       ET from ADIMP area
!    TWX      Time interval available moisture in excess of UZTW
!                 requirements
!    SIMPVT   Sum of ROIMP
!    SPERC    Sum of incremental percolation
!    SPBF     Sum of the incremental LZ primary baseflow component only
!    NINC     Number of time sub-increments that the time interval is
!                 diveded into for further soil moisture accounting
!    DINC     Length of each sub-increment (calculated by NINC) in days
!    PINC     Amount of available moisture for each time sub-increment
!    DUZ      Depletion in the upper zone
!    DLZP     Depletion in the lower zone, primary
!    DLZS     Depletion in the lower zone, secondary
!    PAREA    Pervious area
!    I        Loop counter
!    ADSUR    Surface runoff from portion of ADIMP not currently
!                 generating direct runoff (ADDRO)
!    RATIO    Ratio of excess water in the upper zone from ADIMC to the
!                 maximum lower zone tension water. Used to calculate
!                 ADDRO
!    ADDRO    Additional "impervious" direct runoff from ADIMP.
!                 Essentially saturation excess runoff from ADIMP area
!    BF       Used for multiple baseflow calculations in the code
!                 1. Incremental baseflow, lower zone primary
!                 2. Incremental baseflow, lower zone secondary
!    SBF      Sum of the incremental baseflow components (LZ primary,
!                 secondary).
!    PERCM    Limiting percolation value (aka maximum percolation). In
!                 some documentation it is referred to as PBASE
!    PERC     Percolation
!    DEFR     Lower zone moisture deficiency ratio
!    FR       Change in percolation withdrawal due to frozen ground
!    FI       Change in interflow withdrawal due to frozen ground
!    UZDEFR   Calculated, but not used. RECOMMEND removing
!    CHECK    A check to see if percolation exceeds the lower zone
!                 deficiency
!    SPERC    Sum of interval percolation
!    PERCT    Percolation to tension water
!    PERCF    Percolation to free water
!    HPL      Relative size of the lower zone max free water, primary
!                 storage to the lower zone total max free water storage
!    RATLP    Content capacity ratio (LZ, primary) (i.e. relative
!                 fullness)
!    RATLS    Content capacity ratio (LZ, secondary) (i.e. relative
!                 fullness)
!    FRACP    Fraction going to primary store during each interval
!    PERCP    Amount of excess percolation going to the LZ primary store
!    PERCS    Amount of excess percolation going to the LZ secondary
!                 store
!    EXCESS   LZ free water in excess of the maximum to be removed from
!                 LZFPC and added to LZTWC
!    SUR      Incremental surface runoff.  Not multiplied by PAREA until
!                 added to the sum (SSUR)
!    EUSED    Total ET from the pervious area (PAREA) = E1+E2+E3
!    TBF      Total baseflow
!    BFCC     Baseflow channel component (reduces TBF by fraction SIDE)
!    SINTFT   Monthly sum of SIF (NOT USED)
!    SGWFP    Monthly sum of BFP (NOT USED)
!    SGWFS    Monthly sum of BFS (NOT USED)
!    SRECHT   Monthly sum of BFNCC (NOT USED)
!    SROST    Monthly sum of SSUR (NOT USED)
!    SRODT    Monthly sum of SDRO (NOT USED)
!    E4       ET from riparian vegetation using RIVA
!    SROT     Assuming this is the monthly sum of TCI (NOT USED)
!    TET      Total evapotranspiration
!    SETT     Assuming this is the monthly sum of TET (NOT USED)
!    SE1      Assuming this is the monthly sum of E1 (NOT USED)
!    SE2      Assuming this is the monthly sum of E2 (NOT USED)
!    SE3      Assuming this is the monthly sum of E3 (NOT USED)
!    SE4      Assuming this is the monthly sum of E4 (NOT USED)
!    SE5      Assuming this is the monthly sum of E5 (NOT USED)
!    RSUM(7)  Sums of (1) TCI, (2) ROIMP, (3) SDRO, (4) SSUR, (5) SIF,
!                 (6) BFS, (7) BFP. (NOT USED)

      REAL DT,EDMND,E1,RED,E2,UZRAT,E3,RATLZT,SAVED,RATLZ,DEL,E5
      REAL TWX,SIMPVT,SPERC,DINC,PINC,DUZ,DLZP,DLZS,PAREA
      REAL ADSUR,RATIO,ADDRO,BF,SBF,SPBF,PERCM,PERC,DEFR,FR,FI
      REAL UZDEFR,CHECK,PERCT,PERCF,HPL,RATLP,RATLS,FRACP
      REAL PERCP,PERCS,EXCESS,SUR,EUSED,TBF,BFCC,E4
     
      REAL TA,LWE,WE,AESC

      INTEGER IFRZE,I,ISC,NINC


      REAL SINTFT,SGWFP,SGWFS,SRECHT,SROST,SRODT,SROT,SETT
      REAL SE1,SE2,SE3,SE4,SE5,RSUM(7)

      COMMON/FSMCO1/FGCO(6),RSUM,PPE,PSC,PTA,PWE
      COMMON/FSUMS1/SROT,SIMPVT,SRODT,SROST,SINTFT,SGWFP,SGWFS,SRECHT,
     1              SETT,SE1,SE3,SE4,SE5


! ----- OUTPUTS -----
!    TCI      Total Channel Inflow
!    ROIMP    Impervious runoff from the permanent impervious area
!                 (PCTIM)
!    SDRO     Sum of the direct runoff (from ADIMP)
!    SSUR     Sum of the indirect surface runoff components from ADIMP
!                 and PAREA
!    SIF      Sum of the interflow
!    BFS      Secondary baseflow, channel component
!    BFP      Primary baseflow, channel component
!    TET      Total evapotranspiration (E1+E2+E3+E4+E5)
!    BFNCC    Baseflow, non-channel component
!    UZTWC    Upper zone tension water content. (not multiplied by area)
!    UZFWC    Upper zone free water content. (not multiplied by area)
!    LZTWC    Lower zone tension water content (not multiplied by area)
!    LZFSC    Lower zone free water content, secondary. (not multiplied
!                 by area)
!    LZFPC    Lower zone free water content, primary. (not multiplied by
!                 area)
!    ADIMC    Additional impervious area water content. (not multiplied
!                 by area)

      REAL TCI,ROIMP,SDRO,SSUR,SIF,BFS,BFP,TET,BFNCC
      REAL UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC


!---------------------------------------------------------------

!.......................................
!     THIS SUBROUTINE EXECUTES THE 'SAC-SMA ' OPERATION FOR ONE TIME
!         PERIOD.
!.......................................
!     SUBROUTINE INITIALLY WRITTEN BY. . .
!            ERIC ANDERSON - HRL     APRIL 1979     VERSION 1
!.......................................

!     RCS Id string, for version control
!      CHARACTER*60 RCSID
!      DATA RCSID/"$Id: sac1.f,v 1.1 2006/09/01 21:59:44 vicadmin Exp $"/

!-------------------------------------------------------------------
!     RLM - Commented out in favor of explicit variable list above 10/22
!      REAL DT
!      REAL PXV
!      REAL EP
!      REAL TCI
!      REAL ROIMP,SDRO,SSUR,SIF,BFS,BFP,TET

!      INTEGER IFRZE, ISC
!      REAL TA,LWE,WE,AESC

!      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC


!     COMMON BLOCKS
!      COMMON/FSMCO1/FGCO(6),RSUM(7),PPE,PSC,PTA,PWE
!      COMMON/FSUMS1/SROT,SIMPVT,SRODT,SROST,SINTFT,SGWFP,SGWFS,SRECHT,
!     1              SETT,SE1,SE3,SE4,SE5
!--------------------------------------------------------------------

!      write(*,*) 'pars - ',UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,
!     1           LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,RSERV
!      write(*,*) 'start sac1 - states ', UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,
!     &           ADIMC
!      write(*,*) '           - runoff ', ROIMP,SDRO,SSUR,SIF,BFS,BFP
!      write(*,*) '           - ET ', E1,E2,E3,E4,E5,TET

!.......................................
!     COMPUTE EVAPOTRANSPIRATION LOSS FOR THE TIME INTERVAL.
!        EDMND IS THE ET-DEMAND FOR THE TIME INTERVAL
      EDMND=EP

!
!     COMPUTE ET FROM UPPER ZONE.
      E1=EDMND*(UZTWC/UZTWM)
      RED=EDMND-E1
!     RED IS RESIDUAL EVAP DEMAND
      UZTWC=UZTWC-E1
      E2=0.0
      IF(UZTWC.GE.0.) GO TO 220
!     E1 CAN NOT EXCEED UZTWC
      E1=E1+UZTWC
      UZTWC=0.0
      RED=EDMND-E1
      IF(UZFWC.GE.RED) GO TO 221
!     E2 IS EVAP FROM UZFWC.
      E2=UZFWC
      UZFWC=0.0
      RED=RED-E2
      GO TO 225
  221 E2=RED
      UZFWC=UZFWC-E2
      RED=0.0
  220 IF((UZTWC/UZTWM).GE.(UZFWC/UZFWM)) GO TO 225
!     UPPER ZONE FREE WATER RATIO EXCEEDS UPPER ZONE
!     TENSION WATER RATIO, THUS TRANSFER FREE WATER TO TENSION
      UZRAT=(UZTWC+UZFWC)/(UZTWM+UZFWM)
      UZTWC=UZTWM*UZRAT
      UZFWC=UZFWM*UZRAT
!  225 IF (UZTWC.LT.0.00001) UZTWC=0.0
!      IF (UZFWC.LT.0.00001) UZFWC=0.0
!
!     COMPUTE ET FROM THE LOWER ZONE.
!     COMPUTE ET FROM LZTWC (E3)
!     RLM - removed setting low UZTWC and UZFWC values to zero. moved
!     225 GO TO
  225 E3=RED*(LZTWC/(UZTWM+LZTWM))
      LZTWC=LZTWC-E3
      IF(LZTWC.GE.0.0) GO TO 226
!     E3 CAN NOT EXCEED LZTWC
      E3=E3+LZTWC
      LZTWC=0.0
  226 RATLZT=LZTWC/LZTWM
!C+
!C+   INFERRED PARAMETER (ADDED BY Q DUAN ON 3/6/95)
      SAVED = RSERV * (LZFPM + LZFSM)
      RATLZ=(LZTWC+LZFPC+LZFSC-SAVED)/(LZTWM+LZFPM+LZFSM-SAVED)
      IF(RATLZT.GE.RATLZ) GO TO 230
!     RESUPPLY LOWER ZONE TENSION WATER FROM LOWER
!     ZONE FREE WATER IF MORE WATER AVAILABLE THERE.
      DEL=(RATLZ-RATLZT)*LZTWM
!     TRANSFER FROM LZFSC TO LZTWC.
      LZTWC=LZTWC+DEL
      LZFSC=LZFSC-DEL
      IF(LZFSC.GE.0.0) GO TO 230
!     IF TRANSFER EXCEEDS LZFSC THEN REMAINDER COMES FROM LZFPC
      LZFPC=LZFPC+LZFSC
      LZFSC=0.0
!  230 IF (LZTWC.LT.0.00001) LZTWC=0.0
!
!     COMPUTE ET FROM ADIMP AREA.-E5
!     RLM - removed setting low LZTWC values to zero, moved 230 GO TO
  230 E5=E1+(RED+E2)*((ADIMC-E1-UZTWC)/(UZTWM+LZTWM))
!      ADJUST ADIMC,ADDITIONAL IMPERVIOUS AREA STORAGE, FOR EVAPORATION.
      ADIMC=ADIMC-E5
      IF(ADIMC.GE.0.0) GO TO 231
!     E5 CAN NOT EXCEED ADIMC.
      E5=E5+ADIMC
      ADIMC=0.0
  231 E5=E5*ADIMP
!     E5 IS ET FROM THE AREA ADIMP.
!.......................................
!     COMPUTE PERCOLATION AND RUNOFF AMOUNTS.
      TWX=PXV+UZTWC-UZTWM
!     TWX IS THE TIME INTERVAL AVAILABLE MOISTURE IN EXCESS
!     OF UZTW REQUIREMENTS.
      IF(TWX.GE.0.0) GO TO 232
!     ALL MOISTURE HELD IN UZTW--NO EXCESS.
      UZTWC=UZTWC+PXV
      TWX=0.0
      GO TO 233
!      MOISTURE AVAILABLE IN EXCESS OF UZTW STORAGE.
  232 UZTWC=UZTWM
  233 ADIMC=ADIMC+PXV-TWX
!
!     COMPUTE IMPERVIOUS AREA RUNOFF.
      ROIMP=PXV*PCTIM
!      ROIMP IS RUNOFF FROM THE MINIMUM IMPERVIOUS AREA.
      SIMPVT=SIMPVT+ROIMP
!
!     INITIALIZE TIME INTERVAL SUMS.
      SBF=0.0
      SSUR=0.0
      SIF=0.0
      SPERC=0.0
      SDRO=0.0
      SPBF=0.0

!     DETERMINE COMPUTATIONAL TIME INCREMENTS FOR THE BASIC TIME INTERVAL
      NINC=1.0+0.2*(UZFWC+TWX)
!     NINC=NUMBER OF TIME INCREMENTS THAT THE TIME INTERVAL
!     IS DIVIDED INTO FOR FURTHER
!     SOIL-MOISTURE ACCOUNTING.  NO ONE INCREMENT
!     WILL EXCEED 5.0 MILLIMETERS OF UZFWC+PAV
      DINC=(1.0/NINC)*DT
!     DINC=LENGTH OF EACH INCREMENT IN DAYS.
      PINC=TWX/NINC

!     PINC=AMOUNT OF AVAILABLE MOISTURE FOR EACH INCREMENT.
!      COMPUTE FREE WATER DEPLETION FRACTIONS FOR
!     THE TIME INCREMENT BEING USED-BASIC DEPLETIONS
!      ARE FOR ONE DAY
      DUZ=1.0-((1.0-UZK)**DINC)
      DLZP=1.0-((1.0-LZPK)**DINC)
      DLZS=1.0-((1.0-LZSK)**DINC)
!C+
!C+   INFERRED PARAMETER (ADDED BY Q DUAN ON 3/6/95)
      PAREA = 1.0 - ADIMP - PCTIM
!.......................................
!     START INCREMENTAL DO LOOP FOR THE TIME INTERVAL.
!.......................................
      DO 240 I=1,NINC

      ADSUR=0.0
!     COMPUTE DIRECT RUNOFF (FROM ADIMP AREA).
      RATIO=(ADIMC-UZTWC)/LZTWM
!      WRITE(*,*) 'ADIMC ', ADIMC, UZTWC, LZTWM
      IF (RATIO.LT.0.0) RATIO=0.0
      ADDRO=PINC*(RATIO**2)
!      WRITE(*,*) 'ADDRO = ', ADDRO, 'PINK = ', PINK, RATIO
!     ADDRO IS THE AMOUNT OF DIRECT RUNOFF FROM THE AREA ADIMP.
!
!     COMPUTE BASEFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
      BF=LZFPC*DLZP
      LZFPC=LZFPC-BF
      IF (LZFPC.GT.0.0001) GO TO 234
      BF=BF+LZFPC
      LZFPC=0.0
  234 SBF=SBF+BF
      SPBF=SPBF+BF
      BF=LZFSC*DLZS
      LZFSC=LZFSC-BF
      IF(LZFSC.GT.0.0001) GO TO 235
      BF=BF+LZFSC
      LZFSC=0.0
  235 SBF=SBF+BF

!
!      COMPUTE PERCOLATION-IF NO WATER AVAILABLE THEN SKIP
      IF((PINC+UZFWC).GT.0.01) GO TO 251
      UZFWC=UZFWC+PINC
      GO TO 249
  251 PERCM=LZFPM*DLZP+LZFSM*DLZS
      PERC=PERCM*(UZFWC/UZFWM)
      DEFR=1.0-((LZTWC+LZFPC+LZFSC)/(LZTWM+LZFPM+LZFSM))
!     DEFR IS THE LOWER ZONE MOISTURE DEFICIENCY RATIO
      FR=1.0
!     FR IS THE CHANGE IN PERCOLATION WITHDRAWAL DUE TO FROZEN GROUND.
      FI=1.0
!     FI IS THE CHANGE IN INTERFLOW WITHDRAWAL DUE TO FROZEN GROUND.
      IF (IFRZE.EQ.0) GO TO 239
      UZDEFR=1.0-((UZTWC+UZFWC)/(UZTWM+UZFWM))
      CALL FGFR1(DEFR,FR,FI)
  239 PERC=PERC*(1.0+ZPERC*(DEFR**REXP))*FR
!     NOTE...PERCOLATION OCCURS FROM UZFWC BEFORE PAV IS ADDED.
      IF(PERC.LT.UZFWC) GO TO 241
!      PERCOLATION RATE EXCEEDS UZFWC.
      PERC=UZFWC
!     PERCOLATION RATE IS LESS THAN UZFWC.
  241 UZFWC=UZFWC-PERC
!     CHECK TO SEE IF PERCOLATION EXCEEDS LOWER ZONE DEFICIENCY.
      CHECK=LZTWC+LZFPC+LZFSC+PERC-LZTWM-LZFPM-LZFSM
      IF(CHECK.LE.0.0) GO TO 242
      PERC=PERC-CHECK
      UZFWC=UZFWC+CHECK
  242 SPERC=SPERC+PERC
!     SPERC IS THE TIME INTERVAL SUMMATION OF PERC
!
!     COMPUTE INTERFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
!     NOTE...PINC HAS NOT YET BEEN ADDED
      DEL=UZFWC*DUZ*FI
      SIF=SIF+DEL
      UZFWC=UZFWC-DEL
!     DISTRIBE PERCOLATED WATER INTO THE LOWER ZONES
!     TENSION WATER MUST BE FILLED FIRST EXCEPT FOR THE PFREE AREA.
!     PERCT IS PERCOLATION TO TENSION WATER AND PERCF IS PERCOLATION
!         GOING TO FREE WATER.
      PERCT=PERC*(1.0-PFREE)
      IF ((PERCT+LZTWC).GT.LZTWM) GO TO 243
      LZTWC=LZTWC+PERCT
      PERCF=0.0
      GO TO 244
  243 PERCF=PERCT+LZTWC-LZTWM
      LZTWC=LZTWM
!
!      DISTRIBUTE PERCOLATION IN EXCESS OF TENSION
!      REQUIREMENTS AMONG THE FREE WATER STORAGES.
  244 PERCF=PERCF+PERC*PFREE
      IF(PERCF.EQ.0.0) GO TO 245
      HPL=LZFPM/(LZFPM+LZFSM)
!     HPL IS THE RELATIVE SIZE OF THE PRIMARY STORAGE
!     AS COMPARED WITH TOTAL LOWER ZONE FREE WATER STORAGE.
      RATLP=LZFPC/LZFPM
      RATLS=LZFSC/LZFSM
!     RATLP AND RATLS ARE CONTENT TO CAPACITY RATIOS, OR
!     IN OTHER WORDS, THE RELATIVE FULLNESS OF EACH STORAGE
      FRACP=(HPL*2.0*(1.0-RATLP))/((1.0-RATLP)+(1.0-RATLS))
!     FRACP IS THE FRACTION GOING TO PRIMARY.
      IF (FRACP.GT.1.0) FRACP=1.0
      PERCP=PERCF*FRACP
      PERCS=PERCF-PERCP
!     PERCP AND PERCS ARE THE AMOUNT OF THE EXCESS
!     PERCOLATION GOING TO PRIMARY AND SUPPLEMENTAL
!      STORGES,RESPECTIVELY.
      LZFSC=LZFSC+PERCS
      IF(LZFSC.LE.LZFSM) GO TO 246
      PERCS=PERCS-LZFSC+LZFSM
      LZFSC=LZFSM
  246 LZFPC=LZFPC+(PERCF-PERCS)
!     CHECK TO MAKE SURE LZFPC DOES NOT EXCEED LZFPM.
      IF (LZFPC.LE.LZFPM) GO TO 245
      EXCESS=LZFPC-LZFPM
      LZTWC=LZTWC+EXCESS
      LZFPC=LZFPM
!
!     DISTRIBUTE PINC BETWEEN UZFWC AND SURFACE RUNOFF.
  245 IF(PINC.EQ.0.0) GO TO 249
!     CHECK IF PINC EXCEEDS UZFWM
      IF((PINC+UZFWC).GT.UZFWM) GO TO 248
!     NO SURFACE RUNOFF
      UZFWC=UZFWC+PINC
      GO TO 249
!
!     COMPUTE SURFACE RUNOFF (SUR) AND KEEP TRACK OF TIME INTERVAL SUM.
  248 SUR=PINC+UZFWC-UZFWM
      UZFWC=UZFWM
      SSUR=SSUR+SUR*PAREA
      ADSUR=SUR*(1.0-ADDRO/PINC)
!     ADSUR IS THE AMOUNT OF SURFACE RUNOFF WHICH COMES
!     FROM THAT PORTION OF ADIMP WHICH IS NOT
!     CURRENTLY GENERATING DIRECT RUNOFF.  ADDRO/PINC
!     IS THE FRACTION OF ADIMP CURRENTLY GENERATING
!     DIRECT RUNOFF.
      SSUR=SSUR+ADSUR*ADIMP
!
!     ADIMP AREA WATER BALANCE -- SDRO IS THE 6 HR SUM OF
!          DIRECT RUNOFF.
  249 ADIMC=ADIMC+PINC-ADDRO-ADSUR
      IF (ADIMC.LE.(UZTWM+LZTWM)) GO TO 247
      ADDRO=ADDRO+ADIMC-(UZTWM+LZTWM)
      ADIMC=UZTWM+LZTWM
  247 SDRO=SDRO+ADDRO*ADIMP
!      WRITE(*,*) SDRO, ADDRO, ADIMP
!      RLM - removed setting low ADIMC values to zero
!      IF (ADIMC.LT.0.00001) ADIMC=0.0
  240 CONTINUE
!.......................................
!     END OF INCREMENTAL DO LOOP.
!.......................................
!     COMPUTE SUMS AND ADJUST RUNOFF AMOUNTS BY THE AREA OVER
!     WHICH THEY ARE GENERATED.
      EUSED=E1+E2+E3
!     EUSED IS THE ET FROM PAREA WHICH IS 1.0-ADIMP-PCTIM
      SIF=SIF*PAREA
!
!     SEPARATE CHANNEL COMPONENT OF BASEFLOW
!     FROM THE NON-CHANNEL COMPONENT
      TBF=SBF*PAREA
!     TBF IS TOTAL BASEFLOW
      BFCC=TBF*(1.0/(1.0+SIDE))
!     BFCC IS BASEFLOW, CHANNEL COMPONENT
      BFP=SPBF*PAREA/(1.0+SIDE)
      BFS=BFCC-BFP
      IF(BFS.LT.0.0)BFS=0.0
      BFNCC=TBF-BFCC
!     BFNCC IS BASEFLOW,NON-CHANNEL COMPONENT
!
!     ADD TO MONTHLY SUMS.
      SINTFT=SINTFT+SIF
      SGWFP=SGWFP+BFP
      SGWFS=SGWFS+BFS
      SRECHT=SRECHT+BFNCC
      SROST=SROST+SSUR
      SRODT=SRODT+SDRO
!
!     COMPUTE TOTAL CHANNEL INFLOW FOR THE TIME INTERVAL.
      TCI=ROIMP+SDRO+SSUR+SIF+BFCC
!
!     COMPUTE E4-ET FROM RIPARIAN VEGETATION.
      E4=(EDMND-EUSED)*RIVA
!
!     SUBTRACT E4 FROM CHANNEL INFLOW
      TCI=TCI-E4
      IF(TCI.GE.0.0) GO TO 250
      E4=E4+TCI
      TCI=0.0
  250 SROT=SROT+TCI
!
!     COMPUTE TOTAL EVAPOTRANSPIRATION-TET
      EUSED=EUSED*PAREA
      TET=EUSED+E5+E4
      SETT=SETT+TET
      SE1=SE1+E1*PAREA
      SE3=SE3+E3*PAREA
      SE4=SE4+E4
      SE5=SE5+E5

!     CHECK THAT ADIMC.GE.UZTWC
      IF (ADIMC.LT.UZTWC) ADIMC=UZTWC

!      write(*,*) 'end sac1 - states ', UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,
!     &           ADIMC
!      write(*,*) '           - runoff ', ROIMP,SDRO,SSUR,SIF,BFS,BFP
!      write(*,*) '           - ET ', E1,E2,E3,E4,E5,TET

!
!     COMPUTE NEW FROST INDEX AND MOISTURE TRANSFER.
      IF (IFRZE.GT.0) CALL FROST1(PXV,SSUR,SDRO,TA,LWE,WE,ISC,AESC,DT)
!
!     ADD TO SUMS OF RUNOFF COMPONENTS.
      RSUM(1)=RSUM(1)+TCI
      RSUM(2)=RSUM(2)+ROIMP
      RSUM(3)=RSUM(3)+SDRO
      RSUM(4)=RSUM(4)+SSUR
      RSUM(5)=RSUM(5)+SIF
      RSUM(6)=RSUM(6)+BFS
      RSUM(7)=RSUM(7)+BFP
!.......................................

!      WRITE(*,*) 'sac1.for ', DT,PXV,EP,TCI,ROIMP,SDRO,SSUR,SIF,BFS,BFP,TET,
!     &                IFRZE,TA,LWE,WE,ISC,AESC

      RETURN
      END
!
!====================================================================
!
      SUBROUTINE FGFR1(LZDEFR,FR,FI)
!.......................................
!     THIS SUBROUTINE COMPUTES THE CHANGE IN THE PERCOLATION AND
!        INTERFLOW WITHDRAWAL RATES DUE TO FROZEN GROUND.
!.......................................
!     WRITTEN BY -- ERIC ANDERSON - HRL   JUNE 1980
!.......................................
      REAL LZDEFR,LZTWC,LZFSC,LZFPC
!
!     COMMON BLOCKS
      COMMON/SACSTAT1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC
      COMMON/FSMCO1/FGCO(6),RSUM(7),PPE,PSC,PTA,PWE
      COMMON/FPMFG1/FGPM(10)
!.......................................
!     INITIAL VALUES
      FINDX=FGCO(1)
      FRTEMP=FGPM(5)
      SATR=FGPM(6)
      FREXP=FGPM(7)
!.......................................
!     DETERMINE IF FROZEN GROUND EFFECT EXISTS.
      IF (FINDX.LT.FRTEMP) GO TO 100
      RETURN
!.......................................
!     COMPUTE SATURATED REDUCTION.
  100 EXP=FRTEMP-FINDX
      FSAT=(1.0-SATR)**EXP
!     CHANGE AT DRY CONDITIONS
      FDRY=1.0
!     COMPUTE ACTUAL CHANGE
      IF (LZDEFR.GT.0.0) GO TO 101
      FR=FSAT
      FI=FR
      RETURN
  101 FR=FSAT+(FDRY-FSAT)*(LZDEFR**FREXP)
      FI=FR
!.......................................
      RETURN
      END
!
!====================================================================
!
      SUBROUTINE FROST1(PX,SUR,DIR,TA,LWE,WE,ISC,AESC,DT)
!.......................................
!     THIS SUBROUTINE COMPUTES THE CHANGE IN THE FROZEN GROUND
!        INDEX AND MOISTURE MOVEMENT DUE TO TEMPERATURE GRADIENTS.
!.......................................
!     WRITTEN BY ERIC ANDERSON - HRL   JUNE 1980
!.......................................
      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC
      REAL LWE
!
!     COMMON BLOCKS
      COMMON/SACPARM1/UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,LZTWM,
     1                LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,RSERV
      COMMON/SACSTAT1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC
      COMMON/FSMCO1/FGCO(6),RSUM(7),PPE,PSC,PTA,PWE
      COMMON/FPMFG1/FGPM(10)
!.......................................
!     INITIAL VALUES
      FINDX=FGCO(1)
      FINDX1=FINDX
      CSOIL=4.0*DT*FGPM(1)
      CSNOW=FGPM(2)
      GHC=FGPM(3)*DT
      RTHAW=FGPM(4)
!.......................................
!     COMPUTE MOISTURE MOVEMENT
!        EQUATIONS NOT READY YET.
!.......................................
!     COMPUTE CHANGE IN FROZEN GROUND INDEX.
!     CHANGE DUE TO WATER FREZING IN THE SOIL.
      IF (FINDX.GE.0.0) GO TO 120
      WATER=PX-SUR-DIR
      IF (WATER.LE.0.0) GO TO 120
      FINDX=FINDX+RTHAW*WATER
      IF (FINDX.GT.0.0) FINDX=0.0
!.......................................
!     CHANGE DUE TO TEMPERATURE.
  120 IF ((FINDX.GE.0.0).AND.(TA.GE.0.0)) GO TO 190
!
!     COMPUTE TRANSFER COEFFIENT.
      IF (LWE.EQ.0) GO TO 124
      IF (WE.EQ.0.0) GO TO 124
      IF (ISC.GT.0) GO TO 121
      COVER=1.0
      GO TO 122
  121 COVER=AESC
      IF (COVER.EQ.0.0) GO TO 124
  122 TWE=WE/COVER
      C=CSOIL*(1.0-COVER)+CSOIL*((1.0-CSNOW)**TWE)*COVER
      GO TO 125
  124 C=CSOIL
!
!     COMPUTE CHANGE IN FROST INDEX.
  125 IF (TA.GE.0.0) GO TO 126
      CFI=-C*SQRT(TA*TA+FINDX*FINDX)-C*FINDX+GHC
      FINDX=FINDX+CFI
      GO TO 190
  126 FINDX=FINDX+C*TA+GHC
!.......................................
!     CHECK FROST INDEX
  190 IF (FINDX.LT.0.0) GO TO 195
      FINDX=0.0
      GO TO 199
!.......................................
  195 CONTINUE
!.......................................
!     SAVE NEW FROST INDEX
  199 FGCO(1)=FINDX
!.......................................

      RETURN
      END
