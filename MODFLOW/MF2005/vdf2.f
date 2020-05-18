c   Copyright (C) Stichting Deltares, 2005-2019.
c
c   This file is part iMOD-WQ, which itself is part of iMOD.
c
c   This program is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, either version 3 of the License, or
c   (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with this program.  If not, see <http://www.gnu.org/licenses/>.
c
c   Contact: imod.support@deltares.nl
c   Stichting Deltares
c   P.O. Box 177
c   2600 MH Delft, The Netherlands.
c
c   iMOD-WQ is partly based on the following source codes:
c   1. MT3DMS 5.3 source code of the University of Alabama.
c      The original MT3DMS 5.3 source code can be downloaded from the
c      university of Alabama website https://hydro.geo.ua.edu/mt3d/htm.
c   2. SEAWAT v4 source code of the USGS.
c      The original SEAWAT v4 source code can be downloaded from the
c      USGS website https://www.usgs.gov/software/seawat-a-computer-
c      program-simulation-three-dimensional-variable-density-ground-water-flow
c   3. RT3D2.5. The original RT3D2.5 source code can be downloaded from
c      http://tpclement.weebly.com/computer-tools.html.
c   For iMOD-WQ these source codes have been expanded
c   and extensively modified by Stichting Deltares.
  
C
      MODULE VDFMODULE
CVDF1        INTEGER, SAVE, POINTER              ::MT3DRHOFLG
        INTEGER, SAVE, POINTER              ::MFNADVFD
        INTEGER, SAVE, POINTER              ::NSWTCPL
        INTEGER, SAVE, POINTER              ::IWTABLE
        INTEGER, SAVE, POINTER              ::NSRHOEOS
        INTEGER, SAVE, POINTER              ::NCOL
        INTEGER, SAVE, POINTER              ::NROW
        INTEGER, SAVE, POINTER              ::NLAY
        REAL,    SAVE, POINTER              ::DENSEMIN        
        REAL,    SAVE, POINTER              ::DENSEMAX
        REAL,    SAVE, POINTER              ::DENSEREF
        REAL,    SAVE, POINTER              ::FIRSTDT
CVDF1        REAL,    SAVE, POINTER              ::DNSCRIT
        REAL,    SAVE, POINTER              ::DRHODPRHD
        REAL,    SAVE, POINTER              ::PRHDREF
        REAL,    SAVE, POINTER              ::HDRY
        REAL,    SAVE, POINTER              ::HNOFLO
        INTEGER, SAVE, POINTER,  DIMENSION(:)                ::MTRHOSPEC
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::PS
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::RHOCR
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::RHOCC
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::RHOCV
        DOUBLE PRECISION, SAVE, POINTER,  DIMENSION(:,:,:)   ::HSALT
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::ELEV
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::DCDT
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:,:)          ::COLDFLW
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::PSOLDITER
        REAL,    SAVE, POINTER,  DIMENSION(:)                ::DRHODC
        REAL,    SAVE, POINTER,  DIMENSION(:)                ::CRHOREF
      TYPE VDFTYPE
CVDF1        INTEGER, POINTER              ::MT3DRHOFLG
        INTEGER, POINTER              ::MFNADVFD
        INTEGER, POINTER              ::NSWTCPL
        INTEGER, POINTER              ::IWTABLE
        INTEGER, POINTER              ::NSRHOEOS
        INTEGER, POINTER              ::NCOL
        INTEGER, POINTER              ::NROW
        INTEGER, POINTER              ::NLAY
        REAL,    POINTER              ::DENSEMIN        
        REAL,    POINTER              ::DENSEMAX
        REAL,    POINTER              ::DENSEREF
        REAL,    POINTER              ::FIRSTDT
CVDF1        REAL,    POINTER              ::DNSCRIT
        REAL,    POINTER              ::DRHODPRHD
        REAL,    POINTER              ::PRHDREF
        REAL,    POINTER              ::HDRY
        REAL,    POINTER              ::HNOFLO
        INTEGER, POINTER,  DIMENSION(:)                ::MTRHOSPEC
        REAL,    POINTER,  DIMENSION(:,:,:)            ::PS
        REAL,    POINTER,  DIMENSION(:,:,:)            ::RHOCR
        REAL,    POINTER,  DIMENSION(:,:,:)            ::RHOCC
        REAL,    POINTER,  DIMENSION(:,:,:)            ::RHOCV
        DOUBLE PRECISION,  POINTER,  DIMENSION(:,:,:)  ::HSALT
        REAL,    POINTER,  DIMENSION(:,:,:)            ::ELEV
        REAL,    POINTER,  DIMENSION(:,:,:)            ::DCDT
        REAL,    POINTER,  DIMENSION(:,:,:,:)          ::COLDFLW
        REAL,    POINTER,  DIMENSION(:,:,:)            ::PSOLDITER
        REAL,    POINTER,  DIMENSION(:)                ::DRHODC
        REAL,    POINTER,  DIMENSION(:)                ::CRHOREF
               
      END TYPE
      TYPE(VDFTYPE),SAVE:: VDFDAT(10)
      END MODULE VDFMODULE
C
      SUBROUTINE VDF2AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAYS AND READ DATA FOR VISCOSITY PACKAGE
C     COLDFLW WAS INTRODUCED TO STORE THE CONCENTRATION ARRAY AT THE 
C     TIME OF THE LAST FLOW SOLUTION.  THIS ARRAY IS USED TO DETERMINE
C     HOW MUCH THE DENISTY HAS CHANGED AND IN THE CALCULATION OF DCDT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C-----WHEN SWITCHING TO MODFLOW2005 STRUCTURE, UNCOMMENT FOLLOWING LINE
C     USE GLOBAL,         ONLY: IOUT,NCOL,NROW,NLAY
      USE GLOBAL,      ONLY: NCOL,NROW,NLAY,IFREFM,IOUT
      USE VDFMODULE,   ONLY: MFNADVFD,NSWTCPL,IWTABLE,
     1                       NSRHOEOS,DENSEMIN,DENSEMAX,DENSEREF,
     2                       FIRSTDT,DRHODPRHD,PRHDREF,HDRY,
     3                       HNOFLO,MTRHOSPEC,PS,RHOCR,RHOCC,RHOCV,
     4                       HSALT,ELEV,DCDT,COLDFLW,PSOLDITER,DRHODC,
     5                       CRHOREF,
     6                       VDFNCOL=>NCOL,
     7                       VDFNROW=>NROW,
     8                       VDFNLAY=>NLAY
      CHARACTER*200 LINE
C
C     ------------------------------------------------------------------      
C1----ALLOCATE AND INITIALIZE VARIABLES (NOTE: NSWTCPL ALLOCATED IN MAIN)
      ALLOCATE(MFNADVFD,IWTABLE,NSRHOEOS,DENSEMIN,
     1         DENSEMAX,DENSEREF,DRHODPRHD,PRHDREF,HDRY,HNOFLO)
      ALLOCATE(PS(NCOL,NROW,NLAY),RHOCR(NCOL,NROW,NLAY),
     1         RHOCC(NCOL,NROW,NLAY),RHOCV(NCOL,NROW,NLAY),
     2         HSALT(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY),
     3         DCDT(NCOL,NROW,NLAY),COLDFLW(NCOL,NROW,NLAY,1),
     4         PSOLDITER(NCOL,NROW,NLAY))
C
C------STORE NCOL, NROW, AND NLAY IN THE VDFMODULE
      ALLOCATE(VDFNCOL,VDFNROW,VDFNLAY,NSWTCPL)
      VDFNCOL=NCOL
      VDFNROW=NROW
      VDFNLAY=NLAY
C
C1-----INITIALIZE VARIABLES
      DRHODPRHD=0.0
      PRHDREF=0.0
C
C2-----IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'VDF -- VARIABLE DENSITY FLOW, VERSION 1',
     1', 2/13/2004',/,9X,'INPUT READ FROM UNIT',I3,/)
     
C-----SET VDF INPUT
      DENSEMIN = 0. !computed fluid density is not limited by DENSEMIN
      DENSEMAX = 0. !computed fluid density is not limited by DENSEMAX
      DENSEREF = 1000.
      IWTABLE = 0   !Flag used to activate the variable-density water-table corrections (= 0: inactive)
      NSWTCPL = 0   !Maximum number of nonlinear coupling iterations for the flow and transport solutions
      MFNADVFD = 1  !Flag that determines how the internodal density values used to conserve fluid mass will be calculated (= 2: central-in-space; else: upstream-weighted)
      
      
C
C3----READ INPUT VARIABLES FROM VDF INPUT FILE
CVDF1      CALL URDCOM(IN,IOUT,LINE)
CVDF1      LLOC = 1
CVDF1      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MT3DRHOFLG,DUM,IOUT,IN)
CVDF1      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MFNADVFD,DUM,IOUT,IN)
CVDF1      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWTCPL,DUM,IOUT,IN)
CVDF1      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWTABLE,DUM,IOUT,IN)
C
C4----READ DENSITY LIMITERS
CVDF1      IF(IFREFM.EQ.0) THEN
CVDF1        READ(IN,'(2F10.0)') DENSEMIN,DENSEMAX
CVDF1      ELSEIF(IFREFM.NE.0) THEN
CVDF1        READ(IN,*) DENSEMIN,DENSEMAX
CVDF1      ENDIF
C
C5----SEAWAT: READ DNSCRIT
CVDF1      IF((NSWTCPL.GT.1.OR.NSWTCPL.EQ.-1).AND.IFREFM.EQ.0) THEN
CVDF1        READ(IN,'(F10.0)') DNSCRIT
CVDF1     ELSEIF((NSWTCPL.GT.1.OR.NSWTCPL.EQ.-1).AND.IFREFM.NE.0) THEN
CVDF1        READ(IN,*) DNSCRIT
CVDF1      ENDIF
C-----SEAWAT: RESET DNSCRIT TO DEFAULT IF NOT ENTERED
CVDF1      IF (DNSCRIT.EQ.0.) DNSCRIT=0.0
C
C6----SEAWAT: PRINT IMPLICIT COUPLING INFO TO IOUT
CVDF1      IF (NSWTCPL.EQ.-1) WRITE(IOUT,556) DNSCRIT
      IF (NSWTCPL.EQ.0) NSWTCPL=1
CVDF1      IF (NSWTCPL.GT.1) THEN
CVDF1        WRITE(IOUT,558) NSWTCPL,DNSCRIT
CVDF1      ELSE
        WRITE(IOUT,559)
CVDF1      ENDIF
  556 FORMAT(1X,'FLOW FIELD UPDATE OPTION IS ACTIVE',
     &       /1X,'FLOW FIELD WILL BE RECALCULATED ONLY IF',
     &       /1X,'MAXIMUM DENSITY CHANGE IS GREATER THAN ',G10.4)
  558 FORMAT(1X,'COUPLING BETWEEN FLOW AND TRANSPORT IS ITERATIVE',
     &       /1X,G10.4,' COUPLING ITERATIONS',
     &       /1X,G10.4,' IS THE DENSITY CONVERGENCE CRITERIA')
  559 FORMAT(1X,'COUPLING BETWEEN FLOW AND TRANSPORT IS EXPLICIT')
C
C7----WRITE INFO ABOUT WATER-TABLE CORRECTION
      IF (IWTABLE.EQ.0) WRITE(IOUT,600)
  600 FORMAT(1X,'VARIABLE-DENSITY WATER-TABLE CORRECTIONS NOT ADDED')
      IF (IWTABLE.GT.0) WRITE(IOUT,610)
  610 FORMAT(1X,'VARIABLE-DENSITY WATER-TABLE CORRECTIONS ARE ADDED')
C
C8----PRINT MT3DRHOFLG INFO TO OUTPUT FILE
CVDF1      IF(MT3DRHOFLG.EQ.0) THEN
        WRITE(IOUT,500)
CVDF1      ELSE
CVDF1        WRITE(IOUT,510) MT3DRHOFLG
CVDF1      ENDIF
  500 FORMAT(1X,'FLUID DENSITY IS SPECIFIED BY USER IN THE VDF FILE')
  510 FORMAT(1X,'MT3DMS SPECIES USED IN EQUATION OF STATE FOR FLUID DENS
     &ITY: ',I10)
C
C7----PRINT MFNADVFD INFO TO OUTPUT FILE
      IF(MFNADVFD.EQ.2) THEN
        WRITE(IOUT,553)
      ELSE
        WRITE(IOUT,557)
      ENDIF
  553 FORMAT(1X,'A CENTRAL-IN-SPACE-WEIGHTED ALGORITHM IS USED TO CALCUT
     &E FLUID DENSITY TERMS THAT CONSERVE MASS')
  557 FORMAT(1X,'AN UPSTREAM-WEIGHTED ALGORITHM IS USED TO CALCULATE FLU
     &ID DENSITY TERMS THAT CONSERVE MASS')
C
C9----READ DENSITY EQUATION OF STATE INFORMATION USING SIMPLIFIED METHOD
CVDF1      IF(MT3DRHOFLG.GE.0) THEN
        NSRHOEOS=1
        ALLOCATE(MTRHOSPEC(NSRHOEOS),DRHODC(NSRHOEOS),CRHOREF(NSRHOEOS))
        MTRHOSPEC(1)=1
        CRHOREF(1)=0.
        DRHODC(1) = 1.316
CVDF1        IF(IFREFM.EQ.0) THEN
CVDF1            READ(IN,'(2F10.0)') DENSEREF,DRHODC(1)
CVDF1        ELSE
CVDF1            READ(IN,*) DENSEREF,DRHODC(1)
CVDF1        ENDIF

        WRITE(IOUT,560) DENSEREF,DRHODC(1)
  560 FORMAT(1X,G10.4,' REFERENCE DENSITY',
     &       /1X,G10.4,' DENSITY SLOPE FOR EQUATION OF STATE')
CVDF1      ENDIF
C

C
C11---ECHO DENSITY INFORMATION TO OUTPUT FILE
      WRITE(IOUT,2)
    2 FORMAT(1X,'DENSITY EQUATION OF STATE')
      WRITE(IOUT,3) DENSEREF
    3 FORMAT(1X,'RHO = ',G15.6)
      DO I=1,NSRHOEOS
        WRITE(IOUT,4) DRHODC(I),MTRHOSPEC(I),CRHOREF(I)
      ENDDO
    4 FORMAT(1X,' + ',G10.4,' * ( CONC(',I4,') - ',G10.4,' )')
      IF(DRHODPRHD.NE.0) WRITE(IOUT,5) DRHODPRHD,PRHDREF
    5 FORMAT(1X,' + ',G10.4,' * ( PRESS HEAD - ',G10.4,' )')
      WRITE(IOUT,'(//)')
C
C12---READ FIRSTDT, AND WRITE TO IOUT
CVDF1      IF(IFREFM.EQ.0) THEN
CVDF1        READ(IN,'(F10.0)') FIRSTDT
CVDF1      ELSEIF(IFREFM.NE.0) THEN
CVDF1        READ(IN,*) FIRSTDT
CVDF1      ENDIF
CVDF1      WRITE(IOUT,590) FIRSTDT
CVDF1  590 FORMAT(1X,'FIRSTDT SPECIFIED BY USER IN THE VDF FILE IS: ',
CVDF1     &       G15.7)
C
C7----RESET POINTERS      
      CALL SVDF2PSV(IGRID)
C     
      RETURN
      END
C
C
      SUBROUTINE VDF2DF(IN,
     &                  ISUMY,ISUMIY,LCCNEW,
     &                  LTCRCH,LTCEVT)
C **********************************************************************
CVDF THIS SUBROUTINE DEFINES THE VARIABLE-DENSITY FLOW PROCESS
C **********************************************************************
CVDF CREATED 11/20/01 FOR MF2K VDF PROCESS
C
      USE GLOBAL,      ONLY: NCOL,NROW,NLAY,IFREFM,IOUT
      INTEGER(KIND=8) :: ISUMY,ISUMIY,LCCNEW,LCSSM,LCSSMC,LTCRCH,LTCEVT ! JV
C----------------------------------------------------------------------
C
C--SEAWAT: ALLOCATE SPACE FOR MT3D ARRAYS EVEN IF NOT USED
      NSSVL=7
CVDF    IF(INBTN.EQ.0) THEN
        NCOMP=1
        ISUMY=1
        ISUMIY=1
        LCCNEW=ISUMY
        ISUMY=ISUMY+NCOL*NROW*NLAY*NCOMP
        LTCRCH=ISUMY
        ISUMY=ISUMY+NCOL*NROW*NCOMP
        LTCEVT=ISUMY
        ISUMY=ISUMY+NCOL*NROW*NCOMP
        WRITE(IOUT,1100) ISUMY,ISUMIY
 1100 FORMAT(1X,I8,' DUMMY MT3DMS ELEMENTS USED FOR Y ARRAY ',
     &       1X,I8,' DUMMY MT3DMS ELEMENTS USED FOR IY ARRAY'/)
CVDF  ENDIF
C
C
C--SWT: STOP PROGRAM IN THIS CIRCUMSTANCE
CVDF      IF(MT3DRHOFLG.GT.0.AND.INBTN.EQ.0) THEN
CVDF        WRITE(IOUT,580)
CVDF        STOP
CVDF      ENDIF
CVDF  580 FORMAT(1X,'ERROR: MT3DRHOFLG GREATER THAN ZERO, BUT MT3DMS NOT USE
CVDF     &D')
C
      RETURN
      END
C
C
      SUBROUTINE VDF2IZ()
C     ******************************************************************
C     INITIALIZE ELEVATION ARRAY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C-----WHEN SWITCHING TO MODFLOW2005 STRUCTURE, UNCOMMENT FOLLOWING LINE
      USE GLOBAL,         ONLY: IOUT,NCOL,NROW,NLAY,IBOUND,BOTM,NBOTM,
     1                          HNEW
      USE VDFMODULE,   ONLY: NSWTCPL,HSALT,ELEV,DCDT,
     1                       HDRY,HNOFLO
C
C
C     ------------------------------------------------------------------
C
C       INITIALIZE ELEVATION ARRAY
C       WILL NOT WORK IF QUASI-3D LAYER INCLUDED IN MODEL
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
c         IF(IBOUND(J,I,K).EQ.0) CYCLE
                ELEV(J,I,K)=(BOTM(J,I,K-1)-BOTM(J,I,K))/2+BOTM(J,I,K)
      ENDDO
      ENDDO
      ENDDO
C
C--SEAWAT: SET HSALT EQUAL TO HNEW (AT THIS POINT, HNEW AND HSALT ARE STARTING HEADS)
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
       HSALT(J,I,K)=HNEW(J,I,K)
      ENDDO
      ENDDO
      ENDDO
!      HSALT=HNEW
C
C--SEAWAT: INITIALIZE DCDT TO ZERO
      DCDT=0.
C
C--SEAWAT: COPY THE VALUE OF MODFLOW HDRY INTO THE VDF MODULE HDRY VARIABLE
CVDF      HDRY=HDRYMFLW
CVDF      HNOFLO=HNOFLOMFLW
C
C
C-----PERFORM CHECK FOR SEAWAT'S ITERATIVE COUPLING SCHEME
CVDF      IF (MT3DRHOFLG.NE.0.AND.NSWTCPL.GT.1.AND.MIXELM.GT.0) THEN
CVDF          WRITE(IOUT,*) 'MIXELM MUST BE LESS THAN OR EQUAL TO ZERO TO US
CVDF     +E ITERATIVE FLOW-TRANSPORT COUPLING'
CVDF          WRITE(*,*)    'MIXELM MUST BE LESS THAN OR EQUAL TO ZERO TO US
CVDF     +E ITERATIVE FLOW-TRANSPORT COUPLING'
CVDF          CALL USTOP('')
CVDF      ENDIF
      IF(NSWTCPL.EQ.0) NSWTCPL=1
C
C
C--SEAWAT: CHECK FOR A TIMPRS VALUE OF ZERO AND STOP IF DETECTED
CVDF      IF(INBTN.GT.0.AND.MT3DRHOFLG.NE.0) THEN
CVDF          DO I=1,NPRS
CVDF            IF(TIMPRS(I).EQ.0.0) THEN
CVDF                WRITE(IOUT,100)
CVDF                WRITE(*,100)
CVDF                CALL USTOP('')
CVDF            ENDIF
CVDF          ENDDO
CVDF      ENDIF
C
C
 100  FORMAT(/'Error: If flow and transport are coupled, SEAWAT cannot '
     &        ,'accept an MT3DMS print time of zero.  Stopping...')
C
      RETURN
      END     
C
C
      SUBROUTINE VDF2RPSS(KKPER,IN,IGRID)
C***********************************************************************
C--SEAWAT: INITIALIZE DENSITY AND CALCULATE HNEW AS FRESHWATER HEAD
C--SEAWAT:  THIS SUBROUTINE CALLED ONLY ONCE AT BEGINNING OF SIMULATION
C--SEAWAT:  CANNOT BE DONE WITHIN 
C      CREATED 11/20/01
C      THE USER IS ALLOWED TWO DIFFERENT OPTIONS FOR
C      TREATING FLUID DENSITY.
C          1.  CALCULATED FROM CONCENTRATION
C          2.  SPECIFIED IN VDF INPUT FILE
C***********************************************************************
      USE VDFMODULE,   ONLY: DENSEREF,PS,HSALT,ELEV,
     &                       MTRHOSPEC
      USE GLOBAL,      ONLY: NCOL,NROW,NLAY,HNEW,IFREFM,IBOUND,BOTM,IOUT
C
      CHARACTER*24 ANAME(4)
      DATA ANAME(1) /'     DENSITY LAYER INDEX'/
      DATA ANAME(2) /'           FLUID DENSITY'/
      DATA ANAME(3) /'           CONCENTRATION'/
C-----------------------------------------------------------------------
C     POPULATE FLUID DENSITY ARRAY (PS)
CVDF1      IF(MT3DRHOFLG.EQ.0) THEN
CVDF1        IF(IFREFM.EQ.0) THEN
CVDF1          READ(IN,'(I10)') INDENSE
CVDF1        ELSE
CVDF1          READ(IN,*) INDENSE
CVDF1        ENDIF
        INDENSE = 2
        WRITE(IOUT,'(//)')
        WRITE(IOUT,500) INDENSE
        IF(INDENSE.LT.0) WRITE(IOUT,510)
        IF(INDENSE.EQ.0) WRITE(IOUT,520)
        IF(INDENSE.GT.0) WRITE(IOUT,530)
        IF(INDENSE.EQ.2) WRITE(IOUT,540)

C     IF INDENSE GREATER THAN ZERO, THEN READ DENSE ARRAY
        ITEMP=2
        IF(INDENSE.EQ.2) ITEMP=3
        IF(INDENSE.GT.0) THEN
          DO K=1,NLAY
            CALL U2DREL(PS(1,1,K),ANAME(ITEMP),NROW,NCOL,K,IN,
     &                        IOUT)
          ENDDO
        ENDIF
C     IF INDENSE EQUAL TO 2, THEN CONVERT DENSITY ARRAY USING EQUATION OF STATE
        IF (INDENSE.EQ.2) THEN
          DO K=1,NLAY
          DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0)
     &      PS(J,I,K)=CALCDENS(J,I,K,PS(J,I,K))
          ENDDO
          ENDDO
          ENDDO
        ENDIF
CVDF1      ELSE
CVDF1C       SET DENSITY ARRAY USING CONCENTRATIONS FROM MT3D IF FIRST STRESS PERIOD
CVDF1        IF(KKPER.EQ.1) THEN
CVDF1          PS=DENSEREF
CVDF1          DO K=1,NLAY
CVDF1          DO I=1,NROW
CVDF1          DO J=1,NCOL
CVDF1           IF(CNEW(J,I,K,MTRHOSPEC(1)).NE.CINACT) 
CVDF1     &         PS(J,I,K)=CALCDENS(J,I,K,CNEW(J,I,K,1:NCOMP))
CVDF1          ENDDO
CVDF1          ENDDO
CVDF1          ENDDO
CVDF1        ENDIF
CVDF1      ENDIF
C
C     IF FIRST STRESS PERIOD, CALCULATE HNEW
      IF(KKPER.EQ.1) THEN
        DO K=1,NLAY
        DO I=1,NROW
        DO J=1,NCOL
            IF(IBOUND(J,I,K).EQ.0) CYCLE
            HNEW(J,I,K)=FEHEAD(HSALT(J,I,K),PS(J,I,K),ELEV(J,I,K))
        ENDDO
        ENDDO
        ENDDO
      ENDIF
  500 FORMAT(1X,'INDENSE VALUE SPECIFIED AS:',I4)
  510 FORMAT(1X,'VALUES FOR DENSE ARRAY WILL BE REUSED OR SET TO DENSERE
     +F')
  520 FORMAT(1X,'VALUES FOR THE DENSE ARRAY WILL BE SET TO DENSEREF')
  530 FORMAT(1X,'VALUES FOR THE DENSE ARRAY WILL BE READ FROM VDF FILE')
  540 FORMAT(1X,'VALUES READ AS CONCENTRATION WILL BE CONVERTED TO FLUID
     + DENSITY USING EQUATION OF STATE')
      RETURN
      END
C
C
      SUBROUTINE VDF2FM(IUNITBCF,IUNITLPF,IUNITHUF)
******************************************************************************
C--SEAWAT: CALCULATE DENSITY TERMS THAT ARE SUBTRACTED FROM THE RHS ACCUMULATOR
C--SEAWAT: CALCULATE RHOCR, RHOCC, AND RHOCV
C--SEAWAT: CALCULATE WATER TABLE CORRECTIONS
****************************************************************************** 
      USE GLOBAL,      ONLY: IOUT,NCOL,NROW,NLAY,HNEW,STRT,DDREF,IBOUND,
     &                       HNEW,RHS,HCOF,DELR,DELC,BOTM,NBOTM,CR,CC,CV
      USE VDFMODULE,   ONLY: MFNADVFD,IWTABLE,DENSEREF,PS,
     &                       RHOCR,RHOCC,RHOCV,HSALT,ELEV,DCDT
C
      DOUBLE PRECISION H1,HS1,H2,HS2,HDIFF
      LOGICAL CORRECT
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /BCFCOM/LAYCON(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C-----------------------------------------------------------------------------
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
       if(k.eq.3.and.i.eq.24.and.j.eq.45)then
        write(*,*) 
       endif
         D1=0.
         D2=0.
         D3=0.
         D4=0.
         D5=0.
         D6=0.
         IF(IBOUND(J,I,K).LE.0) GOTO 70
C--SEAWAT: CALCULATE DENSITY TERM LEFT (J-1,I,K): D1
         IF(J.EQ.1) GOTO 10
         IF(IBOUND(J-1,I,K).EQ.0) GOTO 10
            H1=HNEW(J,I,K)
            HS1=HSALT(J,I,K)
            H2=HNEW(J-1,I,K)
            HS2=HSALT(J-1,I,K)
            Z1=ELEV(J,I,K)
            Z2=ELEV(J-1,I,K)
            PS1=PS(J,I,K)
            PS2=PS(J-1,I,K)
            TOP1=BOTM(J,I,K-1)
            TOP2=BOTM(J-1,I,K-1)
            BOT1=BOTM(J,I,K)
            BOT2=BOTM(J-1,I,K)
C--SEAWAT: WATER TABLE CORRECTIONS
C--SEAWAT: WTCORR-BCF
            IF(IWTABLE.EQ.1) THEN
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
             ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELR(J-1)/2
            DIS2=DELR(J)/2
            AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D1=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL INSPACE
            IF(MFNADVFD.EQ.2) THEN   
               D1=D1*AVGDENS
            ELSE    
C--SEAWAT: CONSERVE MASS WITH UPSTREAM VALUES
C               HDIFF=HNEW(J-1,I,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CR(J-1,I,K)*HDIFF+D1
               IF (FLOWDIR.GT.0.) THEN
                   D1=D1*PS(J-1,I,K)
               ELSE
                   D1=D1*PS(J,I,K)
               ENDIF
            ENDIF
C--SEAWAT: CALCULATE DENSITY TERM RIGHT (J+1,I,K): D2
   10       IF(J.EQ.NCOL) GOTO 20
            IF(IBOUND(J+1,I,K).EQ.0) GOTO 20
            H1=HNEW(J,I,K)
            HS1=HSALT(J,I,K)
            H2=HNEW(J+1,I,K)
            HS2=HSALT(J+1,I,K)
            Z1=ELEV(J,I,K)
            Z2=ELEV(J+1,I,K)
            PS1=PS(J,I,K)
            PS2=PS(J+1,I,K)
            TOP1=BOTM(J,I,K-1)
            TOP2=BOTM(J+1,I,K-1)
            BOT1=BOTM(J,I,K)
            BOT2=BOTM(J+1,I,K)
C--SEAWAT: WATER TABLE CORRECTIONS
C--SEAWAT: WTCORR-BCF
            IF(IWTABLE.EQ.1) THEN
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
            ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELR(J+1)/2
            DIS2=DELR(J)/2
            AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL INSPACE
            IF(MFNADVFD.EQ.2) THEN
               D2=D2*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM VALUES
C              HDIFF=HNEW(J+1,I,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CR(J,I,K)*HDIFF+D2
             IF (FLOWDIR.GT.0.) THEN
               D2=D2*PS(J+1,I,K)
             ELSE
               D2=D2*PS(J,I,K)
             ENDIF
            ENDIF
C--SEAWAT: CALCULATE DENSITY TERM BACK (J,I-1,K): D3
   20       IF(I.EQ.1) GOTO 30
            IF(IBOUND(J,I-1,K).EQ.0) GOTO 30
            H1=HNEW(J,I,K)
            HS1=HSALT(J,I,K)
            H2=HNEW(J,I-1,K)
            HS2=HSALT(J,I-1,K)
            Z1=ELEV(J,I,K)
            Z2=ELEV(J,I-1,K)
            PS1=PS(J,I,K)
            PS2=PS(J,I-1,K)
            TOP1=BOTM(J,I,K-1)
            TOP2=BOTM(J,I-1,K-1)
            BOT1=BOTM(J,I,K)
            BOT2=BOTM(J,I-1,K)
C--SEAWAT: WATER TABLE CORRECTIONS
            IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
             ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELC(I-1)/2
            DIS2=DELC(I)/2
            AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D3=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL INSPACE
            IF(MFNADVFD.EQ.2) THEN 
               D3=D3*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
C              HDIFF=HNEW(J,I-1,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CC(J,I-1,K)*HDIFF+D3
             IF (FLOWDIR.GT.0.) THEN
               D3=D3*PS(J,I-1,K)
             ELSE
               D3=D3*PS(J,I,K)
             ENDIF
            ENDIF

C--SEAWAT: CALCULATE DENSITY TERM FRONT (J,I+1,K): D4
   30       IF(I.EQ.NROW) GOTO 40
            IF(IBOUND(J,I+1,K).EQ.0) GOTO 40
             H1=HNEW(J,I,K)
             HS1=HSALT(J,I,K)
             H2=HNEW(J,I+1,K)
             HS2=HSALT(J,I+1,K)
             Z1=ELEV(J,I,K)
             Z2=ELEV(J,I+1,K)
             PS1=PS(J,I,K)
             PS2=PS(J,I+1,K)
             TOP1=BOTM(J,I,K-1)
             TOP2=BOTM(J,I+1,K-1)
             BOT1=BOTM(J,I,K)
             BOT2=BOTM(J,I+1,K)
C--SEAWAT: WATER TABLE CORRECTIONS
            IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
             ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELC(I+1)/2
            DIS2=DELC(I)/2
            AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL IN SPACE
            IF(MFNADVFD.EQ.2) THEN 
               D4=D4*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
C                HDIFF=HNEW(J,I+1,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CC(J,I,K)*HDIFF+D4
               IF (FLOWDIR.GT.0.) THEN
                  D4=D4*PS(J,I+1,K)
               ELSE
                  D4=D4*PS(J,I,K)
               ENDIF
            ENDIF

C--SEAWAT: CALCULATE DENSITY TERM UP (J,I,K-1): D5
   40       IF(K.EQ.1) GOTO 50
            IF(IBOUND(J,I,K-1).EQ.0) GOTO 50
            DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
            DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
            AVGDENS=1000.0; IF(DIS1+DIS2.GT.0.0)THEN
             AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            ENDIF
            D5=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K-1)-ELEV(J,I,K))
C--SEAWAT: CONSERVE MASS WITH CENTRAL IN SPACE
            IF(MFNADVFD.EQ.2) THEN 
               D5=D5*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
              HDIFF=HNEW(J,I,K-1)-HNEW(J,I,K)
              FLOWDIR=CV(J,I,K-1)*HDIFF+D5
C--SEAWAT: CHECK AND CORRECT FOR DEWATERED CASE
              IF(IUNITBCF.GT.0) THEN
                IFLG=0
                IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) IFLG=1
              ENDIF
              IF(IUNITLPF.GT.0) IFLG=LAYTYP(K)
              IF(IUNITHUF.GT.0) IFLG=LTHUF(K)
              IF(IFLG.GT.0) THEN
                HS2=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
                BOT1=BOTM(J,I,LBOTM(K)-1)
                IF(HS2.LT.BOT1) THEN
                  HS1=SALTHEAD(HNEW(J,I,K-1),PS(J,I,K-1),ELEV(J,I,K-1))
                  FLOWDIR=PS(J,I,K-1)/DENSEREF*CV(J,I,K-1)*(HS1-BOT1)
                ENDIF
              ENDIF
C--SEAWAT: END DEWATERED CORRECTION
              IF (FLOWDIR.GT.0.) THEN
                 D5=D5*PS(J,I,K-1)
              ELSE
                 D5=D5*PS(J,I,K)
              ENDIF
            ENDIF
C
C--SEAWAT: CALCULATE DENSITY TERM DOWN (J,I,K+1): D6
   50       IF(K.EQ.NLAY) GOTO 60
            IF(IBOUND(J,I,K+1).EQ.0) GOTO 60
            DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
            DIS2=ELEV(J,I,K)-BOTM(J,I,K)
            AVGDENS=1000.0; IF(DIS1+DIS2.GT.0.0)THEN
             AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            ENDIF
            D6=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K+1)-ELEV(J,I,K))
C--SEAWAT: CONSERVE MASS WITH CENTRAL IN SPACE
            IF(MFNADVFD.EQ.2) THEN 
              D6=D6*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
              HDIFF=HNEW(J,I,K+1)-HNEW(J,I,K)
              FLOWDIR=CV(J,I,K)*HDIFF+D6
C--CHECK AND CORRECT FOR DEWATERED CASE
              IF(IUNITBCF.GT.0) THEN
                IFLG=0
                IF(LAYCON(K+1).EQ.3 .OR. LAYCON(K+1).EQ.2) IFLG=1
              ENDIF
              IF(IUNITLPF.GT.0) IFLG=LAYTYP(K+1)
              IF(IUNITHUF.GT.0) IFLG=LTHUF(K+1)
              IF(IFLG.GT.0) THEN
                HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
                BOT1=BOTM(J,I,LBOTM(K+1)-1)
                IF(HS2.LT.BOT1) THEN
                  HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
                  FLOWDIR=PS(J,I,K)/DENSEREF*CV(J,I,K)*(BOT1-HS1)
                ENDIF
              ENDIF
C--SEAWAT: END DEWATERED CORRECTION
             IF (FLOWDIR.GT.0.) THEN
                D6=D6*PS(J,I,K+1)
             ELSE
                D6=D6*PS(J,I,K)
             ENDIF
            ENDIF
   60      CONTINUE
C--SEAWAT: SUBTRACT DENSITY TERMS AND DCDT FROM RHS ACCUMULATOR
           RHS(J,I,K)=RHS(J,I,K)-D1-D2-D3-D4-D5-D6
           if(j.eq.45.and.i.eq.24.and.k.eq.3)then
            write(*,*) rhs(45,24,3)           
           endif
CVDF           IF(MT3DRHOFLG.NE.0) RHS(J,I,K)=RHS(J,I,K)+DCDT(J,I,K)       
   70 CONTINUE
      ENDDO
      ENDDO
      ENDDO
C--SEAWAT:**********CALCULATE MASS CONDUCTANCES*************************
C--SEAWAT: CALCULATE RHOCR (PASSED TO SOLVER TO CONSERVE MASS)
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL-1
       RHOCR(J,I,K)=CR(J,I,K)*PS(J,I,K)
       IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J+1,I,K).NE.0) THEN
        H1=HNEW(J,I,K)
        HS1=HSALT(J,I,K)
        H2=HNEW(J+1,I,K)
        HS2=HSALT(J+1,I,K)
        Z1=ELEV(J,I,K)
        Z2=ELEV(J+1,I,K)
        PS1=PS(J,I,K)
        PS2=PS(J+1,I,K)
        TOP1=BOTM(J,I,K-1)
        TOP2=BOTM(J+1,I,K-1)
        BOT1=BOTM(J,I,K)
        BOT2=BOTM(J+1,I,K)
C--SEAWAT WATER TABLE CORRECTIONS
        IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
          IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
          IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
          IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
        ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
        DIS1=DELR(J+1)/2
        DIS2=DELR(J)/2
        AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
        D2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
        HDIFF=H2-H1
        FLOWDIR=CR(J,I,K)*HDIFF+D2
        IF(MFNADVFD.EQ.2) THEN 
           RHOCR(J,I,K)=CR(J,I,K)*AVGDENS
        ELSE
         IF (FLOWDIR.GT.0.) THEN
           RHOCR(J,I,K)=CR(J,I,K)*PS(J+1,I,K)
         ELSE
           RHOCR(J,I,K)=CR(J,I,K)*PS(J,I,K)
         ENDIF
        ENDIF
       ENDIF
      ENDDO
      ENDDO
      ENDDO
C       CALCULATE RHOCC
      DO K=1,NLAY
      DO I=1,NROW-1
      DO J=1,NCOL
       RHOCC(J,I,K)=CC(J,I,K)*PS(J,I,K)
       IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I+1,K).NE.0) THEN
        H1=HNEW(J,I,K)
        HS1=HSALT(J,I,K)
        H2=HNEW(J,I+1,K)
        HS2=HSALT(J,I+1,K)
        Z1=ELEV(J,I,K)
        Z2=ELEV(J,I+1,K)
        PS1=PS(J,I,K)
        PS2=PS(J,I+1,K)
        TOP1=BOTM(J,I,K-1)
        TOP2=BOTM(J,I+1,K-1)
        BOT1=BOTM(J,I,K)
        BOT2=BOTM(J,I+1,K)
C--SEAWAT WATER TABLE CORRECTIONS
        IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
          IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
          IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
          IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
        ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
          DIS1=DELC(I+1)/2
          DIS2=DELC(I)/2
          AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
          D4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
          HDIFF=H2-H1
          FLOWDIR=CC(J,I,K)*HDIFF+D4
          IF(MFNADVFD.EQ.2) THEN 
            RHOCC(J,I,K)=CC(J,I,K)*AVGDENS
          ELSE
            IF (FLOWDIR.GT.0.) THEN
               RHOCC(J,I,K)=CC(J,I,K)*PS(J,I+1,K)
            ELSE
               RHOCC(J,I,K)=CC(J,I,K)*PS(J,I,K)
            ENDIF
          ENDIF
       ENDIF
      ENDDO
      ENDDO
      ENDDO
                
C       CALCULATE RHOCV
      DO K=1,NLAY-1
      DO I=1,NROW
      DO J=1,NCOL
        RHOCV(J,I,K)=CV(J,I,K)*PS(J,I,K)
        IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I,K+1).NE.0) THEN
          DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
          DIS2=ELEV(J,I,K)-BOTM(J,I,K)
          AVGDENS=1000.0; IF(DIS1+DIS2.GT.0.0)THEN
           AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
          ENDIF
          D6=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K+1)-ELEV(J,I,K))
          HDIFF=HNEW(J,I,K+1)-HNEW(J,I,K)
          FLOWDIR=CV(J,I,K)*HDIFF+D6
C--CHECK AND CORRECT FOR DEWATERED CASE
          IF(IUNITBCF.GT.0) THEN
            IFLG=0
            IF(LAYCON(K+1).EQ.3 .OR. LAYCON(K+1).EQ.2) IFLG=1
          ENDIF
          IF(IUNITLPF.GT.0) IFLG=LAYTYP(K+1)
          IF(IUNITHUF.GT.0) IFLG=LTHUF(K+1)
          IF(IFLG.GT.0) THEN
            HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
            BOT1=BOTM(J,I,LBOTM(K+1)-1)
            IF(HS2.LT.BOT1) THEN
              HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
              FLOWDIR=PS(J,I,K)/DENSEREF*CV(J,I,K)*(BOT1-HS1)
            ENDIF
          ENDIF
C--SEAWAT: END DEWATERED CORRECTION
          IF(MFNADVFD.EQ.2) THEN
            RHOCV(J,I,K)=CV(J,I,K)*AVGDENS
          ELSE
           IF (FLOWDIR.GT.0.) THEN
            RHOCV(J,I,K)=CV(J,I,K)*PS(J,I,K+1)
           ELSE
            RHOCV(J,I,K)=CV(J,I,K)*PS(J,I,K)
           ENDIF
          ENDIF
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C--SEAWAT:MAKE CORRECTIONS FOR WATER-TABLE CASE
      IF (IWTABLE.EQ.0) GOTO 110
        DO K=1,NLAY
        DO I=1,NROW
        DO J=1,NCOL
          IF(LAYCON(K).NE.0.OR.LAYTYP(K).NE.0.OR.LTHUF(K).NE.0) THEN 
                C1=0.
                C2=0.
                C3=0.
                C4=0.
                B1=0.
                B2=0.
                B3=0.
                B4=0.
                IF (IBOUND(J,I,K).LE.0) GOTO 90
                ZWT2I=(HSALT(J,I,K)+BOTM(J,I,K))/2
C       CORRECT FOR FLOW TO LEFT
                IF(J.EQ.1) GOTO 85
                IF(IBOUND(J-1,I,K).EQ.0) GOTO 85
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J-1,I,K).LT.BOTM(J-1,I,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 85
                AVGDENS=0.5*(PS(J-1,I,K)+PS(J,I,K))
                B1=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &             (ELEV(J-1,I,K)-ELEV(J,I,K))
                HDIFF=HNEW(J-1,I,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J-1,I,K)+BOTM(J-1,I,K))/2

                C1=CR(J-1,I,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J-1,I,K)-ELEV(J,I,K)+
     &        ZWT2I-ZWT2NEXT)+(PS(J-1,I,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &         ELEV(J-1,I,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &         (ELEV(J,I,K)-ZWT2I))
                FLOWDIR=(CR(J-1,I,K)*HDIFF+B1)-C1
                IF (FLOWDIR.GT.0.) THEN
                  C1=C1*PS(J-1,I,K)
                ELSE
                  C1=C1*PS(J,I,K)
                ENDIF
   85     CONTINUE
C       CORRECT FOR FLOW TO RIGHT
                IF(J.EQ.NCOL) GOTO 86
                IF(IBOUND(J+1,I,K).EQ.0) GOTO 86
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J+1,I,K).LT.BOTM(J+1,I,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 86
                AVGDENS=0.5*(PS(J+1,I,K)+PS(J,I,K))
                B2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J+1,I,K)-ELEV(J,I,K))
                HDIFF=HNEW(J+1,I,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J+1,I,K)+BOTM(J+1,I,K))/2

                C2=CR(J,I,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J+1,I,K)-ELEV(J,I,K)+ZWT2I-ZWT2NEXT)+
     &       (PS(J+1,I,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &       ELEV(J+1,I,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K)-ZWT2I))

                FLOWDIR=(CR(J,I,K)*HDIFF+B2)-C2
                IF (FLOWDIR.GT.0.) THEN
                   C2=C2*PS(J+1,I,K)
                ELSE
                   C2=C2*PS(J,I,K)
                ENDIF
   86     CONTINUE
C       CORRECT FOR FLOW TO BACK
                IF(I.EQ.1) GOTO 87
                IF(IBOUND(J,I-1,K).EQ.0) GOTO 87
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J,I-1,K).LT.BOTM(J,I-1,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 87
                AVGDENS=0.5*(PS(J,I-1,K)+PS(J,I,K))
                B3=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J,I-1,K)-ELEV(J,I,K))
                HDIFF=HNEW(J,I-1,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J,I-1,K)+BOTM(J,I-1,K))/2
                C3=CC(J,I-1,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I-1,K)-ELEV(J,I,K)+ZWT2I-ZWT2NEXT)+
     &          (PS(J,I-1,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &          ELEV(J,I-1,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &          (ELEV(J,I,K)-ZWT2I))
                FLOWDIR=(CC(J,I-1,K)*HDIFF+B3)-C3
                IF (FLOWDIR.GT.0.) THEN
                  C3=C3*PS(J,I-1,K)
                ELSE
                  C3=C3*PS(J,I,K)
                ENDIF
   87           CONTINUE

C       CORRECT FOR FLOW TO FRONT
                IF(I.EQ.NROW) GOTO 90
                IF(IBOUND(J,I+1,K).EQ.0) GOTO 90
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J,I+1,K).LT.BOTM(J,I+1,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 90
                AVGDENS=0.5*(PS(J,I+1,K)+PS(J,I,K))
                B4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I+1,K)-ELEV(J,I,K))
                HDIFF=HNEW(J,I+1,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J,I+1,K)+BOTM(J,I+1,K))/2
                C4=CC(J,I,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I+1,K)-ELEV(J,I,K)+ZWT2I-ZWT2NEXT)+
     &          (PS(J,I+1,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &          ELEV(J,I+1,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &          (ELEV(J,I,K)-ZWT2I))
                FLOWDIR=(CC(J,I,K)*HDIFF+B4)-C4
                IF (FLOWDIR.GT.0.) THEN
                  C4=C4*PS(J,I+1,K)
                ELSE
                  C4=C4*PS(J,I,K)
                ENDIF
   90           CONTINUE
                RHS(J,I,K)=RHS(J,I,K)+C1+C2+C3+C4
C--SEAWAT: END IF FOR WT CORRECTIONS
       ENDIF
      ENDDO
      ENDDO
      ENDDO  
  110 CONTINUE
      RETURN
      END


      SUBROUTINE VDF2HSALT()
C **********************************************************************
C THIS SUBROUTINE UPDATES THE HSALT ARRAY
C 
C***********************************************************************
C CREATED 3/12/03
C
      USE GLOBAL,      ONLY: NCOL,NROW,NLAY,HNEW,IBOUND
      USE VDFMODULE,   ONLY: HSALT,PS,ELEV
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
        IF(IBOUND(J,I,K).NE.0) THEN
C       CALCULATE NATIVE HEAD FOR CONSTANT HEADS AND ACTIVE CELLS
        HSALT(J,I,K)=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
        ELSE
C         THIS WILL PUT HDRY INTO A CELL THAT WENT DRY
          HSALT(J,I,K)=HNEW(J,I,K)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      RETURN
      END
C
C
      SUBROUTINE VDF2BD(VBNM,VBVL,MSUM,IBOUND,DELT,NCOL,NROW,NLAY,
     &  KSTP,KPER,IBCFCB,ILPFCB,IHUFCB,ICBCFL,BUFF,IOUT,PERTIM,
     &  TOTIM)
C-----VERSION  9OCT1999 SWTBD
C     ******************************************************************
C     SUM THE DCDT TERMS FOR BUDGET
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      USE VDFMODULE,   ONLY: DENSEREF,PS,DCDT
C
      CHARACTER*4 VBNM
      CHARACTER*16 TEXT
      DIMENSION IBOUND(NCOL,NROW,NLAY),VBNM(4,MSUM),VBVL(4,MSUM), !VBNM(4,20),VBVL(4,20),
     +  BUFF(NCOL,NROW,NLAY)
      DATA TEXT /'            DCDT'/
C     ------------------------------------------------------------------
C1--INITIALIZE BUDGET ACCUMULATORS
        ZERO=0.
        DCDTIN=ZERO
        DCDTOUT=ZERO
C
C--SEAWAT: EXIT IF MT3D NOT USED (DCDT EQUAL TO ZERO)
CVDF      IF (MT3DRHOFLG.EQ.0)
        GOTO 500

C2--IF CELL-BY-CELL FLOWS ARE NEEDED THEN SET FLAG IBD
        IBD=0

C3--IF CELL-BY-CELL FLOWS ARE NEEDED (IBD IS SET) CLEAR BUFFER
        IF(IBCFCB.GT.0) IBD=ICBCFL
        IF(ILPFCB.GT.0) IBD=ICBCFL
        IF(IHUFCB.GT.0) IBD=ICBCFL


C4--------CLEAR BUFFER
        DO 210 K=1,NLAY
        DO 210 I=1,NROW
        DO 210 J=1,NCOL
        BUFF(J,I,K)=0.
  210 CONTINUE

C4--LOOP THROUGH EVERY CELL IN GRID
        do 400 K=1,NLAY
        do 400 I=1,NROW
        do 400 J=1,NCOL
C       SKIP NO-FLOW AND CONSTANT HEAD CELLS
                IF (IBOUND(J,I,K).LE.0) GOTO 400
c               switch sign of dcdt for mass balance calculation
                DCDTVOL=-1*DCDT(J,I,K)*DELT
                IF(IBD.EQ.1) BUFF(J,I,K)=DCDTVOL/PS(J,I,K)
                IF(DCDTVOL) 392,400,394
  392     DCDTOUT=DCDTOUT-DCDTVOL
          GOTO 400
  394     DCDTIN=DCDTIN+DCDTVOL
  400 CONTINUE  

C5--IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER
      ITEMP=0
      IF (IBCFCB.GT.0) ITEMP=IBCFCB
      IF (ILPFCB.GT.0) ITEMP=ILPFCB
      IF (IHUFCB.GT.0) ITEMP=IHUFCB
      IF(ITEMP.GT.0) THEN
        IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,ITEMP,BUFF,NCOL,NROW,
     1                           NLAY,IOUT)
        IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,ITEMP,BUFF,NCOL,NROW,
     1                           NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF


C6--ADD TOTAL RATES AND VOLUMES TO VBVL AND PUT TITLES IN VBNM
  500 CONTINUE      
      VBVL(1,MSUM)=VBVL(1,MSUM)+DCDTIN
      VBVL(2,MSUM)=VBVL(2,MSUM)+DCDTOUT
      IF(DELT.GT.0.) THEN
         VBVL(3,MSUM)=DCDTIN/DELT
         VBVL(4,MSUM)=DCDTOUT/DELT
      ENDIF
      VBNM(1,MSUM)='    '
      VBNM(2,MSUM)='    '
      VBNM(3,MSUM)='    '
      VBNM(4,MSUM)='DCDT'
      MSUM=MSUM+1

C7--RETURN
      RETURN
      END     
C
C
      SUBROUTINE VDF2CPL1(M,IOUT)
C--SEAWAT:***********************************************************
C--SEAWAT:  INITIALIZE SEAWAT'S ITERATIVE COUPLING SCHEME
C--SEAWAT:**********************************************************
      USE VDFMODULE,   ONLY: PS,PSOLDITER
C
      PRINT *
      PRINT *,'ITERATIVE COUPLING ITERATION ',M
      WRITE(IOUT,*) 'ITERATIVE COUPLING ITERATION ',M
      PRINT *
      PSOLDITER=PS
C
      RETURN
      END
C
C

C--SEAWAT: SALTHEAD FUNCTION
      FUNCTION SALTHEAD(HF,DENSE,ELEV)
C************************************************************************
C   FUNCTION SALTHEAD
C   MODIFIED 11/25/01 BY CDL
C   MODIFIED 3/25/02 BY WBS
C   FUNCTION RETURNS SALTHEAD 
C************************************************************************
      USE VDFMODULE,   ONLY: DENSEREF
C
      DOUBLE PRECISION HF
C-----------------------------------------------------------------------

!      if(dense.le.0.0)then
!       write(*,*) 'dense',dense; pause
!      endif
      
      SALTHEAD=HF*DENSEREF/DENSE+(DENSE-DENSEREF)/DENSE*ELEV
      END

C--SEAWAT: FEHEAD FUNCTION
      FUNCTION FEHEAD(HS,DENSE,ELEV)
C************************************************************************
C   FUNCTION FEHEAD
C   MODIFIED 11/25/01 BY CDL
C   MODIFIED 3/25/02 BY WBS
C   FUNCTION RETURNS FEHEAD
C************************************************************************
      USE VDFMODULE,   ONLY: DENSEREF
C
      DOUBLE PRECISION HS
C------------------------------------------------------------------------
      FEHEAD=HS*DENSE/DENSEREF-(DENSE-DENSEREF)/DENSEREF*ELEV
      END
C
C        
      FUNCTION CALCDENS(J,I,K,CONCENTRATION)
C************************************************************************
C   THIS FUNCTION CALCULATES FLUID DENSITY AS A FUNCTION OF AN ARRAY
C   OF CONCENTRATIONS AND THE FRESH PRESSURE HEAD.  THE CONCENTRATION 
C   ARRAY SHOULD CONSIST OF ONE CONCENTRATION FOR EACH MT3DMS SPECIES.  
C   BECAUSE THERE IS NO REQUIREMENT ON THE ORDER OF THE SPECIES IN 
C   MTRHOSPEC, THE CONCENTRATION ARRAY MAY BE OF SIZE NSRHOEOS OR NCOMP,
C   HENCE CONCENTRATION(*)
C************************************************************************
      USE VDFMODULE,   ONLY: DENSEREF,DENSEMIN,DENSEMAX,
     &                       DRHODC,NSRHOEOS,MTRHOSPEC,CRHOREF,
     &                       DRHODPRHD,PRHDREF,HSALT,ELEV,PS,
     &                       HDRY,HNOFLO
C
      DIMENSION CONCENTRATION(*)
C-----------------------------------------------------------------------
C
C-----CALCULATE DENSITY AS A FUNCTION OF MT3DMS SPECIES
      TEMP=DENSEREF
      DO ISPEC=1,NSRHOEOS
        TEMP=TEMP+
     +            DRHODC(ISPEC)*(CONCENTRATION(MTRHOSPEC(ISPEC))-
     +                           CRHOREF(ISPEC))
      ENDDO
C
C-----INCLUDE THE EFFECTS OF PRESSURE ON FLUID DENSITY IF CELL IS ACTIVE
C-----AND DRHODPRHD.NE.0
      IF(DRHODPRHD.NE.0.0) THEN
        IF(HSALT(J,I,K).NE.HDRY.AND.HSALT(J,I,K).NE.HNOFLO) THEN
            PRHD=FEHEAD(HSALT(J,I,K),PS(J,I,K),ELEV(J,I,K))-ELEV(J,I,K)
            TEMP=TEMP+DRHODPRHD*(PRHD-PRHDREF)
        ENDIF
      ENDIF
C
C-----ENFORCE DENSITY LIMITERS IF CALCULATED DENSITY OUTSIDE OF SPECIFIED RANGE
      IF(DENSEMAX.NE.0.AND.TEMP.GT.DENSEMAX) THEN
        TEMP=DENSEMAX
      ENDIF
      IF(DENSEMIN.NE.0.AND.TEMP.LT.DENSEMIN) THEN
        TEMP=DENSEMIN
      ENDIF
C
C-----SET FUNCTION CALCDENS
      CALCDENS=TEMP
      RETURN
      END
C
C
      SUBROUTINE VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,H2,HS2,PS2,Z2,TOP2,
     &                    BOT2)
C************************************************************************
C       THIS SUBROUTINE CALCULATES NEW VALUES FOR Z1 AND H1
C       Z1 IS THE ELEVATION HALFWAY BETWEEN THE WATER TABLE AND THE CELL BOTTOM
C       H1 IS THE EQUIVALENT FRESHWATER HEAD AT Z1
C************************************************************************
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      USE VDFMODULE,   ONLY: DENSEREF
C
      DOUBLE PRECISION H1,HS1,H2,HS2
      LOGICAL CORRECT
C-----------------------------------------------------------------------
C--SEAWAT: NEW LAYCONS TO WORRY ABOUT IN HUF AND LPF (CONVERTIBLE, NONCONVERTIBLE)
      CORRECT=.FALSE.
      IF (HS1.LT.TOP1) CORRECT=.TRUE.
      IF (CORRECT) THEN
         TEMP=(H1+BOT1)/2
         H1=H1+(PS1-DENSEREF)/DENSEREF*(Z1-TEMP)
         Z1=TEMP
      ENDIF
C--SEAWAT: CORRECT NEIGHBOR CELL
      CORRECT=.FALSE.
      IF (HS2.LT.TOP2) CORRECT=.TRUE.
      IF (CORRECT) THEN
         TEMP=(H2+BOT2)/2
         H2=H2+(PS2-DENSEREF)/DENSEREF*(Z2-TEMP)
         Z2=TEMP
      ENDIF
      RETURN
      END     
C

C
      SUBROUTINE VDF2UPDTFLWFLD2(DELT,N,TIME2,HT2,COLD,NCOL,NROW,NLAY,
     +                           NCOMP)
C     CREATED 12/06/2006 BY CDL
C--SEAWAT:***********************************************************
C--SEAWAT: RESET DELT AND COLDFLW IF FLOW EQUATION WAS SOLVED
C--SEAWAT:**********************************************************
      USE VDFMODULE,   ONLY: COLDFLW,NSWTCPL
      DIMENSION COLD(NCOL,NROW,NLAY,NCOMP)
C
C----------------------------------------------------------------
C
C--FLOW EQUATION WAS JUST SOLVED.  NEED TO RESET DELT TO ZERO
C--ONLY DO THIS IF HAVEN'T REACHED END OF FLOW TIMESTEP YET (I.E. TIME2.LT.HT2)
C--BECAUSE DELT NEEDED FOR BUDGET CALCULATIONS.
      COLDFLW=COLD
      RETURN
      END
C
      SUBROUTINE SVDF2PSV(IGRID)
      USE VDFMODULE
CVDF1      VDFDAT(IGRID)%MT3DRHOFLG=>MT3DRHOFLG
      VDFDAT(IGRID)%MFNADVFD=>MFNADVFD
      VDFDAT(IGRID)%NSWTCPL=>NSWTCPL
      VDFDAT(IGRID)%IWTABLE=>IWTABLE
      VDFDAT(IGRID)%NSRHOEOS=>NSRHOEOS
      VDFDAT(IGRID)%NCOL=>NCOL
      VDFDAT(IGRID)%NROW=>NROW
      VDFDAT(IGRID)%NLAY=>NLAY
      VDFDAT(IGRID)%DENSEMIN=>DENSEMIN
      VDFDAT(IGRID)%DENSEMAX=>DENSEMAX
      VDFDAT(IGRID)%DENSEREF=>DENSEREF
      VDFDAT(IGRID)%FIRSTDT=>FIRSTDT
CVDF1      VDFDAT(IGRID)%DNSCRIT=>DNSCRIT
      VDFDAT(IGRID)%DRHODPRHD=>DRHODPRHD
      VDFDAT(IGRID)%PRHDREF=>PRHDREF
      VDFDAT(IGRID)%HDRY=>HDRY
      VDFDAT(IGRID)%HNOFLO=>HNOFLO
      VDFDAT(IGRID)%MTRHOSPEC=>MTRHOSPEC
      VDFDAT(IGRID)%PS=>PS
      VDFDAT(IGRID)%RHOCR=>RHOCR
      VDFDAT(IGRID)%RHOCC=>RHOCC
      VDFDAT(IGRID)%RHOCV=>RHOCV
      VDFDAT(IGRID)%HSALT=>HSALT
      VDFDAT(IGRID)%ELEV=>ELEV
      VDFDAT(IGRID)%DCDT=>DCDT
      VDFDAT(IGRID)%COLDFLW=>COLDFLW
      VDFDAT(IGRID)%PSOLDITER=>PSOLDITER
      VDFDAT(IGRID)%DRHODC=>DRHODC
      VDFDAT(IGRID)%CRHOREF=>CRHOREF
C
      RETURN
      END
C
      SUBROUTINE VDF2DA()
C     DEALLOCATE VDF MEMORY
      USE VDFMODULE
CVDF      DEALLOCATE(MT3DRHOFLG)
      DEALLOCATE(MFNADVFD)
      DEALLOCATE(NSWTCPL)
      DEALLOCATE(IWTABLE)
      DEALLOCATE(NSRHOEOS)
      DEALLOCATE(NCOL)
      DEALLOCATE(NROW)
      DEALLOCATE(NLAY)
      DEALLOCATE(DENSEMIN)
      DEALLOCATE(DENSEMAX)
      DEALLOCATE(DENSEREF)
      DEALLOCATE(FIRSTDT)
CVDF      DEALLOCATE(DNSCRIT)
      DEALLOCATE(DRHODPRHD)
      DEALLOCATE(PRHDREF)
      DEALLOCATE(HDRY)
      DEALLOCATE(HNOFLO)
      DEALLOCATE(MTRHOSPEC)
      DEALLOCATE(PS)
      DEALLOCATE(RHOCR)
      DEALLOCATE(RHOCC)
      DEALLOCATE(RHOCV)
      DEALLOCATE(HSALT)
      DEALLOCATE(ELEV)
      DEALLOCATE(DCDT)
      DEALLOCATE(COLDFLW)
      DEALLOCATE(PSOLDITER)
      DEALLOCATE(DRHODC)
      DEALLOCATE(CRHOREF)
C
      END