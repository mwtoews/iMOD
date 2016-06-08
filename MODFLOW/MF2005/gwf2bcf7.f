c   Copyright (C) Stichting Deltares, 2005-2014.
c
c   This file is part of iMOD.
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
c   iMod is partly based on the USGS MODFLOW2005 source code;
c   for iMOD the USGS MODFLOW2005 source code has been expanded
c   and extensively modified by Stichting Deltares.
c   The original USGS MODFLOW2005 source code can be downloaded from the USGS
c   website http://www.usgs.gov/. The original MODFLOW2005 code incorporated
c   in this file is covered by the USGS Software User Rights Notice;
c   you should have received a copy of this notice along with this program.
c   If not, see <http://water.usgs.gov/software/help/notice/>.

      MODULE GWFBCFMODULE
       real, parameter :: seepagemv = -12345.0                          ! DLT

       INTEGER, SAVE, POINTER ::IBCFCB,IWDFLG,IWETIT,IHDWET
       REAL, SAVE, POINTER    ::WETFCT
       INTEGER, SAVE,   POINTER ::IMINKD,IMINC                          ! DLT
       REAL,    SAVE,   POINTER ::MINKD,MINC                            ! DLT
       INTEGER, SAVE,  POINTER,   DIMENSION(:)     ::LAYCON
       INTEGER, SAVE,  POINTER,   DIMENSION(:)     ::LAYAVG
       REAL, SAVE,     POINTER,   DIMENSION(:,:,:) ::HY
       REAL, SAVE,     POINTER,   DIMENSION(:,:,:) ::SC1
       REAL, SAVE,     POINTER,   DIMENSION(:,:,:) ::SC2
       REAL, SAVE,     POINTER,   DIMENSION(:,:,:) ::WETDRY
       REAL, SAVE,     POINTER,   DIMENSION(:,:,:) ::CVWD
       REAL, SAVE,     POINTER,   DIMENSION(:)     ::TRPY
       real, save,     pointer,   dimension(:,:)   ::seepage            ! DLT

      TYPE GWFBCFTYPE
       INTEGER, POINTER  ::IBCFCB,IWDFLG,IWETIT,IHDWET
       REAL, POINTER     ::WETFCT
       INTEGER, POINTER ::IMINKD,IMINC                                  ! DLT
       REAL,    POINTER ::MINKD,MINC                                    ! DLT
       INTEGER,  POINTER,   DIMENSION(:)     ::LAYCON
       INTEGER,  POINTER,   DIMENSION(:)     ::LAYAVG
       REAL,     POINTER,   DIMENSION(:,:,:) ::HY
       REAL,     POINTER,   DIMENSION(:,:,:) ::SC1
       REAL,     POINTER,   DIMENSION(:,:,:) ::SC2
       REAL,     POINTER,   DIMENSION(:,:,:) ::WETDRY
       REAL,     POINTER,   DIMENSION(:,:,:) ::CVWD
       REAL,     POINTER,   DIMENSION(:)     ::TRPY
       real,     pointer,   dimension(:,:)   ::seepage                  ! DLT
      END TYPE
      TYPE(GWFBCFTYPE), SAVE  ::GWFBCFDAT(10)
      END MODULE GWFBCFMODULE



      SUBROUTINE GWF2BCF7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAYS AND READ DATA FOR BLOCK-CENTERED FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,
     1                      CC,CV,IFREFM,
     1                      iunit,                                      ! ANIPWT
     1                      kdsv                                        ! ANIPWT
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFBCFMODULE,ONLY:IBCFCB,IWDFLG,IWETIT,IHDWET,WETFCT,
     1                      LAYCON,LAYAVG,HY,SC1,SC2,WETDRY,CVWD,TRPY,
     1                      seepage, seepagemv,                         ! DLT
     1                      iminkd, iminc, minkd, minc                  ! DLT
      USE M_MF2005_IU, only: iuani, iupwt
C
      integer icol, irow                                                ! DLT
      CHARACTER*24 ANAME(7)
      CHARACTER*12 AVGNAM(4)
      CHARACTER*200 LINE                                                ! DLT
      DATA AVGNAM/'HARMONIC    ','ARITHMETIC  ',
     1            'LOGARITHMIC ','*UNCONFINED*'/
C
      DATA ANAME(1) /'    PRIMARY STORAGE COEF'/
      DATA ANAME(2) /'    TRANSMIS. ALONG ROWS'/
      DATA ANAME(3) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(4) /'VERT HYD COND /THICKNESS'/
      DATA ANAME(5) /'  SECONDARY STORAGE COEF'/
      DATA ANAME(6) /'COLUMN TO ROW ANISOTROPY'/
      DATA ANAME(7) /'        WETDRY PARAMETER'/
C     ------------------------------------------------------------------
C1------ALLOCATE SCALAR VARIABLES IN FORTRAN MODULE.
      ALLOCATE(IBCFCB,IWDFLG,IWETIT,IHDWET)
      ALLOCATE(IMINKD,IMINC,MINKD,MINC)                                 ! DLT
      ALLOCATE(WETFCT)
      ALLOCATE(LAYCON(NLAY),LAYAVG(NLAY))
C
C2------IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'BCF -- BLOCK-CENTERED FLOW PACKAGE, VERSION 7',
     1', 5/2/2005',/,9X,'INPUT READ FROM UNIT',I3)
C
C3------READ AND PRINT IBCFCB (FLAG FOR PRINTING
C3------OR UNIT# FOR RECORDING CELL-BY-CELL FLOW TERMS), HDRY
C3------(HEAD AT CELLS THAT CONVERT TO DRY), AND WETTING PARAMETERS.
      CALL URDCOM(IN,IOUT,LINE)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(I10,F10.0,I10,F10.0,2I10)')
     1              IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET
      ELSE
         READ(LINE,*) IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET
      END IF
C
C read options
      LLOC=1
      IMINKD=0                                                          ! DLT
      IMINC=0                                                           ! DLT
      MINKD=0.                                                          ! DLT
      MINC=0.                                                           ! DLT
   20 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)                 ! DLT
      IF(LINE(ISTART:ISTOP).EQ.'MINKD') THEN                            ! DLT
         IMINKD=1                                                       ! DLT
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)              ! DLT
         READ(LINE(ISTART:ISTOP),*) MINKD                               ! DLT
      ELSE IF(LINE(ISTART:ISTOP).EQ.'MINC') THEN                        ! DLT
         IMINC=1                                                        ! DLT
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)              ! DLT
         READ(LINE(ISTART:ISTOP),*) MINC                                ! DLT
      ENDIF
      IF(LLOC.LT.200) GO TO 20

C3A-----DETERMINE ISS FROM ITRSS
      IF(ITRSS.EQ.0) THEN
         ISS=1
      ELSE
         ISS=0
      END IF
C
C3B-----PRINT VALUES
      IF(ISS.EQ.0) WRITE(IOUT,3)
    3 FORMAT(1X,'TRANSIENT SIMULATION')
      IF(ISS.NE.0) WRITE(IOUT,4)
    4 FORMAT(1X,'STEADY-STATE SIMULATION')
      IF(IBCFCB.LT.0) WRITE(IOUT,8)
    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
     1     ' WHEN ICBCFL IS NOT 0')
      IF(IBCFCB.GT.0) WRITE(IOUT,9) IBCFCB
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',I3)
      WRITE(IOUT,11) HDRY
   11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',G13.5)
      IF(IWDFLG.NE.0) GO TO 35
      WRITE(IOUT,12)
   12 FORMAT(1X,'WETTING CAPABILITY IS NOT ACTIVE')
      GO TO 50
C
   35 WRITE(IOUT,36)
   36 FORMAT(1X,'WETTING CAPABILITY IS ACTIVE')
      IF(IWETIT.LE.0) IWETIT=1
      WRITE(IOUT,37)WETFCT,IWETIT
   37 FORMAT(1X,'WETTING FACTOR=',F10.5,
     1     '     WETTING ITERATION INTERVAL=',I4)
      WRITE(IOUT,38)IHDWET
   38 FORMAT(1X,'FLAG THAT SPECIFIES THE EQUATION TO USE FOR HEAD',
     1    ' AT WETTED CELLS=',I4)
C
C4------READ LAYCON & PRINT TITLE FOR LAYCON TABLE.
   50 IF(IFREFM.EQ.0) THEN
         READ(IN,'(40I2)') (LAYCON(I),I=1,NLAY)
      ELSE
         READ(IN,*) (LAYCON(I),I=1,NLAY)
      END IF
      WRITE(IOUT,52)
   52 FORMAT(1X,5X,'LAYER  LAYER-TYPE CODE     INTERBLOCK T',
     1      /1X,5X,44('-'))
C
C5------LOOP THROUGH LAYERS CALCULATING LAYAVG, PRINTING THE LAYER-TYPE
C5------CODE, AND COUNTING LAYERS THAT NEED TOP & BOT ARRAYS.
      NBOT=0
      NTOP=0
      DO 100 I=1,NLAY
      IF(LAYCON(I).EQ.30 .OR. LAYCON(I).EQ.32) LAYCON(I)=LAYCON(I)-10
      INAM=LAYCON(I)/10
      LAYAVG(I)=INAM*10
      IF(LAYAVG(I).LT.0 .OR. LAYAVG(I).GT.30) THEN
         WRITE(IOUT,53) LAYAVG(I)
   53    FORMAT(1X,'INVALID INTERBLOCK T CODE:',I4)
         CALL USTOP(' ')
      END IF
      LAYCON(I)=LAYCON(I)-LAYAVG(I)
      L=LAYCON(I)
      INAM=INAM+1
      WRITE(IOUT,55) I,L,LAYAVG(I),AVGNAM(INAM)
   55 FORMAT(1X,I9,I13,I11,' -- ',A)
      IF(LAYCON(I).LT.0 .OR. LAYCON(I).GT.3) THEN
         WRITE(IOUT,56) LAYCON(I)
   56    FORMAT(1X,'INVALID LAYER TYPE:',I4)
         CALL USTOP(' ')
      END IF
C
C5A-----SET GLOBAL HEAD-DEPENDENT THICKNESS FLAGS.
      IF (L.EQ.0) THEN
        LAYHDT(I)=0
        LAYHDS(I)=0
      ELSEIF (L.EQ.1) THEN
        LAYHDT(I)=1
        LAYHDS(I)=0
      ELSEIF (L.EQ.2) THEN
        LAYHDT(I)=0
        LAYHDS(I)=1
      ELSE
        LAYHDT(I)=1
        LAYHDS(I)=1
      ENDIF
C
C5B-----ONLY THE TOP LAYER CAN BE UNCONFINED(LAYCON=1).
      IF(L.NE.1 .OR. I.EQ.1) GO TO 70
      WRITE(IOUT,57)
   57 FORMAT(1X,/1X,'LAYER TYPE 1 IS ONLY ALLOWED IN TOP LAYER')
      CALL USTOP(' ')
C
C5C-----LAYER TYPES 1 AND 3 NEED A BOTTOM. ADD 1 TO KB.
   70 IF(L.EQ.1 .OR. L.EQ.3) NBOT=NBOT+1
C
C5D-----LAYER TYPES 2 AND 3 NEED A TOP. ADD 1 TO KT.
      IF(L.EQ.2 .OR. L.EQ.3) NTOP=NTOP+1
  100 CONTINUE
C
C6------ALLOCATE SPACE FOR ARRAYS.
      IF(ISS.EQ.0) THEN
         ALLOCATE(SC1(NCOL,NROW,NLAY))
      ELSE
         ALLOCATE(SC1(1,1,1))
      END IF
      IF(NTOP.GT.0 .AND. ISS.EQ.0) THEN
         ALLOCATE(SC2(NCOL,NROW,NTOP))
      ELSE
         ALLOCATE(SC2(1,1,1))
      END IF
      ALLOCATE(TRPY(NLAY))
      IF(NBOT.GT.0) THEN
         ALLOCATE(HY(NCOL,NROW,NBOT))
      ELSE
         ALLOCATE(HY(1,1,1))
      END IF
      IF(IWDFLG.NE.0 .AND. NBOT.GT.0) THEN
         ALLOCATE(WETDRY(NCOL,NROW,NBOT))
      ELSE
         ALLOCATE(WETDRY(1,1,1))
      END IF
      IF(IWDFLG.NE.0 .AND. NLAY.GT.1) THEN
         ALLOCATE(CVWD(NCOL,NROW,NLAY-1))
      ELSE
         ALLOCATE(CVWD(1,1,1))
      END IF
      allocate(seepage(ncol,nrow))                                      ! DLT
      seepage = seepagemv                                               ! DLT
C
C
C7------READ TRPY
      CALL U1DREL(TRPY,ANAME(6),NLAY,IN,IOUT)
C
C8------READ ARRAYS FOR EACH LAYER.
      KT=0
      KB=0
      DO 200 K=1,NLAY
      KK=K
C
C8A-----FIND ADDRESS OF EACH LAYER IN THREE DIMENSION ARRAYS.
      IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1
      IF(LAYCON(K).EQ.2 .OR. LAYCON(K).EQ.3) KT=KT+1
C
C8B-----READ PRIMARY STORAGE COEFFICIENT INTO ARRAY SC1 IF TRANSIENT.
      IF(ISS.EQ.0)CALL U2DREL(SC1(:,:,K),ANAME(1),NROW,NCOL,KK,IN,IOUT)
C
C8C-----READ TRANSMISSIVITY INTO ARRAY CC IF LAYER TYPE IS 0 OR 2.
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) GO TO 105
      CALL U2DREL(CC(:,:,K),ANAME(2),NROW,NCOL,KK,IN,IOUT)
      if (IUNIT(IUANI).gt.0.or.IUNIT(IUPWT).gt.0) then                  ! ANIPWT
         do irow = 1, nrow                                              ! ANIPWT
            do icol = 1, ncol                                           ! ANIPWT
               kdsv(icol,irow,k) = cc(icol,irow,k)                      ! ANIPWT
               if (iminkd.eq.1)                                         ! ANIPWT
     1            kdsv(icol,irow,k) = max(minkd,kdsv(icol,irow,k))      ! ANIPWT
            end do                                                      ! ANIPWT
         end do                                                         ! ANIPWT
      end if                                                            ! ANIPWT
      GO TO 110
C
C8D-----READ HYDRAULIC CONDUCTIVITY(HY) IF LAYER TYPE IS 1 OR 3.
  105 CALL U2DREL(HY(:,:,KB),ANAME(3),NROW,NCOL,KK,IN,IOUT)
C
C8E-----READ VERTICAL HYCOND/THICK INTO ARRAY CV IF NOT BOTTOM LAYER;
C2E-----MULTIPLIED BY CELL AREA TO CONVERT TO CONDUCTANCE LATER.
  110 IF(K.EQ.NLAY) GO TO 120
      CALL U2DREL(CV(:,:,K),ANAME(4),NROW,NCOL,KK,IN,IOUT)
C
C8F-----READ SECONDARY STORAGE COEFFICIENT INTO ARRAY SC2 IF TRANSIENT
C8F-----AND LAYER TYPE IS 2 OR 3.
  120 IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 130
      IF(ISS.EQ.0)CALL U2DREL(SC2(:,:,KT),ANAME(5),NROW,NCOL,KK,IN,IOUT)
C
C8H-----READ WETDRY CODES IF LAYER TYPE IS 1 OR 3 AND WETTING
C8H-----CAPABILITY HAS BEEN INVOKED (IWDFLG NOT 0).
  130 IF(LAYCON(K).NE.3.AND.LAYCON(K).NE.1)GO TO 200
      IF(IWDFLG.EQ.0)GO TO 200
      CALL U2DREL(WETDRY(:,:,KB),ANAME(7),NROW,NCOL,KK,IN,IOUT)
  200 CONTINUE
C
      call pest1alpha_grid('KD',cc,nrow,ncol,nlay)                      ! IPEST
      call pest1alpha_grid('VC',cv,nrow,ncol,nlay-1)                    ! IPEST
      if (iss.eq.0) call pest1alpha_grid('SC',sc1,nrow,ncol,nlay)       ! IPEST
C
C9------PREPARE AND CHECK BCF DATA.
      CALL SGWF2BCF7N(ISS)
C
c let cc and cv be greater then 0., Necessary for ANI package
      if (IUNIT(IUANI).gt.0.or.IUNIT(IUPWT).gt.0) then                  ! ANI
         do ilay=1,nlay                                                 ! ANI
            do irow=1,nrow                                              ! ANI
               do icol=1,ncol                                           ! ANI
                  !prevent transmissivity to be zero                    ! ANI
                  cc(icol,irow,ilay)=max(0.0,cc(icol,irow,ilay))        ! ANI
!                  cc(icol,irow,ilay)=max(1.0e-12,cc(icol,irow,ilay))   ! ANI
               end do                                                   ! ANI
            end do                                                      ! ANI
         end do                                                         ! ANI
         do ilay=1,nlay-1                                               ! ANI
            do irow=1,nrow                                              ! ANI
               do icol=1,ncol                                           ! ANI
                  !prevent vcont to be zero                             ! ANI
                  cv(icol,irow,ilay)=max(0.0,cv(icol,irow,ilay))        ! ANI
!                  cv(icol,irow,ilay)=max(1.0e-12,cv(icol,irow,ilay))    ! ANI
               end do                                                   ! ANI
            end do                                                      ! ANI
         end do                                                         ! ANI
      end if                                                            ! ANI

C
C10-----SAVE POINTERS FOR GRID AND RETURN.
      CALL SGWF2BCF7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2BCF7AD(KPER,IGRID)
C     ******************************************************************
C     SET HOLD TO BOT WHENEVER A WETTABLE CELL IS DRY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HOLD,BOTM,LBOTM
      USE GWFBCFMODULE,ONLY:IWDFLG,WETDRY,LAYCON
C     ------------------------------------------------------------------
C
      CALL SGWF2BCF7PNT(IGRID)
      ISS=ISSFLG(KPER)
C
C1------RETURN IF STEADY STATE OR IF NOT USING WETTING CAPABILITY
      IF(IWDFLG.EQ.0 .OR. ISS.NE.0) RETURN
C
C2------LOOP THROUGH ALL LAYERS TO SET HOLD=BOT IF A WETTABLE CELL IS DRY
      ZERO=0.
      KB=0
      DO 100 K=1,NLAY
C
C2A-----SKIP LAYERS THAT CANNOT CONVERT BETWEEN WET AND DRY
      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.1) GO TO 100
      KB=KB+1
      DO 90 I=1,NROW
      DO 90 J=1,NCOL
C
C2B-----SKIP CELLS THAT ARE CURRENTLY WET OR ARE NOT WETTABLE
      IF(IBOUND(J,I,K).NE.0) GO TO 90
      IF(WETDRY(J,I,KB).EQ.ZERO) GO TO 90
C
C2C-----SET HOLD=BOT
      HOLD(J,I,K)=BOTM(J,I,LBOTM(K))
   90 CONTINUE
  100 CONTINUE
C
C3-----RETURN
      RETURN
      END
      SUBROUTINE GWF2BCF7FM(KITER,KSTP,KPER,IGRID)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BOTM,NBOTM,
     1                      LBOTM,CV,HNEW,RHS,HCOF,HOLD,ISSFLG
      USE GWFBASMODULE,ONLY:DELT
      USE GWFBCFMODULE,ONLY:LAYCON,SC1,SC2
C     ------------------------------------------------------------------
      CALL SGWF2BCF7PNT(IGRID)
      ISS=ISSFLG(KPER)
      KB=0
      KT=0
      ONE=1.
      IF(ISS.EQ.0) TLED=ONE/DELT
C
C1------FOR EACH LAYER: IF T VARIES CALCULATE HORIZONTAL CONDUCTANCES
      DO 100 K=1,NLAY
      KK=K
C
C1A-----IF LAYER TYPE IS NOT 1 OR 3 THEN SKIP THIS LAYER.
      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.1) GO TO 100
      KB=KB+1
C
C1B-----FOR LAYER TYPES 1 & 3 CALL SGWF2BCF7H TO CALCULATE
C1B-----HORIZONTAL CONDUCTANCES.
      CALL SGWF2BCF7H(KK,KB,KITER,KSTP,KPER)
  100 CONTINUE
C
C2------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO HCOF AND RHS
      IF(ISS.NE.0) GO TO 201
      KT=0
      DO 200 K=1,NLAY
C
C3------SEE IF THIS LAYER IS CONVERTIBLE OR NON-CONVERTIBLE.
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) GO TO 150
C4------NON-CONVERTIBLE LAYER, SO USE PRIMARY STORAGE
      DO 140 I=1,NROW
      DO 140 J=1,NCOL
      IF(IBOUND(J,I,K).LE.0) GO TO 140
      RHO=SC1(J,I,K)*TLED
      HCOF(J,I,K)=HCOF(J,I,K)-RHO
      RHS(J,I,K)=RHS(J,I,K)-RHO*HOLD(J,I,K)
  140 CONTINUE
      GO TO 200
C
C5------A CONVERTIBLE LAYER, SO CHECK OLD AND NEW HEADS TO DETERMINE
C5------WHEN TO USE PRIMARY AND SECONDARY STORAGE
  150 KT=KT+1
      DO 180 I=1,NROW
      DO 180 J=1,NCOL
C
C5A-----IF THE CELL IS EXTERNAL THEN SKIP IT.
      IF(IBOUND(J,I,K).LE.0) GO TO 180
      TP=BOTM(J,I,LBOTM(K)-1)
      RHO2=SC2(J,I,KT)*TLED
      RHO1=SC1(J,I,K)*TLED
C
C5B-----FIND STORAGE FACTOR AT START OF TIME STEP.
      SOLD=RHO2
      IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
C
C5C-----FIND STORAGE FACTOR AT END OF TIME STEP.
      HTMP=HNEW(J,I,K)
      SNEW=RHO2
      IF(HTMP.GT.TP) SNEW=RHO1
C
C5D-----ADD STORAGE TERMS TO RHS AND HCOF.
      HCOF(J,I,K)=HCOF(J,I,K)-SNEW
      RHS(J,I,K)=RHS(J,I,K) - SOLD*(HOLD(J,I,K)-TP) - SNEW*TP
C
  180 CONTINUE
C
  200 CONTINUE
C
C6------FOR EACH LAYER DETERMINE IF CORRECTION TERMS ARE NEEDED FOR
C6------FLOW DOWN INTO PARTIALLY SATURATED LAYERS.
  201 DO 300 K=1,NLAY
C
C7------SEE IF CORRECTION IS NEEDED FOR LEAKAGE FROM ABOVE.
      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 250
      IF(K.EQ.1) GO TO 250
C
C7A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
      DO 220 I=1,NROW
      DO 220 J=1,NCOL
C
C7B-----IF THE CELL IS EXTERNAL(IBOUND<=0) THEN SKIP IT.
      IF(IBOUND(J,I,K).LE.0) GO TO 220
      HTMP=HNEW(J,I,K)
C
C7C-----IF HEAD IS ABOVE TOP THEN CORRECTION NOT NEEDED
      IF(HTMP.GE.BOTM(J,I,LBOTM(K)-1)) GO TO 220
C
C7D-----WITH HEAD BELOW TOP ADD CORRECTION TERMS TO RHS.
      RHS(J,I,K)=RHS(J,I,K) + CV(J,I,K-1)*(BOTM(J,I,LBOTM(K)-1)-HTMP)
  220 CONTINUE
C
C8------SEE IF THIS LAYER MAY NEED CORRECTION FOR LEAKAGE TO BELOW.
  250 IF(K.EQ.NLAY) GO TO 300
      IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 300
C
C8A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
      DO 280 I=1,NROW
      DO 280 J=1,NCOL
C
C8B-----IF CELL IS EXTERNAL (IBOUND<=0) THEN SKIP IT.
      IF(IBOUND(J,I,K).LE.0) GO TO 280
C
C8C-----IF HEAD IN THE LOWER CELL IS LESS THAN TOP ADD CORRECTION
C8C-----TERM TO RHS.
      HTMP=HNEW(J,I,K+1)
      IF(HTMP.LT.BOTM(J,I,LBOTM(K+1)-1)) RHS(J,I,K)=RHS(J,I,K)
     1                        - CV(J,I,K)*(BOTM(J,I,LBOTM(K+1)-1)-HTMP)
  280 CONTINUE
  300 CONTINUE
C
C9------RETURN
      RETURN
      END
      SUBROUTINE GWF2BCF7BDS(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR BCF.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HNEW,HOLD,
     1                      BUFF,BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,VBVL,VBNM,DELT,PERTIM,TOTIM
      USE GWFBCFMODULE,ONLY:IBCFCB,LAYCON,SC1,SC2
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION STOIN,STOUT,SSTRG
C
      DATA TEXT /'         STORAGE'/
C     ------------------------------------------------------------------
      CALL SGWF2BCF7PNT(IGRID)
      ISS=ISSFLG(KPER)
C
C1------INITIALIZE BUDGET ACCUMULATORS AND 1/DELT.
      ZERO=0.
      STOIN=ZERO
      STOUT=ZERO
C2------IF STEADY STATE, STORAGE TERM IS ZERO
      IF(ISS.NE.0) GOTO 400
      ONE=1.
      TLED=ONE/DELT
C
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(IBCFCB.GT.0) IBD=ICBCFL
C
C4------CLEAR BUFFER.
      DO 210 K=1,NLAY
      DO 210 I=1,NROW
      DO 210 J=1,NCOL
      BUFF(J,I,K)=ZERO
210   CONTINUE
C
C5------LOOP THROUGH EVERY CELL IN THE GRID.
      KT=0
      DO 300 K=1,NLAY
      LC=LAYCON(K)
      IF(LC.EQ.3 .OR. LC.EQ.2) KT=KT+1
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
C
C6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
      IF(IBOUND(J,I,K).LE.0) GO TO 300
      HSING=HNEW(J,I,K)
C
C7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
      IF(LC.NE.3 .AND. LC.NE.2) GO TO 285
C
C7A----TWO STORAGE CAPACITIES.
      TP=BOTM(J,I,LBOTM(K)-1)
      RHO2=SC2(J,I,KT)*TLED
      RHO1=SC1(J,I,K)*TLED
      SOLD=RHO2
      IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
      SNEW=RHO2
      IF(HSING.GT.TP) SNEW=RHO1
      STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
      GO TO 288
C
C7B----ONE STORAGE CAPACITY.
  285 RHO=SC1(J,I,K)*TLED
      STRG=RHO*HOLD(J,I,K) - RHO*HSING

C
C8-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS.
  288 BUFF(J,I,K)=STRG
      SSTRG=STRG
      IF(STRG.LT.ZERO) THEN
        STOUT=STOUT-SSTRG
      ELSE
        STOIN=STOIN+SSTRG
      END IF
C
  300 CONTINUE
C

C9-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                       IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,IBCFCB,
     1            BUFF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C10-----ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      SIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+SIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=SIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C11----RETURN.
      RETURN
      END
      SUBROUTINE GWF2BCF7BDCH(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,CR,CC,CV,
     1                      BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,DELT,PERTIM,TOTIM,ICBCFL,
     1                      ICHFLG
      USE GWFBCFMODULE,ONLY:IBCFCB,LAYCON
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION HD,CHIN,CHOUT,XX1,XX2,XX3,XX4,XX5,XX6
C
      DATA TEXT /'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
      CALL SGWF2BCF7PNT(IGRID)
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD=0
      IF(IBCFCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IBCFCB.GT.0) IBD=ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      ZERO=0.
      CHIN=ZERO
      CHOUT=ZERO
      IBDLBL=0
C
C3------CLEAR BUFFER.
      DO 5 K=1,NLAY
      DO 5 I=1,NROW
      DO 5 J=1,NCOL
      BUFF(J,I,K)=ZERO
5     CONTINUE
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF(IBD.EQ.2) THEN
         NCH=0
         DO 7 K=1,NLAY
         DO 7 I=1,NROW
         DO 7 J=1,NCOL
         IF(IBOUND(J,I,K).LT.0) NCH=NCH+1
7        CONTINUE
         CALL UBDSV2(KSTP,KPER,TEXT,IBCFCB,NCOL,NROW,NLAY,
     1          NCH,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO 200 K=1,NLAY
      LC=LAYCON(K)
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
      IF (IBOUND(J,I,K).GE.0)GO TO 200
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
      X1=ZERO
      X2=ZERO
      X3=ZERO
      X4=ZERO
      X5=ZERO
      X6=ZERO
      CHCH1=ZERO
      CHCH2=ZERO
      CHCH3=ZERO
      CHCH4=ZERO
      CHCH5=ZERO
      CHCH6=ZERO
C
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(IBOUND(J-1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 30
C
C7B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
      HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
      CHCH1=HDIFF*CR(J-1,I,K)
      IF(IBOUND(J-1,I,K).LT.0) GO TO 30
      X1=CHCH1
      XX1=X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
      IF (X1.LT.ZERO) THEN
        CHOUT=CHOUT-XX1
      ELSE
        CHIN=CHIN+XX1
      END IF
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(IBOUND(J+1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 60
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      CHCH2=HDIFF*CR(J,I,K)
      IF(IBOUND(J+1,I,K).LT.0) GO TO 60
      X2=CHCH2
      XX2=X2
      IF(X2.LT.ZERO) THEN
        CHOUT=CHOUT-XX2
      ELSE
        CHIN=CHIN+XX2
      END IF
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF(IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF(IBOUND(J,I-1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 90
      HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
      CHCH3=HDIFF*CC(J,I-1,K)
      IF(IBOUND(J,I-1,K).LT.0) GO TO 90
      X3=CHCH3
      XX3=X3
      IF(X3.LT.ZERO) THEN
        CHOUT=CHOUT-XX3
      ELSE
        CHIN=CHIN+XX3
      END IF
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(IBOUND(J,I+1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 120
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      CHCH4=HDIFF*CC(J,I,K)
      IF(IBOUND(J,I+1,K).LT.0) GO TO 120
      X4=CHCH4
      XX4=X4
      IF(X4.LT.ZERO) THEN
        CHOUT=CHOUT-XX4
      ELSE
        CHIN=CHIN+XX4
      END IF
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF(IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF(IBOUND(J,I,K-1).LT.0 .AND. ICHFLG.EQ.0) GO TO 150
      HD=HNEW(J,I,K)
      IF(LC.NE.3 .AND. LC.NE.2) GO TO 122
      TMP=HD
      IF(TMP.LT.BOTM(J,I,LBOTM(K)-1)) HD=BOTM(J,I,LBOTM(K)-1)
  122 HDIFF=HD-HNEW(J,I,K-1)
      CHCH5=HDIFF*CV(J,I,K-1)
      IF(IBOUND(J,I,K-1).LT.0) GO TO 150
      X5=CHCH5
      XX5=X5
      IF(X5.LT.ZERO) THEN
        CHOUT=CHOUT-XX5
      ELSE
        CHIN=CHIN+XX5
      END IF
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(IBOUND(J,I,K+1).LT.0 .AND. ICHFLG.EQ.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 152
      TMP=HD
      IF(TMP.LT.BOTM(J,I,LBOTM(K+1)-1)) HD=BOTM(J,I,LBOTM(K+1)-1)
  152 HDIFF=HNEW(J,I,K)-HD
      CHCH6=HDIFF*CV(J,I,K)
      IF(IBOUND(J,I,K+1).LT.0) GO TO 180
      X6=CHCH6
      XX6=X6
      IF(X6.LT.ZERO) THEN
        CHOUT=CHOUT-XX6
      ELSE
        CHIN=CHIN+XX6
      END IF
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
 180  RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
      BUFF(J,I,K)=RATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,899) TEXT,KPER,KSTP
  899    FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
         WRITE(IOUT,900) K,I,J,RATE
  900    FORMAT(1X,'LAYER',I3,'   ROW',I4,'   COL',I4,
     1       '   RATE',1PG15.6)
         IBDLBL=1
      END IF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
      IF(IBD.EQ.2) CALL UBDSVA(IBCFCB,NCOL,NROW,J,I,K,RATE,IBOUND,NLAY)
  200 CONTINUE
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                   IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN=CHIN
      COUT=CHOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+CIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+COUT*DELT
      VBVL(3,MSUM)=CIN
      VBVL(4,MSUM)=COUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C18-----RETURN.
      RETURN
      END
      SUBROUTINE GWF2BCF7BDADJ(KSTP,KPER,IDIR,IBDRET,
     1             IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,CR,CC,CV,
     1                      BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:ICBCFL,DELT,PERTIM,TOTIM,ICHFLG
      USE GWFBCFMODULE,ONLY:IBCFCB,LAYCON,
     1                      seepage, seepagemv                          ! DLT
      use global, only: iunit                                           ! DLT
      USE M_MF2005_IU
C
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HD
C
      DATA TEXT(1),TEXT(2),TEXT(3)
     1 /'FLOW RIGHT FACE ','FLOW FRONT FACE ','FLOW LOWER FACE '/
C     ------------------------------------------------------------------
      CALL SGWF2BCF7PNT(IGRID)
c
c     determine the seepage
      if (idir.eq.3 .and. nlay.gt.1) then                               ! DLT
         seepage = seepagemv                                            ! DLT
         K=1                                                            ! DLT
         DO 290 I=IR1,IR2                                               ! DLT
         DO 290 J=IC1,IC2                                               ! DLT
         IF(ICHFLG.EQ.0) THEN                                           ! DLT
            IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I,K+1).LE.0))       ! DLT
     1         GO TO 290                                                ! DLT
         ELSE                                                           ! DLT
            IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I,K+1).EQ.0))        ! DLT
     1         GO TO 290                                                ! DLT
         END IF                                                         ! DLT
         HD=HNEW(J,I,K+1)                                               ! DLT
         IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 280          ! DLT
         TMP=HD                                                         ! DLT
         IF(TMP.LT.BOTM(J,I,LBOTM(K+1)-1)) HD=BOTM(J,I,LBOTM(K+1)-1)    ! DLT
  280    HDIFF=HNEW(J,I,K)-HD                                           ! DLT
         seepage(j,i) = hdiff*cv(j,i,k)                                 ! DLT
  290    CONTINUE                                                       ! DLT
         call sgwf2bcf7psv(igrid)                                       ! DLT
      end if                                                            ! DLT
C
C1------IF CELL-BY-CELL FLOWS WILL BE SAVED IN A FILE, SET FLAG IBD.
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      IBD=0
      ZERO=0.
      IF(IBCFCB.GT.0) IBD=ICBCFL
      IF(IBD.EQ.0 .AND. IBDRET.EQ.0) RETURN
C
C2------SET THE SUBREGION EQUAL TO THE ENTIRE GRID IF VALUES ARE BEING
C2------SAVED IN A FILE.
      IF(IBD.NE.0) THEN
         K1=1
         K2=NLAY
         I1=1
         I2=NROW
         J1=1
         J2=NCOL
      END IF
C
C3------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS COLUMNS, GO TO
C3------STEP 4.  IF ONLY 1 COLUMN, RETURN.
      IF(IDIR.NE.1) GO TO 405
      IF(NCOL.EQ.1) RETURN
C
C3A-----CALCULATE FLOW ACROSS COLUMNS (THROUGH RIGHT FACE).  IF NOT
C3A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1-1
         IF(J1.LT.1) J1=1
         J2=IC2
      END IF
      DO 310 K=K1,K2
      DO 310 I=I1,I2
      DO 310 J=J1,J2
      BUFF(J,I,K)=ZERO
  310 CONTINUE
C
C3B-----FOR EACH CELL CALCULATE FLOW THRU RIGHT FACE & STORE IN BUFFER.
      IF(J2.EQ.NCOL) J2=J2-1
      DO 400 K=K1,K2
      DO 400 I=I1,I2
      DO 400 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J+1,I,K).LE.0)) GO TO 400
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J+1,I,K).EQ.0)) GO TO 400
      END IF
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      BUFF(J,I,K)=HDIFF*CR(J,I,K)
  400 CONTINUE

      if(IUNIT(IUANI).gt.0) call gwf2ani7bd(igrid,idir)                 ! DLT
C
C3C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(1),IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(1),IBCFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C4------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS ROWS, GO TO
C4------STEP 5.  IF ONLY 1 ROW, RETURN.
  405 IF(IDIR.NE.2) GO TO 505
      IF(NROW.EQ.1) RETURN
C
C4A-----CALCULATE FLOW ACROSS ROWS (THROUGH FRONT FACE).  IF NOT SAVING
C4A-----IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1-1
         IF(I1.LT.1) I1=1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 410 K=K1,K2
      DO 410 I=I1,I2
      DO 410 J=J1,J2
      BUFF(J,I,K)=ZERO
  410 CONTINUE
C
C4B-----FOR EACH CELL CALCULATE FLOW THRU FRONT FACE & STORE IN BUFFER.
      IF(I2.EQ.NROW) I2=I2-1
      DO 500 K=K1,K2
      DO 500 I=I1,I2
      DO 500 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I+1,K).LE.0)) GO TO 500
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I+1,K).EQ.0)) GO TO 500
      END IF
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      BUFF(J,I,K)=HDIFF*CC(J,I,K)
  500 CONTINUE

      if(IUNIT(IUANI).gt.0) call gwf2ani7bd(igrid,idir)                 ! DLT
C
C4C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(2),IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(2),IBCFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C5------DIRECTION OF CALCULATION IS ACROSS LAYERS BY ELIMINATION.  IF
C5------ONLY 1 LAYER, RETURN.
  505 continue                                                          ! DLT
      IF(NLAY.EQ.1) RETURN                                              ! DLT
C
C5A-----CALCULATE FLOW ACROSS LAYERS (THROUGH LOWER FACE).  IF NOT
C5A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1-1
         IF(K1.LT.1) K1=1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 510 K=K1,K2
      DO 510 I=I1,I2
      DO 510 J=J1,J2
      BUFF(J,I,K)=ZERO
  510 CONTINUE
C
C5B-----FOR EACH CELL CALCULATE FLOW THRU LOWER FACE & STORE IN BUFFER.
      IF(K2.EQ.NLAY) K2=K2-1
      DO 600 K=1,K2
      IF(K.LT.K1) GO TO 600
      DO 590 I=I1,I2
      DO 590 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I,K+1).LE.0)) GO TO 590
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I,K+1).EQ.0)) GO TO 590
      END IF
      HD=HNEW(J,I,K+1)
      IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 580
      TMP=HD
      IF(TMP.LT.BOTM(J,I,LBOTM(K+1)-1)) HD=BOTM(J,I,LBOTM(K+1)-1)
  580 HDIFF=HNEW(J,I,K)-HD
      BUFF(J,I,K)=HDIFF*CV(J,I,K)
  590 CONTINUE
  600 CONTINUE
C
C5C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(3),IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(3),IBCFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)

      RETURN
      END
      SUBROUTINE SGWF2BCF7C(K,cc,cr)
C     ******************************************************************
C     COMPUTE BRANCH CONDUCTANCE USING HARMONIC MEAN OF BLOCK
C     CONDUCTANCES -- BLOCK TRANSMISSIVITY IS IN CC UPON ENTRY
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
!      USE GLOBAL,      ONLY:NCOL,NROW,CR,CC,DELR,DELC
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,DELR,DELC
      USE GWFBCFMODULE,ONLY:TRPY,
     1                      IMINKD,MINKD                                ! DLT

c arguments
      integer, intent(in) :: k                                          ! DLT
      real, dimension(ncol,nrow,nlay), intent(inout) :: cc              ! DLT
      real, dimension(ncol,nrow,nlay), intent(inout) :: cr              ! DLT
C     ------------------------------------------------------------------
C
      ZERO=0.
      TWO=2.
      YX=TRPY(K)*TWO
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 40 I=1,NROW
      DO 40 J=1,NCOL
      T1=CC(J,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T1 = MAX(T1,MINKD)                                             ! DLT
      END IF                                                            ! DLT
C
C2------IF T=0 THEN SET CONDUCTANCE EQUAL TO 0. GO ON TO NEXT CELL.
      IF(T1.NE.ZERO) GO TO 10
      CR(J,I,K)=ZERO
      GO TO 40
C
C3------IF THIS IS NOT THE LAST COLUMN(RIGHTMOST) THEN CALCULATE
C3------BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
   10 IF(J.EQ.NCOL) GO TO 30
      T2=CC(J+1,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
      CR(J,I,K)=TWO*T2*T1*DELC(I)/(T1*DELR(J+1)+T2*DELR(J))
C
C4------IF THIS IS NOT THE LAST ROW(FRONTMOST) THEN CALCULATE
C4------BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
   30 IF(I.EQ.NROW) GO TO 40
      T2=CC(J,I+1,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
      CC(J,I,K)=YX*T2*T1*DELR(J)/(T1*DELC(I+1)+T2*DELC(I))
   40 CONTINUE
C
C5------RETURN
      RETURN
      END
      SUBROUTINE SGWF2BCF7H(K,KB,KITER,KSTP,KPER)
C     ******************************************************************
C     COMPUTE CONDUCTANCE FOR ONE LAYER FROM SATURATED THICKNESS AND
C     HYDRAULIC CONDUCTIVITY
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,BOTM,NBOTM,
     1                      LBOTM,CC,CR,CV,IOUT
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFBCFMODULE,ONLY:IWDFLG,WETFCT,IHDWET,IWETIT,LAYCON,
     1                      HY,CVWD,WETDRY,LAYAVG,
     1                      IMINKD,MINKD                                ! DLT
C
      DOUBLE PRECISION HD,BBOT,TTOP
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
C
C1------LOOP THROUGH EACH CELL IN LAYER AND CALCULATE TRANSMISSIVITY AT
C1------EACH ACTIVE CELL.
      ZERO=0.
      NCNVRT=0
      IHDCNV=0
      ITFLG=1
      IF(IWDFLG.NE.0) ITFLG=MOD(KITER,IWETIT)
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C2------IF CELL IS ACTIVE, THEN SKIP TO CODE THAT CALCULATES SATURATED
C2------THICKNESS.
      IF(IBOUND(J,I,K).NE.0) GO TO 20
C
C3------DETERMINE IF THE CELL CAN CONVERT BETWEEN CONFINED AND
C3------UNCONFINED.  IF NOT, SKIP TO CODE THAT SETS TRANSMISSIVITY TO 0.
      IF(ITFLG.NE.0) GO TO 6
      IF(WETDRY(J,I,KB).EQ.ZERO)GO TO 6
      WD=WETDRY(J,I,KB)
      IF(WD.LT.ZERO) WD=-WD
      TURNON=BOTM(J,I,LBOTM(K))+WD
C
C3A-----CHECK HEAD IN CELL BELOW TO SEE IF WETTING THRESHOLD HAS BEEN
C3A-----REACHED.
      IF(K.EQ.NLAY)GO TO 2
      HTMP=HNEW(J,I,K+1)
      IF(IBOUND(J,I,K+1).GT.0.AND.HTMP.GE.TURNON)GO TO 9
C
C3B-----CHECK HEAD IN ADJACENT HORIZONTAL CELLS TO SEE IF WETTING
C3B-----THRESHOLD HAS BEEN REACHED.
    2 IF(WETDRY(J,I,KB).LT.ZERO) GO TO 6
      IF(J.EQ.1)GO TO 3
      HTMP=HNEW(J-1,I,K)
      IF(IBOUND(J-1,I,K).GT.0.AND.IBOUND(J-1,I,K).NE.30000.AND.
     1                           HTMP.GE.TURNON)GO TO 9
    3 IF(J.EQ.NCOL)GO TO 4
      HTMP=HNEW(J+1,I,K)
      IF(IBOUND(J+1,I,K).GT.0.AND.HTMP.GE.TURNON)GO TO 9
    4 IF(I.EQ.1)GO TO 5
      HTMP=HNEW(J,I-1,K)
      IF(IBOUND(J,I-1,K).GT.0.AND.IBOUND(J,I-1,K).NE.30000.AND.
     1                            HTMP.GE.TURNON)GO TO 9
    5 IF(I.EQ.NROW)GO TO 6
      HTMP=HNEW(J,I+1,K)
      IF(IBOUND(J,I+1,K).GT.0.AND.HTMP.GE.TURNON)GO TO 9
C
C3C-----CELL IS DRY AND STAYS DRY.  SET TRANSMISSIVITY TO 0, SET
C3C-----SATURATED THICKNESS (BUFF) TO 0, AND SKIP TO THE NEXT CELL.
    6 CC(J,I,K)=ZERO
      IF(LAYAVG(K).EQ.30) BUFF(J,I,K)=ZERO
      GO TO 200
C
C4------CELL BECOMES WET.  SET INITIAL HEAD AND VERTICAL CONDUCTANCE.
    9 IF(IHDWET.NE.0) HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+WETFCT*WD
      IF(IHDWET.EQ.0) HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+WETFCT*
     1          (HTMP-BOTM(J,I,LBOTM(K)))
      IF(K.EQ.NLAY) GO TO 12
      IF(IBOUND(J,I,K+1).NE.0) CV(J,I,K)= CVWD(J,I,K)
   12 IF(K.EQ.1) GO TO 14
      IF(IBOUND(J,I,K-1).NE.0) CV(J,I,K-1)= CVWD(J,I,K-1)
   14 IBOUND(J,I,K)=30000
C
C4A-----PRINT MESSAGE SAYING CELL HAS BEEN CONVERTED TO WET.
      NCNVRT=NCNVRT+1
      ICNVRT(NCNVRT)=I
      JCNVRT(NCNVRT)=J
      ACNVRT(NCNVRT)='WET'
      IF(NCNVRT.LT.5) GO TO 20
         IF(IHDCNV.EQ.0) WRITE(IOUT,17) KITER,K,KSTP,KPER
   17    FORMAT(1X,/1X,'CELL CONVERSIONS FOR ITER.=',I3,'  LAYER=',
     1    I3,'  STEP=',I3,'  PERIOD=',I3,'   (ROW,COL)')
         IHDCNV=1
         WRITE(IOUT,18) (ACNVRT(L),ICNVRT(L),JCNVRT(L),L=1,NCNVRT)
   18    FORMAT(1X,3X,5(A,'(',I3,',',I3,')   '))
         NCNVRT=0
C
C5------CALCULATE SATURATED THICKNESS.
   20 HD=HNEW(J,I,K)
      BBOT=BOTM(J,I,LBOTM(K))
      IF(LAYCON(K).EQ.1) GO TO 50
      TTOP=BOTM(J,I,LBOTM(K)-1)
      IF(BBOT.GT.TTOP) THEN
         WRITE(IOUT,35) K,I,J
   35    FORMAT(1X,'Negative cell thickness at (Layer,row,col)',
     1   I4,',',I4,',',I4)
         CALL USTOP(' ')
      END IF
      IF(HD.GT.TTOP) HD=TTOP
   50 THCK=HD-BBOT
C
C6------CHECK TO SEE IF SATURATED THICKNESS IS GREATER THAN ZERO.
      IF(THCK.LE.ZERO.AND.IMINKD.EQ.0) GO TO 100                        ! DLT
C
C6A-----IF SATURATED THICKNESS>0 THEN EITHER CALCULATE TRANSMISSIVITY
C6A-----AS HYDRAULIC CONDUCTIVITY TIMES SATURATED THICKNESS OR STORE
C6A-----K IN CC AND SATURATED THICKNESS IN BUFF.
      IF(LAYAVG(K).EQ.30) THEN
         CC(J,I,K)=HY(J,I,KB)
         BUFF(J,I,K)=THCK
      ELSE
         CC(J,I,K)=THCK*HY(J,I,KB)
         IF (IMINKD.EQ.1) THEN                                          ! DLT
            CC(J,I,K) = MAX(CC(J,I,K),MINKD)                            ! DLT
         END IF
      END IF
      GO TO 200
C
C6B-----WHEN SATURATED THICKNESS < 0, PRINT A MESSAGE AND SET
C6B-----TRANSMISSIVITY, IBOUND, AND VERTICAL CONDUCTANCE =0
  100 NCNVRT=NCNVRT+1
      ICNVRT(NCNVRT)=I
      JCNVRT(NCNVRT)=J
      ACNVRT(NCNVRT)='DRY'
      IF(NCNVRT.LT.5) GO TO 150
         IF(IHDCNV.EQ.0) WRITE(IOUT,17) KITER,K,KSTP,KPER
         IHDCNV=1
         WRITE(IOUT,18) (ACNVRT(L),ICNVRT(L),JCNVRT(L),L=1,NCNVRT)
         NCNVRT=0
  150 HNEW(J,I,K)=HDRY
      CC(J,I,K)=ZERO
      IF(IBOUND(J,I,K).GE.0) GO TO 160
         WRITE(IOUT,151)
  151    FORMAT(1X,/1X,'CONSTANT-HEAD CELL WENT DRY',
     1          ' -- SIMULATION ABORTED')
         WRITE(IOUT,152) K,I,J,KITER,KSTP,KPER
  152    FORMAT(1X,'LAYER=',I2,'   ROW=',I3,'   COLUMN=',I3,
     1    '   ITERATION=',I3,'   TIME STEP=',I3,'   STRESS PERIOD=',I3)
         WRITE(IOUT,*) BBOT,HD
         CALL USTOP(' ')
  160 IBOUND(J,I,K)=0
      IF(K.LT.NLAY) CV(J,I,K)=ZERO
      IF(K.GT.1) CV(J,I,K-1)=ZERO
  200 CONTINUE
C
C7------PRINT ANY REMAINING CELL CONVERSIONS NOT YET PRINTED
      IF(NCNVRT.EQ.0) GO TO 203
         IF(IHDCNV.EQ.0) WRITE(IOUT,17) KITER,K,KSTP,KPER
         IHDCNV=1
         WRITE(IOUT,18) (ACNVRT(L),ICNVRT(L),JCNVRT(L),L=1,NCNVRT)
         NCNVRT=0
C
C8------CHANGE IBOUND VALUE FOR CELLS THAT CONVERTED TO WET THIS
C8------ITERATION FROM 30000 to 1.
  203 IF(IWDFLG.EQ.0) GO TO 210
      DO 205 I=1,NROW
      DO 205 J=1,NCOL
      IF(IBOUND(J,I,K).EQ.30000) IBOUND(J,I,K)=1
  205 CONTINUE
C
C9------COMPUTE HORIZONTAL BRANCH CONDUCTANCES FROM TRANSMISSIVITY.
  210 IF(LAYAVG(K).EQ.0) THEN
         CALL SGWF2BCF7C(K,cc,cr)
      ELSE IF(LAYAVG(K).EQ.10) THEN
         CALL SGWF2BCF7A(K)
      ELSE IF(LAYAVG(K).EQ.20) THEN
         CALL SGWF2BCF7L(K)
      ELSE
         CALL SGWF2BCF7U(K)
      END IF
C
C10-----RETURN.
      RETURN
      END
      SUBROUTINE SGWF2BCF7N(ISS)
C     ******************************************************************
C     INITIALIZE AND CHECK BCF DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,LAYCBD,CC,CR,CV,
     1                      DELR,DELC,IOUT
      USE GWFBASMODULE,ONLY:HNOFLO
      USE GWFBCFMODULE,ONLY:IWDFLG,WETDRY,HY,CVWD,LAYCON,LAYAVG,SC1,SC2,
     1                      IMINC,MINC                                  ! DLT
C
      DOUBLE PRECISION HCNV
      REAL :: C, MAXC, TINY, MAXVCOND                                   ! DLT
      PARAMETER( MAXC = 1.0E6,TINY=1.0E-20,MAXVCOND=1.0E6)              ! DLT
C     ------------------------------------------------------------------
C
C1------MULTIPLY VERTICAL LEAKANCE BY AREA TO MAKE CONDUCTANCE.
      ZERO=0.
      IF(NLAY.EQ.1) GO TO 20
      K1=NLAY-1
      DO 10 K=1,K1
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
         IF (IMINC.EQ.1) THEN                                           ! DLT
            C = 1.0/(CV(J,I,K)+TINY)                                    ! DLT
!            C = MIN(C,MAXC)                                             ! DLT
            C = MAX(C,MINC)                                             ! DLT
            CV(J,I,K)=DELR(J)*DELC(I)/C                                 ! DLT
         ELSE
            CV(J,I,K)=CV(J,I,K)*DELR(J)*DELC(I)
         END IF
         CV(j,i,k)=MIN(MAXVCOND,CV(j,i,k))
   10 CONTINUE
C
C2------IF WETTING CAPABILITY IS ACTIVATED, SAVE CV IN CVWD FOR USE WHEN
C2------WETTING CELLS.
      IF(IWDFLG.EQ.0) GO TO 20
      DO 15 K=1,K1
      DO 15 I=1,NROW
      DO 15 J=1,NCOL
      CVWD(J,I,K)=CV(J,I,K)
   15 CONTINUE
C
C3------IF IBOUND=0, SET CV=0 AND CC=0.
   20 DO 30 K=1,NLAY
      DO 30 I=1,NROW
      DO 30 J=1,NCOL
      IF(IBOUND(J,I,K).NE.0) GO TO 30
      IF(K.NE.NLAY) CV(J,I,K)=ZERO
      IF(K.NE.1) CV(J,I,K-1)=ZERO
      CC(J,I,K)=ZERO
   30 CONTINUE
C
C4------INSURE THAT EACH ACTIVE CELL HAS AT LEAST ONE NON-ZERO
C4------TRANSMISSIVE PROPERTY.
      HCNV=HNOFLO
      KB=0
      DO 60 K=1,NLAY
      IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) GO TO 50
C
C4A-----WHEN LAYER TYPE IS 0 OR 2, TRANSMISSIVITY OR CV MUST BE NONZERO.
      DO 45 I=1,NROW
      DO 45 J=1,NCOL
      IF(IBOUND(J,I,K).EQ.0) GO TO 45
      IF(CC(J,I,K).NE.ZERO) GO TO 45
      IF(K.EQ.NLAY) GO TO 41
      IF(CV(J,I,K).NE.ZERO) GO TO 45
   41 IF(K.EQ.1) GO TO 42
      IF(CV(J,I,K-1).NE.ZERO) GO TO 45
   42 IBOUND(J,I,K)=0
      HNEW(J,I,K)=HCNV
      WRITE(IOUT,43) K,I,J
   43 FORMAT(1X,'NODE (LAYER,ROW,COL)',3I4,
     1      ' ELIMINATED BECAUSE ALL CONDUCTANCES TO NODE ARE 0')
   45 CONTINUE
      GO TO 60
C
C4B-----WHEN LAYER TYPE IS 1 OR 3, HY OR CV MUST BE NONZERO.
   50 KB=KB+1
      DO 59 I=1,NROW
      DO 59 J=1,NCOL
C
C4B1----IF WETTING CAPABILITY IS ACTIVE, CHECK CVWD.
      IF(IWDFLG.EQ.0) GO TO 55
      IF(WETDRY(J,I,KB).EQ.ZERO) GO TO 55
      IF(K.EQ.NLAY) GO TO 51
      IF(CVWD(J,I,K).NE.ZERO) GO TO 59
   51 IF(K.EQ.1) GO TO 57
      IF(CVWD(J,I,K-1).NE.ZERO) GO TO 59
      GO TO 57
C
C4B2----WETTING CAPABILITY IS INACTIVE, SO CHECK CV AT ACTIVE CELLS.
   55 IF(IBOUND(J,I,K).EQ.0) GO TO 59
      IF(K.EQ.NLAY) GO TO 56
      IF(CV(J,I,K).NE.ZERO) GO TO 59
   56 IF(K.EQ.1) GO TO 57
      IF(CV(J,I,K-1).NE.ZERO) GO TO 59
C
C4B3----CHECK HYDRAULIC CONDUCTIVITY.
   57 IF(HY(J,I,KB).NE.ZERO) GO TO 59
C
C4B4----HY AND CV ARE ALL 0, SO CONVERT CELL TO NO FLOW.
      IBOUND(J,I,K)=0
      HNEW(J,I,K)=HCNV
      IF(IWDFLG.NE.0) WETDRY(J,I,KB)=ZERO
      WRITE(IOUT,43) K,I,J
   59 CONTINUE
   60 CONTINUE
C
C5------CALCULATE HOR. CONDUCTANCE(CR AND CC) FOR CONSTANT T LAYERS.
      DO 70 K=1,NLAY
      KK=K
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) GO TO 70
      IF(LAYAVG(K).EQ.0) THEN
         CALL SGWF2BCF7C(KK,cc,cr)
      ELSE IF(LAYAVG(K).EQ.10) THEN
         CALL SGWF2BCF7A(KK)
      ELSE
         CALL SGWF2BCF7L(KK)
      END IF
   70 CONTINUE
C
C6------IF TRANSIENT, LOOP THROUGH LAYERS AND CALCULATE STORAGE
C6------CAPACITY.
      IF(ISS.NE.0) GO TO 100
      KT=0
      DO 90 K=1,NLAY
C
C6A-----MULTIPLY PRIMARY STORAGE COEFFICIENT BY DELR & DELC TO GET
C6A-----PRIMARY STORAGE CAPACITY.
      DO 80 I=1,NROW
      DO 80 J=1,NCOL
      SC1(J,I,K)=SC1(J,I,K)*DELR(J)*DELC(I)
   80 CONTINUE
C
C6B-----IF LAYER IS CONF/UNCONF MULTIPLY SECONDARY STORAGE COEFFICIENT
C6B-----BY DELR AND DELC TO GET SECONDARY STORAGE CAPACITY(SC2).
      IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 90
      KT=KT+1
      DO 85 I=1,NROW
      DO 85 J=1,NCOL
      SC2(J,I,KT)=SC2(J,I,KT)*DELR(J)*DELC(I)
   85 CONTINUE
   90 CONTINUE
C
C7------RETURN.
  100 RETURN
      END
      SUBROUTINE SGWF2BCF7A(K)
C     ******************************************************************
C-------COMPUTE CONDUCTANCE USING ARITHMETIC MEAN TRANSMISSIVITY
C-------ACTIVATED BY LAYAVG=10
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,CR,CC,DELR,DELC
      USE GWFBCFMODULE,ONLY:TRPY,
     1                      IMINKD,MINKD                                ! DLT
C     ------------------------------------------------------------------
C
      ZERO=0.
      YX=TRPY(K)
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 40 I=1,NROW
      DO 40 J=1,NCOL
      T1=CC(J,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T1 = MAX(T1,MINKD)                                             ! DLT
      END IF                                                            ! DLT
C
C2------IF T=0 THEN SET CONDUCTANCE EQUAL TO 0. GO ON TO NEXT CELL.
      IF(T1.NE.ZERO) GO TO 10
      CR(J,I,K)=ZERO
      GO TO 40
C
C3------IF THIS IS NOT THE LAST COLUMN(RIGHTMOST) THEN CALCULATE
C3------BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
   10 IF(J.EQ.NCOL) GO TO 30
      T2=CC(J+1,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
C3A-----ARITHMETIC MEAN INTERBLOCK TRANSMISSIVITY
      IF(T2.EQ.ZERO) THEN
         CR(J,I,K)=ZERO
      ELSE
         CR(J,I,K)=DELC(I)*(T1+T2)/(DELR(J+1)+DELR(J))
      END IF
C
C4------IF THIS IS NOT THE LAST ROW(FRONTMOST) THEN CALCULATE
C4------BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
   30 IF(I.EQ.NROW) GO TO 40
      T2=CC(J,I+1,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
      IF(T2.EQ.ZERO) THEN
         CC(J,I,K)=ZERO
      ELSE
         CC(J,I,K)=YX*DELR(J)*(T1+T2)/(DELC(I+1)+DELC(I))
      END IF
   40 CONTINUE
C
C5------RETURN
      RETURN
      END
      SUBROUTINE SGWF2BCF7L(K)
C     ******************************************************************
C-------COMPUTE CONDUCTANCE USING LOGARITHMIC MEAN TRANSMISSIVITY
C-------ACTIVATED BY LAYAVG=20
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,CR,CC,DELR,DELC
      USE GWFBCFMODULE,ONLY:TRPY,
     1                      IMINKD,MINKD                                ! DLT
C     ------------------------------------------------------------------
C
      ZERO=0.
      TWO=2.
      HALF=0.5
      FRAC1=1.005
      FRAC2=0.995
      YX=TRPY(K)*TWO
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 40 I=1,NROW
      DO 40 J=1,NCOL
      T1=CC(J,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T1 = MAX(T1,MINKD)                                             ! DLT
      END IF                                                            ! DLT
C
C2------IF T=0 THEN SET CONDUCTANCE EQUAL TO 0. GO ON TO NEXT CELL.
      IF(T1.NE.ZERO) GO TO 10
      CR(J,I,K)=ZERO
      GO TO 40
C
C3------IF THIS IS NOT THE LAST COLUMN(RIGHTMOST) THEN CALCULATE
C3------BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
   10 IF(J.EQ.NCOL) GO TO 30
      T2=CC(J+1,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
      IF(T2.EQ.ZERO) THEN
C3A-----SET TO ZERO AND EXIT IF T2 IS ZERO
         CR(J,I,K)=ZERO
         GO TO 30
      END IF
C3B-----LOGARITHMIC MEAN INTERBLOCK TRANSMISSIVITY
      RATIO=T2/T1
      IF(RATIO.GT.FRAC1.OR.RATIO.LT.FRAC2) THEN
         T=(T2-T1)/LOG(RATIO)
      ELSE
         T=HALF*(T1+T2)
      END IF
      CR(J,I,K)=TWO*DELC(I)*T/(DELR(J+1)+DELR(J))
C
C4------IF THIS IS NOT THE LAST ROW(FRONTMOST) THEN CALCULATE
C4------BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
   30 IF(I.EQ.NROW) GO TO 40
      T2=CC(J,I+1,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
      IF(T2.EQ.ZERO) THEN
         CC(J,I,K)=ZERO
         GO TO 40
      END IF
      RATIO=T2/T1
      IF(RATIO.GT.FRAC1.OR.RATIO.LT.FRAC2) THEN
         T=(T2-T1)/LOG(RATIO)
      ELSE
         T=HALF*(T1+T2)
      END IF
      CC(J,I,K)=YX*DELR(J)*T/(DELC(I+1)+DELC(I))
   40 CONTINUE
C
C5------RETURN
      RETURN
      END
      SUBROUTINE SGWF2BCF7U(K)
C     ******************************************************************
C-------COMPUTE CONDUCTANCE USING ARITHMETIC MEAN SATURATED THICKNESS
C-------AND LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY
C-------NODE HYDRAULIC CONDUCTIVITY IS IN CC,
C-------NODE SATURATED THICKNESS IS IN BUFF
C-------ACTIVATED BY LAYAVG=30
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,CR,CC,BUFF,DELR,DELC
      USE GWFBCFMODULE,ONLY:TRPY,
     1                      IMINKD,MINKD                                ! DLT
C     ------------------------------------------------------------------
C
      ZERO=0.
      HALF=0.5
      FRAC1=1.005
      FRAC2=0.995
      YX=TRPY(K)
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 40 I=1,NROW
      DO 40 J=1,NCOL
      T1=CC(J,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T1 = MAX(T1,MINKD)                                             ! DLT
      END IF                                                            ! DLT
C
C2------IF T=0 THEN SET CONDUCTANCE EQUAL TO 0. GO ON TO NEXT CELL.
      IF(T1.NE.ZERO) GO TO 10
      CR(J,I,K)=ZERO
      GO TO 40
C
C3------IF THIS IS NOT THE LAST COLUMN(RIGHTMOST) THEN CALCULATE
C3------BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
   10 IF(J.EQ.NCOL) GO TO 30
      T2=CC(J+1,I,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
      IF(T2.EQ.ZERO) THEN
C3A-----SET TO ZERO AND EXIT IF T2 IS ZERO
         CR(J,I,K)=ZERO
         GO TO 30
      END IF
C3B-----LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY
      RATIO=T2/T1
      IF(RATIO.GT.FRAC1.OR.RATIO.LT.FRAC2) THEN
         T=(T2-T1)/LOG(RATIO)
      ELSE
         T=HALF*(T1+T2)
      END IF
C3C-----MULTIPLY LOGARITHMIC K BY ARITHMETIC SAT THICK
      CR(J,I,K)=DELC(I)*T*(BUFF(J,I,K)+BUFF(J+1,I,K))
     *               /(DELR(J+1)+DELR(J))
C
C4------IF THIS IS NOT THE LAST ROW(FRONTMOST) THEN CALCULATE
C4------BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
   30 IF(I.EQ.NROW) GO TO 40
      T2=CC(J,I+1,K)
      IF (IMINKD.EQ.1) THEN                                             ! DLT
         T2 = MAX(T2,MINKD)                                             ! DLT
      END IF                                                            ! DLT
      IF(T2.EQ.ZERO) THEN
         CC(J,I,K)=ZERO
         GO TO 40
      END IF
      RATIO=T2/T1
      IF(RATIO.GT.FRAC1.OR.RATIO.LT.FRAC2) THEN
         T=(T2-T1)/LOG(RATIO)
      ELSE
         T=HALF*(T1+T2)
      END IF
      CC(J,I,K)=YX*DELR(J)*T*(BUFF(J,I,K)+BUFF(J,I+1,K))
     *            /(DELC(I+1)+DELC(I))
   40 CONTINUE
C
C5------RETURN
      RETURN
      END
      SUBROUTINE GWF2BCF7DA(IGRID)
      USE GWFBCFMODULE
C
      DEALLOCATE(GWFBCFDAT(IGRID)%IBCFCB)
      DEALLOCATE(GWFBCFDAT(IGRID)%IWDFLG)
      DEALLOCATE(GWFBCFDAT(IGRID)%IWETIT)
      DEALLOCATE(GWFBCFDAT(IGRID)%IHDWET)
      DEALLOCATE(GWFBCFDAT(IGRID)%WETFCT)
      DEALLOCATE(GWFBCFDAT(IGRID)%LAYCON)
      DEALLOCATE(GWFBCFDAT(IGRID)%LAYAVG)
      DEALLOCATE(GWFBCFDAT(IGRID)%HY)
      DEALLOCATE(GWFBCFDAT(IGRID)%SC1)
      DEALLOCATE(GWFBCFDAT(IGRID)%SC2)
      DEALLOCATE(GWFBCFDAT(IGRID)%WETDRY)
      DEALLOCATE(GWFBCFDAT(IGRID)%CVWD)
      DEALLOCATE(GWFBCFDAT(IGRID)%TRPY)
      deallocate(gwfbcfdat(igrid)%seepage)                              ! DLT
      DEALLOCATE(GWFBCFDAT(IGRID)%IMINKD)                               ! DLT
      DEALLOCATE(GWFBCFDAT(IGRID)%IMINC)                                ! DLT
      DEALLOCATE(GWFBCFDAT(IGRID)%MINKD)                                ! DLT
      DEALLOCATE(GWFBCFDAT(IGRID)%MINC)                                 ! DLT
C
      RETURN
      END
      SUBROUTINE SGWF2BCF7PNT(IGRID)
      USE GWFBCFMODULE
C
      IBCFCB=>GWFBCFDAT(IGRID)%IBCFCB
      IWDFLG=>GWFBCFDAT(IGRID)%IWDFLG
      IWETIT=>GWFBCFDAT(IGRID)%IWETIT
      IHDWET=>GWFBCFDAT(IGRID)%IHDWET
      WETFCT=>GWFBCFDAT(IGRID)%WETFCT
      LAYCON=>GWFBCFDAT(IGRID)%LAYCON
      LAYAVG=>GWFBCFDAT(IGRID)%LAYAVG
      HY=>GWFBCFDAT(IGRID)%HY
      SC1=>GWFBCFDAT(IGRID)%SC1
      SC2=>GWFBCFDAT(IGRID)%SC2
      WETDRY=>GWFBCFDAT(IGRID)%WETDRY
      CVWD=>GWFBCFDAT(IGRID)%CVWD
      TRPY=>GWFBCFDAT(IGRID)%TRPY
      seepage=>gwfbcfdat(igrid)%seepage                                 ! DLT
      IMINKD=>GWFBCFDAT(IGRID)%IMINKD                                   ! DLT
      IMINC=>GWFBCFDAT(IGRID)%IMINC                                     ! DLT
      MINKD=>GWFBCFDAT(IGRID)%MINKD                                     ! DLT
      MINC=>GWFBCFDAT(IGRID)%MINC                                       ! DLT
C
      RETURN
      END
      SUBROUTINE SGWF2BCF7PSV(IGRID)
      USE GWFBCFMODULE
C
      GWFBCFDAT(IGRID)%IBCFCB=>IBCFCB
      GWFBCFDAT(IGRID)%IWDFLG=>IWDFLG
      GWFBCFDAT(IGRID)%IWETIT=>IWETIT
      GWFBCFDAT(IGRID)%IHDWET=>IHDWET
      GWFBCFDAT(IGRID)%WETFCT=>WETFCT
      GWFBCFDAT(IGRID)%LAYCON=>LAYCON
      GWFBCFDAT(IGRID)%LAYAVG=>LAYAVG
      GWFBCFDAT(IGRID)%HY=>HY
      GWFBCFDAT(IGRID)%SC1=>SC1
      GWFBCFDAT(IGRID)%SC2=>SC2
      GWFBCFDAT(IGRID)%WETDRY=>WETDRY
      GWFBCFDAT(IGRID)%CVWD=>CVWD
      GWFBCFDAT(IGRID)%TRPY=>TRPY
      gwfbcfdat(igrid)%seepage=>seepage                                 ! DLT
      GWFBCFDAT(IGRID)%IMINKD=>IMINKD                                   ! DLT
      GWFBCFDAT(IGRID)%IMINC=>IMINC                                     ! DLT
      GWFBCFDAT(IGRID)%MINKD=>MINKD                                     ! DLT
      GWFBCFDAT(IGRID)%MINC=>MINC                                       ! DLT
C
      RETURN
      END
