c   Copyright (C) Stichting Deltares, 2005-2017.
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

      MODULE GWFRCHMODULE
        INTEGER, SAVE, POINTER                 ::NRCHOP,IRCHCB
        INTEGER, SAVE, POINTER                 ::NPRCH,IRCHPF
        INTEGER, SAVE, POINTER                 ::IADDRECH               ! DLT
        REAL,    SAVE,   DIMENSION(:,:),  POINTER      ::RECH
        INTEGER, SAVE,   DIMENSION(:,:),  POINTER      ::IRCH
      TYPE GWFRCHTYPE
        INTEGER,  POINTER                 ::NRCHOP,IRCHCB
        INTEGER,  POINTER                 ::NPRCH,IRCHPF
        INTEGER,  POINTER                 ::IADDRECH                    ! DLT
        REAL,       DIMENSION(:,:),  POINTER      ::RECH
        INTEGER,    DIMENSION(:,:),  POINTER      ::IRCH
      END TYPE
      TYPE(GWFRCHTYPE), SAVE ::GWFRCHDAT(10)
      END MODULE GWFRCHMODULE


      SUBROUTINE GWF2RCH7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR RECHARGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,IFREFM
      USE GWFRCHMODULE,ONLY:NRCHOP,IRCHCB,NPRCH,IRCHPF,RECH,IRCH,
     1                      IADDRECH                                    ! DLT
C
      CHARACTER*200 LINE
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
C
C1-------ALLOCATE SCALAR VARIABLES.
      ALLOCATE(NRCHOP,IRCHCB)
      ALLOCATE(NPRCH,IRCHPF)
      ALLOCATE(IADDRECH)                                                ! DLT
C
C2------IDENTIFY PACKAGE.
      IRCHPF=0
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'RCH -- RECHARGE PACKAGE, VERSION 7, 5/2/2005',
     1' INPUT READ FROM UNIT ',I4)
C
C3------READ NRCHOP AND IRCHCB.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARARRAL(IN,IOUT,LINE,NPRCH)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') NRCHOP,IRCHCB
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NRCHOP,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRCHCB,R,IOUT,IN)
      END IF
C
      LLOC=1
      IADDRECH=0                                                        ! DLT
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)                 ! DLT
      IF(LINE(ISTART:ISTOP).EQ.'ADDRECH') THEN                          ! DLT
         IADDRECH=1                                                     ! DLT
      ENDIF                                                             ! DLT
      IF(LLOC.LT.200) GO TO 10                                          ! DLT
C
C4------CHECK TO SEE THAT OPTION IS LEGAL.
      IF(NRCHOP.LT.1.OR.NRCHOP.GT.3) THEN
        WRITE(IOUT,8) NRCHOP
    8   FORMAT(1X,'ILLEGAL RECHARGE OPTION CODE (NRCHOP = ',I5,
     &       ') -- SIMULATION ABORTING')
        CALL USTOP(' ')
      END IF
C
C5------OPTION IS LEGAL -- PRINT OPTION CODE.
      IF(NRCHOP.EQ.1) WRITE(IOUT,201)
  201 FORMAT(1X,'OPTION 1 -- RECHARGE TO TOP LAYER')
      IF(NRCHOP.EQ.2) WRITE(IOUT,202)
  202 FORMAT(1X,'OPTION 2 -- RECHARGE TO ONE SPECIFIED NODE IN EACH',
     1     ' VERTICAL COLUMN')
      IF(NRCHOP.EQ.3) WRITE(IOUT,203)
  203 FORMAT(1X,'OPTION 3 -- RECHARGE TO HIGHEST ACTIVE NODE IN',
     1     ' EACH VERTICAL COLUMN')
C
C6------IF CELL-BY-CELL FLOWS ARE TO BE SAVED, THEN PRINT UNIT NUMBER.
      IF(IRCHCB.GT.0) WRITE(IOUT,204) IRCHCB
  204 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C7------ALLOCATE SPACE FOR THE RECHARGE (RECH) AND INDICATOR (IRCH)
C7------ARRAYS.
      ALLOCATE (RECH(NCOL,NROW))
      ALLOCATE (IRCH(NCOL,NROW))
C
C8------READ NAMED PARAMETERS
      WRITE(IOUT,5) NPRCH
    5 FORMAT(1X,//1X,I5,' Recharge parameters')
      IF(NPRCH.GT.0) THEN
         DO 20 K=1,NPRCH
         CALL UPARARRRP(IN,IOUT,N,0,PTYP,1,1,0)
         IF(PTYP.NE.'RCH') THEN
            WRITE(IOUT,7)
    7       FORMAT(1X,'Parameter type must be RCH')
            CALL USTOP(' ')
         END IF
   20    CONTINUE
      END IF
C
C9------RETURN
      CALL SGWF2RCH7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2RCH7RP(IN,IGRID)
C     ******************************************************************
C     READ RECHARGE DATA FOR STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,DELR,DELC,
     1                      BUFF                                        ! DLT
      USE GWFRCHMODULE,ONLY:NRCHOP,NPRCH,IRCHPF,RECH,IRCH,
     1                      IADDRECH                                    ! DLT
      use rdrsmodule, only: nodata                                      ! DLT
C
      CHARACTER*24 ANAME(2)
C
      DATA ANAME(1) /'    RECHARGE LAYER INDEX'/
      DATA ANAME(2) /'                RECHARGE'/
C
      INTEGER :: IL,IR,IC                                               ! DLT
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2RCH7PNT(IGRID)
C
C2------READ FLAGS SHOWING WHETHER DATA IS TO BE REUSED.
      IF(NRCHOP.EQ.2) THEN
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(2I10)') INRECH,INIRCH
         ELSE
            READ(IN,*) INRECH,INIRCH
         END IF
      ELSE
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') INRECH
         ELSE
            READ(IN,*) INRECH
         END IF
      END IF
C
C3------TEST INRECH TO SEE HOW TO DEFINE RECH.
      IF(INRECH.LT.0) THEN
C
C3A-----INRECH<0, SO REUSE RECHARGE ARRAY FROM LAST STRESS PERIOD.
        call sts2nodata(in)                                              ! STS
        WRITE(IOUT,3)
    3   FORMAT(1X,/1X,'REUSING RECH FROM LAST STRESS PERIOD')
      ELSE
C
C3B-----INRECH=>0, SO READ RECHARGE RATE.
        call sts2data(in)                                                ! STS
        IF(NPRCH.EQ.0) THEN
C
C3B1----THERE ARE NO PARAMETERS, SO READ RECH USING U2DREL.
          nodata = 0.                                                   ! DLT
          CALL U2DREL(RECH,ANAME(2),NROW,NCOL,0,IN,IOUT)
          IF (IADDRECH.EQ.1) THEN                                       ! DLT
             DO I = 2, INRECH                                           ! DLT
                CALL U2DREL(BUFF(1,1,1),ANAME(2),NROW,NCOL,0,IN,IOUT)   ! DLT
                DO IC=1,NCOL                                            ! DLT
                   DO IR=1,NROW                                         ! DLT
                      RECH(IC,IR)=RECH(IC,IR)+BUFF(IC,IR,1)             ! DLT
                   END DO                                               ! DLT
                END DO                                                  ! DLT
             END DO                                                     ! DLT
          END IF                                                        ! DLT
          nodata = -9999.                                               ! DLT
        ELSE
C
C3B2----DEFINE RECH USING PARAMETERS.  INRECH IS THE NUMBER OF
C3B2----PARAMETERS TO USE THIS STRESS PERIOD.
          CALL PRESET('RCH')
          WRITE(IOUT,33)
   33     FORMAT(1X,///1X,
     1      'RECH array defined by the following parameters:')
          IF(INRECH.EQ.0) THEN
              WRITE(IOUT,34)
   34         FORMAT(' ERROR: When parameters are defined for the RCH',
     &      ' Package, at least one parameter',/,' must be specified',
     &      ' each stress period -- STOP EXECUTION (GWF2RCH7RPLL)')
              CALL USTOP(' ')
            END IF
            CALL UPARARRSUB2(RECH,NCOL,NROW,0,INRECH,IN,IOUT,'RCH',
     1            ANAME(2),'RCH',IRCHPF)
        END IF
C
C4------MULTIPLY RECHARGE RATE BY CELL AREA TO GET VOLUMETRIC RATE.
        DO 50 IR=1,NROW
        DO 50 IC=1,NCOL
        RECH(IC,IR)=RECH(IC,IR)*DELR(IC)*DELC(IR)
   50   CONTINUE
      END IF

      call pest1alpha_grid('RE',rech,nrow,ncol,1,iout)                 ! IPEST

C
C5------IF NRCHOP=2 THEN A LAYER INDICATOR ARRAY IS NEEDED.  TEST INIRCH
C5------TO SEE HOW TO DEFINE IRCH.
      IF(NRCHOP.EQ.2) THEN
        IF(INIRCH.LT.0) THEN
C
C5A-----INIRCH<0, SO REUSE LAYER INDICATOR ARRAY FROM LAST STRESS PERIOD.
          call sts2nodata(in)                                            ! STS
          WRITE(IOUT,2)
    2     FORMAT(1X,/1X,'REUSING IRCH FROM LAST STRESS PERIOD')
        ELSE
C
C5B-----INIRCH=>0, SO CALL U2DINT TO READ LAYER INDICATOR ARRAY(IRCH)
          call sts2data(in)                                              ! STS
          CALL U2DINT(IRCH,ANAME(1),NROW,NCOL,0,IN,IOUT)
          DO 57 IR=1,NROW
          DO 57 IC=1,NCOL
          IF(IRCH(IC,IR).LT.1 .OR. IRCH(IC,IR).GT.NLAY) THEN
            WRITE(IOUT,56) IC,IR,IRCH(IC,IR)
   56       FORMAT(1X,/1X,'INVALID LAYER NUMBER IN IRCH FOR COLUMN',I4,
     1        '  ROW',I4,'  :',I4)
            CALL USTOP(' ')
          END IF
   57     CONTINUE
        END IF
      END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF2RCH7FM(IGRID)
C     ******************************************************************
C     SUBTRACT RECHARGE FROM RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,RHS
      USE GWFRCHMODULE,ONLY:NRCHOP,RECH,IRCH
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2RCH7PNT(IGRID)
C
C2------DETERMINE WHICH RECHARGE OPTION.
      IF(NRCHOP.EQ.1) THEN
C
C3------NRCHOP IS 1, SO RECHARGE IS IN TOP LAYER. LAYER INDEX IS 1.
        DO 10 IR=1,NROW
        DO 10 IC=1,NCOL
C
C3A-----IF CELL IS VARIABLE HEAD, SUBTRACT RECHARGE RATE FROM
C3A-----RIGHT-HAND-SIDE.
        IF(IBOUND(IC,IR,1).GT.0) RHS(IC,IR,1)=RHS(IC,IR,1)-RECH(IC,IR)
   10   CONTINUE
      ELSE IF(NRCHOP.EQ.2) THEN
C
C4------NRCHOP IS 2, SO RECHARGE IS INTO LAYER IN INDICATOR ARRAY
        DO 20 IR=1,NROW
        DO 20 IC=1,NCOL
        IL=IRCH(IC,IR)
C
C4A-----IF THE CELL IS VARIABLE HEAD, SUBTRACT RECHARGE FROM
C4A-----RIGHT-HAND-SIDE.
        IF(IL.EQ.0) GO TO 20
        IF(IBOUND(IC,IR,IL).GT.0)
     1               RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)
   20   CONTINUE
      ELSE
C
C5------NRCHOP IS 3, RECHARGE IS INTO HIGHEST VARIABLE-HEAD CELL, EXCEPT
C5------CANNOT PASS THROUGH CONSTANT HEAD NODE
        DO 30 IR=1,NROW
        DO 30 IC=1,NCOL
        DO 28 IL=1,NLAY
C
C5A-----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
        IF(IBOUND(IC,IR,IL).LT.0) GO TO 30
C
C5B-----IF THE CELL IS VARIABLE HEAD, SUBTRACT RECHARGE FROM
C5B-----RIGHT-HAND-SIDE AND MOVE TO NEXT HORIZONTAL LOCATION.
        IF(IBOUND(IC,IR,IL).GT.0) THEN
          RHS(IC,IR,IL)=RHS(IC,IR,IL)-RECH(IC,IR)
          GO TO 30
        END IF
   28   CONTINUE
   30   CONTINUE
      END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF2RCH7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR RECHARGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,ICBCFL,DELT,PERTIM,TOTIM
      USE GWFRCHMODULE,ONLY:NRCHOP,IRCHCB,RECH,IRCH
C
      DOUBLE PRECISION RATIN,RATOUT,QQ
      CHARACTER*16 TEXT
      DATA TEXT /'        RECHARGE'/
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2RCH7PNT(IGRID)
C
C2------CLEAR THE RATE ACCUMULATORS.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
C
C3------CLEAR THE BUFFER & SET FLAG FOR SAVING CELL-BY-CELL FLOW TERMS.
      DO 2 IL=1,NLAY
      DO 2 IR=1,NROW
      DO 2 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
    2 CONTINUE
      IBD=0
      IF(IRCHCB.GT.0) IBD=ICBCFL
C
C4------DETERMINE THE RECHARGE OPTION.
      IF(NRCHOP.EQ.1) THEN
C
C5------NRCHOP=1, SO RECH GOES INTO LAYER 1. PROCESS EACH HORIZONTAL
C5------CELL LOCATION.
        DO 10 IR=1,NROW
        DO 10 IC=1,NCOL
C
C5A-----IF CELL IS VARIABLE HEAD, THEN DO BUDGET FOR IT.
        IF(IBOUND(IC,IR,1).GT.0) THEN
          Q=RECH(IC,IR)
          QQ=Q
C
C5B-----ADD RECH TO BUFF.
          BUFF(IC,IR,1)=Q
C
C5C-----IF RECH POSITIVE ADD IT TO RATIN, ELSE ADD IT TO RATOUT.
          IF(Q.GE.ZERO) THEN
            RATIN=RATIN+QQ
          ELSE
            RATOUT=RATOUT-QQ
          END IF
        END IF
   10   CONTINUE
      ELSE IF(NRCHOP.EQ.2) THEN
C
C6------NRCHOP=2, RECH IS IN LAYER SPECIFIED IN INDICATOR ARRAY(IRCH).
C6------PROCESS EACH HORIZONTAL CELL LOCATION.
        DO 20 IR=1,NROW
        DO 20 IC=1,NCOL
C
C6A-----GET LAYER INDEX FROM INDICATOR ARRAY(IRCH).
        IL=IRCH(IC,IR)
C
C6B-----IF CELL IS VARIABLE HEAD, THEN DO BUDGET FOR IT.
        IF(IL.EQ.0) GO TO 20
        IF(IBOUND(IC,IR,IL).GT.0) THEN
          Q=RECH(IC,IR)
          QQ=Q
C
C6C-----ADD RECHARGE TO BUFF.
          BUFF(IC,IR,IL)=Q
C
C6D-----IF RECHARGE IS POSITIVE ADD TO RATIN, ELSE ADD IT TO RATOUT.
          IF(Q.GE.ZERO) THEN
            RATIN=RATIN+QQ
          ELSE
            RATOUT=RATOUT-QQ
          END IF
        END IF
   20   CONTINUE
      ELSE
C
C7------NRCHOP=3; RECHARGE IS INTO HIGHEST CELL IN A VERTICAL COLUMN
C7------THAT IS NOT NO FLOW.  PROCESS EACH HORIZONTAL CELL LOCATION.
        DO 30 IR=1,NROW
        DO 29 IC=1,NCOL
C
C7A-----INITIALIZE IRCH TO 1, AND LOOP THROUGH CELLS IN A VERTICAL
C7A-----COLUMN TO FIND WHERE TO PLACE RECHARGE.
        IRCH(IC,IR)=1
        DO 28 IL=1,NLAY
C
C7B-----IF CELL IS CONSTANT HEAD, MOVE ON TO NEXT HORIZONTAL LOCATION.
        IF(IBOUND(IC,IR,IL).LT.0) GO TO 29
C
C7C-----IF CELL IS VARIABLE HEAD, THEN DO BUDGET FOR IT.
        IF (IBOUND(IC,IR,IL).GT.0) THEN
          Q=RECH(IC,IR)
          QQ=Q
C
C7D-----ADD RECHARGE TO BUFFER, AND STORE LAYER NUMBER IN IRCH.
          BUFF(IC,IR,IL)=Q
          IRCH(IC,IR)=IL
C
C7E-----IF RECH IS POSITIVE ADD IT TO RATIN, ELSE ADD IT TO RATOUT.
          IF(Q.GE.ZERO) THEN
            RATIN=RATIN+QQ
          ELSE
            RATOUT=RATOUT-QQ
          END IF
          GO TO 29
        END IF
28      CONTINUE
29      CONTINUE
30      CONTINUE
C
      END IF
C
C8------IF CELL-BY-CELL FLOW TERMS SHOULD BE SAVED, CALL APPROPRIATE
C8------UTILITY MODULE TO WRITE THEM.
100   IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IRCHCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV3(KSTP,KPER,TEXT,IRCHCB,BUFF,IRCH,NRCHOP,
     1                   NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C9------MOVE TOTAL RECHARGE RATE INTO VBVL FOR PRINTING BY BAS1OT.
      ROUT=RATOUT
      RIN=RATIN
      VBVL(4,MSUM)=ROUT
      VBVL(3,MSUM)=RIN
C
C10-----ADD RECHARGE FOR TIME STEP TO RECHARGE ACCUMULATOR IN VBVL.
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
C
C11-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS_OT.
      VBNM(MSUM)=TEXT
C
C12-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C13-----RETURN
      RETURN
      END
      SUBROUTINE GWF2RCH7DA(IGRID)
C  Deallocate RCH DATA
      USE GWFRCHMODULE
C
        DEALLOCATE(GWFRCHDAT(IGRID)%NRCHOP)
        DEALLOCATE(GWFRCHDAT(IGRID)%IRCHCB)
        DEALLOCATE(GWFRCHDAT(IGRID)%NPRCH)
        DEALLOCATE(GWFRCHDAT(IGRID)%IRCHPF)
        DEALLOCATE(GWFRCHDAT(IGRID)%RECH)
        DEALLOCATE(GWFRCHDAT(IGRID)%IRCH)
        DEALLOCATE(GWFRCHDAT(IGRID)%IADDRECH)                           ! DLT
C
      RETURN
      END
      SUBROUTINE SGWF2RCH7PNT(IGRID)
C  Set RCH pointers for grid.
      USE GWFRCHMODULE
C
        NRCHOP=>GWFRCHDAT(IGRID)%NRCHOP
        IRCHCB=>GWFRCHDAT(IGRID)%IRCHCB
        NPRCH=>GWFRCHDAT(IGRID)%NPRCH
        IRCHPF=>GWFRCHDAT(IGRID)%IRCHPF
        RECH=>GWFRCHDAT(IGRID)%RECH
        IRCH=>GWFRCHDAT(IGRID)%IRCH
        IADDRECH=>GWFRCHDAT(IGRID)%IADDRECH                             ! DLT
C
      RETURN
      END
      SUBROUTINE SGWF2RCH7PSV(IGRID)
C  Save RCH pointers for grid.
      USE GWFRCHMODULE
C
        GWFRCHDAT(IGRID)%NRCHOP=>NRCHOP
        GWFRCHDAT(IGRID)%IRCHCB=>IRCHCB
        GWFRCHDAT(IGRID)%NPRCH=>NPRCH
        GWFRCHDAT(IGRID)%IRCHPF=>IRCHPF
        GWFRCHDAT(IGRID)%RECH=>RECH
        GWFRCHDAT(IGRID)%IRCH=>IRCH
        GWFRCHDAT(IGRID)%IADDRECH=>IADDRECH                             ! DLT
C
      RETURN
      END
