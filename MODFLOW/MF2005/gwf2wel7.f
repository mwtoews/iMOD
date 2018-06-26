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

      MODULE GWFWELMODULE
        INTEGER,SAVE,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,SAVE,POINTER  ::NPWEL,IWELPB,NNPWEL
        integer,save,pointer  ::iwelsubsys
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::WELAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::WELL
      TYPE GWFWELTYPE
        INTEGER,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,POINTER  ::NPWEL,IWELPB,NNPWEL
        integer,pointer  ::iwelsubsys
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::WELAUX
        REAL,              DIMENSION(:,:), POINTER     ::WELL
      END TYPE
      TYPE(GWFWELTYPE), SAVE:: GWFWELDAT(10)
      END MODULE GWFWELMODULE


      SUBROUTINE GWF2WEL7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR WELL PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFWELMODULE, ONLY:NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL,NPWEL,
     1                       IWELPB,NNPWEL,WELAUX,WELL,
     1                       iwelsubsys
C
      character     welsubsys*16                                        ! wsubsys

      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      ALLOCATE(NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL)
      ALLOCATE(NPWEL,IWELPB,NNPWEL)
      allocate(iwelsubsys)                                              ! wsubsys
      
C
C1------IDENTIFY PACKAGE AND INITIALIZE NWELLS.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'WEL -- WELL PACKAGE, VERSION 7, 5/2/2005',
     1' INPUT READ FROM UNIT ',I4)
      NWELLS=0
      NNPWEL=0
      iwelsubsys=0                                                      ! wsubsys
C
C2------READ MAXIMUM NUMBER OF WELLS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPWEL,MXPW)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTW,IWELCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTW,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTW
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE WELLS AT ONE TIME')
      IF(IWELCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(IWELCB.GT.0) WRITE(IOUT,8) IWELCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C3------READ AUXILIARY VARIABLES AND PRINT FLAG.
      ALLOCATE(WELAUX(20))
      NAUX=0
      IPRWEL=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            WELAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) WELAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY WELL VARIABLE: ',A)
         END IF
         GO TO 10
      else if(line(istart:istop).eq.'WSUBSYS') then                     ! wsubsys
         call urword(line,lloc,istart,istop,1,n,r,iout,in)              ! wsubsys
         welsubsys=line(istart:istop)                                   ! wsubsys
         iwelsubsys=999                                                 ! wsubsys
         go to 10                                                       ! wsubsys
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF WELL CELLS WILL NOT BE PRINTED')
         IPRWEL = 0
         GO TO 10
      END IF
      
c check or RSUBSYS has been defined and AUX variabel exist              ! wsubsys
      if (iwelsubsys.gt.0) then                                         ! wsubsys
         iwelsubsys=0                                                   ! wsubsys
         do i=1,naux                                                    ! wsubsys
            if (welsubsys.eq.welaux(i)) then                            ! wsubsys
               iwelsubsys=i                                             ! wsubsys
            endif                                                       ! wsubsys
         enddo                                                          ! wsubsys
         if (iwelsubsys.eq.0) then                                      ! wsubsys
            ! ERROR defined variable not found                          ! wsubsys
            write(iout,'(1x,3a)') 'ERROR RSUBSYS variable ',welsubsys,  ! wsubsys
     1                     ' not defined as an auxiliary variable.'     ! wsubsys
            call ustop(' ')                                             ! wsubsys
         else                                                           ! wsubsys
            ! rsubsys found                                             ! wsubsys
            write(iout,'(1x,3a)') 'RSUBSYS variabel ',welsubsys,        ! wsubsys
     1                    ' used for sub-system indices.'               ! wsubsys
            ! isubsys gets the column number of WEL in which the       ! wsubsys
            ! sub system indices                                        ! wsubsys
            iwelsubsys=iwelsubsys+4                                     ! wsubsys
         endif                                                          ! wsubsys
      endif       
      
C3A-----THERE ARE FOUR INPUT VALUES PLUS ONE LOCATION FOR
C3A-----CELL-BY-CELL FLOW.
      NWELVL=5+NAUX
C
C4------ALLOCATE SPACE FOR THE WELL DATA.
      IWELPB=MXACTW+1
      MXWELL=MXACTW+MXPW
      IF(MXACTW.LT.1) THEN
         WRITE(IOUT,17)
   17    FORMAT(1X,
     1'Deactivating the Well Package because MXACTW=0')
         IN=0
      END IF
      ALLOCATE (WELL(NWELVL,MXWELL))
C
C5------READ NAMED PARAMETERS.
      WRITE(IOUT,18) NPWEL
   18 FORMAT(1X,//1X,I5,' Well parameters')
      IF(NPWEL.GT.0) THEN
        LSTSUM=IWELPB
        DO 120 K=1,NPWEL
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXWELL,IN,IOUT,IP,'WEL','Q',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
C5A-----READ PARAMETER WITHOUT INSTANCES.
            CALL ULSTRD(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,
     &        IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',
     &        WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
          ELSE
C5B-----READ INSTANCES.
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRWEL)
            CALL ULSTRD(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,
     &        IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',
     &        WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
C
C6------RETURN
      CALL SGWF2WEL7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2WEL7RP(IN,IGRID)
C     ******************************************************************
C     READ WELL DATA FOR A STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFWELMODULE, ONLY:NWELLS,MXWELL,NWELVL,IPRWEL,NPWEL,
     1                       IWELPB,NNPWEL,WELAUX,WELL,
     1                       iwelsubsys
C
      CHARACTER*6 CWELL
C     ------------------------------------------------------------------
      CALL SGWF2WEL7PNT(IGRID)
C
C1----READ NUMBER OF WELLS (OR FLAG SAYING REUSE WELL DATA).
C1----AND NUMBER OF PARAMETERS
      IF(NPWEL.GT.0) THEN
        IF(IFREFM.EQ.0) THEN
           READ(IN,'(2I10)') ITMP,NP
        ELSE
           READ(IN,*) ITMP,NP
        END IF
      ELSE
         NP=0
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') ITMP
         ELSE
            READ(IN,*) ITMP
         END IF
      END IF
C
C------Calculate some constants.
      NAUX=NWELVL-5
      IOUTU = IOUT
      IF (IPRWEL.EQ.0) IOUTU=-IOUTU
C
C1A-----IF ITMP LESS THAN ZERO REUSE NON-PARAMETER DATA. PRINT MESSAGE.
C1A-----IF ITMP=>0, SET NUMBER OF NON-PARAMETER WELLS EQUAL TO ITMP.
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,6)
    6    FORMAT(1X,/
     1    1X,'REUSING NON-PARAMETER WELLS FROM LAST STRESS PERIOD')
         call sts2nodata(in)                                                 ! DLT: save/state
      ELSE
         NNPWEL=ITMP
         call sts2data(in)                                                   ! DLT: save/state
      END IF
C
C1B-----IF THERE ARE NEW NON-PARAMETER WELLS, READ THEM.
      MXACTW=IWELPB-1
      IF(ITMP.GT.0) THEN
         IF(NNPWEL.GT.MXACTW) THEN
            WRITE(IOUT,99) NNPWEL,MXACTW
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE WELLS (',I6,
     1                     ') IS GREATER THAN MXACTW(',I6,')')
            CALL USTOP(' ')
         END IF
         CALL ULSTRD(NNPWEL,WELL,1,NWELVL,MXWELL,1,IN,IOUT,
     1            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     2             WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
  
         call pest1alpha_list('QR',NNPWEL,WELL,NWELVL,MXWELL,           ! IPEST
     1                        iwelsubsys)                               ! IPEST

      END IF
      NWELLS=NNPWEL
C
C1C-----IF THERE ARE ACTIVE WELL PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('Q')
      NREAD=NWELVL-1
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'WEL',IOUTU,'Q',WELL,NWELVL,MXWELL,NREAD,
     1                MXACTW,NWELLS,4,4,
     2            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     3            WELAUX,20,NAUX)
   30    CONTINUE
      END IF
C
C3------PRINT NUMBER OF WELLS IN CURRENT STRESS PERIOD.
      CWELL=' WELLS'
      IF(NWELLS.EQ.1) CWELL=' WELL '
      WRITE(IOUT,101) NWELLS,CWELL
  101 FORMAT(1X,/1X,I6,A)
C
      CALL SGWF2WEL7PSV(IGRID)                                          ! GCD

C6------RETURN
      RETURN
      END
      SUBROUTINE GWF2WEL7FM(IGRID)
C     ******************************************************************
C     SUBTRACT Q FROM RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,RHS,HCOF
      USE GWFWELMODULE, ONLY:NWELLS,WELL
C     ------------------------------------------------------------------
      CALL SGWF2WEL7PNT(IGRID)
C
C1------IF NUMBER OF WELLS <= 0 THEN RETURN.
      IF(NWELLS.LE.0) RETURN
C
C2------PROCESS EACH WELL IN THE WELL LIST.
      DO 100 L=1,NWELLS
      IR=WELL(2,L)
      IC=WELL(3,L)
      IL=WELL(1,L)
      Q=WELL(4,L)
C
C2A-----IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C2B-----IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
C       THE RHS ACCUMULATOR.
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q
  100 CONTINUE
C
C3------RETURN
      RETURN
      END
      SUBROUTINE GWF2WEL7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR WELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GWFWELMODULE,ONLY:NWELLS,IWELCB,WELL,NWELVL,WELAUX
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION RATIN,RATOUT,QQ
      DATA TEXT /'           WELLS'/
C     ------------------------------------------------------------------
      CALL SGWF2WEL7PNT(IGRID)
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
C1------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IWELCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IWELCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NWELVL-5
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,WELAUX,IWELCB,NCOL,NROW,NLAY,
     1          NWELLS,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C4------IF THERE ARE NO WELLS, DO NOT ACCUMULATE FLOW.
      IF(NWELLS.EQ.0) GO TO 200
C
C5------LOOP THROUGH EACH WELL CALCULATING FLOW.
      DO 100 L=1,NWELLS
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING WELL.
      IR=WELL(2,L)
      IC=WELL(3,L)
      IL=WELL(1,L)
      Q=ZERO
C
C5B-----IF THE CELL IS NO-FLOW OR CONSTANT_HEAD, IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 99
C
C5C-----GET FLOW RATE FROM WELL LIST.
      Q=WELL(4,L)
      QQ=Q
C
C5D-----PRINT FLOW RATE IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
         WRITE(IOUT,62) L,IL,IR,IC,Q
   62    FORMAT(1X,'WELL ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '   RATE ',1PG15.6)
         IBDLBL=1
      END IF
C
C5E-----ADD FLOW RATE TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+Q
C
C5F-----SEE IF FLOW IS POSITIVE OR NEGATIVE.
      IF(Q.GE.ZERO) THEN
C
C5G-----FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
        RATIN=RATIN+QQ
      ELSE
C
C5H-----FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
        RATOUT=RATOUT-QQ
      END IF
C
C5I-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  ALSO
C5I-----COPY FLOW TO WELL LIST.
   99 IF(IBD.EQ.2) CALL UBDSVB(IWELCB,NCOL,NROW,IC,IR,IL,Q,
     1                  WELL(:,L),NWELVL,NAUX,5,IBOUND,NLAY)
      WELL(NWELVL,L)=Q
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IWELCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUM=MSUM+1
C
C9------RETURN
      RETURN
      END
      SUBROUTINE GWF2WEL7DA(IGRID)
C  Deallocate WEL MEMORY
      USE GWFWELMODULE
C
        CALL SGWF2WEL7PNT(IGRID)
        deallocate(iwelsubsys)                                          ! wsubsys
        DEALLOCATE(NWELLS)
        DEALLOCATE(MXWELL)
        DEALLOCATE(NWELVL)
        DEALLOCATE(IWELCB)
        DEALLOCATE(IPRWEL)
        DEALLOCATE(NPWEL)
        DEALLOCATE(IWELPB)
        DEALLOCATE(NNPWEL)
        DEALLOCATE(WELAUX)
        DEALLOCATE(WELL)
C
      RETURN
      END
      SUBROUTINE SGWF2WEL7PNT(IGRID)
C  Change WEL data to a different grid.
      USE GWFWELMODULE
C
        iwelsubsys=>gwfweldat(igrid)%iwelsubsys                         ! wsubsys
        NWELLS=>GWFWELDAT(IGRID)%NWELLS
        MXWELL=>GWFWELDAT(IGRID)%MXWELL
        NWELVL=>GWFWELDAT(IGRID)%NWELVL
        IWELCB=>GWFWELDAT(IGRID)%IWELCB
        IPRWEL=>GWFWELDAT(IGRID)%IPRWEL
        NPWEL=>GWFWELDAT(IGRID)%NPWEL
        IWELPB=>GWFWELDAT(IGRID)%IWELPB
        NNPWEL=>GWFWELDAT(IGRID)%NNPWEL
        WELAUX=>GWFWELDAT(IGRID)%WELAUX
        WELL=>GWFWELDAT(IGRID)%WELL
C
      RETURN
      END
      SUBROUTINE SGWF2WEL7PSV(IGRID)
C  Save WEL data for a grid.
      USE GWFWELMODULE
C
        gwfweldat(igrid)%iwelsubsys=>iwelsubsys                         ! wsubsys
        GWFWELDAT(IGRID)%NWELLS=>NWELLS
        GWFWELDAT(IGRID)%MXWELL=>MXWELL
        GWFWELDAT(IGRID)%NWELVL=>NWELVL
        GWFWELDAT(IGRID)%IWELCB=>IWELCB
        GWFWELDAT(IGRID)%IPRWEL=>IPRWEL
        GWFWELDAT(IGRID)%NPWEL=>NPWEL
        GWFWELDAT(IGRID)%IWELPB=>IWELPB
        GWFWELDAT(IGRID)%NNPWEL=>NNPWEL
        GWFWELDAT(IGRID)%WELAUX=>WELAUX
        GWFWELDAT(IGRID)%WELL=>WELL
C
      RETURN
      END
