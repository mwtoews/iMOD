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

      MODULE GWFCHDMODULE
        INTEGER,SAVE,POINTER  ::NCHDS,MXCHD,NCHDVL,IPRCHD
        INTEGER,SAVE,POINTER  ::NPCHD,ICHDPB,NNPCHD
        integer,save,pointer  ::inegbnd                                 ! DLT
        integer,save,pointer  ::iinterp                                 ! DLT
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::CHDAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::CHDS
      TYPE GWFCHDTYPE
        INTEGER,POINTER  ::NCHDS,MXCHD,NCHDVL,IPRCHD
        INTEGER,POINTER  ::NPCHD,ICHDPB,NNPCHD
        integer,pointer  ::inegbnd                                      ! DLT
        integer,pointer  ::iinterp                                      ! DLT
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::CHDAUX
        REAL,              DIMENSION(:,:), POINTER     ::CHDS
      END TYPE
      TYPE(GWFCHDTYPE),SAVE   ::GWFCHDDAT(10)
      END MODULE

      SUBROUTINE GWF2CHD7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR TIME-VARIANT SPECIFIED-HEAD CELLS AND
C     READ NAMED PARAMETER DEFINITIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFCHDMODULE,ONLY:NCHDS,MXCHD,NCHDVL,IPRCHD,NPCHD,ICHDPB,
     1                      NNPCHD,CHDAUX,CHDS,
     1                      inegbnd, iinterp                            ! DLT
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      ALLOCATE(NCHDS,MXCHD,NCHDVL,IPRCHD)
      ALLOCATE(NPCHD,ICHDPB,NNPCHD)
      allocate(inegbnd)                                                 ! DLT
      allocate(iinterp)                                                 ! DLT
C
C1------IDENTIFY OPTION AND INITIALIZE # OF SPECIFIED-HEAD CELLS
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'CHD -- TIME-VARIANT SPECIFIED-HEAD OPTION,',
     1  ' VERSION 7, 5/2/2005',/1X,'INPUT READ FROM UNIT ',I4)
      NCHDS=0
      NNPCHD=0
      inegbnd=0                                                         ! DLT
      iinterp=0                                                         ! DLT
C
C2------READ AND PRINT MXCHD (MAXIMUM NUMBER OF SPECIFIED-HEAD
C2------CELLS TO BE SPECIFIED EACH STRESS PERIOD)
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPCHD,MXPC)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(I10)') MXACTC
         LLOC=11
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTC,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTC
    3 FORMAT(1X,'MAXIMUM OF ',I6,
     1  ' TIME-VARIANT SPECIFIED-HEAD CELLS AT ONE TIME')
C
C3------READ AUXILIARY VARIABLES AND PRINT OPTION
      ALLOCATE (CHDAUX(20))
      NAUX=0
      IPRCHD=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            CHDAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) CHDAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY CHD VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,
     &'LISTS OF TIME-VARIANT SPECIFIED-HEAD CELLS WILL NOT BE PRINTED')
         IPRCHD = 0
         GO TO 10
      else if(line(istart:istop).eq.'NEGBND') then                      ! DLT
         inegbnd=1                                                      ! DLT
         goto 10                                                        ! DLT
      else if(line(istart:istop).eq.'INTERP') then                      ! DLT
         iinterp=1                                                      ! DLT
         goto 10                                                        ! DLT
      END IF                                                            ! DLT
      NCHDVL=5+NAUX
C
C4------ALLOCATE SPACE FOR TIME-VARIANT SPECIFIED-HEAD LIST.
      ICHDPB=MXACTC+1
      MXCHD=MXACTC+MXPC
      ALLOCATE (CHDS(NCHDVL,MXCHD))
C
C1------READ NAMED PARAMETERS.
      WRITE(IOUT,1000) NPCHD
 1000 FORMAT(1X,//1X,I5,' TIME-VARIANT SPECIFIED-HEAD PARAMETERS')
      IF(NPCHD.GT.0) THEN
        NAUX=NCHDVL-5
        LSTSUM=ICHDPB
        DO 120 K=1,NPCHD
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXCHD,IN,IOUT,IP,'CHD','CHD',1,
     &                  NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
C         ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          ENDIF
C         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 110 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,IPRCHD)
            ENDIF
            CALL ULSTRD(NLST,CHDS,LB,NCHDVL,MXCHD,0,IN,IOUT,
     &     'CHD NO.   LAYER   ROW   COL   START FACTOR      END FACTOR',
     &      CHDAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,4,5,IPRCHD)
            LB=LB+NLST
  110     CONTINUE
  120   CONTINUE
      END IF
C
C3------RETURN.
      CALL SGWF2CHD7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2CHD7RP(IN,IGRID)
C     ******************************************************************
C     READ STRES PERIOD DATA FOR CHD
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      use ulstrd_inferface                                              ! GCD
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IBOUND
      USE GWFCHDMODULE,ONLY:NCHDS,MXCHD,NCHDVL,IPRCHD,NPCHD,ICHDPB,
     1                      NNPCHD,CHDAUX,CHDS,
     1                      inegbnd, iinterp                            ! DLT
      real, dimension(:,:), allocatable :: tmplist                      ! DLT
      integer :: il,ir,ic                                               ! DLT
      real :: val                                                       ! DLT
C     ------------------------------------------------------------------
      CALL SGWF2CHD7PNT(IGRID)
C
C1------READ ITMP(FLAG TO REUSE DATA AND NUMBER OF PARAMETERS.
      IF(NPCHD.GT.0) THEN
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
C2------CALCULATE NUMBER OF AUXILIARY VALUES
      NAUX=NCHDVL-5
      IOUTU = IOUT
      IF (IPRCHD.EQ.0) IOUTU = -IOUT
C
C2------TEST ITMP
C2A-----IF ITMP<0 THEN REUSE DATA FROM LAST STRESS PERIOD
      IF(ITMP.LT.0) THEN
         call sts2nodata(in)                                             ! STS
         WRITE(IOUT,7)
    7    FORMAT(1X,/1X,'REUSING NON-PARAMETER SPECIFIED-HEAD DATA FROM',
     1     ' LAST STRESS PERIOD')
      ELSE
         call sts2data(in)                                               ! STS
         NNPCHD=ITMP
      END IF
C
C3------IF THERE ARE NEW NON-PARAMETER CHDS, READ THEM
      MXACTC=ICHDPB-1
      IF(ITMP.GT.0) THEN
         IF(NNPCHD.GT.MXACTC) THEN
            WRITE(IOUT,99) NNPCHD,MXACTC
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE CHD CELLS (',I6,
     1                     ') IS GREATER THAN MXACTC(',I6,')')
            CALL USTOP(' ')
         END IF
         CALL ULSTRD(NNPCHD,CHDS,1,NCHDVL,MXCHD,0,IN,IOUT,
     1    'CHD NO.   LAYER   ROW   COL    START HEAD        END HEAD',
     2     CHDAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,4,5,IPRCHD)
      END IF

c...      clip for aux variable < 0 in case NEGBND is specified
      if (inegbnd.gt.0) then                              ! DLT
         nchds = 0                                                      ! DLT
         do i = 1, nnpchd                                               ! DLT
            il=chds(1,i)                                                ! DLT
            ir=chds(2,i)                                                ! DLT
            ic=chds(3,i)                                                ! DLT
            if (ibound(ic,ir,il).lt.0) nchds = nchds + 1                ! DLT
         end do                                                         ! DLT
         if (nchds.gt.0) then                                           ! DLT
            allocate(tmplist(nchdvl,nchds))                             ! DLT
            j = 0                                                       ! DLT
            do i = 1, nnpchd                                            ! DLT
               il=chds(1,i)                                             ! DLT
               ir=chds(2,i)                                             ! DLT
               ic=chds(3,i)                                             ! DLT
               if (ibound(ic,ir,il).lt.0) then                          ! DLT
                  j = j + 1                                             ! DLT
                  tmplist(1:nchdvl,j) = chds(1:nchdvl,i)                ! DLT
               end if                                                   ! DLT
            end do                                                      ! DLT
            deallocate(chds)                                            ! DLT
            allocate(chds(nchdvl,nchds))                                ! DLT
            chds = tmplist                                              ! DLT
            deallocate(tmplist)                                         ! DLT
         end if                                                         ! DLT
         nnpchd = nchds                                                 ! DLT
         mxchd = nchds                                                  ! DLT
      end if                                                            ! DLT
      
c...      set average value
      if (iinterp.gt.0.and.itmp.gt.0) then                              ! DLT
         do i = 1, nchds                                                ! DLT
            val = chds(4,i) + chds(5,i)                                 ! DLT
            chds(4,i) = val                                             ! DLT
            chds(5,i) = val                                             ! DLT
          end do                                                        ! DLT
      end if                                                            ! DLT

      NCHDS=NNPCHD
C
Cx------IF THERE ARE ACTIVE CHD PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('CHD')
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'CHD',IOUTU,'CHD',CHDS,NCHDVL,MXCHD,NCHDVL,
     1             MXACTC,NCHDS,4,5,
     2    'CHD NO.   LAYER   ROW   COL    START HEAD        END HEAD',
     3            CHDAUX,5,NAUX)
   30    CONTINUE
      END IF
C
C4------PRINT # OF SPECIFIED-HEAD CELLS THIS STRESS PERIOD
      WRITE(IOUT,1) NCHDS
    1 FORMAT(1X,//1X,I6,' TIME-VARIANT SPECIFIED-HEAD CELLS')
C
C5------SET IBOUND NEGATIVE AT SPECIFIED-HEAD CELLS.
      DO 250 II=1,NCHDS
      IL=CHDS(1,II)
      IR=CHDS(2,II)
      IC=CHDS(3,II)
      IF(IBOUND(IC,IR,IL).GT.0) IBOUND(IC,IR,IL)=-IBOUND(IC,IR,IL)
      IF(IBOUND(IC,IR,IL).EQ.0) THEN
         WRITE(IOUT,6) IL,IR,IC
    6    FORMAT(1X,'CELL (',I3,',',I5,',',I5,') IS NO FLOW (IBOUND=0)',/
     1      1X,'NO-FLOW CELLS CANNOT BE CONVERTED TO SPECIFIED HEAD')
         CALL USTOP(' ')
      END IF
  250 CONTINUE
C
C8------RETURN
      CALL SGWF2CHD7PSV(IGRID)                                          ! DLT
      RETURN
      END
      SUBROUTINE GWF2CHD7AD(KPER,IGRID)
C     ******************************************************************
C     COMPUTE HEAD FOR TIME STEP AT EACH TIME-VARIANT SPECIFIED HEAD
C     CELL.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,HNEW,HOLD,PERLEN
      USE GWFBASMODULE,ONLY:PERTIM
      USE GWFCHDMODULE,ONLY:NCHDS,CHDS
C
      DOUBLE PRECISION DZERO,HB
C     ------------------------------------------------------------------
      CALL SGWF2CHD7PNT(IGRID)
      DZERO=0.
C
C1------IF NCHDS<=0 THEN THERE ARE NO TIME VARIANT SPECIFIED-HEAD CELLS.
C1------RETURN.
      IF(NCHDS.LE.0) RETURN
C
C6------INITIALIZE HNEW TO 0 AT SPECIFIED-HEAD CELLS.
      DO 50 L=1,NCHDS
      IL=CHDS(1,L)
      IR=CHDS(2,L)
      IC=CHDS(3,L)
      HNEW(IC,IR,IL)=DZERO
   50 CONTINUE
C
C2------COMPUTE PROPORTION OF STRESS PERIOD TO CENTER OF THIS TIME STEP
      IF (PERLEN(KPER).EQ.0.0) THEN
        FRAC=1.0
      ELSE
        FRAC=PERTIM/PERLEN(KPER)
      ENDIF
C
C2------PROCESS EACH ENTRY IN THE SPECIFIED-HEAD CELL LIST (CHDS)
      DO 100 L=1,NCHDS
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY
      IL=CHDS(1,L)
      IR=CHDS(2,L)
      IC=CHDS(3,L)     
C
      IF (PERLEN(KPER).EQ.0.0 .AND. CHDS(4,L).NE.CHDS(5,L)) THEN
        WRITE(IOUT,200)IL,IR,IC
 200    FORMAT(/,' ***WARNING***  FOR CHD CELL (',I3,',',I5,',',I5,
     &'), START HEAD AND END HEAD DIFFER',/,
     &' FOR A STRESS PERIOD OF ZERO LENGTH --',/,
     &' USING ENDING HEAD AS CONSTANT HEAD',
     &' (GWF2CHD7AD)',/)
      ENDIF
C5------COMPUTE HEAD AT CELL BY LINEAR INTERPOLATION.
      HB=CHDS(4,L)+(CHDS(5,L)-CHDS(4,L))*FRAC
C
C6------UPDATE THE APPROPRIATE HNEW VALUE
      HNEW(IC,IR,IL)=HNEW(IC,IR,IL)+HB
      HOLD(IC,IR,IL)=HNEW(IC,IR,IL)
  100 CONTINUE
C
C7------RETURN
      RETURN
      END
      SUBROUTINE GWF2CHD7DA(IGRID)
C  Deallocate CHD data for a grid
      USE GWFCHDMODULE
C
        DEALLOCATE(GWFCHDDAT(IGRID)%NCHDS)
        DEALLOCATE(GWFCHDDAT(IGRID)%MXCHD)
        DEALLOCATE(GWFCHDDAT(IGRID)%NCHDVL)
        DEALLOCATE(GWFCHDDAT(IGRID)%IPRCHD)
        DEALLOCATE(GWFCHDDAT(IGRID)%NPCHD)
        DEALLOCATE(GWFCHDDAT(IGRID)%ICHDPB)
        DEALLOCATE(GWFCHDDAT(IGRID)%NNPCHD)
        DEALLOCATE(GWFCHDDAT(IGRID)%CHDAUX)
        DEALLOCATE(GWFCHDDAT(IGRID)%CHDS)
        deallocate(gwfchddat(igrid)%inegbnd)                            ! DLT
        deallocate(gwfchddat(igrid)%iinterp)                            ! DLT
C
      RETURN
      END
      SUBROUTINE SGWF2CHD7PNT(IGRID)
C  Set pointers to CHD data for a grid
      USE GWFCHDMODULE
C
        NCHDS=>GWFCHDDAT(IGRID)%NCHDS
        MXCHD=>GWFCHDDAT(IGRID)%MXCHD
        NCHDVL=>GWFCHDDAT(IGRID)%NCHDVL
        IPRCHD=>GWFCHDDAT(IGRID)%IPRCHD
        NPCHD=>GWFCHDDAT(IGRID)%NPCHD
        ICHDPB=>GWFCHDDAT(IGRID)%ICHDPB
        NNPCHD=>GWFCHDDAT(IGRID)%NNPCHD
        CHDAUX=>GWFCHDDAT(IGRID)%CHDAUX
        CHDS=>GWFCHDDAT(IGRID)%CHDS
        inegbnd=>gwfchddat(igrid)%inegbnd                               ! DLT
        iinterp=>gwfchddat(igrid)%iinterp                               ! DLT
C
      RETURN
      END
      SUBROUTINE SGWF2CHD7PSV(IGRID)
C  Save pointers to CHD data for a grid
      USE GWFCHDMODULE
C
        GWFCHDDAT(IGRID)%NCHDS=>NCHDS
        GWFCHDDAT(IGRID)%MXCHD=>MXCHD
        GWFCHDDAT(IGRID)%NCHDVL=>NCHDVL
        GWFCHDDAT(IGRID)%IPRCHD=>IPRCHD
        GWFCHDDAT(IGRID)%NPCHD=>NPCHD
        GWFCHDDAT(IGRID)%ICHDPB=>ICHDPB
        GWFCHDDAT(IGRID)%NNPCHD=>NNPCHD
        GWFCHDDAT(IGRID)%CHDAUX=>CHDAUX
        GWFCHDDAT(IGRID)%CHDS=>CHDS
        gwfchddat(igrid)%inegbnd=>inegbnd                               ! DLT
        gwfchddat(igrid)%iinterp=>iinterp                               ! DLT
C
      RETURN
      END
