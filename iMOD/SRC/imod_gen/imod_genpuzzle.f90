!!  Copyright (C) Stichting Deltares, 2005-2014.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.

MODULE MOD_GEN2GEN_PUZZLE

USE MOD_UTL, ONLY : ITOS,UTL_GETUNIT,UTL_CAP
USE MOD_OSD, ONLY : OSD_OPEN

CHARACTER(LEN=256) :: GENFNAME

INTEGER,PRIVATE :: NP,ND,NBRCH,DIMXY 
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:),PRIVATE :: PNTX,PNTY,PNTX_BU,PNTY_BU
INTEGER,ALLOCATABLE,DIMENSION(:,:),PRIVATE :: IP,IPNT

INTEGER,PRIVATE :: IU
DOUBLE PRECISION,DIMENSION(2,2),PRIVATE :: XC,YC
DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:),PRIVATE :: DUMX,DUMY

REAL :: SBX,SBY,SBMINX,SBMAXY,SBMAXX,SBMINY
INTEGER :: NSBX,NSBY

CONTAINS

 !##=====================================================================
 SUBROUTINE PUZZLEMAIN()
 !##=====================================================================
 IMPLICIT NONE
 INTEGER :: I

 I=1
 CALL PUZZLE_SPLIT(GENFNAME,PUZZLE_GETFNAME(GENFNAME,I))
 CALL PUZZLE_JOIN(PUZZLE_GETFNAME(GENFNAME,I),PUZZLE_GETFNAME(GENFNAME,I+1))
 I=I+1
 CALL PUZZLE_CONNECTED(PUZZLE_GETFNAME(GENFNAME,I),PUZZLE_GETFNAME(GENFNAME,I+1))
 GENFNAME=PUZZLE_GETFNAME(GENFNAME,I+1)
 
 END SUBROUTINE PUZZLEMAIN

 !##=====================================================================
 SUBROUTINE PUZZLE_JOIN(FNAME_IN,FNAME_OUT)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME_IN,FNAME_OUT
 INTEGER :: I,J,I1,I2,ND,I3

 WRITE(*,'(/1X,A)') 'Start joining segments ...'
 CALL PUZZLE_READ(FNAME_IN)

 !## phase A: find locations to bracket
 PNTX_BU=PNTX; PNTY_BU=PNTY

 !## sort x-values indices
 CALL SORTEMD(1,NP,PNTX,3,IPNT(:,1),IPNT(:,2),IPNT(:,3),(/0.0/),(/0.0/),(/0.0/),(/0.0/))
 !## adjust pnty based on sorted vector ipnt(:,1)
 DO I=1,NP; PNTY(I)=PNTY_BU(IPNT(I,1)); ENDDO

 !## get number of duplicates within segments, these could be joined eventually
 IPNT(:,4)=0; I1=1
 DO I2=1,NP-1
  IF(PNTX(I2+1).EQ.PNTX(I2))THEN
   DO J=I1,I2+1
    ND=0
    DO I=I1,I2+1
     IF(I.NE.J.AND.PNTY(J).EQ.PNTY(I))THEN
      ND=ND+1
      I3=IPNT(I,2)
     ENDIF
    END DO
    !## possible connection point
    IF(ND.EQ.1)THEN
!     IPNT(I,4)=I3 !## branch number
     IPNT(J,4)=I3 !## branch number
    ELSE
     IPNT(J,4)=0  !## no connection to another branch possible
    ENDIF
   ENDDO
   I1=I2+1
  ENDIF
 ENDDO

 !## resort superblock indices
 CALL SORTEMI(1,NP,IPNT(:,1),3,IPNT(:,2),IPNT(:,3),IPNT(:,4),(/0.0/),(/0.0/),(/0.0/),(/0.0/))
 !## adjust pnty based on sorted vector ipnt(:,1)
 DO I=1,NP; PNTX(I)=PNTX_BU(IPNT(I,1)); PNTY(I)=PNTY_BU(IPNT(I,1)); END DO

 !## copy connections to ip()
 DO I=1,NP
  IF(IPNT(I,4).NE.0)THEN
   !## start connection
   IF(IPNT(I,3).EQ.1)THEN
    IP(IPNT(I,2),3)=IPNT(I,4)
   !## end connection
   ELSE
    IP(IPNT(I,2),4)=IPNT(I,4)
   ENDIF
  ENDIF
 END DO

 CALL PUZZLE()

 IPNT(:,4)=0
 CALL PUZZLE_SAVE(FNAME_OUT,0)

 END SUBROUTINE PUZZLE_JOIN

 !##=====================================================================
 SUBROUTINE PUZZLE_SPLIT(FNAME_IN,FNAME_OUT)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME_IN,FNAME_OUT
 INTEGER :: I,J,I1,I2,ND

 WRITE(*,'(/1X,A)') 'Start splitting segments ...'
 CALL PUZZLE_READ(FNAME_IN)

 !## phase A: find locations to bracket
 PNTX_BU=PNTX; PNTY_BU=PNTY

 !## sort x-values indices (real sort)
 CALL SORTEMD(1,NP,PNTX,3,IPNT(:,1),IPNT(:,2),IPNT(:,3),(/0.0/),(/0.0/),(/0.0/),(/0.0/))
 !## adjust pnty based on sorted vector ipnt(:,1)
 DO I=1,NP; PNTY(I)=PNTY_BU(IPNT(I,1)); END DO

 !## get number of duplicates in between segments, these should be splitted in the end
 IPNT(:,4)=0; I1=1
 DO I2=1,NP-1
  IF(PNTX(I2+1).EQ.PNTX(I2))THEN
   !## add number of connections to previous identical points
   DO J=I1,I2
    ND=0
    DO I=I1,I2
     !## one duplicate found itself only, so leave it
     IF(I.NE.J.AND.PNTY(J).EQ.PNTY(I))ND=ND+1
    END DO
    IPNT(J,4)=ND
   ENDDO
   I1=I2+1
  ENDIF
 END DO

 !## resort superblock indices
 CALL SORTEMI(1,NP,IPNT(:,1),3,IPNT(:,2),IPNT(:,3),IPNT(:,4),(/0.0/),(/0.0/),(/0.0/),(/0.0/))
 !## adjust pnty based on sorted vector ipnt(:,1)
 DO I=1,NP; PNTX(I)=PNTX_BU(IPNT(I,1)); PNTY(I)=PNTY_BU(IPNT(I,1)); END DO

 CALL PUZZLE_SAVE(FNAME_OUT,0)

 END SUBROUTINE PUZZLE_SPLIT

 !##=====================================================================
 SUBROUTINE PUZZLE_CONNECTED(FNAME_IN,FNAME_OUT)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME_IN,FNAME_OUT
 INTEGER :: I,J,I1,I2,ND

 WRITE(*,'(/1X,A)') 'Start connecting segments ...'
 CALL PUZZLE_READ(FNAME_IN)

 !## phase A: find locations to bracket
 PNTX_BU=PNTX
 PNTY_BU=PNTY

 !## sort x-values indices
 CALL SORTEMD(1,NP,PNTX,3,IPNT(:,1),IPNT(:,2),IPNT(:,3),(/0.0/),(/0.0/),(/0.0/),(/0.0/))
 !## adjust pnty based on sorted vector ipnt(:,1)
 DO I=1,NP
  PNTY(I)=PNTY_BU(IPNT(I,1))
 END DO

 !## get number of duplicates within segments, these should be connected
 IPNT(:,4)=0
 IPNT(:,5)=0
 I1=1
 DO I2=1,NP-1
  IF(PNTX(I2+1).NE.PNTX(I2))THEN
   DO J=I1,I2
    IF(IPNT(J,4).EQ.0)THEN
     ND=0
     DO I=I1,I2
      IF(PNTY(J).EQ.PNTY(I))THEN
       ND=ND+1
       IPNT(I,4)=-IPNT(J,2)
       IPNT(I,5)= IPNT(J,3)
      ENDIF
     END DO

     DO I=I1,I2
      IF(ND.EQ.1.AND.IPNT(I,4).LT.0)THEN
       IPNT(I,4)=0
       IPNT(I,5)=0
      ENDIF
      IF(ND.GT.1.AND.IPNT(I,4).LT.0)IPNT(I,4)=ABS(IPNT(I,4))
     END DO

    ENDIF
   ENDDO
   I1=I2+1
  ENDIF
 END DO

 !## resort superblock indices
 CALL SORTEMI(1,NP,IPNT(:,1),4,IPNT(:,2),IPNT(:,3),IPNT(:,4),IPNT(:,5),(/0.0/),(/0.0/),(/0.0/))
 !## adjust pnty based on sorted vector ipnt(:,1)
 DO I=1,NP
  PNTX(I)=PNTX_BU(IPNT(I,1))
  PNTY(I)=PNTY_BU(IPNT(I,1))
 END DO

 CALL PUZZLE_SAVE(FNAME_OUT,1)

 END SUBROUTINE PUZZLE_CONNECTED

 !##=====================================================================
 SUBROUTINE PUZZLE_SAVE(FNAME_OUT,ICON)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME_OUT
 INTEGER,INTENT(IN) :: ICON
 CHARACTER(LEN=50) :: LINE
 INTEGER :: I,J,ND,I1,I2

 OPEN(IU,FILE=FNAME_OUT,STATUS='REPLACE',ACTION='WRITE')
 ND=0
 DO I=1,NBRCH
  IF(IP(I,1).GT.0)THEN
   ND=ND+1
   IF(ICON.EQ.0)THEN
    LINE=TRIM(ITOS(ND))
   ELSE
    I1=IP(I,1)
    I2=IP(I,2)
    LINE=TRIM(ITOS(ND))
    !## no connection at the beginning
    IF(IPNT(I1,4).EQ.0)THEN
     LINE=TRIM(LINE)//',0,0'
    ELSE
     LINE=TRIM(LINE)//','//TRIM(ITOS(IPNT(I1,4)))//','//TRIM(ITOS(IPNT(I1,5)))
    ENDIF
    !## no connection at the end
    IF(IPNT(I2,4).EQ.0)THEN
     LINE=TRIM(LINE)//',0,0'
    ELSE
     LINE=TRIM(LINE)//','//TRIM(ITOS(IPNT(I2,4)))//','//TRIM(ITOS(IPNT(I2,5)))
    ENDIF
   ENDIF
   WRITE(IU,'(A)') TRIM(LINE)
   DO J=IP(I,1),IP(I,2)
    IF(J.GT.IP(I,1).AND.J.LT.IP(I,2))THEN
     !## start new branch, ipnt(j,4).gt.0
     IF(ICON.EQ.0.AND.IPNT(J,4).GT.0)THEN
      WRITE(IU,*) PNTX(J),PNTY(J)
      WRITE(IU,'(A)') 'END'
      ND=ND+1
      LINE=TRIM(ITOS(ND))
      WRITE(IU,'(A)') TRIM(LINE)
      WRITE(IU,*) PNTX(J),PNTY(J)
     ELSE
      IF(PNTX(J).NE.PNTX(J-1).OR.PNTY(J).NE.PNTY(J-1))WRITE(IU,*) PNTX(J),PNTY(J)
     ENDIF
    ELSE
     IF(J.EQ.IP(I,1))THEN
      WRITE(IU,*) PNTX(J),PNTY(J)
     ELSE
      IF(PNTX(J).NE.PNTX(J-1).OR.PNTY(J).NE.PNTY(J-1))WRITE(IU,*) PNTX(J),PNTY(J)
     ENDIF
    ENDIF
   ENDDO
   WRITE(IU,'(A)') 'END'
  ENDIF
 END DO
 WRITE(IU,'(A)') 'END'
 CLOSE(IU)

 WRITE(*,'(1X,A,I10,A)') 'Wrote   ',ND,' segments to '//TRIM(FNAME_OUT)//' ...'

 END SUBROUTINE PUZZLE_SAVE

 !##=====================================================================
 CHARACTER(LEN=256) FUNCTION PUZZLE_GETFNAME(FNAME,IPOS)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPOS
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I

 I=INDEX(FNAME,'\',.TRUE.)
 IF(I.EQ.0)THEN
  PUZZLE_GETFNAME=TRIM(ITOS(IPOS))//'_'//TRIM(FNAME)
 ELSE
  PUZZLE_GETFNAME=FNAME(:I-1)//'\'//TRIM(ITOS(IPOS))//'_'//TRIM(FNAME(I+1:))
 ENDIF

 END FUNCTION PUZZLE_GETFNAME

 !##=====================================================================
 SUBROUTINE PUZZLE()
 !##=====================================================================
 IMPLICIT NONE
 INTEGER :: IG,JG,I,IGPREV

 DIMXY=0

 DO I=1,NBRCH
  !## to be joined
  IG=I
  !## start/end branch of segment not yet visited location ig
  IF(MINVAL(IP(IG,1:2)).GT.0.AND. &
     MINVAL(IP(IG,3:4)).EQ.0.AND. &
     MAXVAL(IP(IG,3:4)).GT.0)THEN

   !## get entire segment at once
   IGPREV=0
   JG=IG
   DO

    !## get start/end location current group to be tested
    !## start point
    XC(1,1)=PNTX(IP(IG,1)); YC(1,1)=PNTY(IP(IG,1))
    !## end   point
    XC(1,2)=PNTX(IP(IG,2)); YC(1,2)=PNTY(IP(IG,2))

    !## get proper reference
    IF(IP(JG,3).NE.IGPREV)THEN
     IGPREV=JG
     JG=IP(JG,3)
    !## do not reference back
    ELSE
     IGPREV=JG
     JG=IP(JG,4)
    ENDIF
    !## finished
    IF(JG.EQ.0)EXIT

    !## get start/end location current group to be added?
    XC(2,1)=PNTX(IP(JG,1)); YC(2,1)=PNTY(IP(JG,1))
    XC(2,2)=PNTX(IP(JG,2)); YC(2,2)=PNTY(IP(JG,2))

    !## join element ig and jg?
    CALL PUZZLEPUT(PUZZLETEST(),IG,JG)

   ENDDO

  ENDIF
  WRITE(6,'(A,F10.2)') '+Progress ',REAL(I*100)/REAL(NBRCH)    !FR 20131007
 END DO

 IF(ALLOCATED(DUMX))DEALLOCATE(DUMX)
 IF(ALLOCATED(DUMY))DEALLOCATE(DUMY)

 END SUBROUTINE PUZZLE

 !##=====================================================================
 SUBROUTINE PUZZLEPUT(IPOS,IG,JG)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPOS,IG,JG
 INTEGER :: I

 IF(IPOS.EQ.0)RETURN

 !## turn coordinates - first part of list-of-coordinates
 SELECT CASE (IPOS)
  CASE (2)
   I=JG
   CALL PUZZLEMIRROR(I)
  CASE (3)
   I=IG
   CALL PUZZLEMIRROR(I)
  CASE (4)
   I=IG
   CALL PUZZLEMIRROR(I)
   I=JG
   CALL PUZZLEMIRROR(I)
 END SELECT

 !## join part-of-coordinates from position jg next to position jg
 CALL PUZZLESHIFT(IG,JG)

 END SUBROUTINE PUZZLEPUT

 !##=====================================================================
 SUBROUTINE PUZZLESHIFT(IG,JG)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IG,JG
 INTEGER :: DP,I,J,II

 DP=   IP(IG,2)+1-IP(IG,1)
 DP=DP+IP(JG,2)+1-IP(JG,1)

 IF(DP.GT.DIMXY)THEN
  DIMXY=DP
  IF(ALLOCATED(DUMX))DEALLOCATE(DUMX)
  IF(ALLOCATED(DUMY))DEALLOCATE(DUMY)
  ALLOCATE(DUMX(DIMXY),DUMY(DIMXY))
 ENDIF

 !## copy ALL coordinates
 J=0
 DO I=IP(IG,1),IP(IG,2)
  J      =J+1
  DUMX(J)=PNTX(I)
  DUMY(J)=PNTY(I)
 ENDDO
 DO I=IP(JG,1),IP(JG,2)
  J      =J+1
  DUMX(J)=PNTX(I)
  DUMY(J)=PNTY(I)
 ENDDO

 IF(IG.LE.JG)THEN

  !## shift coordinates from
  DP=IP(JG,2)+1-IP(JG,1)
  I =IP(IG,2)+DP+1
  J =IP(JG,2)!+DP!1)+DP

  !## shift backwards --> nbrch direction
  DO II=J,I,-1
   PNTX(II)=PNTX(II-DP)
   PNTY(II)=PNTY(II-DP)
  ENDDO

  !## add # coordinates to current group sg
  IP(IG,2)=IP(IG,2)+DP

 ELSE

  !## shift coordinates from
  DP=IP(JG,2)+1-IP(JG,1)
  I =IP(JG,1)
  J =IP(IG,1)-DP-1 !JG,1)+DP

  !## shift forwards <-- nbrch direction
  DO II=I,J
   PNTX(II)=PNTX(II+DP)
   PNTY(II)=PNTY(II+DP)
  ENDDO

  !## add # coordinates to current group sg
  IP(IG,1)=IP(IG,1)-DP

 ENDIF

 !## put coordinates
 J=0
 DO I=IP(IG,1),IP(IG,2)
  J      =J+1
  PNTX(I)=DUMX(J)
  PNTY(I)=DUMY(J)
 ENDDO

 !## force different pointers between [sg] and [eg]
 IF(IG.LE.JG)THEN
  DO I=IG+1,JG-1
   IF(IP(I,1).NE.0)IP(I,1)=IP(I,1)+DP
   IF(IP(I,2).NE.0)IP(I,2)=IP(I,2)+DP
  END DO
 ELSE
  DO I=JG+1,IG-1
   IF(IP(I,1).NE.0)IP(I,1)=IP(I,1)-DP
   IF(IP(I,2).NE.0)IP(I,2)=IP(I,2)-DP
  END DO
 ENDIF

 !## "remove" current segment from ip-list
 IP(JG,1)=0; IP(JG,2)=0

 END SUBROUTINE PUZZLESHIFT

 !##=====================================================================
 SUBROUTINE PUZZLEMIRROR(IJG)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IJG
 DOUBLE PRECISION :: XC,YC
 INTEGER :: I,J,DP

 DP=IP(IJG,2)+1-IP(IJG,1)
 DP=(DP/2)-1

 J =IP(IJG,2)+1
 DO I=IP(IJG,1),IP(IJG,1)+DP
  J      =J-1
  XC     =PNTX(I)
  YC     =PNTY(I)
  PNTX(I)=PNTX(J)
  PNTY(I)=PNTY(J)
  PNTX(J)=XC
  PNTY(J)=YC
 ENDDO

 END SUBROUTINE PUZZLEMIRROR

 !##=====================================================================
 INTEGER FUNCTION PUZZLETEST()
 !##=====================================================================
 IMPLICIT NONE

 !#try first/first coordinate
 PUZZLETEST=3
 IF(XC(1,1).EQ.XC(2,1).AND.YC(1,1).EQ.YC(2,1))RETURN
 !#try first/last coordinate
 PUZZLETEST=4
 IF(XC(1,1).EQ.XC(2,2).AND.YC(1,1).EQ.YC(2,2))RETURN
 !#try last/first coordinate
 PUZZLETEST=1
 IF(XC(1,2).EQ.XC(2,1).AND.YC(1,2).EQ.YC(2,1))RETURN
 !#try last/last coordinate
 PUZZLETEST=2
 IF(XC(1,2).EQ.XC(2,2).AND.YC(1,2).EQ.YC(2,2))RETURN

 !##failed
 PUZZLETEST=0

 END FUNCTION PUZZLETEST

 !##=====================================================================
 SUBROUTINE PUZZLE_READ(FNAME)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME

 !## allocate memory
 IF(ALLOCATED(PNTX_BU))DEALLOCATE(PNTX_BU)
 IF(ALLOCATED(PNTY_BU))DEALLOCATE(PNTY_BU)
 IF(ALLOCATED(PNTX))DEALLOCATE(PNTX)
 IF(ALLOCATED(PNTY))DEALLOCATE(PNTY)
 IF(ALLOCATED(IPNT))DEALLOCATE(IPNT)
 IF(ALLOCATED(IP))  DEALLOCATE(IP)
 ALLOCATE(PNTX(0:1),PNTY(0:1),IPNT(0:1,5),IP(0:1,3))
 !## read number of gen-elements
 CALL PUZZLE_READIT(0,FNAME)
 DEALLOCATE(PNTX,PNTY,IPNT,IP)
 !## read entire file and initiate groups
 ALLOCATE(PNTX(NP),PNTY(NP),IPNT(NP,5),IP(NP,6),PNTX_BU(NP),PNTY_BU(NP))
 IP=0
 CALL PUZZLE_READIT(1,FNAME)

 END SUBROUTINE PUZZLE_READ

 !##=====================================================================
 SUBROUTINE PUZZLE_READIT(ISTEP,FNAME)
 !##=====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER, INTENT(IN) :: ISTEP
 INTEGER :: IOS,I,ID,NPB
 CHARACTER(LEN=52) :: CID
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)STOP 'error opening file'

 I =0
 NP=0

 IF(ISTEP.EQ.1)WRITE(*,'(1X,A,I10,A)') 'Reading ',NBRCH,' segments from '//TRIM(FNAME)//' ...'

 NBRCH=0
 DO
  READ(IU,*,IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
  IF(TRIM(UTL_CAP(CID,'U')).EQ.'END')EXIT
  !## increase groups
  NBRCH=NBRCH+1
  READ(CID,*,IOSTAT=IOS) ID; IF(IOS.NE.0)ID=NBRCH
  IF(ISTEP.EQ.1)IP(NBRCH,1)=I+1
  NPB=0
  DO
   READ(IU,*,IOSTAT=IOS) XC(1,1),YC(1,1)
   IF(IOS.NE.0)EXIT
   NP     =NP+1
   I      =I+ISTEP
   PNTX(I)=XC(1,1)
   PNTY(I)=YC(1,1)
   !## store position of segment
   NPB      =NPB+1
   IPNT(I,1)=NP
   IPNT(I,2)=NBRCH
   IPNT(I,3)=NPB
  ENDDO
  IF(NPB.LE.1)THEN
   WRITE(*,*) 'ERROR: number of points in segment is le 1'
   WRITE(*,*) NPB,' '//TRIM(CID)
   STOP
  ENDIF
  IF(ISTEP.EQ.1)IP(NBRCH,2)=I
 ENDDO

 CLOSE(IU)

 END SUBROUTINE PUZZLE_READIT

END MODULE MOD_GEN2GEN_PUZZLE

