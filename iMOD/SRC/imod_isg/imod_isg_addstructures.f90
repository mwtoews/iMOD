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
MODULE MOD_ISG_STRUCTURES

USE WINTERACTER
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,RTOS,JD,UTL_JDATETOIDATE,UTL_IDATETOJDATE
USE MOD_ISG_PAR 
USE MOD_ISG_UTL, ONLY : UTL_GETUNITSISG,ISGREAD,ISGGETPOSID,ISGMEMORYIST,ISGMEMORYDATIST
USE MOD_OSD, ONLY : OSD_OPEN

CHARACTER(LEN=256) :: IPFFNAME,LOGFNAME,LINE
INTEGER :: IX,IY,ID,IO,IS,IW,IBATCH,SY,EY
REAL :: MAXDIST
CHARACTER(LEN=5) :: CSPS,CEPS,CSPW,CEPW
CHARACTER(LEN=10) :: CMD

!functions in module
PRIVATE :: GETSTRINGVALUE,ISGSTUWEN_CHECKID,GETDATE,ORIENT

REAL,PRIVATE,PARAMETER :: R2G=360.0/(2.0*3.1415)  !1 rad = 57.15 degrees
REAL,PRIVATE,PARAMETER :: HNODATA=-999.99
INTEGER,PRIVATE :: IUIPF,NCOLIPF,NROWIPF,IOS,IASS,NY,NIP,JDS,IDMD,IMMD,IYMD
INTEGER,PRIVATE,ALLOCATABLE,DIMENSION(:,:) :: IP
REAL,PRIVATE :: XC,YC,WP,ZP,OR,ANGL,DIST,DORTHO
CHARACTER(LEN=MAXLEN),PRIVATE :: CI
CHARACTER(LEN=3),PRIVATE :: TXT

CONTAINS

 !###===============================================================================
 LOGICAL FUNCTION ISG_ADDSTRUCTURES()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IWIN

 ISG_ADDSTRUCTURES=.FALSE.

 !#create logfile
 IULOG=UTL_GETUNIT(); CALL OSD_OPEN(IULOG,FILE=LOGFNAME,STATUS='UNKNOWN',IOSTAT=IOS,ACTION='WRITE')
 IF(IOS.NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not create logfile: '//CHAR(13)// &
    TRIM(LOGFNAME),'Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'iMOD can not create logfile: '//TRIM(LOGFNAME)
  RETURN
 ENDIF

 !#read isg file
 IF(IBATCH.EQ.1)THEN

  NISGFILES=1; IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU); ALLOCATE(ISGIU(MAXFILES,NISGFILES))

  CALL UTL_GETUNITSISG(ISGIU(:,1),ISGFNAME,'OLD')
  IF(MINVAL(ISGIU(:,1)).LE.0)THEN
   DO I=1,NISGFILES; DO J=1,MAXFILES
    IF(ISGIU(J,I).GT.0)CLOSE(ISGIU(J,I))
   END DO; END DO
   WRITE(*,*) 'Can not allocate files for writing ! '; RETURN
  ENDIF
  WRITE(*,'(/1X,A/)') 'Reading '//TRIM(ISGFNAME)//'...'
  CALL ISGREAD()
  IF(NISG.LE.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not read any element from:'//CHAR(13)// &
         TRIM(ISGFNAME),'Error')
   IF(IBATCH.EQ.1)WRITE(*,'(/1X,A/)') 'iMOD can not read any element from: '//TRIM(ISGFNAME)
   RETURN
  ENDIF
 ENDIF

 CALL ISGSTUWEN_DATE()
 IF(NY.LE.0)RETURN

 !## apply addition of structures to isg
 CALL ISGSTUWEN_ADD()

 IF(ALLOCATED(IP))DEALLOCATE(IP)

! IF(LISG)THEN
  IF(IBATCH.EQ.0)THEN
   CALL WINDOWOPENCHILD(IWIN,FLAGS=SYSMENUON,WIDTH=1000,HEIGHT=500)
   CALL WINDOWSELECT(IWIN)
   IULOG=UTL_GETUNIT()
   CALL OSD_OPEN(IULOG,FILE=LOGFNAME,STATUS='OLD',IOSTAT=I)
   IF(I.NE.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not view the created file: '//CHAR(13)// &
     TRIM(LOGFNAME)//'.'//CHAR(13)//'It is probably opened allready in another application','Error')
   ELSE
    CLOSE(IULOG)
    CALL WEDITFILE(LOGFNAME,ITYPE=MODAL,IDMENU=0,IFLAGS=NOTOOLBAR+VIEWONLY+WORDWRAP+NOFILENEWOPEN+NOFILESAVEAS,&
                   IFONT=4,ISIZE=10)
   ENDIF
  ELSE
   WRITE(*,*)
   WRITE(*,*) 'Succesfully completed ISG Stuwen, results written in:'
   WRITE(*,*) TRIM(LOGFNAME)
  ENDIF
! ENDIF

 ISG_ADDSTRUCTURES=.TRUE.
 
 END FUNCTION ISG_ADDSTRUCTURES

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_DATE()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 NY=EY-SY+1
 IF(NY.LE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Number of years is less than 1','Error')
  IF(IBATCH.EQ.1)WRITE(*,'(/1X,A/)') 'Number of years is less than 1'
  RETURN
 ENDIF

 !#start julian date
 JDS=JD(SY,1,1)

 NIP=NY*2

 !## start julian/end julian/period-id
 IF(ALLOCATED(IP))DEALLOCATE(IP); ALLOCATE(IP(NIP,3))   

 !## put summer/winter in vector ip
 J=0
 DO I=SY,EY
  J=J+1
  CALL ISGSTUWEN_FILLIP(GETDATE(CSPS,1),GETDATE(CSPS,2),GETDATE(CEPS,1),GETDATE(CEPS,2),I,J,1)
  J=J+1
  CALL ISGSTUWEN_FILLIP(GETDATE(CSPW,1),GETDATE(CSPW,2),GETDATE(CEPW,1),GETDATE(CEPW,2),I,J,2)
 ENDDO

 NIP=J

 I   =INDEX(CMD,'-',.TRUE.)
 IDMD=GETDATE(CMD(:I-1),1)
 IMMD=GETDATE(CMD(:I-1),2)
 I   =INDEX(CMD,'-')
 IYMD=GETDATE(CMD(I+1:),2)

 IF(MINVAL(IP(:,1)).LE.0)NY=0

 END SUBROUTINE ISGSTUWEN_DATE

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_FILLIP(ID1,IM1,ID2,IM2,IYEAR,JP,IPERIOD)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID1,ID2,IM1,IM2,IYEAR,JP,IPERIOD
 INTEGER :: IY1,IY2

 IY1=IYEAR; IY2=IY1

 IP(JP,1)=JD(IY1,IM1,ID1)-JDS
 !#take next year if im2.le.im1
 IF(IM2.LT.IM1)THEN
  IY2=IY1+1
 ELSEIF(IM2.EQ.IM1)THEN
  IF(ID2.LE.ID1)IY2=IY1+1
 ENDIF
 IP(JP,2)=JD(IY2,IM2,ID2)-JDS
 IP(JP,3)=IPERIOD

 END SUBROUTINE ISGSTUWEN_FILLIP

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_ADD()
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=MAXLEN),ALLOCATABLE,DIMENSION(:) :: STRING
 INTEGER :: I,J,N,IISG,IIST,ISTW,IOKAY
 REAL :: P,H,SP

 !## read/write header of ipf
 CALL ISGSTUWEN_READIPF()

 !## allocate memory
 IF(ALLOCATED(STRING))DEALLOCATE(STRING); ALLOCATE(STRING(NCOLIPF))

 IF(IBATCH.EQ.1)WRITE(*,'(1X,A/)') 'Busy adding structures ...'

 !## apply application for each line in ipf
 DO J=1,NROWIPF
  STRING=''
  READ(IUIPF,*,IOSTAT=IOS) (STRING(I),I=1,NCOLIPF)

  IF(IOS.NE.0)EXIT
  
  !##put variables into reals/characters
  IF(GETSTRINGVALUE(STRING(IX),XC).AND. &
     GETSTRINGVALUE(STRING(IY),YC).AND. &
     GETSTRINGVALUE(STRING(IS),ZP).AND. &
     GETSTRINGVALUE(STRING(IW),WP).AND. &
     GETSTRINGVALUE(STRING(IO),OR))THEN
   CI=STRING(ID); IF(LEN_TRIM(CI).EQ.0)CI='Structure_Added_'//TRIM(ITOS(J))
   
!   !## handmatig/vast
!   IF(TRIM(STRING(5)).EQ.'H'.OR.TRIM(STRING(5)).EQ.'V'.OR. &
!      TRIM(STRING(5)).EQ.'h'.OR.TRIM(STRING(5)).EQ.'v')THEN
!    ZP=ZP+0.05
!    WP=WP+0.15
!   ENDIF
   
   !## check whether current structure id allready exists within isg, get distance too!
   IF(ISGSTUWEN_CHECKID(IISG,IIST))THEN
    !## compute x/y coordinates and angle for current structure
    CALL ISGSTUWEN_COMPUTEXY(IISG,IIST)
   ELSE
    !## compute nearest location for current structure
    CALL ISGSTUWEN_INTERSECT(IISG)
    IIST=0
   ENDIF

   !## found proper segment
   IF(IISG.GT.0.AND.IISG.LE.NISG)THEN

    IOKAY=1

    WRITE(STRING(IX),'(F12.2)') ISGX
    WRITE(STRING(IY),'(F12.2)') ISGY

    !## compare angle and or
    IF(OR.GT.HNODATA)THEN
     WRITE(STRING(IO),'(F12.2)') ORIENT(OR)
     !## do something with comparison with given and computed angle
     IF(ABS(ORIENT(OR)-ORIENT(ANGL)).GT.90.0)THEN
      !## turn around push direction?
      IOKAY=-1
     ENDIF
    ENDIF

    !## number of records to be put in ist(.) initially
    N=NIP*2

    !## find position
    IF(IIST.EQ.0)THEN
     !## add structure inside isg, dist=()%dist, ()%name=ci
     !## get nearest id and record-position
     ISELISG=IISG
     CALL ISGGETPOSID(DIST,IIST,3)

     !## increase memory for ist(.), add one structure
     CALL ISGMEMORYIST(1,IISG,IIST)

     IST(IIST)%N    = 0
     IST(IIST)%IREF = NDIST+1
     IST(IIST)%DIST = DIST
     IST(IIST)%CNAME= CI

    ELSE
     IST(IIST)%DIST = DIST
     N              =(N-IST(IIST)%N)
     IOKAY          = 2
    ENDIF

    !## get mean waterleveldown based upon calc. point after and before current structrue
    CALL ISGSTUWEN_GETMEANDOWNLEVEL(IISG,H)

    !## increase memory
    CALL ISGMEMORYDATIST(N,IIST,ISTW)

    ISTW=ISTW-1

    DO I=1,NIP
     IF(IP(I,3).EQ.1)P=ZP  !summer (first period)
     IF(IP(I,3).EQ.2)P=WP  !winter (second period)

     !## start of structure
     ISTW=ISTW+1
     DATIST(ISTW)%IDATE    =UTL_JDATETOIDATE(JDS+IP(I,1))
     DATIST(ISTW)%WLVL_UP  =MAX(H,P)
     DATIST(ISTW)%WLVL_DOWN=H

     SP=MAX(H,P)
     IF(I.LT.NIP)THEN
      IF(IP(I+1,1)-IP(I,2).GT.1)SP=H
     ENDIF

     !## end of structure
     ISTW=ISTW+1
     DATIST(ISTW)%IDATE    =UTL_JDATETOIDATE(JDS+IP(I,2))
     DATIST(ISTW)%WLVL_UP  =SP !H
     DATIST(ISTW)%WLVL_DOWN=H

    END DO

   ELSE
    IOKAY=0
   ENDIF
  ELSE
   IOKAY=-2; DORTHO=-999.99; ANGL=-999.99
  ENDIF
  
  !## write results
  WRITE(LINE,*) ('"'//TRIM(ADJUSTL(STRING(I)))//'",',I=1,NCOLIPF)
  LINE=TRIM(LINE)//'"'//TRIM(ITOS(IOKAY))//'"'
  LINE=TRIM(LINE)//',"'//TRIM(RTOS(DORTHO,'F',2))//'"'
  LINE=TRIM(LINE)//',"'//TRIM(RTOS(ANGL,'F',2))//'"'
  WRITE(IULOG,'(A)') TRIM(LINE)

 END DO

 CLOSE(IUIPF); CLOSE(IULOG)

 !## deallocate memory
 IF(ALLOCATED(STRING))DEALLOCATE(STRING)

 END SUBROUTINE ISGSTUWEN_ADD

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_READIPF()
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=MAXLEN) :: STRING
 INTEGER :: I

 IUIPF=UTL_GETUNIT()
 CALL OSD_OPEN(IUIPF,FILE=IPFFNAME,STATUS='OLD',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open file:'//CHAR(13)//TRIM(IPFFNAME),'Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'iMOD can not open file: '//TRIM(IPFFNAME)
  RETURN
 ENDIF
 READ(IUIPF,*) NROWIPF
 READ(IUIPF,*) NCOLIPF
 WRITE(IULOG,*) NROWIPF
 WRITE(IULOG,*) NCOLIPF+3

 DO I=1,NCOLIPF
  READ(IUIPF,*) STRING
  WRITE(IULOG,*) TRIM(STRING)
 ENDDO
 WRITE(IULOG,*) 'ADDED'
 WRITE(IULOG,*) 'DISTANCE'
 WRITE(IULOG,*) 'ANGLE(flow-direction)'
 READ(IUIPF,*) IASS,TXT
 WRITE(IULOG,*) IASS,',"'//TXT//'"'

 !##less columns than needed
 IF(MAX(IX,IY,ID,IO,IS,IW).GT.NCOLIPF)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'There are not enough columns within:' &
                  //CHAR(13)//TRIM(IPFFNAME),'Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'There are nog enough columns within: '//TRIM(IPFFNAME)
  RETURN
 ENDIF

 END SUBROUTINE ISGSTUWEN_READIPF

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_INTERSECT(IISG)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IISG
 INTEGER :: I,J,K,ISTATUS
 REAL :: DX,DY,TD,D,MD,X,Y

 !#initialize
 ISGX  = XC
 ISGY  = YC
 ANGL  = HNODATA
 DORTHO= HNODATA
 DIST  = HNODATA

 IISG=0
 MD  =MAXDIST
 DO I=1,NISG
  TD=0.0
  K =ISG(I)%ISEG-1
  DO J=1,ISG(I)%NSEG-1
   K =K+1
   DX=ISP(K+1)%X-ISP(K)%X
   DY=ISP(K+1)%Y-ISP(K)%Y
   !perform intersection
   CALL IGRINTERSECTLINE(ISP(K)%X,ISP(K)%Y,ISP(K+1)%X,ISP(K+1)%Y,XC,YC,XC+DY,YC-DX,X,Y,ISTATUS)

   IF(ISTATUS.EQ.3.OR.ISTATUS.EQ.5)THEN
    D=SQRT((X-XC)**2.0+(Y-YC)**2.0)
    !#first time to put results, or replace it whenever new point is closer
    IF(D.LT.MD)THEN
     MD    =D
     DIST  =TD+SQRT((ISP(K)%X-X)**2.0+(ISP(K)%Y-Y)**2.0)
     DORTHO=D
     ISGX  =X
     ISGY  =Y
     IISG  =I
     ANGL  =ATAN2(DY,DX)
     ANGL  =R2G*ANGL
    ENDIF
   ENDIF

   !#include position of nodes
   D=SQRT((XC-ISP(K)%X)**2.0+(YC-ISP(K)%Y)**2.0)
   IF(D.LT.MD)THEN
    MD    =D
    DIST=0.0
    IF(J.GT.1)DIST=TD+SQRT(DX**2.0+DY**2.0)
    DORTHO=D
    ISGX  =ISP(K)%X
    ISGY  =ISP(K)%Y
    IISG  =I
    ANGL  =ATAN2(DY,DX)
    ANGL  =R2G*ANGL
   ENDIF
   !#evaluate last point
   IF(J.EQ.ISG(I)%NSEG-1)THEN
    D=SQRT((XC-ISP(K+1)%X)**2.0+(YC-ISP(K+1)%Y)**2.0)
    IF(D.LT.MD)THEN
     MD    =D
     DIST  =TD+SQRT(DX**2.0+DY**2.0)
     DORTHO=D
     ISGX  =ISP(K+1)%X
     ISGY  =ISP(K+1)%Y
     IISG  =I
     ANGL  =ATAN2(DY,DX)
     ANGL  =R2G*ANGL
    ENDIF
   ENDIF

   !## get total distance
   TD=TD+SQRT(DX**2.0+DY**2.0)

  ENDDO
 END DO

 !## apply angle correction according to from and to node flow scheme
 IF(IISG.NE.0)CALL ISGSTUWEN_CORANGLE(IISG)

 END SUBROUTINE ISGSTUWEN_INTERSECT

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_COMPUTEXY(IISG,IIST)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IIST
 INTEGER :: I,J
 REAL :: DXY,F,TD

 TD=0.0
 J =ISG(IISG)%ISEG
 DO I=2,ISG(IISG)%NSEG
  J=J+1
  DXY=((ISP(J)%X-ISP(J-1)%X)**2.0)+((ISP(J)%Y-ISP(J-1)%Y)**2.0)
  IF(DXY.GT.0.0)DXY=SQRT(DXY)
  TD=TD+DXY
  IF(TD.GE.IST(IIST)%DIST)EXIT
 END DO

 !## distance current segment
 F   =(IST(IIST)%DIST-(TD-DXY))/DXY
 ISGX= ISP(J-1)%X+(ISP(J)%X-ISP(J-1)%X)*F
 ISGY= ISP(J-1)%Y+(ISP(J)%Y-ISP(J-1)%Y)*F
 ANGL= ATAN2(ISP(J)%Y-ISP(J-1)%Y,ISP(J)%X-ISP(J-1)%X)
 ANGL= R2G*ANGL

 CALL ISGSTUWEN_CORANGLE(IISG)

 END SUBROUTINE ISGSTUWEN_COMPUTEXY

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_CORANGLE(IISG)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG
 INTEGER :: I
 REAL :: H1,H2

 !## involve levels from the from and to node to adjust structure angle
 H1=HNODATA
 H2=H1

 I =ISG(IISG)%ICLC
 CALL ISGSTUWEN_GETMEANLEVEL(I,H1)
 I =ISG(IISG)%ICLC+ISG(IISG)%NCLC-1
 CALL ISGSTUWEN_GETMEANLEVEL(I,H2)
 !## turn around push-direction
 IF(H1.NE.HNODATA.AND.H2.NE.HNODATA.AND.H2.GT.H1)ANGL=ANGL+180.0
 ANGL=ORIENT(ANGL)

 END SUBROUTINE ISGSTUWEN_CORANGLE

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_GETMEANLEVEL(I,H)
 !###===============================================================================
 IMPLICIT NONE
 REAL,INTENT(OUT) :: H
 INTEGER,INTENT(IN) :: I
 INTEGER :: J,K,N,ID,IM,IY,DD,MIND,MAXD,ND
 REAL :: H1,H2
 INTEGER,DIMENSION(4) :: JULD

 J=ISD(I)%IREF
 N=ISD(I)%N
 IF(N.LE.0)RETURN

 !## compute mean of levels
 IF(IDMD.EQ.0.OR.IMMD.EQ.0.OR.IYMD.EQ.0)THEN
  H=0.0
  DO K=J,J+N-1
   H=H+DATISD(K)%WLVL
  ENDDO
  H=H/REAL(N)

 !## get level of particular date
 ELSE

  MIND   =-100000
  MAXD   = 100000
  JULD(2)=JD(IYMD,IMMD,IDMD)

  DO K=J,J+N-1
   JULD(1)=UTL_IDATETOJDATE(DATISD(K)%IDATE)
   DD     =JULD(1)-JULD(2)
   IF(DD.LE.0.AND.DD.GT.MIND)THEN
    JULD(3)=JULD(1)
    MIND   =DD
    H1     =DATISD(K)%WLVL
   ENDIF
   IF(DD.GE.0.AND.DD.LT.MAXD)THEN
    JULD(4)=JULD(1)
    MAXD   =DD
    H2     =DATISD(K)%WLVL
   ENDIF
  ENDDO
  !## not able to compute head
  IF(JULD(3).EQ.0.AND.JULD(4).EQ.0)THEN
   H=HNODATA
  ELSE
   IF(JULD(3).EQ.0)THEN
    H=H2
   ELSEIF(JULD(4).EQ.0)THEN
    H=H1
   ELSE
    ND=(MAXD+MIND)
    H = H1
    IF(ND.GT.0)H=(MAXD*H1+MIND*H2)/REAL(ND)
   ENDIF
  ENDIF
 ENDIF

 END SUBROUTINE ISGSTUWEN_GETMEANLEVEL

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_GETMEANDOWNLEVEL(IISG,H)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG
 REAL,INTENT(OUT) :: H
 INTEGER :: IREC,NREC,I,J
 REAL :: H1,H2,D
 INTEGER,DIMENSION(2) :: ID
 REAL,DIMENSION(2) :: DI

 IREC=ISG(IISG)%ICLC-1
 NREC=ISG(IISG)%NCLC

 !## initialize maximal distance values
 DI   =MAXVAL(ISD(IREC+1:IREC+NREC)%DIST)
 !## initialize pointers to calc. points
 ID(1)=IREC+1
 ID(2)=IREC+NREC
 DO I=1,NREC
  IREC=IREC+1
  D   =ISD(IREC)%DIST-DIST

  IF(D.GE.0.0)THEN
   !## after
   J=2
  ELSE
   !## before
   J=1
  ENDIF

  D=ABS(D)

  IF(D.LE.DI(J))THEN
   DI(J)=D
   ID(J)=IREC
  ENDIF

 ENDDO

 H1=HNODATA
 H2=H1
 CALL ISGSTUWEN_GETMEANLEVEL(ID(1),H1)
 CALL ISGSTUWEN_GETMEANLEVEL(ID(2),H2)
 H=((H1*DI(2))+(H2*DI(1)))/(DI(1)+DI(2))  
! H=(H1+H2)/2.0

 END SUBROUTINE ISGSTUWEN_GETMEANDOWNLEVEL

 !###===============================================================================
 FUNCTION ISGSTUWEN_CHECKID(IISG,IIST)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IISG,IIST
 LOGICAL :: ISGSTUWEN_CHECKID
 INTEGER :: J

 ISGSTUWEN_CHECKID=.FALSE.

! IIST=0
 DO IISG=1,NISG
  IIST=ISG(IISG)%ISTW-1
  DO J=1,ISG(IISG)%NSTW
   IIST=IIST+1
   IF(TRIM(IST(IIST)%CNAME).EQ.TRIM(CI))THEN
    ISGSTUWEN_CHECKID=.TRUE.
    DIST             =IST(IIST)%DIST
    DORTHO           =HNODATA!D
    RETURN
   ENDIF
  ENDDO
 END DO

 END FUNCTION ISGSTUWEN_CHECKID

 !###===============================================================================
 LOGICAL FUNCTION GETSTRINGVALUE(STRING,X)
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: STRING
 REAL,INTENT(OUT) :: X
 
 GETSTRINGVALUE=.FALSE.

 IF(LEN_TRIM(STRING).EQ.0)RETURN
 READ(STRING,*,IOSTAT=IOS) X
 IF(IOS.EQ.0)THEN; GETSTRINGVALUE=.TRUE.; RETURN; ENDIF

 IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
             'iMOD can not translate ['//TRIM(STRING)//'] into a real variable','Error')
 IF(IBATCH.EQ.1)WRITE(*,*) 'iMOD can not translate ['//TRIM(STRING)//'] into a real variable'
 
 END FUNCTION GETSTRINGVALUE

 !###===============================================================================
 FUNCTION GETDATE(DATE,IFN)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: GETDATE
 INTEGER,INTENT(IN) :: IFN
 CHARACTER(LEN=*),INTENT(IN) :: DATE
 INTEGER :: I,J

 GETDATE=0

 I=INDEX(DATE,'-')
 IF(I.EQ.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
              'iMOD can not translate ['//TRIM(DATE)//'] into a day or month, missing "-"','Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'iMOD can not translate ['//TRIM(DATE)//'] into a day or month, missing "-"'
 ENDIF
 !#before '-'
 IF(IFN.EQ.1)THEN
  READ(DATE(:I-1),*,IOSTAT=IOS) J
 !#after '-'
 ELSEIF(IFN.EQ.2)THEN
  READ(DATE(I+1:),*,IOSTAT=IOS) J
 ENDIF

 IF(IOS.NE.0)THEN
  IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK, &
              'iMOD can not translate ['//TRIM(DATE)//'] into a day or month','Error')
  IF(IBATCH.EQ.1)WRITE(*,*) 'iMOD can not translate ['//TRIM(DATE)//'] into a day or month'
  RETURN
 ENDIF

 GETDATE=J

 END FUNCTION GETDATE

 !###===============================================================================
 FUNCTION ORIENT(OR)
 !###===============================================================================
 IMPLICIT NONE
 REAL :: ORIENT
 REAL,INTENT(IN) :: OR

 ORIENT=OR
 DO
  IF(ORIENT.LT.0.0)ORIENT=OR+360.0
  IF(ORIENT.GT.360)ORIENT=OR-360.0
  IF(ORIENT.GE.0.0.AND.ORIENT.LE.360.0)EXIT
 ENDDO

 END FUNCTION ORIENT

END MODULE MOD_ISG_STRUCTURES
