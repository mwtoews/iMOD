!!  Copyright (C) Stichting Deltares, 2005-2016.
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
!!
MODULE MOD_IFF

USE WINTERACTER
USE MODPLOT
USE MOD_UTL, ONLY : UTL_WAITMESSAGE,UTL_GETUNIT,ITOS,RTOS,UTL_CAP,UTL_MESSAGEHANDLE,UTL_IDFGETCLASS, &
                    UTL_PROFILE_GETVIEWBOX,UTL_FADEOUTCOLOUR
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG

TYPE IFFTYPE
 INTEGER :: IL,IPART
 REAL :: X,Y,Z
 REAL,POINTER,DIMENSION(:) :: XVAL
END TYPE IFFTYPE
TYPE(IFFTYPE),DIMENSION(:),ALLOCATABLE :: IFF

CONTAINS

 !###======================================================================
 SUBROUTINE IFFDRAW()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,IIFF,IPLOT

 IIFF=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL.AND.MP(IPLOT)%IPLOT.EQ.3)THEN
   IIFF=IIFF+1

   !## open idf-file
   IU=UTL_GETUNITIFF(MP(IPLOT)%IDFNAME,'OLD')
   IF(IU.GT.0)THEN
    CALL WINDOWSELECT(0)

    CALL IGRAREA(0.0,0.0,1.0,1.0)
    CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
    CALL IFFPLOT(IU,MPW%XMIN,MPW%XMAX,MPW%YMIN,MPW%YMAX,IPLOT,(/0.0,0.0,0.0,0.0/),0.0)
!    CALL IFFPLOT(IU,MPW%XMIN,MPW%XMAX,MPW%YMIN,MPW%YMAX, &
!                 ZMIN,ZMAX,IPLOT,(/0.0,0.0,0.0,0.0/),0.0)
    CLOSE(IU)
    DRWLIST(IPLOT)=1
   ENDIF

  ENDIF
 END DO

 END SUBROUTINE IFFDRAW

 !###===============================================================================
 SUBROUTINE IFFPLOT(IU,XMIN,XMAX,YMIN,YMAX,IPLOT,XY,OFFSETX)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IPLOT
 REAL,DIMENSION(2,2),INTENT(IN) :: XY
 REAL,INTENT(IN) :: XMIN,XMAX,YMIN,YMAX,OFFSETX 
 INTEGER :: IOS,ICLR,I,J,II,NC,N
 REAL :: X1,X2,Y1,Y2,XVAL,DX,DY,XDIS,YDIS,TDIS,RAD,XS1,XS2,YS1,YS2,XMN,YMN,XMX,YMX
 REAL,DIMENSION(4,2) :: XYPOL

 READ(IU,*) NC

 ALLOCATE(IFF(2)); N=NC-5
 DO I=1,SIZE(IFF); NULLIFY(IFF(I)%XVAL); ALLOCATE(IFF(I)%XVAL(N)); ENDDO

 !## read attributes
 DO I=1,NC; READ(IU,*); END DO

 !## make sure number of column selected is within range of available linetype
 IF(MP(IPLOT)%SYMBOL.LT.0.OR.MP(IPLOT)%SYMBOL.GT.7)MP(IPLOT)%SYMBOL=0
 IF(MP(IPLOT)%SCOLOR.LE.0)MP(IPLOT)%SCOLOR=WRGB(0,0,200)
 !## make sure number of column selected is within range of available attributes
 IF(MP(IPLOT)%IATTRIB.LE.0 )MP(IPLOT)%IATTRIB=1
 IF(MP(IPLOT)%IATTRIB.GT.NC)MP(IPLOT)%IATTRIB=1

 CALL IGRLINETYPE(MP(IPLOT)%SYMBOL); CALL IGRLINEWIDTH(MP(IPLOT)%THICKNESS); CALL IGRCOLOURN(WRGB(0,0,0))

 TDIS=REAL(MP(IPLOT)%IDFI)
 IF(SUM(XY).NE.0.0)THEN
  DX =XY(1,2)-XY(1,1)  !## x2-x1
  DY =XY(2,2)-XY(2,1)  !## y2-y1
!  XDIS=SQRT(DX**2.0+DY**2.0)
  RAD=0.0
  IF(DY.NE.0.0)RAD=ATAN2(DY,DX)
  CALL UTL_PROFILE_GETVIEWBOX(XY(1,1),XY(2,1),XY(1,2),XY(2,2),TDIS,XYPOL,XMN,YMN,XMX,YMX)
 ENDIF

 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Drawing IFF ...')
 CALL WCURSORSHAPE(CURHOURGLASS)

 MP(IPLOT)%XMIN= 1.0E10; MP(IPLOT)%XMAX=-1.0E10
 MP(IPLOT)%YMIN= 1.0E10; MP(IPLOT)%YMAX=-1.0E10

 IF(MP(IPLOT)%ILEG.EQ.0)N=0

 IFF(2)%IPART=0; II=0
 DO
  READ(IU,*,IOSTAT=IOS) IFF(1)%IPART,IFF(1)%IL,IFF(1)%X,IFF(1)%Y,IFF(1)%Z,(IFF(1)%XVAL(J),J=1,N)
  IF(IOS.NE.0)EXIT

  MP(IPLOT)%XMIN=MIN(MP(IPLOT)%XMIN,IFF(1)%X); MP(IPLOT)%XMAX=MAX(MP(IPLOT)%XMAX,IFF(1)%X)
  MP(IPLOT)%YMIN=MIN(MP(IPLOT)%YMIN,IFF(1)%Y); MP(IPLOT)%YMAX=MAX(MP(IPLOT)%YMAX,IFF(1)%Y)

  !## same particle
  IF(IFF(1)%IPART.EQ.IFF(2)%IPART)THEN

   !## current point inside viewable extent
   IF(IFF(1)%X.LT.XMAX.AND.IFF(1)%X.GE.XMIN.AND. &
      IFF(1)%Y.LT.YMAX.AND.IFF(1)%Y.GE.YMIN)THEN 

    !## use of line-colouring
    IF(MP(IPLOT)%ILEG.EQ.1)THEN 
     CALL IFFPLOT_GETIFFVAL(MP(IPLOT)%IATTRIB,XVAL)

     ICLR=UTL_IDFGETCLASS(MP(IPLOT)%LEG,XVAL)

     II=II+1
     IF(II.EQ.1)THEN
      MP(IPLOT)%UMIN=XVAL; MP(IPLOT)%UMAX=MP(IPLOT)%UMIN
     ELSE
      MP(IPLOT)%UMIN=MIN(MP(IPLOT)%UMIN,XVAL); MP(IPLOT)%UMAX=MAX(MP(IPLOT)%UMAX,XVAL)
     ENDIF
    ELSE
     !## colouring is default: black!
     ICLR=MP(IPLOT)%SCOLOR
    ENDIF

    !## normal 2d plot
    IF(SUM(XY).EQ.0.0)THEN

     !## length line is zero
     IF((IFF(1)%X-IFF(2)%X).NE.0.0.OR.(IFF(1)%Y-IFF(2)%Y).NE.0.0)THEN
      !## correct for sight-depth
 !      IF((XYZT(3,1)+XYZT(3,2))/2.0.GE.YDIS)THEN
 !     IF(MP(IPLOT)%FADEOUT.EQ.1)CALL UTL_FADEOUTCOLOUR(ICLR,0.5*(IFF(1)%Z+IFF(2)%Z)/YDIS)
      CALL IGRCOLOURN(ICLR)   
      CALL IGRJOIN(IFF(1)%X,IFF(1)%Y,IFF(2)%X,IFF(2)%Y)
     ENDIF

    !## perform coordinate transformation for serie-plotting!
    ELSE
     
     IF(IGRINSIDEPOLYGON(XYPOL(:,1),XYPOL(:,2),4,IFF(1)%X,IFF(1)%Y).OR. &
        IGRINSIDEPOLYGON(XYPOL(:,1),XYPOL(:,2),4,IFF(2)%X,IFF(2)%Y))THEN

      !## perform coordinate shift first ... related to first point in segment!
      XS1=IFF(1)%X-XY(1,1)  !x1
      XS2=IFF(2)%X-XY(1,1)  !x2
      YS1=IFF(1)%Y-XY(2,1)  !y1
      YS2=IFF(2)%Y-XY(2,1)  !y2

      !## clock-wise rotation
      !## rotated coordinates become ...
      X1=XS1* COS(RAD)+YS1*SIN(RAD)       !x1'
      X2=XS2* COS(RAD)+YS2*SIN(RAD)       !x2'
!      Y1=YS1*(-1.0*SIN(RAD))+YS1*COS(RAD) !y1'
!      Y2=YS2*(-1.0*SIN(RAD))+YS2*COS(RAD) !y2'

      IF(MP(IPLOT)%FADEOUT.EQ.1)THEN
       XS1=(IFF(1)%X+IFF(2)%X)/2.0
       YS1=(IFF(1)%Y+IFF(2)%Y)/2.0
       YDIS=IGRDISTANCELINE(XY(1,1),XY(1,2),XY(2,1),XY(2,2),XS1,YS1,1) 
       CALL UTL_FADEOUTCOLOUR(ICLR,TDIS/YDIS/2.0) 
      ENDIF
      CALL IGRCOLOURN(ICLR)

      !## draw line X-Z only when length line is zero!
      IF((X1+OFFSETX-X2+OFFSETX).NE.0.0.OR.(IFF(1)%Z-IFF(2)%Z).NE.0.0)THEN
       CALL IGRJOIN(X2+OFFSETX,IFF(2)%Z,X1+OFFSETX,IFF(1)%Z)
       CALL IGRARROWJOIN(X2+OFFSETX,IFF(2)%Z,X1+OFFSETX,IFF(1)%Z,1)
      ENDIF
     ENDIF

    ENDIF
   ENDIF
  ENDIF
 
  IFF(2)%IPART=IFF(1)%IPART
  IFF(2)%IL   =IFF(1)%IL
  IFF(2)%X    =IFF(1)%X
  IFF(2)%Y    =IFF(1)%Y
  IFF(2)%Z    =IFF(1)%Z
  DO J=1,N; IFF(2)%XVAL(J)=IFF(1)%XVAL(J); ENDDO
  
 ENDDO

 DO I=1,SIZE(IFF); DEALLOCATE(IFF(I)%XVAL); ENDDO; DEALLOCATE(IFF)

 CALL IGRCOLOURN(WRGB(255,255,255))  !## white

 CALL IGRFILLPATTERN(OUTLINE); CALL IGRLINEWIDTH(1)
 CALL WINDOWOUTSTATUSBAR(2,''); CALL WINDOWOUTSTATUSBAR(4,'')

 CALL WCURSORSHAPE(CURARROW)

 END SUBROUTINE IFFPLOT

 !###======================================================================
 SUBROUTINE IFFPLOT_GETIFFVAL(IPOS,X)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPOS
 REAL,INTENT(OUT) :: X
 INTEGER :: I
 
 !## get value to be assigned to line
 SELECT CASE (IPOS)
  CASE (1)
   X=REAL(IFF(1)%IPART)
  CASE (2)
   X=REAL(IFF(1)%IL)
  CASE (3)
   X=(IFF(1)%X+IFF(2)%X)/2.0
  CASE (4)
   X=(IFF(1)%Y+IFF(2)%Y)/2.0
  CASE (5)
   X=(IFF(1)%Z+IFF(2)%Z)/2.0
  CASE DEFAULT
   I=IPOS-5; X=(IFF(1)%XVAL(I)+IFF(2)%XVAL(I))/2.0
 END SELECT

 END SUBROUTINE IFFPLOT_GETIFFVAL

 !###======================================================================
 INTEGER FUNCTION UTL_GETUNITIFF(IFFNAME,TSTAT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IFFNAME
 CHARACTER(LEN=*),INTENT(IN) :: TSTAT
 LOGICAL :: LEX,LOPEN
 CHARACTER(LEN=10) :: TSTATUS,YESNO
 CHARACTER(LEN=50) :: MESSAGE
 INTEGER :: IOS

 TSTATUS   =TSTAT
 UTL_GETUNITIFF=0

 INQUIRE(FILE=IFFNAME,OPENED=LOPEN)
 IF(LOPEN)THEN
  INQUIRE(FILE=IFFNAME,NUMBER=UTL_GETUNITIFF)
  CLOSE(UTL_GETUNITIFF)
 ENDIF
 IF(TSTATUS(1:3).EQ.'OLD')THEN
  INQUIRE(FILE=IFFNAME,EXIST=LEX)
  IF(.NOT.LEX)THEN
   IF(TSTATUS(4:4).NE.'1')CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not find'//CHAR(13)//TRIM(IFFNAME),'Warning')
   RETURN
  ENDIF
  INQUIRE(FILE=IFFNAME,DIRECT=YESNO)
  IF(UTL_CAP(YESNO,'U').EQ.'YES')CALL IFFOLD2IFFNEW(IFFNAME)
  TSTATUS=TSTAT(1:3)
  UTL_GETUNITIFF=UTL_GETUNIT()
  CALL OSD_OPEN(UTL_GETUNITIFF,FILE=IFFNAME,STATUS=TSTATUS,FORM='FORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   UTL_GETUNITIFF=0
   CALL OSD_IOSTAT_MSG(IOS,MESSAGE)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open'//CHAR(13)//TRIM(IFFNAME)//CHAR(13)// &
   TRIM(MESSAGE),'Warning')
  ENDIF
 ENDIF

 END FUNCTION UTL_GETUNITIFF

 !###======================================================================
 SUBROUTINE IFFOLD2IFFNEW(IFFNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: RECLEN=10*4+2
 CHARACTER(LEN=*),INTENT(IN) :: IFFNAME
 REAL,DIMENSION(3) :: XYZT
 REAL :: CT,DT,T
 INTEGER,DIMENSION(3) :: IU
 INTEGER :: TPART,JPART,IT
 INTEGER :: I,J,K,IREC,NROW,NCOL,NLAY,IBOX,IPOS,NPART,IPART,KPART
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IIFF
 TYPE IFFOBJ
  REAL :: X1,X2,Y1,Y2,Z1,Z2,T1,T2,V
  INTEGER :: IPART
  INTEGER(KIND=2) :: IL
 END TYPE IFFOBJ
 TYPE(IFFOBJ),DIMENSION(:),ALLOCATABLE :: IFF
 CHARACTER(LEN=256) :: LINE

 CALL UTL_MESSAGEHANDLE(0); CALL WINDOWOUTSTATUSBAR(4,'Converting old IFF format to new one ...')

 IU(1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(1),FILE=IFFNAME,STATUS='OLD',FORM='UNFORMATTED',ACTION='READ,DENYWRITE', &
      ACCESS='DIRECT',RECL=RECLEN)
 I=INDEX(IFFNAME,'.',.TRUE.)-1
 IU(2)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(2),FILE=IFFNAME(:I)//'_new.iff',STATUS='REPLACE',FORM='FORMATTED',ACTION='WRITE')

 READ(IU(1),REC=3) XYZT(1:3)

 NCOL=INT(XYZT(1)); NROW=INT(XYZT(2)); NLAY=INT(XYZT(3))

 TPART=0
 DO K=1,NLAY
  DO J=1,NROW
   DO I=1,NCOL
    IBOX=((K-1)*NCOL*NROW)+(J-1)*NCOL+I
    IREC=4+IBOX
    !## read position of box with coordinates
    READ(IU(1),REC=IREC) IPOS,NPART
    TPART=TPART+NPART
   ENDDO
  ENDDO
 ENDDO

 CALL WINDOWOUTSTATUSBAR(4,'Converting old IFF format to new one, found '//TRIM(ITOS(TPART))//' segments ...')

 ALLOCATE(IFF(TPART),IIFF(TPART))

 TPART=0
 DO K=1,NLAY
  DO J=1,NROW
   DO I=1,NCOL

    IBOX=((K-1)*NCOL*NROW)+(J-1)*NCOL+I
    IREC=4+IBOX

    !##read position of box with coordinates
    READ(IU(1),REC=IREC) IPOS,NPART

    IREC=IPOS-1
    DO IPART=1,NPART
     TPART=TPART+1

     !## determine which box to be drawn
     READ(IU(1),REC=IREC+IPART) IFF(TPART)%X1,IFF(TPART)%Y1,IFF(TPART)%Z1,IFF(TPART)%T1, &
                                IFF(TPART)%X2,IFF(TPART)%Y2,IFF(TPART)%Z2,IFF(TPART)%T2, &
                                IFF(TPART)%V ,IFF(TPART)%IL,IFF(TPART)%IPART

    END DO

   ENDDO
  ENDDO
 ENDDO

 WRITE(IU(2),*) 7
 WRITE(IU(2),*) 'IPARTICLE'
 WRITE(IU(2),*) 'ILAY'
 WRITE(IU(2),*) 'X-COORD.'
 WRITE(IU(2),*) 'Y-COORD.'
 WRITE(IU(2),*) 'Z-COORD.'
 WRITE(IU(2),*) 'TIME(YEARS)'
 WRITE(IU(2),*) 'VELOCITY(M/D)'

 JPART=0
 DO
  IIFF=0
  JPART=JPART+1
  NPART=0
  DO IPART=1,TPART
   IF(IFF(IPART)%IPART.EQ.JPART)THEN
    NPART=NPART+1
    IIFF(NPART)=IPART
   ENDIF
  END DO

  !## finished
  IF(NPART.EQ.0)EXIT

  CT=-1.0
  DO IPART=1,NPART
   !## find sequential time particle
   DT=10.0E10
   DO KPART=1,NPART
    IF(IFF(IIFF(KPART))%T1.GT.CT)THEN
     T=ABS(CT-IFF(IIFF(KPART))%T1)
     IF(T.LT.DT)THEN
      DT=T
      IT=KPART
     ENDIF
    ENDIF
   END DO
   CT=IFF(IIFF(IT))%T1
   LINE=TRIM(ITOS(IFF(IIFF(IT))%IPART))   //','//TRIM(ITOS(INT(IFF(IIFF(IT))%IL)))//','// &
        TRIM(RTOS(IFF(IIFF(IT))%X1,'F',3))//','//TRIM(RTOS(IFF(IIFF(IT))%Y1,'F',3))//','// &
        TRIM(RTOS(IFF(IIFF(IT))%Z1,'F',3))//','//TRIM(RTOS(IFF(IIFF(IT))%T1,'F',3))//','// &
        TRIM(RTOS(IFF(IIFF(IT))%V,'F',3))
   WRITE(IU(2),*) TRIM(LINE)
  END DO
 ENDDO

 DEALLOCATE(IFF,IIFF)

 CLOSE(IU(1)); CLOSE(IU(2))

 I=INDEX(IFFNAME,'.',.TRUE.)-1
 CALL IOSRENAMEFILE(IFFNAME,IFFNAME(:I)//'_old.iff')
 CALL IOSRENAMEFILE(IFFNAME(:I)//'_new.iff',IFFNAME)

 CALL WINDOWOUTSTATUSBAR(4,''); CALL UTL_MESSAGEHANDLE(1)

 END SUBROUTINE IFFOLD2IFFNEW

END MODULE MOD_IFF

