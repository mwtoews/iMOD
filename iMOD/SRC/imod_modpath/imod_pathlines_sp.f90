!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_PLINES_SP

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : DP_KIND,SP_KIND,PI,REPLACESTRING
USE MOD_UTL, ONLY : UTL_GETUNIT,DBL_IGRINSIDEPOLYGON,ITOS,UTL_CREATEDIR,UTL_CAP,UTL_SUBST
USE MOD_PLINES_PAR
USE MOD_SPOINTS_PAR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFDEALLOCATEX,IDFGETVAL,IDFIROWICOL,IDFREADSCALE,IDFCOPY,IDFREADPART,IDFGETLOC
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG

REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE,PRIVATE :: XCRD,YCRD
INTEGER,PRIVATE :: NCRD,IU,JU
TYPE(IDFOBJ),PRIVATE :: TOP,BOT,REF
CHARACTER(LEN=256) :: STARTPOINTFILE

CONTAINS

 !###======================================================================
 SUBROUTINE TRACEREADSP(IG,NPART)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NPART,IG
 INTEGER :: I,IOS
 CHARACTER(LEN=50) :: ERRORMESSAGE

 IU=0; JU=0

 JU=UTL_GETUNIT(); CALL OSD_OPEN(JU,FILE=STARTPOINTFILE,STATUS='OLD',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,ERRORMESSAGE)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(ERRORMESSAGE)//CHAR(13)//TRIM(STARTPOINTFILE),'Error')
  RETURN
 ENDIF
 READ(JU,*)
 
 DO I=1,NPART
!  READ(JU,'(3I10,4(F15.3,1X))') SP(IG)%KLC(I),SP(IG)%ILC(I),SP(IG)%JLC(I), &
!                                SP(IG)%XLC(I),SP(IG)%YLC(I),SP(IG)%ZLC(I), &
!                                SP(IG)%ZLL(I)
  READ(JU,*) SP(IG)%KLC(I),SP(IG)%ILC(I),SP(IG)%JLC(I), &
             SP(IG)%XLC(I),SP(IG)%YLC(I),SP(IG)%ZLC(I), &
             SP(IG)%ZLL(I)
  !## initial age set to zero!
  SP(IG)%TOT(I)=0.0D0
  !## maximal layer
  SP(IG)%MXL(I)=SP(IG)%KLC(I)
 END DO
 SP(IG)%NPART=NPART

 CLOSE(JU)

! !## store startlocations!
! XLC(:,1)=XLC(:,2)
! YLC(:,1)=YLC(:,2)
! ZLC(:,1)=ZLC(:,2)

! SP%SLAY=SP%KLC
! SP%MXL =SP%SLAY
 
 END SUBROUTINE TRACEREADSP

 !###======================================================================
 LOGICAL FUNCTION TRACEPREPARESP(NPART,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 INTEGER,INTENT(OUT) :: NPART
 CHARACTER(LEN=50) :: ERRORMESSAGE
 CHARACTER(LEN=256) :: LINE
 INTEGER :: IOS,I,J,N,M,IERROR
 REAL(KIND=DP_KIND) :: XC,YC,ZC

 TRACEPREPARESP=.FALSE.

 NPART=0; TPART=0

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Creating startpoints ...')
 IF(IBATCH.EQ.1)WRITE(*,'(A)') 'Creating startpoints ...'

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=SPFNAME(ISPFNAME),STATUS='OLD',ACTION='READ,DENYWRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,ERRORMESSAGE)
  CALL TRACEDEALLOCATESP(IBATCH)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file:'//CHAR(13)//TRIM(SPFNAME(ISPFNAME))//CHAR(13)// &
                   TRIM(ERRORMESSAGE),'Error')
  RETURN
 ENDIF

 I=INDEXNOCASE(SPFNAME(ISPFNAME),'.',.TRUE.)-1
 STARTPOINTFILE=SPFNAME(ISPFNAME)(:I)//'.pnt'
 JU=UTL_GETUNIT()
 CALL UTL_CREATEDIR(STARTPOINTFILE(:INDEX(STARTPOINTFILE,'\',.TRUE.)-1))
 CALL OSD_OPEN(JU,FILE=STARTPOINTFILE,STATUS='REPLACE',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL OSD_IOSTAT_MSG(IOS,ERRORMESSAGE)
  CALL TRACEDEALLOCATESP(IBATCH)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(ERRORMESSAGE)//CHAR(13)//TRIM(STARTPOINTFILE),'Error')
  RETURN
 ENDIF
 
 WRITE(JU,'(A)') 'ILAY,IROW,ICOL,X-LOCAL,Y-LOCAL,Z,Z-LOCAL,THICKNESS,AQF/AQT,X-GLOBAL,Y-GLOBAL'
 IF(ALLOCATED(SPNT))DEALLOCATE(SPNT); ALLOCATE(SPNT(1))

 IF(INDEX(UTL_CAP(SPFNAME(ISPFNAME),'U'),'.IPF').GT.0)THEN

  TOP%IU=0; BOT%IU=0; SPNT(1)%IREF=0; REF%IU=0; SPNT(1)%ISZ=1
  READ(IU,*) N
  READ(IU,*) M
  DO I=1,M+1; READ(IU,*); ENDDO
  DO I=1,N
   READ(IU,*) XC,YC,ZC
   CALL TRACEASSIGNSP(NPART,XC,YC,ZC,ZC,ZC,-1.0D0)   
  ENDDO

 ELSE

  I=1; DO
   READ(IU,*,IOSTAT=IOS) ERRORMESSAGE
   IF(IOS.NE.0)EXIT; IF(ERRORMESSAGE(1:4).NE.'====')EXIT
   READ(IU,*,IOSTAT=IOS) 
   READ(IU,*,IOSTAT=IOS)
   READ(IU,*,IOSTAT=IOS) SPNT(I)%ISHAPE 
   SELECT CASE (SPNT(I)%ISHAPE)

    CASE (ID_GRID)
     READ(IU,*) SPNT(I)%IDZ
     IF(SPNT(I)%IDZ.LT.0.0D0)THEN 
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot give NEGATIVE or ZERO values for the IDZ','Error')
      RETURN
     ENDIF

    CASE (ID_POLYGON,ID_RECTANGLE)
     READ(IU,'(A256)') LINE
     READ(LINE,*,IOSTAT=IOS) SPNT(I)%IDX,SPNT(I)%IDY,SPNT(I)%ISNAP
     IF(IOS.NE.0)THEN
      READ(LINE,*,IOSTAT=IOS) SPNT(I)%IDX,SPNT(I)%IDY
      SPNT(I)%ISNAP=0
     ENDIF
     IF(SPNT(I)%IDX.LE.0.0D0.OR.SPNT(I)%IDY.LE.0.0D0)THEN
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot give NEGATIVE or ZERO values for the sampling distance','Error')
      RETURN
     ENDIF

    CASE (ID_CIRCLE,ID_POINT)
     READ(IU,'(A256)') LINE
     READ(LINE,*,IOSTAT=IOS) SPNT(I)%IRADIUS,SPNT(I)%ISX,SPNT(I)%ISNAP
     IF(IOS.NE.0)THEN
      READ(LINE,*,IOSTAT=IOS) SPNT(I)%IRADIUS,SPNT(I)%ISX
      SPNT(I)%ISNAP=0
     ENDIF
     IF(SPNT(I)%ISX.LE.0.0D0.OR.SPNT(I)%IRADIUS.LE.0)THEN
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot give NEGATIVE or ZERO values for the radius OR sampling dist.', &
        'Error')
      RETURN
     ENDIF

    CASE (ID_LINE)
     READ(IU,*) SPNT(I)%ISX
     IF(SPNT(I)%ISX.LE.0)THEN
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You cannot give NEGATIVE or ZERO values for the SAMPLING DIST.','Error') 
      RETURN
     ENDIF

   END SELECT
  
   READ(IU,*,IOSTAT=IOS) SPNT(I)%TOPIDF
   IF(IOS.NE.0)THEN
    CALL TRACEDEALLOCATESP(IBATCH)
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading TOPIDF from isd','Error'); RETURN
   ENDIF 
   SPNT(I)%TOPIDF=UTL_SUBST(SPNT(I)%TOPIDF,TRIM(REPLACESTRING),PREFVAL(5),IERROR=IERROR)
   IF(IERROR.EQ.1)RETURN
   
   SELECT CASE (SPNT(I)%ISHAPE)

    CASE (ID_GRID)
     SPNT(I)%BOTIDF='0.0D0'; SPNT(I)%IREF=0; SPNT(I)%ISZ=1; NCRD=0 

    CASE (ID_LINE,ID_POINT,ID_POLYGON,ID_RECTANGLE,ID_CIRCLE)
     READ(IU,*,IOSTAT=IOS) SPNT(I)%BOTIDF
     IF(IOS.NE.0)THEN
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading BOTIDF from isd','Error'); RETURN
     ENDIF
     SPNT(I)%BOTIDF=UTL_SUBST(SPNT(I)%BOTIDF,TRIM(REPLACESTRING),PREFVAL(5),IERROR=IERROR)
     IF(IERROR.EQ.1)RETURN
     READ(IU,'(A256)') LINE
     READ(LINE,*,IOSTAT=IOS) SPNT(I)%IREF
     IF(IOS.NE.0)THEN
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading IREF from isd','Error'); RETURN
     ENDIF
     IF(SPNT(I)%IREF.EQ.1)THEN
      READ(LINE,*,IOSTAT=IOS) SPNT(I)%IREF,SPNT(I)%REFIDF
      IF(IOS.NE.0)THEN
       CALL TRACEDEALLOCATESP(IBATCH)
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading IREF,REFIDF from isd','Error'); RETURN
      ENDIF
      SPNT(I)%REFIDF=UTL_SUBST(SPNT(I)%REFIDF,TRIM(REPLACESTRING),PREFVAL(5),IERROR=IERROR)
      IF(IERROR.EQ.1)RETURN
     ENDIF
     READ(IU,*,IOSTAT=IOS) SPNT(I)%ISZ
     IF(IOS.NE.0)THEN
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading ISZ from isd','Error'); RETURN
     ENDIF
     READ(IU,*,IOSTAT=IOS)
     READ(IU,*,IOSTAT=IOS) NCRD
     IF(IOS.NE.0)THEN
      CALL TRACEDEALLOCATESP(IBATCH)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading NCRD from isd','Error'); RETURN
     ENDIF
     IF(NCRD.GT.0)THEN
      ALLOCATE(XCRD(NCRD),YCRD(NCRD))
       DO J=1,NCRD
       READ(IU,*,IOSTAT=IOS) XCRD(J),YCRD(J); IF(IOS.NE.0)EXIT
       IF(IOS.NE.0)THEN
        CALL TRACEDEALLOCATESP(IBATCH)
        CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading XCRD,YCRD from isd','Error'); RETURN
       ENDIF 
      END DO
     ENDIF
   END SELECT
  
   IF(.NOT.TRACECOMPUTESP(NPART))EXIT
  
   IF(ALLOCATED(XCRD))DEALLOCATE(XCRD); IF(ALLOCATED(YCRD))DEALLOCATE(YCRD)

   READ(IU,*,IOSTAT=IOS)
  ENDDO
 
 ENDIF
 
 CALL TRACEDEALLOCATESP(IBATCH)

 IF(NPART.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'No Particles active in current model domain!'//CHAR(13)// &
   'Probably caused by:'//CHAR(13)// &
   ' - wrong startpoint file selected;'//CHAR(13)// &
   ' - all startpoints are above surface level or beneath base;'//CHAR(13)// &
   ' - all startpoints are outside current model domain.','Information')
  RETURN
 ELSE
  TRACEPREPARESP=.TRUE.
 ENDIF

 END FUNCTION TRACEPREPARESP

 !###======================================================================
 SUBROUTINE TRACEDEALLOCATESP(IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 LOGICAL :: LEX

 IF(ALLOCATED(XCRD))DEALLOCATE(XCRD)
 IF(ALLOCATED(YCRD))DEALLOCATE(YCRD)
 CALL IDFDEALLOCATEX(TOP)
 IF(TOP%IU.GT.0)THEN
  INQUIRE(UNIT=TOP%IU,OPENED=LEX)
  IF(LEX)CLOSE(TOP%IU)
  TOP%IU=0
 ENDIF
 CALL IDFDEALLOCATEX(BOT)
 IF(BOT%IU.GT.0)THEN
  INQUIRE(UNIT=BOT%IU,OPENED=LEX)
  IF(LEX)CLOSE(BOT%IU)
  BOT%IU=0
 ENDIF
 CALL IDFDEALLOCATEX(REF)
 IF(REF%IU.GT.0)THEN
  INQUIRE(UNIT=REF%IU,OPENED=LEX)
  IF(LEX)CLOSE(REF%IU)
  REF%IU=0
 ENDIF

 IF(ALLOCATED(SPNT))DEALLOCATE(SPNT)
 IF(IU.GT.0)CLOSE(IU)
 IF(JU.GT.0)CLOSE(JU)

 IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'')

 END SUBROUTINE TRACEDEALLOCATESP

 !###======================================================================
 LOGICAL FUNCTION TRACECOMPUTESP(NPART)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: NPART
 INTEGER :: I,J,K,NOR,IOS,IROW,ICOL,N
 REAL(KIND=DP_KIND) :: XC,XCC,YC,YCC,ZC,GRAD2RAD,DIFFX,DIFFY,TL,DL,XTOP,XBOT,XREF
 REAL(KIND=DP_KIND) :: XCMIN,YCMIN,XCMAX,YCMAX,OR,ZFRACTION

 TRACECOMPUTESP=.FALSE.

 I=1

 !## what vertical elevation to be used? (top)
 READ(SPNT(I)%TOPIDF,*,IOSTAT=IOS) XTOP
 IF(IOS.NE.0)THEN
  IF(SPNT(I)%ISHAPE.NE.ID_GRID)THEN
   IF(.NOT.IDFREAD(TOP,SPNT(I)%TOPIDF,0))RETURN
  ENDIF
 ENDIF

 IF(SPNT(I)%ISHAPE.NE.ID_GRID)THEN
 
  !## what vertical elevation to be used? (bot)
  READ(SPNT(I)%BOTIDF,*,IOSTAT=IOS) XBOT
  IF(IOS.NE.0)THEN
   IF(.NOT.IDFREAD(BOT,SPNT(I)%BOTIDF,0))RETURN
  ENDIF

  IF(SPNT(I)%IREF.EQ.1)THEN
   !## what vertical elevation to be used? (ref)
   READ(SPNT(I)%REFIDF,*,IOSTAT=IOS) XREF
   IF(IOS.NE.0)THEN
    IF(.NOT.IDFREAD(REF,SPNT(I)%REFIDF,0))RETURN
   ENDIF
  ENDIF
 
  IF(ALLOCATED(XCRD))THEN
   YCMIN=MINVAL(YCRD); YCMAX=MAXVAL(YCRD)
   XCMIN=MINVAL(XCRD); XCMAX=MAXVAL(XCRD)
  ENDIF
 
 ENDIF
 
 SELECT CASE (SPNT(I)%ISHAPE)

  CASE (ID_GRID)
   IF(.NOT.IDFREAD(TOP,SPNT(I)%TOPIDF,0))RETURN
   IF(.NOT.IDFREADPART(TOP,IDF%XMIN,IDF%YMIN,IDF%XMAX,IDF%YMAX))THEN; RETURN; ENDIF
   IF(TOP%IU.GT.0)THEN; CLOSE(TOP%IU); TOP%IU=0; ENDIF
   DO IROW=1,TOP%NROW; DO ICOL=1,TOP%NCOL
    IF(TOP%X(ICOL,IROW).NE.TOP%NODATA)THEN
     CALL IDFGETLOC(TOP,IROW,ICOL,XC,YC)
     CALL TRACEASSIGNSP(NPART,XC,YC,TOP%X(ICOL,IROW),TOP%X(ICOL,IROW),TOP%X(ICOL,IROW),-1.0D0) 
     IF(SPNT(I)%IDZ.NE.0.0D0)THEN
      IF(ICOL.GT.1)THEN
       IF(TOP%X(ICOL-1,IROW).NE.TOP%NODATA)THEN
        CALL IDFGETLOC(TOP,IROW,ICOL,XC,YC)
        IF(TOP%IEQ.EQ.0)THEN; XC=XC-TOP%DX/2.0
        ELSE; XC=TOP%SX(ICOL-1); ENDIF
        ZC=MAX(TOP%X(ICOL-1,IROW),TOP%X(ICOL,IROW)); DO 
         ZC=ZC-SPNT(I)%IDZ
         IF(ZC.LT.MIN(TOP%X(ICOL-1,IROW),TOP%X(ICOL,IROW)))EXIT
         CALL TRACEASSIGNSP(NPART,XC,YC,ZC,ZC,ZC,-1.0D0)   
        ENDDO
       ENDIF
      ENDIF
      IF(IROW.GT.1)THEN
       IF(TOP%X(ICOL,IROW-1).NE.TOP%NODATA)THEN
        CALL IDFGETLOC(TOP,IROW,ICOL,XC,YC)
        IF(TOP%IEQ.EQ.0)THEN; YC=YC+TOP%DY/2.0
        ELSE; YC=TOP%SY(IROW-1); ENDIF
        ZC=MAX(TOP%X(ICOL,IROW-1),TOP%X(ICOL,IROW)); DO 
         ZC=ZC-SPNT(I)%IDZ
         IF(ZC.LT.MIN(TOP%X(ICOL,IROW-1),TOP%X(ICOL,IROW)))EXIT
         CALL TRACEASSIGNSP(NPART,XC,YC,ZC,ZC,ZC,-1.0D0)   
        ENDDO
       ENDIF
      ENDIF
     ENDIF
    ENDIF
   ENDDO; ENDDO
   CALL IDFDEALLOCATEX(TOP)
  
  CASE (ID_POLYGON)

   YC=YCMAX+0.5D0*SPNT(I)%IDY
   DO
    YC=YC-SPNT(I)%IDY
    IF(YC.LT.YCMIN)EXIT
    XC=XCMIN-0.5D0*SPNT(I)%IDX
    DO
     XC=XC+SPNT(I)%IDX
     IF(XC.GT.XCMAX)EXIT
     IF(SPNT(I)%ISNAP.EQ.1)THEN
      CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
      CALL IDFGETLOC(IDF,IROW,ICOL,XCC,YCC)
     ELSE
      XCC=XC; YCC=YC
     ENDIF
     IF(DBL_IGRINSIDEPOLYGON(XCC,YCC,XCRD,YCRD,NCRD).EQ.1)CALL TRACEASSIGNSP(NPART,XCC,YCC,XTOP,XBOT,XREF,-1.0D0)
    ENDDO
   ENDDO

  CASE (ID_RECTANGLE)

   YC=YCMAX+0.5D0*SPNT(I)%IDY
   DO
    YC=YC-SPNT(I)%IDY
    IF(YC.LT.YCMIN)EXIT
    XC=XCMIN-0.5D0*SPNT(I)%IDX
    DO
     XC=XC+SPNT(I)%IDX
     IF(XC.GT.XCMAX)EXIT
     IF(SPNT(I)%ISNAP.EQ.1)THEN
      CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
      CALL IDFGETLOC(IDF,IROW,ICOL,XCC,YCC)
     ELSE
      XCC=XC; YCC=YC
     ENDIF
     IF(XCC.GE.MIN(XCRD(1),XCRD(2)).AND.XCC.LE.MAX(XCRD(1),XCRD(2)).AND. &
        YCC.GE.MIN(YCRD(1),YCRD(2)).AND.YCC.LE.MAX(YCRD(1),YCRD(2)))THEN
      CALL TRACEASSIGNSP(NPART,XCC,YCC,XTOP,XBOT,XREF,-1.0D0)
     ENDIF
    ENDDO
   ENDDO

  CASE (ID_POINT,ID_CIRCLE)

   !## length between points on circle
   DIFFX=(2.0D0*PI*SPNT(I)%IRADIUS)/SPNT(I)%ISX
   !## number of points on circle (minimal = 1)
   NOR  =INT(DIFFX)+1

   GRAD2RAD=360.0D0/(2.0D0*PI)
   
   IF(SPNT(I)%ISHAPE.EQ.ID_POINT)N=NCRD
   IF(SPNT(I)%ISHAPE.EQ.ID_CIRCLE)N=1
   
   DO J=1,N
    OR=360.0D0/NOR
    IF(SPNT(I)%ISNAP.EQ.1)THEN
     CALL IDFIROWICOL(IDF,IROW,ICOL,XCRD(J),YCRD(J))
     CALL IDFGETLOC(IDF,IROW,ICOL,XCC,YCC)
    ELSE
     XCC=XCRD(J); YCC=YCRD(J)
    ENDIF
    DO K=1,NOR 
     CALL RANDOM_NUMBER(ZFRACTION)
     IF(K*OR.GT.360.0D0)EXIT
     DIFFX=SPNT(I)%IRADIUS*COS((K*OR)/GRAD2RAD)
     DIFFY=SPNT(I)%IRADIUS*SIN((K*OR)/GRAD2RAD)
     CALL TRACEASSIGNSP(NPART,XCC+DIFFX,YCC+DIFFY,XTOP,XBOT,XREF,-1.0D0)
    END DO

   END DO

  CASE (ID_LINE)

   CALL TRACEASSIGNSP(NPART,XCRD(1),YCRD(1),XTOP,XBOT,XREF,-1.0D0)
   DL=0.0D0
   DO J=2,NCRD
    DIFFX=XCRD(J)-XCRD(J-1)
    DIFFY=YCRD(J)-YCRD(J-1)
    TL   =SQRT(DIFFX**2.0D0+DIFFY**2.0D0)
    DIFFX=DIFFX/TL
    DIFFY=DIFFY/TL

    DO
     DL=DL+SPNT(I)%ISX
     IF(DL.LE.TL)THEN
      CALL TRACEASSIGNSP(NPART,XCRD(J-1)+DIFFX*DL,YCRD(J-1)+DIFFY*DL,XTOP,XBOT,XREF,-1.0D0)
     ELSE
      !## resterend deel
      DL=DL-TL-SPNT(I)%ISX
      EXIT
     ENDIF
    ENDDO
   ENDDO

 END SELECT

 TRACECOMPUTESP=.TRUE.

 END FUNCTION TRACECOMPUTESP

 !###======================================================================
 SUBROUTINE TRACEASSIGNSP(NPART,XC,YC,XTOP,XBOT,XREF,ZFRACTION)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: NPART
 REAL(KIND=DP_KIND),INTENT(IN) :: XC,YC,XTOP,XBOT,XREF,ZFRACTION
 INTEGER :: IZ,IROW,ICOL,ILAY,I
 REAL(KIND=DP_KIND) :: XT,XB,XR,THICKNESS
 REAL(KIND=DP_KIND) :: Z,DZ,ZL,DX,DY,DZZ

 !## location outside model dimensions
 IF(XC.LE.IDF%XMIN.OR.XC.GE.IDF%XMAX.OR. &
    YC.LE.IDF%YMIN.OR.YC.GE.IDF%YMAX)RETURN

 !## copy original
 XT=XTOP; XB=XBOT; XR=XREF

 IF(TOP%IU.GT.0)THEN
  CALL IDFIROWICOL(TOP,IROW,ICOL,XC,YC)
  XT=IDFGETVAL(TOP,IROW,ICOL)
 ENDIF
 IF(BOT%IU.GT.0)THEN
  CALL IDFIROWICOL(BOT,IROW,ICOL,XC,YC)
  XB=IDFGETVAL(BOT,IROW,ICOL)
 ENDIF
 IF(SPNT(1)%IREF.EQ.1.AND.REF%IU.GT.0)THEN
  CALL IDFIROWICOL(REF,IROW,ICOL,XC,YC)
  XR=IDFGETVAL(REF,IROW,ICOL)
  XT=XR+XT; XB=XR+XB
 ENDIF

 !## get current irow/icol for model-idf
 CALL IDFIROWICOL(IDF,IROW,ICOL,XC,YC)
 
 !## be sure that top>bot
 DX=MAX(XT,XB); DY=MIN(XT,XB); XT=DX; XB=DY

 !## overrule SPNT(1)%isz in case xtop<=xbot
 IZ=SPNT(1)%ISZ; IF(XT-XB.LE.0.0D0)IZ=1

 !## mid between xt-xb
 IF(IZ.EQ.1)THEN
  DZ=0.0D0
  Z =(XT+XB)/2.0
 ELSE 
  DZ=(XT-XB)/REAL(IZ-1)
  Z =XT
 ENDIF
 IF(ZFRACTION.GE.0.0D0)THEN
  IZ= 1
  DZ=(XT-XB)*ZFRACTION
  Z = XT-DZ
 ENDIF
 
 DO I=1,IZ
 !## determine in which layer location is situated, SKIP POSITIONS WITHIN CLAY-LAYERS!!!!
 NLAYLOOP: DO ILAY=1,NLAY
   !## within current aquifer - thickness > 0.0D0
   THICKNESS=ZTOP(ICOL,IROW,ILAY)-ZBOT(ICOL,IROW,ILAY)
   !## determine whether location is within active cell, SKIP INACTIVE CELLS!
   IF(THICKNESS.GT.0.0D0.AND.IBOUND(ICOL,IROW,ILAY).NE.0)THEN
    IF(Z.LE.ZTOP(ICOL,IROW,ILAY).AND.Z.GE.ZBOT(ICOL,IROW,ILAY))THEN
     !## compute local z
 !top (zl=1)
 !mid (zl=0.5)
 !bot (zl=0)
     DZZ=ZTOP(ICOL,IROW,ILAY)-ZBOT(ICOL,IROW,ILAY)
     IF(DZZ.EQ.0.0D0)ZL=0.0D0
     IF(DZZ.NE.0.0D0)ZL=(Z-ZBOT(ICOL,IROW,ILAY))/DZZ
     WRITE(JU,'(3(I10,A1),4(F15.3,A1),F10.2,A1,2(F15.3,A1))') ILAY,',',IROW,',',ICOL,',',XC-IDF%XMIN,',',YC-IDF%YMIN,',',Z,',',ZL,',',THICKNESS,',AQF,',XC,',',YC
     !## count number of particles
     NPART=NPART+1
     EXIT NLAYLOOP
    ENDIF

   ENDIF

    IF(ILAY.LT.NLAY)THEN
     !## within underlying aquitard - thickness > 0.0D0
     THICKNESS=ZBOT(ICOL,IROW,ILAY)-ZTOP(ICOL,IROW,ILAY+1)
     !## determine whether both locations are within active cell, SKIP INACTIVE CELLS!
     IF(THICKNESS.GT.0.0D0.AND.(IBOUND(ICOL,IROW,ILAY).NE.0.AND.IBOUND(ICOL,IROW,ILAY+1).NE.0))THEN 
      IF(Z.LE.ZBOT(ICOL,IROW,ILAY).AND.Z.GE.ZTOP(ICOL,IROW,ILAY+1))THEN
 !## compute local z
 !bot (zl=-1)
 !mid (zl=-0.5)
 !top (zl=-0)
       DZZ=ZBOT(ICOL,IROW,ILAY)-ZTOP(ICOL,IROW,ILAY+1)
       IF(DZZ.EQ.0.0D0)ZL=0.0D0
       IF(DZZ.NE.0.0D0)ZL=(ZTOP(ICOL,IROW,ILAY+1)-Z)/DZZ
       WRITE(JU,'(3(I10,A1),4(F15.3,A1),F10.2,A1,2(F15.3,A1))') ILAY,',',IROW,',',ICOL,',',XC-IDF%XMIN,',',YC-IDF%YMIN,',',Z,',',ZL,',',THICKNESS,',AQT,',XC,',',YC
       !## count number of particles
       NPART=NPART+1
       EXIT NLAYLOOP
      ENDIF
     ENDIF
    ENDIF

  ENDDO NLAYLOOP
  TPART=TPART+1
  Z    =Z-DZ
 ENDDO

 END SUBROUTINE TRACEASSIGNSP

END MODULE MOD_PLINES_SP

