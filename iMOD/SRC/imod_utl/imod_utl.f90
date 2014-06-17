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

MODULE MOD_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MODPLOT, ONLY : MP,MPW,LEGENDOBJ,MXCLR,MXCLASS
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_POLINT, ONLY : POL1LOCATE
USE IMODVAR, ONLY : MXTP,TP
USE MOD_OSD, ONLY : OSD_OPEN,OSD_GETENV

INTEGER,PARAMETER :: MXMESSAGE=16     !##max. number of messages
INTEGER,DIMENSION(MXMESSAGE) :: IMESSAGE

CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: LISTNAME
INTEGER,ALLOCATABLE,DIMENSION(:) :: ILIST
CHARACTER(LEN=2),PARAMETER :: NEWLINE=CHAR(13)//CHAR(10)

INTEGER,PARAMETER :: MAXLEN=52
CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:,:) :: VAR,VAR_TMP,DVAR
CHARACTER(LEN=MAXLEN),POINTER,DIMENSION(:) :: CCNST
INTEGER,ALLOCATABLE,DIMENSION(:) :: IVAR,ICOL_VAR,IACT_VAR
INTEGER :: NV,NL,IV  !## max. variables/max. lines

TYPE PROCOBJ
 INTEGER,DIMENSION(2) :: IFLAGS
 INTEGER :: ID
 CHARACTER(LEN=52) :: CID
END TYPE PROCOBJ

CONTAINS

 !###======================================================================
 INTEGER FUNCTION UTL_GETIDPROC(PROC,ICLEAN)
 !###======================================================================
 IMPLICIT NONE
 TYPE(PROCOBJ),INTENT(INOUT),POINTER,DIMENSION(:) :: PROC
 TYPE(PROCOBJ),POINTER,DIMENSION(:) :: PROC_BU
 INTEGER,INTENT(IN) :: ICLEAN
 INTEGER :: I,J,N,ISTATUS,IEXCOD
 CHARACTER(LEN=256) :: STRING
 INTEGER,DIMENSION(2) :: PID
 
 IF(ASSOCIATED(PROC))THEN
  !## evaluate current status
  DO I=1,SIZE(PROC)
   PID=PROC(I)%ID

   !## not running free, process
   IF(ISTATUS.EQ.0)THEN
    !## non-blocked process not stopped correctly
    IF(PROC(I)%IFLAGS(2).EQ.0.AND.IEXCOD.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,TRIM(STRING)//CHAR(13)//'in program:'//TRIM(PROC(I)%CID),'Program Terminated')
    ENDIF
    PROC(I)%ID=0; PROC(I)%CID=''; PROC(I)%IFLAGS=0
   !## process is still running
   ELSEIF(ISTATUS.EQ.1)THEN
   
   ENDIF
  ENDDO
 ELSE
  ALLOCATE(PROC(1)); PROC(1)%ID=0; PROC(1)%CID=''; PROC(1)%IFLAGS=0
 ENDIF

 N=SIZE(PROC)

 !## clean 
 J=0; DO I=1,N
  IF(PROC(I)%ID.NE.0)THEN
   J=J+1; IF(I.NE.J)THEN; PROC(J)=PROC(I); ENDIF
  ENDIF
 ENDDO
 DO I=J+1,N; PROC(I)%ID=0; PROC(I)%CID=''; PROC(I)%IFLAGS=0; ENDDO

 !## find empty spot
 DO I=1,SIZE(PROC); IF(PROC(I)%ID.EQ.0)EXIT; ENDDO; N=I
 IF(ICLEAN.EQ.1)N=I-1
   
 IF(N.EQ.0)THEN
  IF(ASSOCIATED(PROC))DEALLOCATE(PROC)
 ELSE
  IF(N.NE.SIZE(PROC))THEN
   ALLOCATE(PROC_BU(N)); DO I=1,N;   PROC_BU(I)=PROC(I); ENDDO; DEALLOCATE(PROC)
   ALLOCATE(PROC(N+1));  DO I=1,N+1; PROC(I)%ID=0; PROC(I)%CID=''; PROC(I)%IFLAGS=0; ENDDO
   DO I=1,N; PROC(I)=PROC_BU(I); ENDDO; DEALLOCATE(PROC_BU)
  ENDIF
 ENDIF
 
 UTL_GETIDPROC=N
 
 END FUNCTION UTL_GETIDPROC

 !###======================================================================
 SUBROUTINE UTL_DELSPACE(LINE1,LINE2)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: LINE1
 CHARACTER(LEN=*),INTENT(OUT) :: LINE2
 INTEGER :: I,J,K
 
 LINE2=''; K=0
 J=0; DO I=1,LEN_TRIM(LINE1)
  IF(LINE1(I:I).EQ.CHAR(34).OR.LINE1(I:I).EQ.CHAR(39))THEN
   K=ABS(K-1)
  ENDIF
  !## copy non-spaces or inside quotes
  IF(LINE1(I:I).NE.CHAR(32).OR.K.EQ.1)THEN
   J=J+1; LINE2(J:J)=LINE1(I:I)
  ENDIF
 ENDDO
 
 END SUBROUTINE UTL_DELSPACE
 
 !###======================================================================
 LOGICAL FUNCTION UTL_DATA_CSV(TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: TXT
 INTEGER :: I,J,ITYPE,NP
 CHARACTER(LEN=256) :: FNAME
 TYPE(WIN_MESSAGE) :: MESSAGE

 UTL_DATA_CSV=.FALSE.
 NP=SIZE(TXT)
 
 IF(.NOT.UTL_WSELECTFILE('Load Comma Separated File (*.csv)|*.csv|',&
                  LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                  'Load Comma Separated File (*.csv)'))RETURN

 CALL UTL_GENLABELSREAD(FNAME)
 IF(NV.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read column info (header) from file!','Error'); RETURN
 ENDIF
 
 CALL WDIALOGLOAD(ID_READCSV,ID_READCSV)
 IF(SIZE(TXT).GT.WINFOGRID(IDF_GRID1,GRIDROWSMAX))THEN
  CALL WDIALOGUNLOAD()
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read more than '//TRIM(ITOS(WINFOGRID(IDF_GRID1,GRIDROWSMAX)))// &
  ' columns in this iMOD version','Error'); RETURN 
 ENDIF
  
 CALL WGRIDROWS(IDF_GRID1,NP)
 
 !## put parameters
 CALL WGRIDPUTSTRING(IDF_GRID1,2,TXT,NP)

 !## assign variable to parameter
 IF(ALLOCATED(IACT_VAR))DEALLOCATE(IACT_VAR); ALLOCATE(IACT_VAR(NP))
 IACT_VAR=1
 CALL WGRIDPUTCHECKBOX(IDF_GRID1,1,IACT_VAR,NP)
 IF(ALLOCATED(ICOL_VAR))DEALLOCATE(ICOL_VAR); ALLOCATE(ICOL_VAR(NP))
 J=0; DO I=1,NP; J=J+1; IF(J.GT.NV)J=1; ICOL_VAR(I)=J; ENDDO
 CALL WGRIDPUTMENU(IDF_GRID1,3,VAR(:,0),NV,ICOL_VAR,NP)
 IF(ASSOCIATED(CCNST))DEALLOCATE(CCNST); ALLOCATE(CCNST(NP))
 CCNST=''
 CALL WGRIDPUTSTRING(IDF_GRID1,4,CCNST,NP) 
  
 CALL WDIALOGSHOW(-1,-1,0,3)
 
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (IDOK)
      CALL WGRIDGETCHECKBOX(IDF_GRID1,1,IACT_VAR,NP)
      CALL WGRIDGETMENU(IDF_GRID1,3,ICOL_VAR,NP)
      CALL WGRIDGETSTRING(IDF_GRID1,4,CCNST,NP)
      EXIT
     CASE (IDHELP)  
      CALL IMODGETHELP('2.5.10','CSV Files')
     CASE (IDCANCEL)
      EXIT
    END SELECT
   CASE (FIELDCHANGED)
   
  END SELECT
 ENDDO
 
 CALL WDIALOGUNLOAD()
 
 UTL_DATA_CSV=.TRUE.
 
 END FUNCTION UTL_DATA_CSV

 !###======================================================================
 SUBROUTINE UTL_GENLABELSGET(ID,JL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER,INTENT(OUT) :: JL
 INTEGER :: SC,IOS,JD
 CHARACTER(LEN=100) :: STRING
 
 JL=0; IF(NV.LE.0.OR.NL.LE.0)RETURN

 SC=1  !## search column

 !## evaluate the first
 DO JL=1,NL
  STRING=VAR(SC,JL)
  READ(STRING,*,IOSTAT=IOS) JD
  !## math found
  IF(IOS.EQ.0.AND.ID.EQ.JD)RETURN
 END DO
 JL=0
 
 END SUBROUTINE UTL_GENLABELSGET

 !###======================================================================
 SUBROUTINE UTL_GENLABELSREAD(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: CFN_N_ELEM,CFN_ELEM_POS,CFN_UNQUOTE
 INTEGER,ALLOCATABLE,DIMENSION(:) :: BPV,EPV
 INTEGER :: ML,I,J,INCL,IOS,IU
 CHARACTER(LEN=1256) :: STRING

 !## initialize table of data for gen polygons
 NV  =0
 NL  =0
 INCL=50

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not OPEN/READ associated data file called:'//CHAR(13)//&
    TRIM(FNAME),'Error')
  RETURN
 ENDIF
 
 !## get number of variables
 READ(IU,'(A1256)',IOSTAT=IOS) STRING
 IF(IOS.NE.0)RETURN
 IF(LEN_TRIM(STRING).EQ.0)RETURN

 NV=CFN_N_ELEM(' ,;',3,STRING)
 ALLOCATE(BPV(NV)); ALLOCATE(EPV(NV))
 ALLOCATE(VAR(NV,0:INCL))
 ML=INCL

 !## read the rest of the table in order to fill var(), 0=label
 NL=-1
 DO
  NL=NL+1
  IF(NL.GT.ML)THEN
   ALLOCATE(DVAR(NV,0:ML+INCL))
   !## copy current part
   DO I=1,SIZE(VAR,1); DO J=0,ML; DVAR(I,J)=VAR(I,J); ENDDO; ENDDO
   DEALLOCATE(VAR)
   VAR=>DVAR
   ML=ML+INCL
   NULLIFY(DVAR)
  ENDIF
  !## get variables
  I=CFN_ELEM_POS(NV,' ,;',3,STRING,1000,BPV,EPV)
  DO I=1,NV
   VAR(I,NL)=''
   IF(BPV(I).LE.LEN(STRING).AND. &
      BPV(I).LE.EPV(I))THEN
       !## maximize lengt of variable
       J=(EPV(I)-BPV(I))+1; EPV(I)=BPV(I)+MIN(MAXLEN,J)-1
       VAR(I,NL)=STRING(BPV(I):EPV(I))
    IF(CFN_UNQUOTE(VAR(I,NL)).LE.0)VAR(I,NL)=''
   ENDIF
  END DO
  READ(IU,'(A1256)',IOSTAT=IOS) STRING
  IF(IOS.NE.0)EXIT
  IF(LEN_TRIM(STRING).EQ.0)EXIT
 ENDDO
 CLOSE(IU)
 IF(ALLOCATED(BPV))DEALLOCATE(BPV); IF(ALLOCATED(EPV))DEALLOCATE(EPV)

 END SUBROUTINE UTL_GENLABELSREAD

 !###======================================================================
 SUBROUTINE UTL_GENLABELSWRITE(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: IU,IOS,I,J
 CHARACTER(LEN=512) :: LINE
 
 !## nothing to write
 IF(NL.LE.0)RETURN
 IF(.NOT.ASSOCIATED(VAR))RETURN
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not OPEN/WRITE associated data file called:'//CHAR(13)//&
    TRIM(FNAME),'Error')
  RETURN
 ENDIF

 DO I=0,NL
  LINE=TRIM(VAR(1,I))
  DO J=2,NV; LINE=TRIM(LINE)//','//TRIM(VAR(J,I)); END DO
  WRITE(IU,'(A)') TRIM(LINE)
 ENDDO 
 CLOSE(IU)
  
 END SUBROUTINE UTL_GENLABELSWRITE

 !###======================================================================
 SUBROUTINE UTL_GENLABELSDEALLOCATE()
 !###======================================================================
 IMPLICIT NONE

 IF(ASSOCIATED(VAR))DEALLOCATE(VAR)
 IF(ALLOCATED(IVAR))DEALLOCATE(IVAR)
 IF(ALLOCATED(ICOL_VAR))DEALLOCATE(ICOL_VAR)
 IF(ALLOCATED(IACT_VAR))DEALLOCATE(IACT_VAR)
 IF(ASSOCIATED(CCNST))DEALLOCATE(CCNST)
 
 END SUBROUTINE UTL_GENLABELSDEALLOCATE

 !###======================================================================
 REAL FUNCTION UTL_POLYGON1AREA(X,Y,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN),DIMENSION(N) :: X,Y
 INTEGER :: I

 UTL_POLYGON1AREA=0.0
 DO I=1,N-1
  UTL_POLYGON1AREA=UTL_POLYGON1AREA+0.5*((X(I)*Y(I+1))-(X(I+1)*Y(I)))
 END DO
 UTL_POLYGON1AREA=UTL_POLYGON1AREA+0.5*((X(N)*Y(1))-(X(1)*Y(N)))

 END FUNCTION UTL_POLYGON1AREA

 !###======================================================================
 INTEGER FUNCTION UTL_INSIDEPOLYGON(PX,PY,XX,YY,XPOL,YPOL,N)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN) :: PX,PY
 REAL,DIMENSION(N),INTENT(IN) :: XX,YY
 REAL,DIMENSION(N),INTENT(INOUT) :: XPOL,YPOL
 
 UTL_INSIDEPOLYGON=-1
 IF(IGRINSIDEPOLYGON(XX,YY,N,PX,PY))UTL_INSIDEPOLYGON=1
 
 END FUNCTION UTL_INSIDEPOLYGON
 
! !###======================================================================
! INTEGER FUNCTION UTL_INSIDEPOLYGON2(PX,PY,XX,YY,X,Y,N)
! !###======================================================================
! !
! !code extracted from the internet:
! !http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html#Fortran%20Code%20for%20the%20Point%20in%20Polygon%20Test
! !
! !    PURPOSE TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
! !
! !    DESCRIPTION OF THE PARAMETERS
! !       PX      - X-COORDINATE OF POINT IN QUESTION.
! !       PY      - Y-COORDINATE OF POINT IN QUESTION.
! !       XX      - N LONG VECTOR CONTAINING X-COORDINATES OF
! !                 VERTICES OF POLYGON.
! !       YY      - N LONG VECTOR CONTAING Y-COORDINATES OF
! !                 VERTICES OF POLYGON.
! !       N       - NUMBER OF VERTICES IN THE POLYGON.
! !       UTL_INSIDEPOLYGON   - THE SIGNAL RETURNED:
! !                 -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
! !                  0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
! !                  1 IF THE POINT IS INSIDE OF THE POLYGON.
!
! !    REMARKS
! !       THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
! !       THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
! !       OPTIONALLY BE INCREASED BY 1.
! !       THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
! !       OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
! !       OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
! !       N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
! !       INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
! !       THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
! !       WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.
! !
! !
! !    METHOD
! !       A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT
! !       CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE
! !       POINT IS INSIDE OF THE POLYGON.
! !
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: N
! REAL,INTENT(IN) :: PX,PY
! REAL,DIMENSION(N),INTENT(IN) :: XX,YY
! REAL,DIMENSION(N),INTENT(INOUT) :: X,Y
! LOGICAL :: MX,MY,NX,NY
! INTEGER :: I,J
!
! DO I=1,N
!  X(I)=XX(I)-PX
!  Y(I)=YY(I)-PY
! ENDDO
! UTL_INSIDEPOLYGON2=-1
! DO I=1,N
!  J =1+MOD(I,N)
!  MX=X(I).GE.0.0
!  NX=X(J).GE.0.0
!  MY=Y(I).GE.0.0
!  NY=Y(J).GE.0.0
!  IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX))THEN
! !  GO TO 2
!  ELSE
!   IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX)))THEN
! !    GO TO 3
!    IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).LT.0.0)THEN
!    ! GOTO2
!    ELSEIF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).EQ.0.0)THEN
!     UTL_INSIDEPOLYGON2=0
!     EXIT 
!    ELSEIF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)).GT.0.0)THEN
!     UTL_INSIDEPOLYGON2=-UTL_INSIDEPOLYGON2
!    ENDIF
!   ELSE
!    UTL_INSIDEPOLYGON2=-UTL_INSIDEPOLYGON2
! !   GO TO 2
!   ENDIF
!  ENDIF
! ENDDO
!
! END FUNCTION UTL_INSIDEPOLYGON2

 !###======================================================================
 SUBROUTINE UTL_STDEF(X,N,NODATA,VAR,XT,NPOP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NPOP
 REAL,DIMENSION(N),INTENT(IN) :: X
 REAL,INTENT(IN) :: NODATA
 REAL,INTENT(OUT) :: XT,VAR
 INTEGER :: I
 REAL :: XV

 VAR=0.0

 NPOP=0
 XT=0.0
 DO I=1,N
  IF(X(I).NE.NODATA)THEN
   NPOP=NPOP+1
   XT=XT+X(I)
  ENDIF
 ENDDO

 IF(NPOP.LE.0)RETURN

 XT=XT/REAL(NPOP)

 NPOP=0
 XV=0.0
 DO I=1,N
  IF(X(I).NE.NODATA)THEN
   NPOP=NPOP+1
   XV=XV+(X(I)-XT)**2.0
  ENDIF
 END DO

 IF(XV.LE.0.0)RETURN
 VAR=SQRT(XV/REAL(NPOP))

 END SUBROUTINE UTL_STDEF

 !###======================================================================
 REAL FUNCTION UTL_DIST(X1,Y1,X2,Y2)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: X1,Y1,X2,Y2
 REAL :: DX,DY
 
 UTL_DIST=0.0
 
 DX=(X1-X2)**2.0
 DY=(Y1-Y2)**2.0
 IF(DX+DY.NE.0.0)UTL_DIST=SQRT(DX+DY)
 
 END FUNCTION UTL_DIST
 
 !###======================================================================
 LOGICAL FUNCTION UTL_WSELECTFILE(FILTERSTR,IFLAGS,FILEDIR,TITLE) !,IFTYPE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FILTERSTR
 CHARACTER(LEN=*),INTENT(INOUT) :: FILEDIR
 INTEGER,INTENT(IN) :: IFLAGS
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: TITLE
 INTEGER :: I,J
 CHARACTER(LEN=10) :: EXT
 
 UTL_WSELECTFILE=.FALSE.
 
 DO
 
  IF(PRESENT(TITLE))THEN
   CALL WSELECTFILE(FILTERSTR,IFLAGS,FILEDIR,TITLE)
  ELSE
   CALL WSELECTFILE(FILTERSTR,IFLAGS,FILEDIR)
  ENDIF
  IF(WINFODIALOG(4).NE.1)THEN
   FILEDIR=''
   RETURN
  ENDIF
  !## check extent ...
  I=INDEX(FILEDIR,'.',.TRUE.) 
  IF(I.EQ.0)EXIT
  
  EXT=FILEDIR(I+1:)
  J=INDEX(UTL_CAP(FILTERSTR(1:MIN(256,LEN(FILTERSTR))),'U'),'*.'//TRIM(UTL_CAP(EXT,'U')))
  IF(J.NE.0)EXIT

  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'You should select a file that agrees the supplied filterstring:'//CHAR(13)// &
   TRIM(FILTERSTR),'Error')
 
 ENDDO
 
 UTL_WSELECTFILE=.TRUE.
 
 END FUNCTION UTL_WSELECTFILE

 !###======================================================================
 SUBROUTINE IDFPLOT1BITMAP()
 !###======================================================================
 IMPLICIT NONE

 CALL IGRSELECT(DRAWBITMAP,MPW%IBITMAP)
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

 END SUBROUTINE IDFPLOT1BITMAP

 !###======================================================================
 SUBROUTINE IDFPLOT2BITMAP()
 !###======================================================================
 IMPLICIT NONE

 CALL IGRSELECT(DRAWWIN)
 CALL WINDOWSELECT(MPW%IWIN)
 CALL WBITMAPVIEW(MPW%IBITMAP,MPW%IX,MPW%IY,MODELESS) !,KEYSCROLL)
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

 END SUBROUTINE IDFPLOT2BITMAP

 !###======================================================================
 SUBROUTINE IDFMEM2BITMAP()
 !###======================================================================
 IMPLICIT NONE

 CALL IGRSELECT(DRAWWIN)
 CALL WINDOWSELECT(MPW%IWIN)
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

 END SUBROUTINE IDFMEM2BITMAP

 !###======================================================================
 FUNCTION REALTOSTRING(X)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=20) :: REALTOSTRING
 REAL,INTENT(IN) :: X
 INTEGER :: I

 WRITE(REALTOSTRING,*) X
 !eliminate all zero at the end!
 DO I=LEN_TRIM(REALTOSTRING),1,-1
  IF(REALTOSTRING(I:I).NE.'0')EXIT
 END DO
 IF(REALTOSTRING(I:I).EQ.'.'.OR.REALTOSTRING(I:I).EQ.',')I=I-1
 REALTOSTRING=REALTOSTRING(1:I)

 END FUNCTION REALTOSTRING

 !###======================================================================
 INTEGER FUNCTION UTL_IDFGETCLASS(LEG,GRD)
 !###======================================================================
 IMPLICIT NONE
 TYPE(LEGENDOBJ),INTENT(IN) :: LEG
 REAL,INTENT(IN) :: GRD
 INTEGER :: I

 !## default=wit!
 UTL_IDFGETCLASS=WRGB(255,255,255)

 CALL POL1LOCATE(LEG%CLASS,LEG%NCLR,REAL(GRD,8),I)
 !## correct if equal to top-class boundary
 IF(I.GT.0.AND.I.LE.MXCLR)THEN
  UTL_IDFGETCLASS=LEG%RGB(I)
 ELSE
  IF(EQUALS_REAL(GRD,LEG%CLASS(0)))UTL_IDFGETCLASS=LEG%RGB(1)
 ENDIF
 
 END FUNCTION UTL_IDFGETCLASS

 !###======================================================================
 SUBROUTINE UTL_IDFCURDIM(XMIN,YMIN,XMAX,YMAX,IDF,NC1,NC2,NR1,NR2)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(INOUT) :: IDF
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 REAL :: D
 INTEGER,INTENT(OUT) :: NC1,NC2,NR1,NR2

 IF(IDF%IEQ.EQ.0)THEN

  !## min. column
  D  =XMIN-IDF%XMIN
  NC1=INT(D/IDF%DX)+1
  IF(MOD(D,IDF%DX).NE.0.0)NC1=NC1+1
  !## max. column
  D  =XMAX-IDF%XMIN
  NC2=INT(D/IDF%DX)
  !## min. row
  D  =IDF%YMAX-YMAX
  NR1=INT(D/IDF%DY)+1
  IF(MOD(D,IDF%DY).NE.0.0)NR1=NR1+1
  !## max. row
  D  =IDF%YMAX-YMIN
  NR2=INT(D/IDF%DY)

 ELSE

  !## min. column
  CALL POL1LOCATE(IDF%SX,IDF%NCOL+1,REAL(XMIN,8),NC1)
  !## max. column
  CALL POL1LOCATE(IDF%SX,IDF%NCOL+1,REAL(XMAX,8),NC2)
  !## min. row
  CALL POL1LOCATE(IDF%SY,IDF%NROW+1,REAL(YMAX,8),NR1)
  !## max. row
  CALL POL1LOCATE(IDF%SY,IDF%NROW+1,REAL(YMIN,8),NR2)

 ENDIF

 NC1=MAX(1,NC1)
 NC1=MIN(NC1,IDF%NCOL)
 NC2=MAX(1,NC2)
 NC2=MIN(NC2,IDF%NCOL)
 NR1=MAX(1,NR1)
 NR1=MIN(NR1,IDF%NROW)
 NR2=MAX(1,NR2)
 NR2=MIN(NR2,IDF%NROW)

 IF(IDF%IEQ.EQ.1)THEN
  IF(MPW%XMIN.GT.IDF%SX(NC1-1))NC1=NC1+1
  IF(MPW%XMAX.LT.IDF%SX(NC2))NC2=NC2-1
  IF(MPW%YMAX.LT.IDF%SY(NR1-1))NR1=NR1+1
  IF(MPW%YMIN.GT.IDF%SY(NR2))NR2=NR2-1
 ENDIF

 END SUBROUTINE UTL_IDFCURDIM

 !###======================================================================
 SUBROUTINE UTL_IDFCRDCOR(X1,X2,Y1,Y2,WIDTH,HEIGTH)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: WIDTH,HEIGTH
 REAL,INTENT(INOUT) :: X1,X2,Y1,Y2
 REAL :: RAT1,RAT2,X,Y,XLEN,YLEN

 RAT1=WIDTH/HEIGTH
 X=X2-X1
 Y=Y2-Y1
 RAT2=X/Y
 IF(RAT2.LT.RAT1)THEN
  YLEN=Y2-Y1
  XLEN=YLEN*RAT1
  X1=X1-((XLEN-X)/2.)
  X2=X1+XLEN
 ELSE
  XLEN=X2-X1
  YLEN=XLEN/RAT1
  Y1=Y1-((YLEN-Y)/2.)
  Y2=Y1+YLEN
 ENDIF

 END SUBROUTINE UTL_IDFCRDCOR

 !###======================================================================
 SUBROUTINE UTL_FILLARRAY(IP,NP,B)
 !###======================================================================
 !# read binair number (e.g. 256) and returns array (/1,0,0,1,0,0,1/)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP,B
 INTEGER,INTENT(OUT),DIMENSION(NP) :: IP
 INTEGER :: I,BB

 IP=0
 BB=B
 DO I=1,NP
  IP(I)=MOD(BB,2)
  BB=BB/2
 END DO

 !## make sure results are only 0/1 values
 DO I=1,NP
  IF(IP(I).LT.0.OR.IP(I).GT.1)IP(I)=0
 END DO

 END SUBROUTINE UTL_FILLARRAY

 !###======================================================================
 SUBROUTINE UTL_READARRAY(IP,NP,B)
 !###======================================================================
 !# write a binair-number given an array (/1,0,0,4,0,0,7/)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NP
 INTEGER,INTENT(OUT) :: B
 INTEGER,INTENT(IN),DIMENSION(NP) :: IP
 INTEGER :: I,J

 B=0
 DO I=1,NP
  J=MAX(0,MIN(IP(I),1))
  B=B+(J*(2**(I-1)))
 END DO

 END SUBROUTINE UTL_READARRAY

 !###======================================================================
 LOGICAL FUNCTION UTL_READINITFILE(CKEY,LINE,IU,IOPT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IOPT
 CHARACTER(LEN=*),INTENT(IN) :: CKEY
 CHARACTER(LEN=*),INTENT(OUT) :: LINE
 INTEGER :: IOS,I,J,N,M,II,ITRY
 CHARACTER(LEN=7) :: FRMT
 CHARACTER(LEN=256) :: FNAME

 UTL_READINITFILE=.FALSE.

 N=LEN(LINE)
 WRITE(FRMT,'(A2,I4.4,A1)') '(A',N,')'

 !## read from current position, if not found try from beginning
 ITRY=1
 DO
  READ(IU,FRMT,IOSTAT=IOS) LINE
  IF(IOS.NE.0)THEN
   IF(ITRY.EQ.2)THEN
    IF(IOPT.EQ.0)THEN
     INQUIRE(UNIT=IU,NAME=FNAME)
     CLOSE(IU)
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD DID NOT find keyword: ['//TRIM(CKEY)//'] within Settings file:'// &
           CHAR(13)//'[ '//TRIM(FNAME)//' ]','Error')
    ENDIF
    RETURN
   ELSE
    REWIND(IU)
    ITRY=ITRY+1
    CYCLE
   ENDIF
  ENDIF

  N=LEN(LINE)
  M=256  !## length of utl_cap() function
  !## split cap function in case
  IF(N.GT.M)THEN
   I=1; J=I+M-1
   DO
    LINE(I:J)=UTL_CAP(LINE(I:J),'U')
    I=I+M
    IF(I.GT.N)EXIT
    J=MIN(N,I+M-1)
   ENDDO
  ELSE
   LINE=UTL_CAP(LINE,'U')
  ENDIF

  II=INDEX(TRIM(LINE),'!') !## skip comment lines for keyword
  I =INDEX(TRIM(LINE),TRIM(CKEY))
  IF(II.EQ.0)II=I
  !## okay, proper line found
  IF(I.NE.0.AND.II.GE.I)THEN
   !## make sure previous to i or j no character is available
   IF(I.GE.2)THEN
    IF(LINE(I-1:I-1).NE.' ')I=0 !## not correct
   ENDIF
   !## make sure next to i or j no character or "=" sign
   IF(LINE(I+LEN_TRIM(CKEY):I+LEN_TRIM(CKEY)).NE.' '.AND. &
      LINE(I+LEN_TRIM(CKEY):I+LEN_TRIM(CKEY)).NE.'=')I=0 !## not correct 
   J=INDEX(TRIM(LINE),'=')
   IF(I.NE.0.AND.J.GT.I)EXIT 
  ENDIF
 ENDDO
 
 I=INDEX(LINE,'=')
 IF(I.LE.0)THEN
  INQUIRE(UNIT=IU,NAME=FNAME)
  CLOSE(IU)
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD misses "=" after keyword: ['//TRIM(CKEY)//'] within Settings file:'// &
             CHAR(13)//'[ '//TRIM(FNAME)//' ]','Error')
  RETURN
 ENDIF

 I=I+1
 LINE(1:N-I+1)=LINE(I:N)
 
 UTL_READINITFILE=.TRUE.

 END FUNCTION UTL_READINITFILE

 !###====================================================================
 SUBROUTINE UTL_DRAWLEGENDBOX(XMIN,YMIN,XMAX,YMAX,ICLR,IWIDTH,ITYPE,IPATTERN,LEG,XT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPATTERN !## 0=solid,1=line,2=dots
 INTEGER,INTENT(IN) :: ICLR,IWIDTH,ITYPE
 REAL,INTENT(IN) :: XMIN,XMAX,YMAX
 REAL,INTENT(INOUT) :: YMIN
 REAL,INTENT(IN),OPTIONAL :: XT
 TYPE(LEGENDOBJ),INTENT(INOUT),OPTIONAL :: LEG
 REAL :: DX,DY,Y
 INTEGER :: I

 !## solid
 IF(IPATTERN.EQ.0)THEN

  CALL IGRCOLOURN(ICLR)
  CALL IGRFILLPATTERN(SOLID)
  CALL IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

 !## lines
 ELSEIF(IPATTERN.EQ.1)THEN

  !## clear it (white)
  CALL IGRFILLPATTERN(SOLID)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

  CALL IGRCOLOURN(ICLR)
  CALL IGRFILLPATTERN(OUTLINE)

  CALL IGRLINETYPE(ITYPE)
  CALL IGRLINEWIDTH(IWIDTH)
  CALL IGRMOVETO(XMIN,YMIN)
  DX=(XMAX-XMIN)/3.0
  DY=(YMAX-YMIN)

  CALL IGRLINETOREL(DX, DY)
  CALL IGRLINETOREL(DX,-DY)
  CALL IGRLINETOREL(DX, DY)

 !## dots
 ELSEIF(IPATTERN.EQ.2)THEN

  !## clear it (white)
  CALL IGRFILLPATTERN(SOLID)
  CALL IGRCOLOURN(WRGB(255,255,255))
  CALL IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

  DY=(XMAX-XMIN)/10.0
  CALL IGRCOLOURN(ICLR)
  CALL IGRFILLPATTERN(SOLID)
  DX=(XMAX-XMIN)/4.0
  DO I=1,3
   CALL IGRCIRCLE(XMIN+(DX*REAL(I)),(YMAX+YMIN)/2.0,DY)
  END DO

 !## filled in (a) if present with legend (b) stripes
 ELSEIF(IPATTERN.EQ.3)THEN

  CALL IGRFILLPATTERN(SOLID)
  
  !## use a legend if present
  IF(PRESENT(LEG))THEN
   IF(LEG%NCLR.GT.MXCLASS)THEN; DY=(3.0*(YMAX-YMIN))/REAL(LEG%NCLR); ELSE; DY=YMAX-YMIN; ENDIF; Y=YMAX
   DO I=1,LEG%NCLR
    CALL IGRCOLOURN(LEG%RGB(I)); CALL IGRRECTANGLE(XMIN,Y-DY,XMAX,Y)
    IF(LEG%NCLR.LE.MXCLASS)THEN
     CALL IGRCOLOURN(WRGB(0,0,0)); CALL WGRTEXTSTRING(XT,Y-(DY/2.0),TRIM(LEG%LEGTXT(I)))
    ELSE
     CALL IGRCOLOURN(WRGB(0,0,0))
     IF(I.EQ.1)       CALL WGRTEXTSTRING(XT,YMAX-(0.5*(YMAX-YMIN)),TRIM(LEG%LEGTXT(I)))
     IF(I.EQ.LEG%NCLR)CALL WGRTEXTSTRING(XT,YMAX-(2.5*(YMAX-YMIN)),TRIM(LEG%LEGTXT(I)))
    ENDIF
    Y=Y-DY
   END DO 
   YMIN=Y  
  ELSE
   DX=(XMAX-XMIN)/10.0
   DO I=1,9
    IF(MOD(I,2).EQ.0)CALL IGRCOLOURN(ICLR)
    IF(MOD(I,2).NE.0)CALL IGRCOLOURN(WRGB(255,255,255))
    CALL IGRRECTANGLE(XMIN+(DX*REAL(I-1)),YMIN,XMIN+(DX*REAL(I)),YMAX)
   END DO
  ENDIF

 ENDIF

 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRLINEWIDTH(1)
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRRECTANGLE(XMIN,YMIN,XMAX,YMAX)

 CALL IGRCOLOURN(ICLR)

 END SUBROUTINE

 !###====================================================================
 SUBROUTINE UTL_GETRELEVANTDIR(DIRNAMES,NDIR)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDIR
 CHARACTER(LEN=*),INTENT(INOUT),DIMENSION(NDIR) :: DIRNAMES
 INTEGER :: I,II,JJ,J

 !## nothing to do
 IF(NDIR.LE.0)RETURN

 IF(NDIR.EQ.1)THEN
  I=INDEX(DIRNAMES(1),'\',.TRUE.)
  IF(I.NE.0)DIRNAMES(1)='..\'//DIRNAMES(1)(I+1:)
  RETURN
 ENDIF

 DO I=1,NDIR
  DIRNAMES(I)=UTL_CAP(DIRNAMES(I),'U')
 END DO
 II=0
 JJ=0
 DO WHILE(JJ.EQ.0)
  II=II+1
  DO I=1,NDIR
   DO J=1,NDIR
    IF(DIRNAMES(I)(II:II).NE.DIRNAMES(J)(II:II).AND.JJ.EQ.0)JJ=II!EXIT! LOOPII
   END DO
  END DO
 ENDDO 
 DO I=1,NDIR
  J=INDEX(DIRNAMES(I)(:II),'\',.TRUE.)
  IF(J.NE.0)DIRNAMES(I)='..\'//DIRNAMES(I)(J+1:)
 ENDDO

 END SUBROUTINE UTL_GETRELEVANTDIR

 !###====================================================================
 SUBROUTINE UTL_GETDIRPART(IPART,DIR,DIRPART)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPART
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 CHARACTER(LEN=*),INTENT(OUT) :: DIRPART
 INTEGER :: I,J,K

 DIRPART=''

 IF(IPART.EQ.0)THEN
  J=INDEX(DIR,':')
  IF(J.EQ.0)RETURN
  DIRPART=DIR(:J-1)
 ELSE
  K=1
  DO I=1,IPART
   J=INDEX(DIR(K:),'\')
   !## nothing found for current ipart
   IF(J.EQ.0)RETURN
   K=J+1
  ENDDO
  J=INDEX(DIR(K:),'\')
  IF(J.NE.0)DIRPART=DIR(K:J-1)
 ENDIF

 END SUBROUTINE UTL_GETDIRPART

 !###====================================================================
 SUBROUTINE UTL_SETTEXTSIZE(CHW,CHH,DY)
 !###====================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: DY
 REAL,INTENT(OUT) :: CHW,CHH
 REAL :: IWD,IHD,X1,X2,Y1,Y2,RAT

 IWD=WINFODRAWABLE(DRAWABLEWIDTH)
 IHD=WINFODRAWABLE(DRAWABLEHEIGHT)
 X1 =INFOGRAPHICS(GRAPHICSAREAMINX)
 X2 =INFOGRAPHICS(GRAPHICSAREAMAXX)
 Y1 =INFOGRAPHICS(GRAPHICSAREAMINY)
 Y2 =INFOGRAPHICS(GRAPHICSAREAMAXY)

 CHH=DY 
 CHW=DY/(0.03333/0.01333)

 RAT=IWD/IHD
 CHW=CHW/RAT

 RAT=(X2-X1)/(Y2-Y1)
 CHW=CHW/RAT

 END SUBROUTINE UTL_SETTEXTSIZE

 !###======================================================================
 SUBROUTINE UTL_IMODFILLMENU(ID,DIRNAME,WC,F,N,IMENUTYPE,ISTORE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: N
 INTEGER,INTENT(IN) :: ID,IMENUTYPE,ISTORE
 CHARACTER(LEN=*),INTENT(IN) :: DIRNAME,WC,F
 INTEGER :: I

 CALL UTL_IMODFILLMENU_DEAL()

 N=0

 IF(LEN_TRIM(DIRNAME).EQ.0)THEN
  IF(ID.NE.0)CALL WDIALOGCLEARFIELD(ID)
  RETURN
 ENDIF

 IF(.NOT.IOSDIREXISTS(DIRNAME))THEN
  IF(ID.NE.0)CALL WDIALOGCLEARFIELD(ID)
  RETURN
 ENDIF

 CALL IOSDIRENTRYTYPE(F)
 CALL IOSDIRCOUNT(DIRNAME,WC,N)

 IF(N.EQ.0)THEN
  IF(ID.NE.0)THEN
   CALL WDIALOGCLEARFIELD(ID)
   CALL WDIALOGFIELDSTATE(ID,2)
  ENDIF
 ELSE
  ALLOCATE(LISTNAME(N))
  CALL UTL_DIRINFO(DIRNAME,WC,LISTNAME,N,F)
  DO I=1,N; LISTNAME(I)=UTL_CAP(LISTNAME(I),'U'); END DO
  IF(N.GT.0.AND.ID.NE.0)THEN
   CALL WDIALOGFIELDSTATE(ID,1)
   IF(IMENUTYPE.EQ.0)THEN
    CALL WDIALOGPUTMENU(ID,LISTNAME,N,1)
   ELSEIF(IMENUTYPE.EQ.1)THEN
    ALLOCATE(ILIST(N))
    ILIST=0
    CALL WDIALOGPUTMENU(ID,LISTNAME,N,ILIST)
   ENDIF
  ELSE
   IF(ID.NE.0)THEN
    CALL WDIALOGCLEARFIELD(ID)
    CALL WDIALOGFIELDSTATE(ID,2)
   ENDIF
  ENDIF
 ENDIF

 IF(ISTORE.EQ.0)CALL UTL_IMODFILLMENU_DEAL()

 END SUBROUTINE UTL_IMODFILLMENU

 !###====================================================================
 SUBROUTINE UTL_IMODFILLMENU_DEAL()
 !###====================================================================
 IMPLICIT NONE

 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
 IF(ALLOCATED(LISTNAME))DEALLOCATE(LISTNAME)

 END SUBROUTINE UTL_IMODFILLMENU_DEAL

 !###====================================================================
 INTEGER FUNCTION GETITOPIC(CKEYWORD)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CKEYWORD
 INTEGER :: I

 GETITOPIC=0
 DO I=1,MXTP
  IF(TRIM(TP(I)%ACRNM).EQ.TRIM(CKEYWORD))GETITOPIC=I
 ENDDO

 END FUNCTION GETITOPIC

 !###======================================================================
 SUBROUTINE UTL_PLOTLOCATIONIDF(IDF,IROW,ICOL)
 !###======================================================================
 IMPLICIT NONE
 TYPE(IDFOBJ),INTENT(IN) :: IDF
 INTEGER,INTENT(IN) :: IROW,ICOL
! REAL,INTENT(IN) :: X,Y

 IF(IROW.EQ.0.OR.ICOL.EQ.0)RETURN

 CALL IGRPLOTMODE(MODEXOR)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINEWIDTH(2)

 CALL IDFPLOT1BITMAP()

 IF(IDF%IEQ.EQ.0)THEN
  CALL IGRRECTANGLE(IDF%XMIN+(ICOL-1)*IDF%DX, &
                    IDF%YMAX-(IROW-1)*IDF%DY, &
                    IDF%XMIN+ ICOL   *IDF%DX, &
                    IDF%YMAX- IROW   *IDF%DY)
 ELSEIF(IDF%IEQ.EQ.1)THEN
  CALL IGRRECTANGLE(IDF%SX(ICOL-1),IDF%SY(IROW-1), &
                    IDF%SX(ICOL)  ,IDF%SY(IROW))
 ENDIF

 CALL IGRLINEWIDTH(1)

 CALL IDFPLOT2BITMAP()

 END SUBROUTINE UTL_PLOTLOCATIONIDF

 !###======================================================================
 SUBROUTINE UTL_HIDESHOWDIALOG(ID,ISHOW)
 !###======================================================================
 INTEGER,INTENT(IN) :: ID,ISHOW
 INTEGER :: IX,IY

 CALL WDIALOGSELECT(ID)
 IX=WINFODIALOG(DIALOGXPOS)
 IY=WINFODIALOG(DIALOGYPOS)
 IF(ISHOW.EQ.0)THEN
  CALL WDIALOGHIDE()
 ELSE
  CALL WDIALOGSHOW(IX,IY,0,ISHOW)
 ENDIF

 END SUBROUTINE UTL_HIDESHOWDIALOG

 !###======================================================================
 SUBROUTINE UTL_IDFGETLAYERS(IDFNAME,N,ILAY,LDIM)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,LDIM
 CHARACTER(LEN=*),DIMENSION(N) :: IDFNAME
 INTEGER :: I,J,K,IL,IOS
 INTEGER,DIMENSION(LDIM) :: ILAY

 ILAY=0
 DO I=1,N
  J=INDEXNOCASE(IDFNAME(I),'_L',.TRUE.)
  IF(J.NE.0)THEN
   K=INDEXNOCASE(IDFNAME(I),'.IDF',.TRUE.)
   IF(K.NE.0)THEN
    J=J+2
    K=K-1
    READ(IDFNAME(I)(J:K),*,IOSTAT=IOS) IL
    IF(IOS.EQ.0.AND.IL.GT.0)ILAY(IL)=1
   ENDIF
  ENDIF
 END DO

 END SUBROUTINE UTL_IDFGETLAYERS

 !###======================================================================
 SUBROUTINE UTL_IDFGETDATES(IDFNAME,N,M,O,MINDATE,MAXDATE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: MINDATE,MAXDATE,M,O
 CHARACTER(LEN=*),DIMENSION(N) :: IDFNAME
 INTEGER :: I,IDATE

 MINDATE=21000101
 MAXDATE=19000101
 M      =0
 O      =0
 DO I=1,N
  IDATE=UTL_IDFGETDATE(IDFNAME(I))
  IF(IDATE.NE.0)THEN
   O=O+1
   MINDATE=MIN(MINDATE,IDATE)
   MAXDATE=MAX(MAXDATE,IDATE)
  ELSE
   IF(INDEX(IDFNAME(I),'_STEADY-STATE_').NE.0)M=M+1
  ENDIF
 END DO

 END SUBROUTINE UTL_IDFGETDATES

 !###======================================================================
 INTEGER FUNCTION UTL_IDFGETDATE(IDFNAME,DAYFRACTION)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 REAL,INTENT(OUT),OPTIONAL :: DAYFRACTION
 INTEGER :: IOS
 INTEGER :: I,II,J,N,HR,MN,SC
 INTEGER,DIMENSION(2) :: NI
! CHARACTER(LEN=6),DIMENSION(2) :: FRM
 DATA NI/14,8/
! DATA FRM/'(I14)','(I8)'/
 
 !## initially no data
 UTL_IDFGETDATE=0

 !## try to find 16 numbers after eachother ...
 !## try to find 8 numbers after eachother ...
 DO II=1,2
  N=0
  !## start after last "\"-symbol
  DO I=INDEX(IDFNAME,'\',.TRUE.)+1,LEN_TRIM(IDFNAME)
   !## part of a number 
   SELECT CASE (ICHAR(IDFNAME(I:I)))
    CASE (48:57)
     !## count numbers
     N=N+1
     !## stop if 8/14
     IF(N.EQ.NI(II))EXIT !8)EXIT
     !## mark first position
     IF(N.EQ.1)J=I
    CASE DEFAULT
     N=0
   END SELECT
  END DO
  IF(N.EQ.NI(II))EXIT
  !## nothing found
  IF(II.EQ.2.AND.N.LT.NI(II))RETURN !8)RETURN
 ENDDO
 
 !## default
 IF(PRESENT(DAYFRACTION))DAYFRACTION=-1.0
 
 IF(II.EQ.1)THEN
  READ(IDFNAME(J:)  ,'(I8) ',IOSTAT=IOS) UTL_IDFGETDATE
  IF(PRESENT(DAYFRACTION))THEN
   READ(IDFNAME(J+8:),'(3I2)',IOSTAT=IOS) HR,MN,SC
   DAYFRACTION=REAL(HR*3600+MN*60+SC)/86400.0
   DAYFRACTION=MAX(0.0,MIN(DAYFRACTION,1.0))
  ENDIF  
 ELSE
  READ(IDFNAME(J:)  ,'(I8)',IOSTAT=IOS) UTL_IDFGETDATE
 ENDIF
 IF(IOS.NE.0)UTL_IDFGETDATE=0

 END FUNCTION UTL_IDFGETDATE

 !###======================================================================
 SUBROUTINE UTL_FILLDATES(IDY,IDM,IDD,JULD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT),OPTIONAL :: JULD
 INTEGER,INTENT(IN) :: IDM,IDY,IDD
 INTEGER :: M,Y,D,NDAY

 CALL WDIALOGGETMENU(IDM,M)
 CALL WDIALOGGETINTEGER(IDY,Y)
 NDAY=WDATEDAYSINMONTH(Y,M)
 CALL WDIALOGGETINTEGER(IDD,D)
 CALL WDIALOGRANGEINTEGER(IDD,1,NDAY)
 IF(D.GT.NDAY)CALL WDIALOGPUTINTEGER(IDD,NDAY)
 D=MIN(D,NDAY)

 IF(.NOT.PRESENT(JULD))RETURN
 JULD=JD(Y,M,D)

 END SUBROUTINE UTL_FILLDATES

 !###======================================================================
 SUBROUTINE UTL_FILLDATESDIALOG(ID,IDD,IDM,IDY,JD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID,IDD,IDM,IDY,JD
 INTEGER :: I,J,K
 
 CALL WDIALOGSELECT(ID)
 !## put begin date
 CALL IDATETOGDATE(JD,I,J,K)  !## ID,IY,IM,ID
 CALL WDIALOGPUTINTEGER(IDD,K)
 CALL WDIALOGPUTINTEGER(IDY,I)
 CALL WDIALOGPUTOPTION(IDM,J)
 
 END SUBROUTINE UTL_FILLDATESDIALOG
 
 !###======================================================================
 FUNCTION ITOS(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 CHARACTER(LEN=10)  :: TXT,ITOS

 WRITE(TXT,'(I10)') I
 ITOS=ADJUSTL(TXT)

 END FUNCTION ITOS

 !###======================================================================
 FUNCTION RTOS(X,F,NDEC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDEC
 REAL,INTENT(IN) :: X
 CHARACTER(LEN=1),INTENT(IN) :: F
 CHARACTER(LEN=15) :: TXT,FRM,RTOS 
 INTEGER :: IOS

 IF(F.EQ.'*')THEN
  WRITE(TXT,*,IOSTAT=IOS) X
 ELSE
  WRITE(FRM,'(2A1,I2.2,A1,I2.2,A1)') '(',F,LEN(RTOS),'.',NDEC,')'
  WRITE(TXT,FRM,IOSTAT=IOS) X
 ENDIF

 IF(IOS.NE.0)TXT='error'

 RTOS=ADJUSTL(TXT)

 END FUNCTION RTOS

 !###======================================================================
 INTEGER FUNCTION UTL_GETUNIT()
 !###======================================================================
 IMPLICIT NONE
 LOGICAL :: LEX

 UTL_GETUNIT=19
 DO 
  UTL_GETUNIT=UTL_GETUNIT+1 !20,5000
  INQUIRE(UNIT=UTL_GETUNIT,OPENED=LEX)
  IF(.NOT.LEX)EXIT
  IF(UTL_GETUNIT.GT.1000000)EXIT
 END DO

 IF(LEX)THEN 
  CALL WMESSAGEBOX(OKONLY,COMMONOK,EXCLAMATIONICON,'iMOD can not open more than 1000000 files simultaneously!','ERROR')
  UTL_GETUNIT=0
 ENDIF

 END FUNCTION UTL_GETUNIT

 !###======================================================================
 SUBROUTINE CLOSEUNITS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 LOGICAL :: LEX

 DO I=20,5000
  INQUIRE(UNIT=I,OPENED=LEX)
  IF(LEX)CLOSE(I)
 END DO

 END SUBROUTINE CLOSEUNITS

 !###======================================================================
 SUBROUTINE INFOUNITS()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: FNAME

 DO I=20,5000
  INQUIRE(UNIT=I,OPENED=LEX)
  IF(LEX)THEN
   INQUIRE(UNIT=I,NAME=FNAME)
   WRITE(*,*) 'UNIT ',I,' '//TRIM(FNAME)
  ENDIF
 END DO

 END SUBROUTINE INFOUNITS

 !###======================================================================
 SUBROUTINE UTL_CREATEDIR(DIRNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIRNAME
 INTEGER                     :: I,J

 !## create/check entire directory-structure
 I=INDEX(DIRNAME,'\')+1
 DO
  J=INDEX(DIRNAME(I:),'\')
  IF(J.EQ.0)EXIT
  J=J+I
  IF(.NOT.IOSDIREXISTS(DIRNAME(:J-2)))CALL IOSDIRMAKE(DIRNAME(:J-2))
  I=J
 END DO
 !## last remaining of string
 IF(.NOT.IOSDIREXISTS(TRIM(DIRNAME)))CALL IOSDIRMAKE(TRIM(DIRNAME))

 END SUBROUTINE UTL_CREATEDIR

 !###======================================================================
 SUBROUTINE UTL_DEL1TREE(DIR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER :: I,IERROR
 CHARACTER(LEN=256) :: CURDIR,DELDIR

 CALL IOSDIRNAME(CURDIR)
 I=INDEXNOCASE(DIR,'\',.TRUE.)
 !## clear existing error?
 IERROR=INFOERROR(1)
 CALL IOSDIRCHANGE(DIR(:I-1))
 IERROR=INFOERROR(1)
 !## dirchange error?
 IF(IERROR.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Could not find directory:'//CHAR(13)// &
               TRIM(DIR(:I-1)),'iMOD: Error')
  RETURN
 ENDIF
 !## make sure to delete directory
 CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Are you sure to delete'//CHAR(13)//TRIM(DIR),'Question?')
 IF(WINFODIALOG(4).NE.1)RETURN
 !## delete entire directory
 DELDIR=DIR(I+1:)
 CALL UTL_DEL2TREE(DELDIR)
 CALL IOSDIRCHANGE(CURDIR)

 END SUBROUTINE UTL_DEL1TREE

 !###======================================================================
 RECURSIVE SUBROUTINE UTL_DEL2TREE(DIR)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256),INTENT(IN)       :: DIR
 INTEGER,PARAMETER                   :: MXDIR=50
 INTEGER                             :: I,NDIR,IERROR
 CHARACTER(LEN=256),DIMENSION(MXDIR) :: RESDIR

 CALL WINDOWOUTSTATUSBAR(4,'Delete directory '//TRIM(DIR)//'...')

 !#clear existing error?
 IERROR=INFOERROR(1)
 !##go one level down
 CALL IOSDIRCHANGE(DIR)
 IERROR=INFOERROR(1)
 !#dirchange error?
 IF(IERROR.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Could not change towards directory:'//CHAR(13)// &
               TRIM(DIR),'iMOD: Error')
  RETURN
 ENDIF

 !##how many subdirectories exist?
 NDIR=MXDIR
 CALL IOSDIRENTRYTYPE('D')
 CALL IOSDIRINFO(' ',' ',RESDIR,NDIR)
 IF(NDIR.GT.MXDIR)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'MXDIR overwritten in del2tree()','ERROR')
  RETURN
 ENDIF
 DO I=3,NDIR
  CALL UTL_DEL2TREE(RESDIR(I))
 END DO
 !##delete all files in directory
 CALL IOSDELETEFILE('*.*')
 !##return one level up
 CALL IOSDIRCHANGE('..')
 CALL IOSDIRDELETE(DIR)

 END SUBROUTINE UTL_DEL2TREE

 !###======================================================================
 SUBROUTINE UTL_WAITMESSAGE(IRAT,IRAT1,I1,I2,WAITTXT,IBOX)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN)          :: I1,I2
 INTEGER,INTENT(IN OUT)      :: IRAT,IRAT1
 CHARACTER(LEN=*),INTENT(IN) :: WAITTXT
 INTEGER,OPTIONAL,INTENT(IN) :: IBOX
 INTEGER :: JBOX
 
 JBOX=4; IF(PRESENT(IBOX))JBOX=IBOX
 
 IRAT=(I1*100)/I2
 IF(IRAT.NE.IRAT1)THEN
  CALL WINDOWOUTSTATUSBAR(JBOX,TRIM(WAITTXT)//' '//TRIM(ITOS(IRAT))//' %')
  IRAT1=IRAT
 ENDIF

 END SUBROUTINE UTL_WAITMESSAGE

 !###======================================================================
 SUBROUTINE UTL_MESSAGEHANDLE(ONOFF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ONOFF
 INTEGER :: I

 IF(ONOFF.EQ.0)CALL WCURSORSHAPE(CURHOURGLASS)
 IF(ONOFF.EQ.1)CALL WCURSORSHAPE(CURARROW)
 DO I=1,MXMESSAGE
  IF(IMESSAGE(I).EQ.1)THEN
   IF(WINFOMESSAGE(I).NE.ONOFF)CALL WMESSAGEENABLE(I,ONOFF)
  ENDIF
 END DO
 IF(ONOFF.EQ.0)RETURN

 END SUBROUTINE UTL_MESSAGEHANDLE

 !###======================================================================
 SUBROUTINE UTL_MESSAGEHANDLE3D(ONOFF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ONOFF

 !## default mousemove off
 IMESSAGE(MOUSEMOVE)=ONOFF

 END SUBROUTINE UTL_MESSAGEHANDLE3D

 !###======================================================================
 FUNCTION JDATETOGDATE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 CHARACTER(LEN=10)  :: JDATETOGDATE
 INTEGER :: IY,IM,ID

 CALL GDATE(I,IY,IM,ID)
 JDATETOGDATE=TRIM(ITOS(ID))//'/'//TRIM(ITOS(IM))//'/'//TRIM(ITOS(IY))

 END FUNCTION JDATETOGDATE

 !###======================================================================
 INTEGER FUNCTION GDATETOJDATE(CDATE)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: CDATE
 INTEGER :: IY,IM,ID,I,J,MD
 INTEGER,DIMENSION(3) :: IOS

 IOS=1

 I=INDEX(CDATE,'/',.FALSE.)
 IF(I.GT.0)THEN
  READ(CDATE(1:I-1),*,IOSTAT=IOS(1)) ID
  J=INDEX(CDATE,'/',.TRUE.)
  IF(J.GT.0)THEN
   READ(CDATE(J+1:),*,IOSTAT=IOS(3)) IY
   IF(J-I.GT.0)READ(CDATE(I+1:J-1),*,IOSTAT=IOS(2)) IM
  ENDIF
 ENDIF

 !## initialize default value
 GDATETOJDATE=0

 IM=MAX(1,MIN(12,IM))
 MD=WDATEDAYSINMONTH(IY,IM)
 ID=MAX(1,MIN(MD,ID))

 !## error reading dates
 IF(SUM(IOS).NE.0)RETURN

 J           =JD(IY,IM,ID)
 CDATE       =JDATETOGDATE(J)
 GDATETOJDATE=J

 END FUNCTION GDATETOJDATE

 !###====================================================================
 INTEGER FUNCTION IDATETOJDATE(IDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATE
 INTEGER :: IY,IM,ID

 CALL IDATETOGDATE(IDATE,IY,IM,ID)
 IDATETOJDATE=JD(IY,IM,ID)

 END FUNCTION IDATETOJDATE

 !###====================================================================
 INTEGER FUNCTION JDATETOIDATE(JDATE)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: JDATE
 INTEGER :: IY,IM,ID

 CALL GDATE(JDATE,IY,IM,ID)
 JDATETOIDATE=IY*10000+IM*100+ID

 END FUNCTION JDATETOIDATE

 !###====================================================================
 SUBROUTINE IDATETOGDATE(IDATE,IY,IM,ID)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDATE
 INTEGER,INTENT(OUT) :: IY,IM,ID

 IY = IDATE / 10000
 IM = MOD( IDATE, 10000 ) / 100
 ID = MOD( IDATE, 100 ) 

 END SUBROUTINE IDATETOGDATE

 !###====================================================================
 REAL FUNCTION ITIMETOJTIME(ITIME)
 !###====================================================================
 IMPLICIT NONE
 REAL,PARAMETER :: SDAY=86400.0
 INTEGER,INTENT(IN) :: ITIME
 INTEGER :: IH,IM,IS

 CALL ITIMETOGTIME(ITIME,IH,IM,IS)
 ITIMETOJTIME=(IH*3600.0+IM*60.0+IS)/SDAY

 END FUNCTION ITIMETOJTIME

 !###====================================================================
 SUBROUTINE DECDEGREES_TO_DMS(DEGREES,D,M,S)
 !###====================================================================
 IMPLICIT NONE
 DOUBLE PRECISION,INTENT(IN) :: DEGREES
 REAL,INTENT(OUT) :: D,M,S
 REAL :: F
 
 D = INT(DEGREES)
 F = 60.0 * (DEGREES - D)
 M = INT(F)
 S = F - M

 END SUBROUTINE DECDEGREES_TO_DMS

 !###====================================================================
 SUBROUTINE ITIMETOGTIME(ITIME,IH,IM,IS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITIME !## seconds
 INTEGER,INTENT(OUT) :: IH,IM,IS

 IH = ITIME / 10000
 IM = MOD( ITIME, 10000 ) / 100
 IS = MOD( ITIME, 100 ) 

 END SUBROUTINE ITIMETOGTIME

 !###====================================================================
 INTEGER FUNCTION JD(YEAR,MONTH,DAY)
 !###====================================================================
 !Reference: Fliegel, H. F. & van Flandern, T. C. 1968, Communications of the ACM, 11, 657.
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: YEAR,MONTH,DAY
 INTEGER            :: I,J,K

 I =YEAR
 J =MONTH
 K =DAY
 JD=K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12) &
    /12-3*((I+4900+(J-14)/12)/100)/4

 END FUNCTION JD

 !###====================================================================
 SUBROUTINE GDATE(JD,YEAR,MONTH,DAY)
 !###====================================================================
 !Reference: Fliegel, H. F. & van Flandern, T. C. 1968, Communications of the ACM, 11, 657.
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: JD
 INTEGER,INTENT(OUT) :: YEAR,MONTH,DAY
 INTEGER             :: I,J,K,L,N

 L=JD+68569
 N=4*L/146097
 L=L-(146097*N+3)/4
 I=4000*(L+1)/1461001
 L=L-1461*I/4+31
 J=80*L/2447
 K=L-2447*J/80
 L=J/11
 J=J+2-12*L
 I=100*(N-49)+I+L

 YEAR =I
 MONTH=J
 DAY  =K

 END SUBROUTINE GDATE

 !###====================================================================
 FUNCTION UTL_SUBST(FNAME,SUB1,SUB2)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: SUB1,SUB2
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,J
 CHARACTER(LEN=256) :: UTL_SUBST

 UTL_SUBST=FNAME

 I=INDEX(FNAME,SUB1)
 IF(I.EQ.0)RETURN
 I=I-1
 J=I+LEN_TRIM(SUB1)+1

 UTL_SUBST=FNAME(:I)//TRIM(SUB2)//FNAME(J:)

 END FUNCTION UTL_SUBST

 !###====================================================
 SUBROUTINE UTL_CHECKNAME(FNAME,EXT)
 !checks for existence of an extension EXT (for instance 'idf')
 !and replaces fname including the extension (EXT) if not found.
 !###====================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: FNAME
 CHARACTER(LEN=*),INTENT(IN) :: EXT
 INTEGER :: I

 I=INDEXNOCASE(FNAME,'.',.TRUE.)
 IF(I.EQ.0)THEN
  FNAME=TRIM(FNAME)//'.'//TRIM(EXT)
 ELSE
  FNAME=FNAME(:I)//TRIM(EXT)
 ENDIF

 END SUBROUTINE UTL_CHECKNAME

 !###======================================================================
 INTEGER FUNCTION INVERSECOLOUR(IRGB)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IRGB
 INTEGER :: IR,IG,IB

 CALL WRGBSPLIT(IRGB,IR,IG,IB)
 INVERSECOLOUR=WRGB(255-IR,255-IG,255-IB)

 END FUNCTION INVERSECOLOUR

 !###======================================================================
 LOGICAL FUNCTION EQUALNAMES(NAME1,NAME2)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: NAME1,NAME2
 LOGICAL :: CHF_LK

 EQUALNAMES=CHF_LK(NAME1,LEN(NAME1),NAME2,LEN(NAME2))

 END FUNCTION EQUALNAMES

 !###======================================================================
 FUNCTION UTL_CAP(STR,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT,STR
 INTEGER :: I,J,K,B1,B2
 CHARACTER(LEN=256) :: UTL_CAP

 IF(TXT.EQ.'l'.OR.TXT.EQ.'L')THEN
  B1= 65
  B2= 90
  K = 32
 ELSEIF(TXT.EQ.'u'.OR.TXT.EQ.'U')THEN
  B1= 97
  B2= 122
  K =-32
 ENDIF

 UTL_CAP=''
 DO I=1,LEN_TRIM(STR)
  J=IACHAR(STR(I:I))
  IF(J.GE.B1.AND.J.LE.B2)J=J+K
  UTL_CAP(I:I)=ACHAR(J)
 END DO

 END FUNCTION UTL_CAP

 !###======================================================================
 FUNCTION UTL_CAP_BIG(STR,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: TXT,STR
 INTEGER :: I,J,K,B1,B2
 CHARACTER(LEN=1052) :: UTL_CAP_BIG

 IF(TXT.EQ.'l'.OR.TXT.EQ.'L')THEN
  B1= 65
  B2= 90
  K = 32
 ELSEIF(TXT.EQ.'u'.OR.TXT.EQ.'U')THEN
  B1= 97
  B2= 122
  K =-32
 ENDIF

 UTL_CAP_BIG=''
 DO I=1,LEN_TRIM(STR)
  J=IACHAR(STR(I:I))
  IF(J.GE.B1.AND.J.LE.B2)J=J+K
  UTL_CAP_BIG(I:I)=ACHAR(J)
 END DO

 END FUNCTION UTL_CAP_BIG

 !###======================================================================
 SUBROUTINE UTL_DIRINFO(DIR,WC,LISTNAME,N,FT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(INOUT) :: N
 CHARACTER(LEN=*),INTENT(IN) :: DIR,WC,FT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(N) :: LISTNAME
 CHARACTER(LEN=512) :: LINE,BATFILE,TXTFILE
 INTEGER :: IU,I,J,IOS
 LOGICAL :: LEX

 BATFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.bat'
 TXTFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.txt'

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=BATFILE,ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to write CREATE: '//CHAR(13)//TRIM(BATFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF

 INQUIRE(FILE=TXTFILE,EXIST=LEX)
 !## succesfully deleted
 IF(LEX)THEN
  I=WINFOERROR(1)
  CALL IOSDELETEFILE(TXTFILE)
  I=WINFOERROR(1)
  IF(I.EQ.ERROSCOMMAND)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to DELETE: '//CHAR(13)//TRIM(TXTFILE),'Error')
   CLOSE(IU); IU=0; N=0; RETURN
  ENDIF
 ENDIF

 IF(FT.EQ.'F'.OR.FT.EQ.'f') &
    LINE='dir /b /o "'    //TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
 IF(FT.EQ.'D'.OR.FT.EQ.'d') &
    LINE='dir /ad /b /o "'//TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'

 !## remove \\
 DO
  I=INDEX(LINE,'\\')
  IF(I.EQ.0)EXIT
  LINE(I+1:256-1)=LINE(I+2:)
 ENDDO

 WRITE(IU,'(A)') TRIM(LINE)
 CLOSE(IU)

#if (defined(WINTERACTER8))
 CALL IOSCOMMAND('"'//TRIM(BATFILE)//'"',PROCSILENT+PROCBLOCKED)
#endif
#if (defined(WINTERACTER9))
 CALL IOSCOMMAND('"'//TRIM(BATFILE)//'"',PROCSILENT+PROCBLOCKED+PROCCMDPROC)
#endif

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TXTFILE,ACTION='READ',FORM='FORMATTED')
 IF(IU.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not OPEN: '//CHAR(13)//TRIM(TXTFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF
  
 I=0
 DO 
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  J=LEN_TRIM(LINE)
  IF(J.EQ.0)EXIT
  LINE(2:J+1)=LINE(1:J)
  LINE(1:1)='"'
  LINE(J+2:J+2)='"'
  I=I+1
  READ(LINE,*,IOSTAT=IOS) LISTNAME(I)
  IF(IOS.NE.0)EXIT
 END DO
 !## delete result txt file
 CLOSE(IU,STATUS='DELETE')
 N=I

 END SUBROUTINE UTL_DIRINFO
 
 !###======================================================================
 LOGICAL FUNCTION UTL_DIRINFO_POINTER(DIR,WC,LISTNAME,FT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR,WC,FT
 CHARACTER(LEN=*),INTENT(OUT),DIMENSION(:),POINTER :: LISTNAME
 CHARACTER(LEN=256),DIMENSION(:),POINTER :: C_LISTNAME
 CHARACTER(LEN=512) :: LINE,BATFILE,TXTFILE
 INTEGER :: IU,I,J,N,IOS
 LOGICAL :: LEX

 UTL_DIRINFO_POINTER=.FALSE.
 
 IF(LEN(C_LISTNAME).LT.LEN(LISTNAME))CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'c_listname()<listname()','error')
 
 BATFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.bat'
 TXTFILE=TRIM(PREFVAL(1))//'\tmp\'//TRIM(OSD_GETENV('USERNAME'))//'_dir_imod.txt'

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=BATFILE,ACTION='WRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to write CREATE: '//CHAR(13)//TRIM(BATFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF

 INQUIRE(FILE=TXTFILE,EXIST=LEX)
 !## succesfully deleted
 IF(LEX)THEN
  I=WINFOERROR(1)
  CALL IOSDELETEFILE(TXTFILE)
  I=WINFOERROR(1)
  IF(I.EQ.ERROSCOMMAND)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD does not have priveleges to DELETE: '//CHAR(13)//TRIM(TXTFILE),'Error')
   CLOSE(IU); IU=0; N=0; RETURN
  ENDIF
 ENDIF

 IF(FT.EQ.'F'.OR.FT.EQ.'f') &
    LINE='dir /b /o "'    //TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'
 IF(FT.EQ.'D'.OR.FT.EQ.'d') &
    LINE='dir /ad /b /o "'//TRIM(DIR)//'\'//TRIM(WC)//'" > "'//TRIM(TXTFILE)//'"'

 !## remove \\
 DO
  I=INDEX(LINE,'\\')
  IF(I.EQ.0)EXIT
  LINE(I+1:256-1)=LINE(I+2:)
 ENDDO

 WRITE(IU,'(A)') TRIM(LINE)
 CLOSE(IU)

#if (defined(WINTERACTER8))
 CALL IOSCOMMAND('"'//TRIM(BATFILE)//'"',PROCSILENT+PROCBLOCKED)
#endif
#if (defined(WINTERACTER9))
 CALL IOSCOMMAND('"'//TRIM(BATFILE)//'"',PROCSILENT+PROCBLOCKED+PROCCMDPROC)
#endif
 
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TXTFILE,ACTION='READ',FORM='FORMATTED')
 IF(IU.LE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not OPEN: '//CHAR(13)//TRIM(TXTFILE),'Error')
  IU=0; N=0; RETURN
 ENDIF
 
 ALLOCATE(C_LISTNAME(50))
 
 I=0
 DO 
  READ(IU,'(A256)',IOSTAT=IOS) LINE
  IF(IOS.NE.0)EXIT
  J=LEN_TRIM(LINE)
  IF(J.EQ.0)EXIT
  LINE(2:J+1)=LINE(1:J)
  LINE(1:1)='"'
  LINE(J+2:J+2)='"'
  I=I+1
  IF(I.GT.SIZE(C_LISTNAME))THEN
   N=SIZE(C_LISTNAME)
   ALLOCATE(LISTNAME(N));  LISTNAME(1:N)=C_LISTNAME(1:N)
   DEALLOCATE(C_LISTNAME); ALLOCATE(C_LISTNAME(N*2))
   C_LISTNAME(1:N)=LISTNAME(1:N); DEALLOCATE(LISTNAME)
  ENDIF
  READ(LINE,*,IOSTAT=IOS) C_LISTNAME(I)
  IF(IOS.NE.0)EXIT
 END DO

 CLOSE(IU)
 
 N=I 
 
 ALLOCATE(LISTNAME(N))
 LISTNAME(1:N)=C_LISTNAME(1:N)
 DEALLOCATE(C_LISTNAME)
 
 UTL_DIRINFO_POINTER=.TRUE.
 
 END FUNCTION UTL_DIRINFO_POINTER

 !###======================================================================
 SUBROUTINE UTL_IDFSNAPTOGRID(MINX,MAXX,MINY,MAXY,CS,NCOL,NROW)
 !###======================================================================
 REAL,INTENT(INOUT) :: MINX,MAXX,MINY,MAXY
 REAL,INTENT(IN)	:: CS
 INTEGER,INTENT(OUT) :: NCOL,NROW
 REAL :: D

 D=MOD(ABS(MINX),CS)
 IF(D.NE.0.0)MINX=(MINX-D)+CS
 D=MOD(ABS(MAXX),CS)
 IF(D.NE.0.0)MAXX=(MAXX+(CS-D))-CS
 D=MOD(ABS(MINY),CS)
 IF(D.NE.0.0)MINY=(MINY-D)+CS
 D=MOD(ABS(MAXY),CS)
 IF(D.NE.0.0)MAXY=(MAXY+(CS-D))-CS

 NCOL=INT((MAXX-MINX)/CS)
 NROW=INT((MAXY-MINY)/CS)

 END SUBROUTINE UTL_IDFSNAPTOGRID

 !###======================================================================
 SUBROUTINE UTL_IDFSNAPTOGRID_LLC(MINX,MAXX,MINY,MAXY,CS,NCOL,NROW)
 !###======================================================================
 REAL,INTENT(INOUT) :: MINX,MAXX,MINY,MAXY
 REAL,INTENT(IN) :: CS
 INTEGER,INTENT(OUT) :: NCOL,NROW
 REAL :: D

 NCOL=(MAXX-MINX)/CS
 NROW=(MAXY-MINY)/CS

 D=MAXX-MINX; D=MOD(D,CS); IF(D.NE.0.0)NCOL=NCOL-1
 MAXX=MINX+NCOL*CS

 D=MAXY-MINY; D=MOD(D,CS); IF(D.NE.0.0)NROW=NROW-1
 MAXY=MINY+NROW*CS

 END SUBROUTINE UTL_IDFSNAPTOGRID_LLC

 !###====================================================================
 REAL FUNCTION UTL_GETMOSTFREQ(FREQ,MFREQ,NFREQ)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MFREQ,NFREQ
 REAL,DIMENSION(MFREQ),INTENT(IN) :: FREQ
 INTEGER :: I,MI,NI

 NI=1  !number of unique
 MI=NI !max. number of unique
 UTL_GETMOSTFREQ=FREQ(NI)

 DO I=2,NFREQ
  IF(FREQ(I).NE.FREQ(I-1))THEN
   IF(NI.GT.MI)THEN
    UTL_GETMOSTFREQ=FREQ(I-1)
    MI=NI
   ENDIF
   NI=1
  ELSE
   NI=NI+1
  ENDIF
 END DO
 !test final
 IF(NI.GT.MI) UTL_GETMOSTFREQ=FREQ(NFREQ)

 END FUNCTION UTL_GETMOSTFREQ

 !###====================================================
 SUBROUTINE UTL_GETHIST(X,NX,NODATA,HIST,NHIST,MX,XHIST)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NHIST            !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX                 !## number of values ne nodata
 REAL,INTENT(IN),DIMENSION(NHIST) :: HIST  !## percentile 0-100%
 REAL,INTENT(OUT),DIMENSION(NHIST) :: XHIST !## yielding percentile(s)
 REAL,INTENT(IN) :: NODATA                 !## nodata value !,PERC
 REAL,DIMENSION(NX),INTENT(INOUT) :: X     !## array
 INTEGER :: I,J

 XHIST=0.0; MX=0; IF(NX.LE.0)RETURN

 DO I=1,NX
  IF(X(I).EQ.NODATA)CYCLE
  MX=MX+1
  DO J=1,NHIST-1
   IF(X(I).GT.HIST(J).AND.X(I).LE.HIST(J+1))THEN
    XHIST(J)=XHIST(J)+1
    EXIT
   ENDIF
  ENDDO
 ENDDO

 END SUBROUTINE UTL_GETHIST

 !###====================================================
 SUBROUTINE UTL_GETMED(X,NX,NODATA,PERC,NPERC,MX,XMED)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NPERC            !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX                 !## number of values ne nodata
 REAL,INTENT(IN),DIMENSION(NPERC) :: PERC  !## percentile 0-100%
 REAL,INTENT(OUT),DIMENSION(NPERC) :: XMED !## yielding percentile(s)
 REAL,INTENT(IN) :: NODATA                 !## nodata value !,PERC
 REAL,DIMENSION(NX),INTENT(INOUT) :: X     !## array
 INTEGER :: I,J,IP
 REAL :: FRAC

 XMED=NODATA 

 IF(NX.LE.0)RETURN

 !## only one sample
 IF(NX.EQ.1)THEN
  XMED=X(1)
  MX  =1
  RETURN
 ENDIF

 !## do not include nodata values for median-computation
 MX=0
 DO I=1,NX
  IF(X(I).NE.NODATA)THEN
   MX   =MX+1
   X(MX)=X(I)
  ENDIF
 END DO

 IF(MX.LE.0)RETURN

 !## sort data, excl. nodata values
 IF(MX.LE.100)THEN
  CALL SHELLSORT(MX,X)
 ELSE
  CALL UTL_QKSORT(MX,MX,X)
 ENDIF

 DO IP=1,NPERC

  IF(PERC(IP).LE.0.0)THEN
   XMED(IP)=X(1)
  ELSEIF(PERC(IP).GE.100.0)THEN
   XMED(IP)=X(MX) 
  ELSE
   FRAC=1.0/(PERC(IP)/100.0)

   IF(MOD(REAL(MX),FRAC).EQ.0.0)THEN
    I=MAX(1,INT(REAL(MX)/FRAC))
    XMED(IP)=X(I)
   ELSE
    I=MAX(1,INT(REAL(MX)/FRAC))
    J=MIN(I+1,MX)
    XMED(IP)=(X(I)+X(J))/2.0
   ENDIF
  ENDIF
 ENDDO

 END SUBROUTINE UTL_GETMED

 !###====================================================
 SUBROUTINE UTL_GETMED_INVERSE(X,NX,NODATA,PERC,NPERC,MX,XMED)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NX,NPERC            !## size array,number of percentiles to be comp.
 INTEGER,INTENT(OUT) :: MX                 !## number of values ne nodata
 REAL,INTENT(IN),DIMENSION(NPERC) :: PERC  !## percentile 0-100%
 REAL,INTENT(OUT),DIMENSION(NPERC) :: XMED !## yielding percentile(s)
 REAL,INTENT(IN) :: NODATA                 !## nodata value !,PERC
 REAL,DIMENSION(NX),INTENT(INOUT) :: X     !## array
 INTEGER :: I,IP
 REAL :: FRAC

 XMED=NODATA 

 IF(NX.LE.0)RETURN

 !## only one sample
 IF(NX.EQ.1)THEN
  DO IP=1,NPERC
   XMED(IP)=0.0
   IF(X(1).LE.PERC(IP))XMED(IP)=1.0
  ENDDO
  MX  =1
  RETURN
 ENDIF

 !## do not include nodata values for median-computation
 MX=0
 DO I=1,NX
  IF(X(I).NE.NODATA)THEN
   MX   =MX+1
   X(MX)=X(I)
  ENDIF
 END DO

 IF(MX.LE.0)RETURN

 !## sort data, excl. nodata values
 IF(MX.LE.100)THEN
  CALL SHELLSORT(MX,X)
 ELSE
  CALL UTL_QKSORT(MX,MX,X)
 ENDIF

 !## find appropriate values for percentiles
 FRAC=1.0
 DO IP=1,NPERC
  IF(MX.EQ.1)THEN
   XMED(IP)=0.0
   IF(X(1).LE.PERC(IP))XMED(IP)=1.0
  ELSE
   IF(PERC(IP).LE.X(1))THEN
    XMED(IP)=0.0
   ELSEIF(PERC(IP).GT.X(MX))THEN
    XMED(IP)=1.0
   ELSE
    DO I=2,MX
     IF(X(I-1).LE.PERC(IP).AND. &
        X(I)  .GE.PERC(IP))THEN
      FRAC=(PERC(IP)-X(I-1))/(X(I)-X(I-1))
      XMED(IP)=(REAL(I-1)+FRAC)/REAL(MX)
      EXIT    
     ENDIF
    ENDDO
   ENDIF
  ENDIF
!  IF(XMED(IP).GT.1.0)THEN
  WRITE(*,*) 'PERC(IP)=',PERC(IP)
  WRITE(*,*) 'XMED(IP)=',XMED(IP)
  WRITE(*,*) 'FRAC    =',FRAC
  DO I=1,MX; WRITE(*,'(I10,F15.7)') I,X(I); ENDDO
!  ENDIF
 ENDDO
   
 END SUBROUTINE UTL_GETMED_INVERSE

 !###======================================================================
 INTEGER FUNCTION CALCPERIODS(ISTARTDATE,IENDDATE)
 !WHERE START/ENDDATES ARE FOR INSTANCES
 !STARTDATE = 20000101
 !ENDDATE   = 20041231
 !Remark: For every month for every specific year (including leapyears)
 !        no of days is calculated
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ISTARTDATE,IENDDATE

 CALCPERIODS=IDATETOJDATE(IENDDATE)-IDATETOJDATE(ISTARTDATE)+1

 END FUNCTION CALCPERIODS

 !###====================================================
 SUBROUTINE UTL_GETUNIQUE(X,N,NU,NODATA)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 REAL,INTENT(IN),OPTIONAL :: NODATA
 REAL,INTENT(INOUT),DIMENSION(N) :: X
 INTEGER :: I

 CALL UTL_QKSORT(N,N,X)
 
 !## determine number of unique classes
 IF(PRESENT(NODATA))THEN
  NU=0
  DO I=1,N
   IF(NU.EQ.0)THEN
    IF(X(I).NE.NODATA)THEN
     NU=NU+1
     X(NU)=X(I)
    ENDIF
   ELSE
    IF(X(I).NE.X(NU).AND.X(I).NE.NODATA)THEN
     NU   =NU+1
     X(NU)=X(I)
    ENDIF
   ENDIF
  END DO
 ELSE 
  NU=1
  DO I=2,N
   IF(X(I).NE.X(NU))THEN
    NU   =NU+1
    X(NU)=X(I)
   ENDIF
  END DO
 ENDIF

 END SUBROUTINE UTL_GETUNIQUE

 !###====================================================
 SUBROUTINE UTL_GETUNIQUE_INT(IX,N,NU)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 INTEGER,INTENT(INOUT),DIMENSION(N) :: IX
 INTEGER :: I

 CALL SHELLSORT_INT(N,IX)

 !## determine number of unique classes
 NU=1
 DO I=2,N
  IF(IX(I).NE.IX(NU))THEN
   NU    =NU+1
   IX(NU)=IX(I)
  ENDIF
 END DO

 END SUBROUTINE UTL_GETUNIQUE_INT
 
 !###====================================================
 SUBROUTINE UTL_GETUNIQUE_CHAR(CH,N,NU)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER,INTENT(OUT) :: NU
 CHARACTER(LEN=*),INTENT(INOUT),DIMENSION(N) :: CH
 INTEGER :: I,J

 !## determine number of unique classes
 NU=1
 DO I=2,N
  DO J=1,NU
   IF(CH(J).EQ.CH(I))EXIT !## get it allready
  ENDDO
  IF(J.LE.NU)CYCLE
  !## add new to unique string
  NU=NU+1
  CH(NU)=CH(I)
 END DO

 END SUBROUTINE UTL_GETUNIQUE_CHAR

 !###====================================================
 LOGICAL FUNCTION EQUALS_REAL(A,B)
 !###====================================================
 IMPLICIT NONE
 REAL, INTENT(IN) :: A, B
 REAL :: EPS

 EPS=ABS(A)*EPSILON(A) ! SCALE EPSILON

 IF(EPS.EQ.0.0)THEN
  EPS=TINY (A) ! IF EPS UNDERFLOWED TO 0
 ! USE A VERY SMALL
 ! POSITIVE VALUE FOR EPSILON
 END IF

 IF(ABS(A-B).GT.EPS)THEN
  EQUALS_REAL=.FALSE. ! NOT EQUAL IF DIFFERENCE>EPS
 ELSE
  EQUALS_REAL=.TRUE.  ! EQUAL OTHERWISE
 ENDIF

 END FUNCTION EQUALS_REAL

 !###====================================================
 SUBROUTINE PEUCKER_SIMPLIFYLINE(XCOORD,YCOORD,GENCODE,PTSINCHAIN)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: PTSINCHAIN
 REAL,INTENT(IN),DIMENSION(PTSINCHAIN) :: XCOORD,YCOORD
 REAL,INTENT(OUT),DIMENSION(PTSINCHAIN) :: GENCODE

 END SUBROUTINE PEUCKER_SIMPLIFYLINE

 !###====================================================
 SUBROUTINE PEUCKER_CLCDIS(DIS,XCDST,YCDST,XCDEND,YCDEND,XPT,YPT)
 !###====================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XCDST,YCDST,XCDEND,YCDEND,XPT,YPT
 REAL,INTENT(OUT) :: DIS

 END SUBROUTINE PEUCKER_CLCDIS

 !###====================================================
 SUBROUTINE UTL_FIT_REGRESSION(X,Y,NDATA,SIG,MWT,A,B,SIGA,SIGB,CHI2,Q)
 !###====================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NDATA,MWT
 REAL,INTENT(OUT) :: A,B,CHI2,Q,SIGA,SIGB
 REAL,DIMENSION(NDATA) :: X,Y,SIG
 INTEGER :: I
 REAL :: SIGDAT,SS,ST2,SX,SXOSS,SY,T,WT

 SX =0.0
 SY =0.0
 ST2=0.0
 B  =0.0
 
 IF(MWT.NE.0)THEN
  SS=0.0
  DO I=1,NDATA
   WT=1.0/(SIG(I)**2)
   SS=SS+WT
   SX=SX+X(I)*WT
   SY=SY+Y(I)*WT
  ENDDO
 ELSE
  DO I=1,NDATA
   SX=SX+X(I)
   SY=SY+Y(I)   
  ENDDO
  SS=FLOAT(NDATA)
 ENDIF

 SXOSS=SX/SS

 IF(MWT.NE.0)THEN
  DO I=1,NDATA
   T=(X(I)-SXOSS)/SIG(I)
   ST2=ST2+T*T
   B=B+T*Y(I)/SIG(I)
  ENDDO
 ELSE
  DO I=1,NDATA
   T=X(I)-SXOSS
   ST2=ST2+T*T
   B=B+T*Y(I)
  ENDDO
 ENDIF

 B=B/ST2
 A=(SY-SX*B)/SS
 SIGA=SQRT((1.0+SX*SX/(SS*ST2))/SS)
 SIGB=SQRT(1.0/ST2)
 CHI2=0.0

 IF(MWT.EQ.0)THEN
  DO I=1,NDATA
   CHI2=CHI2+(Y(I)-A-B*X(I))**2
  ENDDO
  Q=1.0
  SIGDAT=SQRT(CHI2/REAL(NDATA-2))
  SIGA=SIGA*SIGDAT
  SIGB=SIGB*SIGDAT
 ELSE
  DO I=1,NDATA
   CHI2=CHI2+((Y(I)-A-B*X(I))/SIG(I))**2
  ENDDO
  Q=UTL_GAMMQ(0.5*(NDATA-2),0.5*CHI2)
 ENDIF
 
 END SUBROUTINE UTL_FIT_REGRESSION

 !###====================================================
 REAL FUNCTION UTL_GAMMQ(A,X)
 !###====================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: A,X
 REAL :: GAMMCF,GAMSER,GLN
 
 IF(X.LT.0.0.OR.A.LE.0.0)PAUSE 'BAD ARGUMENT IN UTL_GAMMQ'
 IF(X.LT.A+1.0)THEN
  CALL UTL_GSER(GAMSER,A,X,GLN)
  UTL_GAMMQ=1.0-GAMSER
 ELSE
  CALL UTL_GCF(GAMMCF,A,X,GLN)
  UTL_GAMMQ=GAMMCF
 ENDIF
   
 END FUNCTION
 
 !###====================================================
 SUBROUTINE UTL_GSER(GAMSER,A,X,GLN)
 !###====================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: A,X
 REAL,INTENT(OUT) :: GLN,GAMSER
 INTEGER,PARAMETER :: ITMAX=100
 REAL,PARAMETER :: EPS=3.0E-7
 INTEGER :: N
 REAL :: AP,DEL,SUM
 
 GLN=UTL_GAMMLN(A)
 IF(X.LE.0.0)THEN
  IF(X.LT.0.0)PAUSE 'X < 0 IN UTL_GSER'
  GAMSER=0.0
  RETURN
 ENDIF

 AP=A
 SUM=1.0/A
 DEL=SUM

 DO N=1,ITMAX
  AP=AP+1.0
  DEL=DEL*X/AP
  SUM=SUM+DEL
  IF(ABS(DEL).LT.ABS(SUM)*EPS)GOTO 1
 ENDDO

 PAUSE 'A TOO LARGE, ITMAX TOO SMALL IN UTL_GSER'
1 GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)
 
 END SUBROUTINE UTL_GSER

 !###====================================================
 SUBROUTINE UTL_GCF(GAMMCF,A,X,GLN)
 !###====================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: ITMAX=100
 REAL,PARAMETER :: EPS=3.0E-7,FPMIN=1.0-30
 REAL,INTENT(IN) :: A,X 
 REAL,INTENT(OUT) :: GAMMCF,GLN
 INTEGER :: I
 REAL :: AN,B,C,D,DEL,H
 
 GLN=UTL_GAMMLN(A)
 B=X+1.0-A
 C=1.0/FPMIN
 D=1.0/B
 H=D
 DO I=1,ITMAX
  AN=-I*(I-A) 
  B=B+2.0
  D=AN*D+B
  IF(ABS(D).LT.FPMIN)D=FPMIN
  C=B+AN/C
  IF(ABS(C).LT.FPMIN)C=FPMIN
  D=1.0/D
  DEL=D*C
  H=H*DEL
  IF(ABS(DEL-1.0).LT.EPS)GOTO 1
 ENDDO
 PAUSE 'A TOO LARGE, ITMAX TOOP SMALL IN UTL_GCF'
1 GAMMCF=EXP(-X+A*LOG(X)-GLN)*H
 
 END SUBROUTINE UTL_GCF
 
 !###====================================================
 REAL FUNCTION UTL_GAMMLN(XX)
 !###====================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XX
 INTEGER :: J
 DOUBLE PRECISION,SAVE :: SER,STP,TMP,X,Y,COF(6)
 DATA COF,STP/76.18009172947146D0,-86.50532032941677D0, &
              24.01409824083091D0,-1.231739572450155D0,0.1208650973866179D-2, &
              -0.5395239384953D-5,2.5066282746310005D0/
 
 X=XX
 Y=X
 TMP=X+5.5D0
 TMP=(X+0.5D0)*LOG(TMP)-TMP
 SER=1.000000000190015D0
 DO J=1,6
  Y=Y+1.0D0
  SER=SER+COF(J)/Y
 ENDDO
 UTL_GAMMLN=TMP+LOG(STP*SER/X)             
 
 END FUNCTION UTL_GAMMLN
 
END MODULE MOD_UTL
