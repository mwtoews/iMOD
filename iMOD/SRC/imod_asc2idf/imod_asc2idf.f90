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
MODULE MOD_ASC2IDF

USE WINTERACTER
USE RESOURCE
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFALLOCATEX,IDFDEALLOCATEX,IDFGETVAL,IDFNULLIFY,IDFDEALLOCATE,IDFCOPY,IDFIROWICOL, &
           IDFOPEN,IDFWRITEDIM,IDFWRITECOMMENT,IDFFILLCOMMENT,IDFDEALLOCATESX,IDFREADSCALE,IDFGETXYVAL,IDFGETLOC
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,RTOS,UTL_MESSAGEHANDLE,UTL_SUBST,UTL_WAITMESSAGE,UTL_CAP,UTL_DIRINFO,UTL_IDFSNAPTOGRID,&
    UTL_GETMED,UTL_DIRINFO_POINTER,UTL_IDATETOJDATE,UTL_GETMED,UTL_JDATETOIDATE,UTL_GENLABELSGET,VAR,UTL_PCK_READTXT
USE MODPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN,ICF

USE MOD_INTERSECT_PAR
USE MOD_INTERSECT, ONLY : INTERSECT_EQUI,INTERSECT_DEALLOCATE
USE MOD_ASC2IDF_HFB
USE MOD_ASC2IDF_PAR
USE MOD_KRIGING, ONLY : KRIGING_MAIN,KRIGING_VARIOGRAM,KRIGING_UNITTEST
USE MOD_SOLID_PCG, ONLY : SOLID_PCGINT,MICNVG,MXITER2,HCLOSE,RCLOSE,ITIGHT
USE MOD_BIVARIATE, ONLY : BIVARIATE_INT

INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: IOS
CHARACTER(LEN=20),ALLOCATABLE,DIMENSION(:),PRIVATE :: TXT
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
CHARACTER(LEN=256),PRIVATE :: LINE

CONTAINS

 !###======================================================================
 SUBROUTINE ASC2IDF_IMPORTASC_MAIN(DIR,TOPWC,BOTEL,MULT,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(INOUT) :: DIR,TOPWC
 REAL,INTENT(IN) :: BOTEL,MULT
 CHARACTER(LEN=256) :: ROOT,WC,LINE
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: IDFNAMES
 INTEGER :: I,J,K,N,IERROR,IOS
 REAL :: TOP,BOT

 I=INDEX(DIR,'\',.TRUE.); ROOT=DIR(:I-1); WC=TRIM(DIR(I+1:))
 CALL IOSDIRENTRYTYPE('F'); CALL IOSDIRCOUNT(TRIM(ROOT),TRIM(WC),N)
 ALLOCATE(IDFNAMES(N))
 CALL UTL_DIRINFO(TRIM(ROOT),TRIM(WC),IDFNAMES,N,'F')
 IF(N.EQ.0)THEN
  WRITE(*,*) 'No files found in: '//TRIM(DIR)
  RETURN
 ENDIF
 TOPWC=UTL_CAP(TOPWC,'U')
 DO I=1,SIZE(IDFNAMES)
  TOP=0.0; BOT=0.0
  IDFNAMES(I)=UTL_CAP(IDFNAMES(I),'U')
  IF(TOPWC.NE.'')THEN
   J=INDEX(IDFNAMES(I),TOPWC(1:INDEX(TOPWC,'*')-1))
   K=INDEX(IDFNAMES(I),TRIM(TOPWC(INDEX(TOPWC,'*')+1:)))
   IF(J.GT.0.AND.K.GT.J)THEN
    J=J+LEN(TOPWC(1:INDEX(TOPWC,'*')-1))
    K=K-1 
    READ(IDFNAMES(I)(J:K),*,IOSTAT=IOS) TOP
    IF(IOS.NE.0)EXIT
    BOT=TOP+BOTEL
   ENDIF
  ENDIF
  IDFNAMES(I)=TRIM(ROOT)//'\'//TRIM(IDFNAMES(I))
  WRITE(*,'(1X,A)') 'Importing '//TRIM(IDFNAMES(I))
  BOT=BOT*MULT; TOP=TOP*MULT
  LINE='--- skipping: '; IF(TOP.GT.BOT)LINE='+++ adding: '
  LINE=TRIM(LINE)//'TOP='//TRIM(RTOS(TOP,'F',2))//';BOT='//TRIM(RTOS(BOT,'F',2))
  WRITE(*,'(5X,A)') TRIM(LINE)
  CALL ASC2IDF_IMPORTASC(IDFNAMES(I),TOP,BOT,IERROR,IBATCH)
  IF(IERROR.EQ.1)EXIT
 ENDDO

 DEALLOCATE(IDFNAMES)

 END SUBROUTINE ASC2IDF_IMPORTASC_MAIN
 
 !###======================================================================
 SUBROUTINE ASC2IDF_IMPORTASC(IDFNAME,TOP,BOT,IERROR,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 INTEGER,INTENT(INOUT) :: IERROR
 REAL,INTENT(IN) :: TOP,BOT
 INTEGER :: IU,ASC_TYPE,I

 IERROR=1

 ALLOCATE(IOS(6),TXT(6))

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=IDFNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS(1))
 IF(IOS(1).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open file: '//CHAR(13)// &
   '['//TRIM(IDFNAME)//']'//CHAR(13)//'for reading','Error')
  RETURN
 ENDIF

 !## skip header with info ... optional
 DO
  READ(IU,'(A256)',IOSTAT=IOS(1)) LINE
  IF(INDEX(LINE,'*').EQ.0)EXIT
 ENDDO
 !## end of file read
 IF(IOS(1).LT.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'File is probably empty!','Error')
  RETURN
 ENDIF

 !## find out what kind of file ... try to read at least 4 labels ...
 READ(LINE,*,IOSTAT=IOS(1)) (TXT(I),I=1,4)

 !## 3d-ascii-file
 IF(IOS(1).EQ.0)THEN
  ASC_TYPE=1
 !## 2d ascii-file
 ELSE
  ASC_TYPE=0
 ENDIF

 IF(ASC_TYPE.EQ.0)THEN
  IF(.NOT.ASC2IDF_IMPORTASC_TYPE1(IU,IDFNAME,TOP,BOT,IBATCH))IERROR=0
 ELSEIF(ASC_TYPE.EQ.1)THEN
  IF(.NOT.ASC2IDF_IMPORTASC_TYPE2(IU,IDFNAME))IERROR=0
 ENDIF

 CALL ASC2IDF_INT_CLOSE(IU)

 CALL UTL_MESSAGEHANDLE(1)
 IERROR=0

 END SUBROUTINE ASC2IDF_IMPORTASC

 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_IMPORTASC_TYPE1(IU,IDFNAME,TOP,BOT,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IBATCH
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 REAL,INTENT(IN) :: TOP,BOT
 INTEGER :: J,NCOL,NROW,IROW,ICOL
 REAL :: XMIN,XMAX,YMIN,YMAX,CSIZE,NODATA
 DOUBLE PRECISION :: DNODATA
 LOGICAL :: LEX,LBIG

 ASC2IDF_IMPORTASC_TYPE1=.FALSE.

 !## catched in idfwrite()
 J=INDEXNOCASE(IDFNAME,'.',.TRUE.)-1
 INQUIRE(FILE=IDFNAME(:J)//'.IDF',EXIST=LEX)
 IF(LEX)THEN
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Do you want to overwrite the existing file'//CHAR(13)// &
   IDFNAME(:J)//'.IDF ?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)

 ALLOCATE(IDF(1)); CALL IDFNULLIFY(IDF(1))

 READ(LINE,*,IOSTAT=IOS(1)) TXT(1),NCOL
 READ(IU,*,IOSTAT=IOS(2))   TXT(2),NROW

 READ(IU,*,IOSTAT=IOS(3))   TXT(3),XMIN
 !## xllcenter-xllcorner
 TXT(3)=UTL_CAP(TXT(3),'U')

 READ(IU,*,IOSTAT=IOS(4))   TXT(4),YMIN
 !## recompute yllcenter-yllcorner
 TXT(4)=UTL_CAP(TXT(4),'U')

 READ(IU,*,IOSTAT=IOS(5))   TXT(5),CSIZE

 !## nodata is optional
 READ(IU,'(A256)',IOSTAT=IOS(6))   LINE

 IF(SUM(IOS).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading header of ascii file!','Error')
  RETURN
 ENDIF

 !## nodata is optional
 READ(LINE,*,IOSTAT=IOS(6))   TXT(6),DNODATA 
 READ(LINE,*,IOSTAT=IOS(6))   TXT(6),NODATA 
 IF(IOS(6).NE.0)THEN; NODATA=-99999.99; DNODATA=NODATA; ENDIF
 
 IF(TRIM(TXT(3)).EQ.'XLLCENTER')XMIN=XMIN-(CSIZE/2.0)
 IF(TRIM(TXT(4)).EQ.'YLLCENTER')YMIN=YMIN-(CSIZE/2.0)
 YMAX=YMIN+NROW*CSIZE
 XMAX=XMIN+NCOL*CSIZE

 IDF(1)%NCOL  =NCOL
 IDF(1)%NROW  =NROW
 IDF(1)%XMIN  =XMIN
 IDF(1)%XMAX  =XMAX
 IDF(1)%YMIN  =YMIN
 IDF(1)%YMAX  =YMAX
 IDF(1)%NODATA=NODATA
 IDF(1)%IEQ   =0
 IDF(1)%DX    =CSIZE
 IDF(1)%DY    =CSIZE
 IDF(1)%IXV   =0
 IDF(1)%ITB   =0
 IF(TOP-BOT.NE.0.0)THEN
  IDF(1)%ITB=1
  IDF(1)%TOP=TOP
  IDF(1)%BOT=BOT
 ENDIF
 CALL IDFDEALLOCATEX(IDF(1))
 CALL IDFDEALLOCATESX(IDF(1))
 
 LBIG=.FALSE.
 IF(.NOT.IDFALLOCATEX(IDF(1)))THEN
  ALLOCATE(IDF(1)%X(IDF(1)%NCOL,1),STAT=IOS(1))
  IF(IOS(1).NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not allocate enough memory to convert ASC into an IDF file.'//CHAR(13)// &
        'ncol='//TRIM(ITOS(IDF(1)%NCOL))//';nrow='//TRIM(ITOS(IDF(1)%NROW)),'Error')
   RETURN
  ENDIF
  LBIG=.TRUE.
 ENDIF

 IF(LBIG)THEN
  !## open idf
  J=INDEXNOCASE(IDFNAME,'.',.TRUE.)-1
  IF(IDFOPEN(IDF(1)%IU,IDFNAME(:J)//'.IDF','WO',1,IQUESTION=1).AND. &
     IDFWRITEDIM(1,IDF(1)))THEN
  ELSE
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not CREATE file: '//TRIM(IDFNAME)//'.'//CHAR(13)// &
        'Error opening the file and/or writing the IDF header','Error')
   RETURN
  ENDIF
 ENDIF
 
 IOS=0
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Importing '//TRIM(IDFNAME))

 IF(LBIG)THEN
  DO IROW=1,NROW
   READ(IU,*,IOSTAT=IOS(1)) (IDF(1)%X(ICOL,1),ICOL=1,NCOL)
   WRITE(IDF(1)%IU) (IDF(1)%X(ICOL,1),ICOL=1,NCOL)
   IF(IOS(1).NE.0)EXIT
  ENDDO
 ELSE
  DO IROW=1,NROW
   READ(IU,*,IOSTAT=IOS(1)) (IDF(1)%X(ICOL,IROW),ICOL=1,NCOL)
   IF(IOS(1).NE.0)EXIT
  ENDDO
 ENDIF
 IF(IOS(1).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading irow='//TRIM(ITOS(IROW))// &
     ' of data block of ascii file!'//CHAR(13)//'Error code IOSTAT='//TRIM(ITOS(IOS(1))),'Error')
 ENDIF
 CLOSE(IU)

 CALL IDFFILLCOMMENT(IDF(1),'Imported from '//TRIM(IDFNAME))

 IF(LBIG)THEN
  CALL IDFWRITECOMMENT(IDF(1),1)
 ELSE
  J=INDEXNOCASE(IDFNAME,'.',.TRUE.)-1
  IF(.NOT.IDFWRITE(IDF(1),IDFNAME(:J)//'.IDF',ABS(IBATCH-1)))THEN; ENDIF
 ENDIF
 
 ASC2IDF_IMPORTASC_TYPE1=.TRUE.

 END FUNCTION ASC2IDF_IMPORTASC_TYPE1

 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_IMPORTASC_TYPE2(IU,IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 CHARACTER(LEN=256) :: LINE
 REAL,DIMENSION(0:3) :: X,Y
 REAL,DIMENSION(:),ALLOCATABLE :: Z
 REAL :: CS,A,B,C,D
 INTEGER :: NGRID,N,NODES,I,J,IROW,ICOL,JROW,JCOL

 ASC2IDF_IMPORTASC_TYPE2=.FALSE.

 X(1)= 10.0E10
 Y(1)= 10.0E10
 X(2)=-10.0E10
 Y(2)=-10.0E10

 CALL WINDOWSELECT(0)
 CALL WINDOWOUTSTATUSBAR(4,'Importing '//TRIM(IDFNAME))

 N=0
 NGRID=0
 DO
  !## read file to find out dimensions
  READ(IU,*,IOSTAT=IOS(1)) X(0),Y(0)
  IF(IOS(1).NE.0)EXIT
  N=N+1
  X(1)=MIN(X(0),X(1))
  X(2)=MAX(X(0),X(2))
  Y(1)=MIN(Y(0),Y(1))
  Y(2)=MAX(Y(0),Y(2))
  !## store first result
  IF(N.EQ.1)THEN
   X(3)=X(0)
   Y(3)=Y(0)
  ENDIF
  IF(X(3).EQ.X(0).AND.Y(3).EQ.Y(0))NGRID=NGRID+1
 ENDDO

 IF(NGRID.EQ.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not read this ASCII file properly','Error')
  RETURN
 ENDIF

 ALLOCATE(IDF(NGRID))
 DO I=1,NGRID; CALL IDFNULLIFY(IDF(I)); ENDDO

 !## number of rastercells in each grid=n/ngrid
 NODES=N/NGRID
 !## cellsize=
 A=  REAL(NODES)-1.0
 B=-(X(2)-X(1))-(Y(2)-Y(1))
 C=-(X(2)-X(1))*(Y(2)-Y(1))
 D= (B**2.0)-(4.0*A*C)

 IF(D.GT.0.0)THEN
  D=SQRT(D)
  CS=(-B+D)/(2.0*A)
  IF(CS.LT.0.0)CS=(-B-D)/(2.0*A)
  IF(CS.LT.0.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not determine cellsize','Error')
   RETURN
  ENDIF
 ENDIF

 IDF(1)%DX  =CS
 IDF(1)%DY  =CS
 IDF(1)%IEQ =0
 IDF(1)%ITB =1
 IDF(1)%IXV =0
 IDF(1)%XMIN=X(1)-IDF(1)%DX
 IDF(1)%XMAX=X(2)+IDF(1)%DX
 IDF(1)%YMIN=Y(1)-IDF(1)%DY
 IDF(1)%YMAX=Y(2)+IDF(1)%DY
 IDF(1)%NCOL=(IDF(1)%XMAX-IDF(1)%XMIN)/IDF(1)%DX
 IDF(1)%NROW=(IDF(1)%YMAX-IDF(1)%YMIN)/IDF(1)%DY
 IDF(1)%NODATA=999.0
 IF(.NOT.IDFALLOCATEX(IDF(1)))RETURN

 DO I=1,NGRID; CALL IDFCOPY(IDF(1),IDF(I)); IDF(I)%NODATA=999.0; END DO

 REWIND(IU)

 !## skip header with info ... optional
 DO
  READ(IU,'(A256)',IOSTAT=IOS(1)) LINE
  IF(INDEX(LINE,'*').EQ.0)EXIT
 ENDDO

 !## find out what kind of file ... try to read at least 4 labels ...
 READ(LINE,*,IOSTAT=IOS(1)) (TXT(I),I=1,4)

 ALLOCATE(Z(NGRID))

 DO ICOL=1,IDF(1)%NCOL
  DO IROW=1,IDF(1)%NROW
   DO I=NGRID,1,-1
    !## read file to find out dimensions
    READ(IU,*,IOSTAT=IOS(1)) X(0),Y(0),Z(I),A
    CALL IDFIROWICOL(IDF(I),JROW,JCOL,X(0),Y(0))
    IDF(I)%X(JCOL,JROW)=A
    IF(IOS(1).NE.0)EXIT
   ENDDO
  ENDDO
 ENDDO

 DO I=1,NGRID
  IF(I.EQ.1)THEN
   IDF(I)%TOP= Z(I)+(Z(I)-Z(I+1))
   IDF(I)%BOT=(Z(I+1)+Z(I))/2.0
  ELSEIF(I.EQ.NGRID)THEN
   IDF(I)%TOP=(Z(I-1)+Z(I))/2.0
   IDF(I)%BOT= Z(I)-(Z(I-1)-Z(I))
  ELSE
   IDF(I)%TOP=(Z(I-1)+Z(I))/2.0
   IDF(I)%BOT=(Z(I+1)+Z(I))/2.0
  ENDIF
 END DO

 DEALLOCATE(Z)

 J=INDEX(IDFNAME,'.',.TRUE.)-1
 DO I=1,NGRID
  IF(.NOT.IDFWRITE(IDF(I),IDFNAME(:J)//'zone'//TRIM(ITOS(I))//'.IDF',1))THEN
   !## error occured
   RETURN
  ENDIF
 END DO

 ASC2IDF_IMPORTASC_TYPE2=.TRUE.

 END FUNCTION ASC2IDF_IMPORTASC_TYPE2

 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_INT_MAIN(IFILE,XMIN,YMIN,XMAX,YMAX,IDEPTH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFILE,IDEPTH
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 REAL :: X,Y,Z
 REAL,DIMENSION(1) :: ZPERC
 CHARACTER(LEN=5) :: EXT
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 REAL,ALLOCATABLE,DIMENSION(:) :: RC,RR,ZV
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:,:,:) :: IPC
 INTEGER :: IU,ID,IROW,ICOL,J,K,M,MM,II,JJ,MX,IIPF,IERROR,NCOL,IEXT,NP,IMASK
 INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:,:) :: NZ
 LOGICAL :: LEX
 INTEGER(KIND=8) :: STIME,ETIME

 ASC2IDF_INT_MAIN=.FALSE.

 CALL ASC2IDF_INT_NULLIFY()
 
 ALLOCATE(IOS(3))

 IF(TRIMDEPTH_IDF%FNAME.NE.'')THEN
  IF(.NOT.IDFREAD(TRIMDEPTH_IDF,TRIMDEPTH_IDF%FNAME,0))RETURN
 ENDIF
 
 IIPF=0
 IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.XYZ',.TRUE.).GT.0)IIPF=1
 IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.IPF',.TRUE.).GT.0)IIPF=2
 IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.IDF',.TRUE.).GT.0)IIPF=3
 IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.GEN',.TRUE.).GT.0)IIPF=4
! IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.IFF',.TRUE.).GT.0)IIPF=5

 IF(IIPF.EQ.0)THEN; WRITE(*,'(A)') 'Not correct file format specified, choose IPF, IDF, GEN or XYZ'; RETURN; ENDIF
 
 ALLOCATE(IDF(4)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
 !## initiate idf-file
 IDF(1)%DX=CS; IDF(1)%DY=CS; IDF(1)%XMIN=XMIN; IDF(1)%YMIN=YMIN; IDF(1)%XMAX=XMAX; IDF(1)%YMAX=YMAX
 IDF(1)%IEQ=INT(0,1); IDF(1)%ITB=INT(0,1); IDF(1)%IXV=INT(0,1); IDF(1)%NODATA=NODATA
 
 WRITE(*,'(A)') 'Get window ...'

 ALLOCATE(XP(100),YP(100),ZP(100),WP(100))
 
 !## get windows for xyz,ipf,gen,idf types
 IF(.NOT.ASC2IDF_INT_GETEXTENT(XYZFNAMES(IFILE),IIPF))RETURN
  
 IF(.NOT.IDFALLOCATEX(IDF(1)))RETURN; IDF(1)%X=NODATA
 !## make copies
 DO I=2,SIZE(IDF); CALL IDFCOPY(IDF(1),IDF(I)); ENDDO

 !## compute block-faces
 ALLOCATE(IPC(IDF(1)%NCOL,IDF(1)%NROW,2)); IPC=INT(0,1)
 IF(LEN_TRIM(BLNFILE).GT.0)CALL ASC2IDF_HFB(IDF(1),IDF(1)%NROW,IDF(1)%NCOL,IPC,BLNFILE)

 !## collect x,y,z values
 WRITE(*,'(A)') 'Fill grid ...'
 IF(.NOT.ASC2IDF_INT_GETVALUES(XYZFNAMES(IFILE),IIPF,IXCOL,IYCOL,IZCOL))RETURN
 
 !## read mask idf file
 IMASK=0; IF(TRIM(XYZFNAMES(2)).NE.'')THEN
  !## scale most frequent occurence
  IF(.NOT.IDFREADSCALE(XYZFNAMES(2),IDF(3),7,1,0.0,0))RETURN; IMASK=1
 ENDIF

 !## compute interpolated values
 SELECT CASE (IGRIDFUNC)

  !## min,max,mean
  CASE (1,2,3)
   IDF(1)%X=IDF(1)%NODATA; IDF(2)%X=0.0
   DO I=1,SIZE(XP)
    IF(ZP(I).EQ.IDF(1)%NODATA)CYCLE
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,XP(I),YP(I))
    !## min
    IF(IGRIDFUNC.EQ.1)THEN
     IF(IDF(2)%X(ICOL,IROW).EQ.0)THEN
      IDF(1)%X(ICOL,IROW)=ZP(I)
     ELSE
      IDF(1)%X(ICOL,IROW)=MIN(IDF(1)%X(ICOL,IROW),ZP(I))
     ENDIF
    !## max
    ELSEIF(IGRIDFUNC.EQ.2)THEN
     IF(IDF(2)%X(ICOL,IROW).EQ.0)THEN
      IDF(1)%X(ICOL,IROW)=ZP(I)
     ELSE
      IDF(1)%X(ICOL,IROW)=MAX(IDF(1)%X(ICOL,IROW),ZP(I))
     ENDIF
    !## mean
    ELSEIF(IGRIDFUNC.EQ.3)THEN
     IF(IDF(2)%X(ICOL,IROW).EQ.0)THEN
      IDF(1)%X(ICOL,IROW)=ZP(I)
     ELSE
      IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)+ZP(I)
     ENDIF
    ENDIF
    IDF(2)%X(ICOL,IROW)=IDF(2)%X(ICOL,IROW)+1.0
   ENDDO
   !## compute mean
   IF(IGRIDFUNC.EQ.3)THEN
    DO ICOL=1,IDF(1)%NCOL; DO IROW=1,IDF(1)%NROW
     IF(IDF(2)%X(ICOL,IROW).GT.0)IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)/IDF(2)%X(ICOL,IROW)
    ENDDO; ENDDO
   ENDIF
   
  !## percentiles
  CASE (4)
   N=SIZE(XP)
   ALLOCATE(RC(N),STAT=IOS(1)); ALLOCATE(RR(N),STAT=IOS(2)); ALLOCATE(ZV(N),STAT=IOS(3))
   IF(SUM(IOS).NE.0)THEN
    WRITE(*,'(A,I10,A1,I10,A)') 'ERROR, can not allocate memory for IC,IR,ZV(',N,')-arrays.'; RETURN
   ENDIF

   DO I=1,SIZE(XP)
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,XP(I),YP(I))
    RC(I)=REAL(ICOL); RR(I)=REAL(IROW); ZV(I)=ZP(I)
   ENDDO
  
   !## sort array
   WRITE(*,'(A)') 'Sorting grid ...'
   M=INT(N); CALL SORTEM(1,M,RR,2,RC,ZV,ZV,ZV,ZV,ZV,ZV)
   WRITE(*,'(A)') 'Finished Sorting grid ...'
   WRITE(*,'(A)') 'Start assigning percentiles to grid ...'

   I=1
   DO
    !## get offset for current row
    IROW=INT(RR(I))

    J=I+1
    DO
     IF(INT(RR(J)).NE.INT(RR(I)))EXIT
     J=J+1; IF(J.GT.N)EXIT
    ENDDO

    !## sort for columns
    M=J-I

    !## row found
    IF(M.GT.0)THEN

     CALL SORTEM(1,M,RC(I:),2,RR(I:),ZV(I:),ZV(I:),ZV(I:),ZV(I:),ZV(I:),ZV(I:))

     II=I
     DO
      !## get offset for current col
      JJ=MIN(SIZE(RC),II+1)
      DO
       IF(INT(RC(JJ)).NE.INT(RC(II)))EXIT
       JJ=JJ+1
       IF(JJ.GE.M+I)EXIT
       IF(JJ.GE.SIZE(RC))EXIT
      ENDDO
      !## get percentiles for each column in current row
      MM=JJ-II

      IF(MM.GT.0)THEN
       ICOL=INT(RC(II))
       CALL UTL_GETMED(ZV(II:),MM,NODATA,(/PERCENTILE/),1,MX,ZPERC)
       IDF(1)%X(ICOL,IROW)=ZPERC(1)
      ENDIF
      II=JJ
      IF(II.GE.M+I)EXIT
     ENDDO

    ENDIF
    !## stop everything done
    IF(J.GE.N)EXIT
    I=J

   ENDDO
   WRITE(*,'(A)') 'Finished assigning percentiles to grid ...'

  !## bivariate
  CASE (5)
   N=SIZE(XP); CALL BIVARIATE_INT(XP,YP,ZP,INT(N,4),IERROR,IDF(1))
  
  !## kriging
  CASE (-6,6)
   
   IF(MINP.EQ.0)MINP=SIZE(XP)
   !## each cell need to be interpolated
   IDF(1)%X=IDF(1)%NODATA
   !## simple kriging (+), ordinary kriging(-)
   KTYPE=SIGN(KTYPE,IGRIDFUNC)
   CALL KRIGING_MAIN(SIZE(XP),XP,YP,ZP,IDF(1),IDF(4),MINP,RANGE,SILL,NUGGET, KTYPE,PNTSEARCH, &
                              LAGINTERVAL,LAGDISTANCE,IADJRANGE,IQUADRANT,1,'') !## simple kriging

  !## variogram
  CASE (7)
   CALL KRIGING_VARIOGRAM(INT(N,4),XP,YP,ZP,J,IDF(1),LAGINTERVAL,LAGDISTANCE,IBATCH=1,SNAME=XYZFNAMES(IFILE))

  CASE (8)

   DO I=1,SIZE(XP)
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,XP(I),YP(I))
    XP(I)=REAL(ICOL); YP(I)=REAL(IROW)
   ENDDO
   MICNVG=25; ITIGHT=1; IF(IIPF.EQ.4)ITIGHT=2
   CALL SOLID_PCGINT(XP,YP,ZP,SIZE(XP),IERROR,IDF(1),-1,CD=WP,IPC=IPC,CC_GIVEN=IDF(3))
  
 END SELECT
 
 DEALLOCATE(IPC)
 
 !## correct for "depth" mask file
 IF(IGRIDFUNC.NE.7)THEN
  
  !## apply log-transformation on final results
  IF(ILOG.EQ.1)THEN
   DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
    IF(IDF(1)%X(ICOL,IROW).NE.0.0)IDF(1)%X(ICOL,IROW)=EXP(IDF(1)%X(ICOL,IROW))
   ENDDO; ENDDO
  ENDIF
  
  !## apply mask on final results
  IF(IMASK.EQ.1)THEN
   DO IROW=1,IDF(2)%NROW; DO ICOL=1,IDF(2)%NCOL  
    IF(IDF(3)%X(ICOL,IROW).EQ.IDF(3)%NODATA)IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
   ENDDO; ENDDO
  ENDIF

  !## apply depth-correction
  IF(IDEPTH.EQ.1)THEN
   IDF(1)%ITB=INT(1,1)
   IDF(1)%TOP=REAL(ASSF_STARTDATE)
   IDF(1)%BOT=REAL(ASSF_ENDDATE)
   !## blank-out in case surface has been read in
   IF(TRIMDEPTH_IDF%FNAME.NE.'')THEN
    DO IROW=1,IDF(1)%NROW
     DO ICOL=1,IDF(1)%NCOL
      CALL IDFGETLOC(IDF(1),IROW,ICOL,X,Y)
      Z=IDFGETXYVAL(TRIMDEPTH_IDF,X,Y)
      IF(Z.NE.TRIMDEPTH_IDF%NODATA)THEN
       IF((IDF(1)%TOP+IDF(1)%BOT)/2.0.GT.Z)IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
      ELSE
       IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
      ENDIF
     ENDDO
    ENDDO
   ENDIF
  ENDIF
  IF(IDFWRITE(IDF(1),IDFFILE,1))ASC2IDF_INT_MAIN=.TRUE.
  IF(ABS(IGRIDFUNC).EQ.6)THEN; IF(IDFWRITE(IDF(4),STDEVIDF,1))THEN; ENDIF; ENDIF
 ENDIF
 
 CALL ASC2IDF_INT_CLOSE(IU)
 IF(ALLOCATED(RC))DEALLOCATE(RC); IF(ALLOCATED(RR))DEALLOCATE(RR); IF(ALLOCATED(ZV))DEALLOCATE(ZV)
 CALL ASC2IDF_INT_DEALLOCATE()

 ASC2IDF_INT_MAIN=.TRUE.

 END FUNCTION ASC2IDF_INT_MAIN

 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_INT_GETVALUES(FNAME,ITYPE,IXCOL,IYCOL,IZCOL)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE,IXCOL,IYCOL,IZCOL
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER :: I,J,IU,IOS,NCOL,IROW,ICOL,N,M,IEXT,NP !,ID
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 CHARACTER(LEN=5) :: EXT
 CHARACTER(LEN=52) :: CID
 CHARACTER(LEN=256) :: TXTFNAME
 REAL :: X,Y,Z,X1,X2,Y1,Y2,ZV
 LOGICAL :: LEX
 INTEGER(KIND=8) :: STIME,ETIME
 
 ASC2IDF_INT_GETVALUES=.FALSE.
 
 IF(ITYPE.NE.3)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)THEN; WRITE(*,'(A)') 'Can not open file: ['//TRIM(FNAME)//'] for reading'; RETURN; ENDIF
 ENDIF
  
 SELECT CASE (ITYPE)
  !## xyz
  CASE (1)
   READ(IU,*); NP=0
   DO
    READ(IU,*,IOSTAT=IOS) X,Y,Z
    IF(IOS.NE.0)EXIT
    NP=NP+1; CALL ASC2IDF_INT_RESIZEVECTORS(NP,100)
    XP(NP)=X; YP(NP)=Y; ZP(NP)=Z; WP(NP)=1.0
   ENDDO
   
  !## ipf
  CASE (2)
   READ(IU,*) N; READ(IU,*) M; DO I=1,M; READ(IU,*); ENDDO; READ(IU,*) IEXT,EXT
   NCOL=MAX(1,IXCOL,IYCOL,IZCOL,IEXT); ALLOCATE(STRING(NCOL))
   NP=0; DO I=1,N
    READ(IU,*) (STRING(J),J=1,NCOL); READ(STRING(IXCOL),*) X; READ(STRING(IYCOL),*) Y
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,X,Y)
    !## outside window
    IF(IROW.EQ.0.OR.ICOL.EQ.0)CYCLE
  
    LEX=.TRUE.
    IF(ASSF_COLUMN.EQ.0)THEN
     READ(STRING(IZCOL),*) Z
    ELSE
     STIME=UTL_JDATETOIDATE(ASSF_STARTDATE)*1000000
     ETIME=UTL_JDATETOIDATE(ASSF_ENDDATE)  *1000000
     TXTFNAME=FNAME(:INDEX(FNAME,'\',.TRUE.))//TRIM(STRING(IEXT))//'.'//TRIM(EXT)
     LEX=UTL_PCK_READTXT(ASSF_COLUMN,STIME,ETIME,ASSF_MTYPE,Z,TXTFNAME) 
    ENDIF
    
    !## add point to list
    IF(LEX)THEN
     NP=NP+1; CALL ASC2IDF_INT_RESIZEVECTORS(NP,100)
     XP(NP)=X; YP(NP)=Y; ZP(NP)=Z; WP(NP)=1.0
    ENDIF
    
   ENDDO
   DEALLOCATE(STRING)

  !## idf file
  CASE (3)
   !## scale mean/smooth
   IF(.NOT.IDFREADSCALE(FNAME,IDF(2),2,1,0.0,0))RETURN 
   NP=IDF(1)%NCOL*IDF(1)%NROW; CALL ASC2IDF_INT_RESIZEVECTORS(NP,0)
   NP=0; DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
    IF(IDF(2)%X(ICOL,IROW).NE.IDF(2)%NODATA)THEN 
     NP=NP+1; CALL IDFGETLOC(IDF(2),IROW,ICOL,XP(NP),YP(NP)); ZP(NP)=IDF(2)%X(ICOL,IROW); WP(NP)=1.0
    ENDIF
   ENDDO; ENDDO

  !## gen file (intersect)
  CASE (4)
   NP=0
   DO
    READ(IU,*,IOSTAT=IOS) CID; IF(IOS.NE.0)EXIT
    IF(TRIM(UTL_CAP(CID,'U')).EQ.'END')EXIT
    
    !## get value for line
    CALL UTL_GENLABELSGET(CID,J)
    IF(J.LE.0)THEN
     WRITE(*,'(A)') 'Cannot get the label for current line '//TRIM(CID); RETURN
    ENDIF
    READ(VAR(IZCOL,J),*,IOSTAT=IOS) ZV
    IF(IOS.NE.0)THEN; WRITE(*,'(A)') 'Cannot convert '//TRIM(VAR(IZCOL,J))//' into a real.'; RETURN; ENDIF

    I=0
    DO
     READ(IU,*,IOSTAT=IOS) X2,Y2; IF(IOS.NE.0)EXIT
     I=I+1
     IF(I.GT.1)THEN
      !## intersect line
      N=0; CALL INTERSECT_EQUI(IDF(1)%XMIN,IDF(1)%XMAX,IDF(1)%YMIN,IDF(1)%YMAX,IDF(1)%DX,X1,X2,Y1,Y2,N,.FALSE.,.FALSE.)
      DO J=1,N
       X=XA(J); Y=YA(J); CALL IDFIROWICOL(IDF(1),IROW,ICOL,X,Y)
       !## outside window
       IF(IROW.EQ.0.OR.ICOL.EQ.0)CYCLE    
       NP=NP+1; CALL ASC2IDF_INT_RESIZEVECTORS(NP,100)
       XP(NP)=XA(J); YP(NP)=YA(J); ZP(NP)=ZV; WP(NP)=LN(J)
      ENDDO
     ENDIF     
     X1=X2; Y1=Y2
    ENDDO
   ENDDO
   CALL INTERSECT_DEALLOCATE()

 END SELECT
  
 CALL ASC2IDF_INT_RESIZEVECTORS(NP,0)
 
 IF(ILOG.EQ.1)THEN
  DO I=1,SIZE(XP)
   IF(ZP(I).GT.0.0)THEN
    ZP(I)=LOG(ZP(I))
   ELSE
    ZP(I)=-5.0 !LOG(0.000)
   ENDIF
  ENDDO
 ENDIF

 !## scale wp - to unity
 X1=MAXVAL(WP) 
 WP=WP/X1
 
 ASC2IDF_INT_GETVALUES=.TRUE.
 
 END FUNCTION ASC2IDF_INT_GETVALUES
 
 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_INT_GETEXTENT(FNAME,ITYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ITYPE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 INTEGER :: I,J,K,IOS,N,M,IU,ID,NP,NCOL
 REAL :: X,Y

 ASC2IDF_INT_GETEXTENT=.FALSE.
 
 !## create idf based upon given window
 IF(IDF(1)%XMIN.NE.IDF(1)%XMAX.AND.IDF(1)%YMIN.NE.IDF(1)%YMAX)THEN 
  CALL UTL_IDFSNAPTOGRID(IDF(1)%XMIN,IDF(1)%XMAX,IDF(1)%YMIN,IDF(1)%YMAX,IDF(1)%DX,IDF(1)%NCOL,IDF(1)%NROW)
  ASC2IDF_INT_GETEXTENT=.TRUE.; RETURN
 ENDIF

 NP=0 
  
 IDF(1)%XMIN=10.0E10; IDF(1)%YMIN=10.0E10; IDF(1)%XMAX=-10.0E10; IDF(1)%YMAX=-10.0E10

 IF(ITYPE.NE.3)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)THEN; WRITE(*,'(A)') 'Can not open file: ['//TRIM(FNAME)//'] for reading'; RETURN; ENDIF
 ENDIF

 SELECT CASE (ITYPE)
  CASE (1,2)

   NCOL=MAX(IXCOL,IYCOL,1); ALLOCATE(STRING(NCOL))

   !## read header ipf file
   IF(ITYPE.EQ.2)THEN
    READ(IU,*) N; READ(IU,*) M; DO I=1,M; READ(IU,*); ENDDO; READ(IU,*)
   ELSEIF(ITYPE.EQ.1)THEN
    READ(IU,*)
   ENDIF

   !## read file to find out dimensions
   I=0; DO
    READ(IU,*,IOSTAT=IOS) (STRING(K),K=1,NCOL)
    IF(IOS.NE.0)EXIT 
    READ(STRING(IXCOL),*) X; READ(STRING(IYCOL),*) Y
    NP=NP+1
    IDF(1)%XMIN=MIN(IDF(1)%XMIN,X); IDF(1)%XMAX=MAX(IDF(1)%XMAX,X)
    IDF(1)%YMIN=MIN(IDF(1)%YMIN,Y); IDF(1)%YMAX=MAX(IDF(1)%YMAX,Y)
    !## stop in case of ipf file
    IF(ITYPE.EQ.2.AND.I.GE.N)EXIT
   ENDDO
   IDF(1)%XMIN=IDF(1)%XMIN-IDF(1)%DX; IDF(1)%XMAX=IDF(1)%XMAX+IDF(1)%DX
   IDF(1)%YMIN=IDF(1)%YMIN-IDF(1)%DY; IDF(1)%YMAX=IDF(1)%YMAX+IDF(1)%DY
   DEALLOCATE(STRING)
   
  !## idf-files
  CASE (3)
  
   IF(.NOT.IDFREAD(IDF(1),FNAME,0))RETURN; IU=IDF(1)%IU
   NP=IDF(1)%NROW*IDF(1)%NCOL 
   
  !## gen-files
  CASE (4)

   DO
    READ(IU,*,IOSTAT=IOS) ID; IF(IOS.NE.0)EXIT
    DO
     READ(IU,*,IOSTAT=IOS) X,Y; IF(IOS.NE.0)EXIT
     NP=NP+1
     IDF(1)%XMIN=MIN(IDF(1)%XMIN,X); IDF(1)%XMAX=MAX(IDF(1)%XMAX,X)
     IDF(1)%YMIN=MIN(IDF(1)%YMIN,Y); IDF(1)%YMAX=MAX(IDF(1)%YMAX,Y)
    ENDDO
   ENDDO
   IDF(1)%XMIN=IDF(1)%XMIN-IDF(1)%DX; IDF(1)%XMAX=IDF(1)%XMAX+IDF(1)%DX
   IDF(1)%YMIN=IDF(1)%YMIN-IDF(1)%DY; IDF(1)%YMAX=IDF(1)%YMAX+IDF(1)%DY
   
 END SELECT
  
 CLOSE(IU)
 
 WRITE(*,'(A,I20,A)') 'Found ',NP,' points'
 IF(NP.EQ.0)RETURN
 
 CALL UTL_IDFSNAPTOGRID(IDF(1)%XMIN,IDF(1)%XMAX,IDF(1)%YMIN,IDF(1)%YMAX,IDF(1)%DX,IDF(1)%NCOL,IDF(1)%NROW)
 
 ASC2IDF_INT_GETEXTENT=.TRUE.
 
 END FUNCTION ASC2IDF_INT_GETEXTENT

 !###======================================================================
 SUBROUTINE ASC2IDF_INT_CLOSE(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 LOGICAL :: LEX

 IF(ALLOCATED(IOS))DEALLOCATE(IOS)
 IF(ALLOCATED(TXT))DEALLOCATE(TXT)
 INQUIRE(UNIT=IU,OPENED=LEX); IF(LEX)CLOSE(IU)
 IF(ALLOCATED(IDF))THEN; CALL IDFDEALLOCATE(IDF,SIZE(IDF)); DEALLOCATE(IDF); ENDIF

 END SUBROUTINE ASC2IDF_INT_CLOSE

 !###======================================================================
 SUBROUTINE ASC2IDF_EXPORTASC_MAIN(DIR,IQUICK,IBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IQUICK,IBATCH
 CHARACTER(LEN=*),INTENT(INOUT) :: DIR
 CHARACTER(LEN=256) :: ROOT,WC
 CHARACTER(LEN=15) :: CH
 CHARACTER(LEN=256),POINTER,DIMENSION(:) :: IDFNAMES
 INTEGER :: I,J,ICOL,IROW,IU,IREC
 REAL :: X
 TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
 
 I=INDEX(DIR,'\',.TRUE.);
 IF(I.GT.0)THEN
  ROOT=DIR(:I-1); WC=TRIM(DIR(I+1:))
 ELSE
  ROOT='.\'; WC=TRIM(DIR)
 ENDIF
 IF(.NOT.UTL_DIRINFO_POINTER(ROOT,WC,IDFNAMES,'F'))RETURN

 ALLOCATE(IDF(1)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO
 DO I=1,SIZE(IDFNAMES)
 
  WRITE(*,*) TRIM(IDFNAMES(I))
 
  IF(.NOT.IDFREAD(IDF(1),TRIM(ROOT)//'\'//TRIM(IDFNAMES(I)),1))THEN
   IF(.NOT.IDFREAD(IDF(1),TRIM(ROOT)//'\'//TRIM(IDFNAMES(I)),0))EXIT
   WRITE(*,'(A)') ' Reading/exporting IDF record-by-record'
  ELSE
   WRITE(*,'(A)') ' Reading/exporting IDF as a whole'
  ENDIF

  IDFNAMES(I)=TRIM(ROOT)//'\'//TRIM(IDFNAMES(I))
  IU=UTL_GETUNIT(); IDFNAMES(I)=IDFNAMES(I)(:INDEX(IDFNAMES(I),'.',.TRUE.)-1)//'.ASC'
  CALL OSD_OPEN(IU,FILE=IDFNAMES(I),STATUS='UNKNOWN',ACTION='WRITE')
  WRITE(*,'(1X,A)') 'Writing '//TRIM(IDFNAMES(I))
  
  WRITE(IU,'(A14,I10)')   'NCOLS         ',IDF(1)%NCOL
  WRITE(IU,'(A14,I10)')   'NROWS         ',IDF(1)%NROW
  WRITE(IU,'(A14,F15.7)') 'XLLCORNER     ',IDF(1)%XMIN
  WRITE(IU,'(A14,F15.7)') 'YLLCORNER     ',IDF(1)%YMIN
  WRITE(IU,'(A14,F15.7)') 'CELLSIZE      ',IDF(1)%DX
  WRITE(IU,'(A14,F15.7)') 'NODATA_VALUE  ',IDF(1)%NODATA
  IF(ASSOCIATED(IDF(1)%X))THEN
   IF(IQUICK.EQ.1)THEN
    WRITE(IU,*) ((IDF(1)%X(ICOL,IROW), ICOL=1,IDF(1)%NCOL),IROW=1,IDF(1)%NROW)
   ELSE
    DO IROW=1,IDF(1)%NROW; DO ICOL=1,IDF(1)%NCOL
     WRITE(CH,*) IDF(1)%X(ICOL,IROW)
     !## avoid '*****' in idf to be exported - make them nodata values
     IF(INDEX(CH,'*').NE.0)WRITE(CH,*) IDF(1)%NODATA
     WRITE(IU,*) TRIM(ADJUSTL(CH))
    ENDDO; ENDDO
   ENDIF
  ELSE
   IREC=ICF +10  +ABS(IDF(1)%IEQ-1) *2    +IDF(1)%IEQ*(IDF(1)%NROW+IDF(1)%NCOL) +IDF(1)%ITB*2
   IF(IQUICK.EQ.1)THEN
    DO J=1,IDF(1)%NCOL*IDF(1)%NROW; IREC=IREC+1; READ(IDF(1)%IU,REC=IREC) X; WRITE(IU,*) X; ENDDO 
   ELSE
    DO J=1,IDF(1)%NCOL*IDF(1)%NROW
     IREC=IREC+1; READ(IDF(1)%IU,REC=IREC) X
     WRITE(CH,*) IDF(1)%X(ICOL,IROW)
     !## avoid '*****' in idf to be exported - make them nodata values
     IF(INDEX(CH,'*').NE.0)WRITE(CH,*) IDF(1)%NODATA
     WRITE(IU,*) TRIM(ADJUSTL(CH))
    ENDDO
   ENDIF
  ENDIF
  CLOSE(IU)
 ENDDO

 CALL IDFDEALLOCATE(IDF,SIZE(IDF))
 DEALLOCATE(IDFNAMES)

 END SUBROUTINE ASC2IDF_EXPORTASC_MAIN

 !###======================================================================
 SUBROUTINE ASC2IDF_EXPORTASC(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: IROW,ICOL,JU,I,J,N,IPLOT,ITYPE,ISAVE,STRLEN, &
            IC1,IC2,IR1,IR2,IOS,SNROW,SNCOL,IC,IR,NR,NC,IRAT,IRAT1
 REAL :: XMIN,YMIN,XMAX,YMAX,CS,XV
 REAL,ALLOCATABLE,DIMENSION(:) :: X
 CHARACTER(LEN=256) :: PATH
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: IDFS
 LOGICAL :: LEX
 CHARACTER(LEN=500) :: FNAME
 CHARACTER(LEN=:),ALLOCATABLE :: STR

 IF(ID.EQ.ID_MAPEXPORT3)THEN
  CALL WDIALOGLOAD(ID_DZOOMEXTENT,ID_DZOOMEXTENT)
  CALL WDIALOGPUTREAL(IDF_REAL1,MPW%XMIN,'(F15.7)') 
  CALL WDIALOGPUTREAL(IDF_REAL2,MPW%YMIN,'(F15.7)') 
  CALL WDIALOGPUTREAL(IDF_REAL3,MPW%XMAX,'(F15.7)') 
  CALL WDIALOGPUTREAL(IDF_REAL4,MPW%YMAX,'(F15.7)') 
  CALL WDIALOGSHOW(-1,-1,0,3)
  DO
   CALL WMESSAGE(ITYPE,MESSAGE)
   IF(ITYPE.EQ.PUSHBUTTON)THEN
    SELECT CASE (MESSAGE%VALUE1)
     CASE(IDCANCEL,IDOK); EXIT
     CASE(IDHELP)
    END SELECT
   ENDIF
  ENDDO
  CALL WDIALOGGETREAL(IDF_REAL1,MPW%XMIN); CALL WDIALOGGETREAL(IDF_REAL2,MPW%YMIN)
  CALL WDIALOGGETREAL(IDF_REAL3,MPW%XMAX); CALL WDIALOGGETREAL(IDF_REAL4,MPW%YMAX)
  CALL WDIALOGUNLOAD
  IF(MESSAGE%VALUE1.EQ.IDCANCEL)RETURN
 ENDIF

 IF(ALLOCATED(IDFS))DEALLOCATE(IDFS)
 ALLOCATE(IDFS(MXMPLOT))

 IDFS=''

 N=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)THEN
   N=N+1
   IDFS(N)=TRIM(MP(IPLOT)%IDFNAME)
  ENDIF
 ENDDO

 !## Save as dialog
 PATH=PREFVAL(1)//'\TMP'
 CALL WSELECTDIR(DIRCHANGE+DIRCREATE,PATH,'Export IDFs to ...?')
 IF(WINFODIALOG(4).NE.1)THEN
  IF(ALLOCATED(IDFS))DEALLOCATE(IDFS)
  RETURN
 ENDIF
 !## replace current directory for selected directory
 FNAME=TRIM(IDFS(1))
 DO I=1,N
  J=INDEXNOCASE(IDFS(I),'\',.TRUE.)
  IF(I.GT.1)FNAME=TRIM(FNAME)//CHAR(13)//TRIM(IDFS(I)(J+1:))
  IDFS(I)=UTL_SUBST(IDFS(I),IDFS(I)(1:J),TRIM(PATH)//'\')
 ENDDO

 CALL UTL_MESSAGEHANDLE(0)

 I=0; ISAVE=0
 DO IPLOT=1,MXMPLOT
  IF(MP(IPLOT)%ISEL)THEN
   I=I+1

   J=INDEXNOCASE(TRIM(IDFS(I)),'.',.TRUE.)-1
   IDFS(I)=IDFS(I)(1:J)//'.ASC'

   INQUIRE(FILE=IDFS(I),EXIST=LEX)
   IF(LEX)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONOK,'Do you want to overwrite the existing file'//CHAR(13)// &
     TRIM(IDFS(I))//'?','QUESTION')
    IF(WINFODIALOG(4).NE.1)LEX=.FALSE.
   ELSE
    LEX=.TRUE.
   ENDIF

   IF(LEX)THEN

    JU=UTL_GETUNIT()
    CALL OSD_OPEN(JU,FILE=IDFS(I),ACCESS='SEQUENTIAL',STATUS='REPLACE',FORM='FORMATTED',IOSTAT=IOS)

    IF(IOS.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not create output file'//CHAR(13)// &
      TRIM(IDFS(I)),'Error')
    ELSE

     IF(IDFREAD(MP(IPLOT)%IDF,MP(IPLOT)%IDFNAME,0))THEN

      !## total extent
      IF(ID.EQ.ID_MAPEXPORT1)THEN
       IC1=1; IC2=MP(IPLOT)%IDF%NCOL
       IR1=1; IR2=MP(IPLOT)%IDF%NROW
      !## current extent
      ELSEIF(ID.EQ.ID_MAPEXPORT2.OR.ID.EQ.ID_MAPEXPORT3)THEN
       CALL IDFIROWICOL(MP(IPLOT)%IDF,IR2,IC1,MPW%XMIN,MPW%YMIN)
       CALL IDFIROWICOL(MP(IPLOT)%IDF,IR1,IC2,MPW%XMAX,MPW%YMAX)
      ENDIF

      IF(MP(IPLOT)%IDF%IEQ.EQ.0)THEN
       SNCOL=IC2-IC1+1
       SNROW=IR2-IR1+1
       XMIN =MP(IPLOT)%IDF%XMIN+(IC1-1)*MP(IPLOT)%IDF%DX
       YMIN =MP(IPLOT)%IDF%YMIN+(MP(IPLOT)%IDF%NROW-IR2)*MP(IPLOT)%IDF%DY
       CS   =MP(IPLOT)%IDF%DX
      ELSE
       CS=MP(IPLOT)%IDF%XMAX-MP(IPLOT)%IDF%XMIN
       DO IC=1,MP(IPLOT)%IDF%NCOL
        CS=MIN(CS,MP(IPLOT)%IDF%SX(IC)-MP(IPLOT)%IDF%SX(IC-1))
       END DO
       XMIN =MP(IPLOT)%IDF%SX(IC1-1)
       XMAX =MP(IPLOT)%IDF%SX(IC2)
       YMIN =MP(IPLOT)%IDF%SY(IR2)
       YMAX =MP(IPLOT)%IDF%SY(IR1-1)
       SNCOL=(XMAX-XMIN)/CS
       SNROW=(YMAX-YMIN)/CS
      ENDIF

      WRITE(JU,'(A14,I10)')   'NCOLS         ',SNCOL
      WRITE(JU,'(A14,I10)')   'NROWS         ',SNROW
      WRITE(JU,'(A14,F10.2)') 'XLLCORNER     ',XMIN
      WRITE(JU,'(A14,F10.2)') 'YLLCORNER     ',YMIN
      WRITE(JU,'(A14,F10.2)') 'CELLSIZE      ',CS
      WRITE(JU,'(A14,G12.5)') 'NODATA_VALUE  ',MP(IPLOT)%IDF%NODATA

      ALLOCATE(X(SNCOL)); X=MP(IPLOT)%IDF%NODATA

      STRLEN=SNCOL*15; ALLOCATE(CHARACTER(LEN=STRLEN) :: STR)

      IF(MP(IPLOT)%IDF%IEQ.EQ.0)THEN     
       
       IRAT1=0
       DO IROW=IR1,IR2
        CALL WMESSAGEPEEK(ITYPE,MESSAGE)
        J=0; DO ICOL=IC1,IC2
         J=J+1
         IF(IROW.GE.1.AND.IROW.LE.MP(IPLOT)%IDF%NROW.AND. &
            ICOL.GE.1.AND.ICOL.LE.MP(IPLOT)%IDF%NCOL)X(J)=IDFGETVAL(MP(IPLOT)%IDF,IROW,ICOL)
        ENDDO
        WRITE(STR,*) (X(J),J=1,SNCOL)
        WRITE(JU,'(A)') TRIM(STR) 
        CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW-IR1+1,IR2-IR1+1,'Exporting '//TRIM(ITOS(I))//' out of '//TRIM(ITOS(N))//', Progress ')
       ENDDO

      ELSE

       IRAT1=0
       DO IROW=IR1,IR2
        CALL WMESSAGEPEEK(ITYPE,MESSAGE)
        NR=INT((MP(IPLOT)%IDF%SY(IROW-1)-MP(IPLOT)%IDF%SY(IROW))/CS)
        DO IR=1,NR
         J=0; DO ICOL=IC1,IC2
          XV=MP(IPLOT)%IDF%NODATA
          IF(IROW.GE.1.AND.IROW.LE.MP(IPLOT)%IDF%NROW.AND. &
             ICOL.GE.1.AND.ICOL.LE.MP(IPLOT)%IDF%NCOL)XV=IDFGETVAL(MP(IPLOT)%IDF,IROW,ICOL)
          NC=INT((MP(IPLOT)%IDF%SX(ICOL)-MP(IPLOT)%IDF%SX(ICOL-1))/CS)
          DO IC=1,NC
           J=J+1; X(J)=XV
          ENDDO
         ENDDO
         WRITE(STR,*) (X(J),J=1,SNCOL)
         WRITE(JU,'(A)') TRIM(STR) 
        ENDDO
        CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW-IR1+1,IR2-IR1+1,'Exporting '//TRIM(ITOS(I))//' out of '//TRIM(ITOS(N))//', Progress ')
       ENDDO

      ENDIF

      CLOSE(MP(IPLOT)%IDF%IU); CLOSE(JU)
      CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)
      DEALLOCATE(X)
      DEALLOCATE(STR)
      
      ISAVE=ISAVE+1
      
     ENDIF
    ENDIF
   ENDIF
  ENDIF

 ENDDO

 IF(ALLOCATED(IDFS))DEALLOCATE(IDFS)

 CALL UTL_MESSAGEHANDLE(1)

 IF(ISAVE.GT.0)THEN
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Successfully converted selected IDF file(s) to:'//CHAR(13)// &
    TRIM(PATH)//CHAR(13)//' in ESRI-Raster format (*.asc)','Information')
 ENDIF
 
 END SUBROUTINE ASC2IDF_EXPORTASC
  
END MODULE MOD_ASC2IDF
