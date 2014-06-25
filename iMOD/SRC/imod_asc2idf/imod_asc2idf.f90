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

MODULE MOD_ASC2IDF

USE WINTERACTER
USE RESOURCE
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFALLOCATEX,IDFDEALLOCATEX,IDFGETVAL,IDFNULLIFY,IDFDEALLOCATE,IDFCOPY,IDFIROWICOL, &
           IDFOPEN,IDFWRITEDIM,IDFWRITECOMMENT,IDFFILLCOMMENT,IDFDEALLOCATESX,IDFREADSCALE
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,RTOS,UTL_MESSAGEHANDLE,UTL_SUBST,UTL_WAITMESSAGE,UTL_CAP,UTL_DIRINFO,UTL_IDFSNAPTOGRID,&
    UTL_GETMED,UTL_DIRINFO_POINTER,IDATETOJDATE,UTL_GETMED
USE MODPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_OSD, ONLY : OSD_OPEN,ICF

INTEGER,ALLOCATABLE,DIMENSION(:),PRIVATE :: IOS
CHARACTER(LEN=20),ALLOCATABLE,DIMENSION(:),PRIVATE :: TXT
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF
CHARACTER(LEN=256),PRIVATE :: LINE

CONTAINS

 !###======================================================================
 SUBROUTINE ASC2IDF_IMPORTASC_MAIN(DIR,TOPWC,BOTEL,MULT)
 !###======================================================================
 IMPLICIT NONE
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
    K=K-1 !+INDEX(TOPWC,'*')+1:)
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
  CALL ASC2IDF_IMPORTASC(IDFNAMES(I),TOP,BOT,IERROR)
  IF(IERROR.EQ.1)EXIT
 ENDDO

 DEALLOCATE(IDFNAMES)

 END SUBROUTINE ASC2IDF_IMPORTASC_MAIN

 !###======================================================================
 SUBROUTINE ASC2IDF_EXPORTASC_MAIN(DIR,IQUICK)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IQUICK
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
  WRITE(IU,'(A14,F10.2)') 'XLLCORNER     ',IDF(1)%XMIN
  WRITE(IU,'(A14,F10.2)') 'YLLCORNER     ',IDF(1)%YMIN
  WRITE(IU,'(A14,F10.2)') 'CELLSIZE      ',IDF(1)%DX
  WRITE(IU,'(A14,G15.7)') 'NODATA_VALUE  ',IDF(1)%NODATA
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
 SUBROUTINE ASC2IDF_IMPORTASC(IDFNAME,TOP,BOT,IERROR)
 !###======================================================================
 IMPLICIT NONE
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
  IF(.NOT.ASC2IDF_TYPE1(IU,IDFNAME,TOP,BOT))IERROR=0
 ELSEIF(ASC_TYPE.EQ.1)THEN
  IF(.NOT.ASC2IDF_TYPE2(IU,IDFNAME))IERROR=0
 ENDIF

 CALL ASC2IDF_CLOSE(IU)

 CALL UTL_MESSAGEHANDLE(1)
 IERROR=0

 END SUBROUTINE ASC2IDF_IMPORTASC

 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_TYPE1(IU,IDFNAME,TOP,BOT)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 REAL,INTENT(IN) :: TOP,BOT
 INTEGER :: J,NCOL,NROW,IROW,ICOL
 REAL :: XMIN,XMAX,YMIN,YMAX,CSIZE,NODATA
 LOGICAL :: LEX,LBIG

 ASC2IDF_TYPE1=.FALSE.

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
 READ(IU,*,IOSTAT=IOS(6))   LINE

 IF(SUM(IOS).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading header of ascii file!','Error')
  RETURN
 ENDIF

 !## nodata is optional
 READ(LINE,*,IOSTAT=IOS(6))   TXT(6),NODATA 
 IF(IOS(6).NE.0)NODATA=-99999.99

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
  READ(IU,*,IOSTAT=IOS(1)) ((IDF(1)%X(ICOL,IROW),ICOL=1,NCOL),IROW=1,NROW)
 ENDIF
 IF(IOS(1).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading irow='//TRIM(ITOS(IROW))// &
     ' of data block of ascii file!','Error')
  RETURN
 ENDIF
 CLOSE(IU)

 CALL IDFFILLCOMMENT(IDF(1),'Imported from '//TRIM(IDFNAME))

 IF(LBIG)THEN
  CALL IDFWRITECOMMENT(IDF(1),1)
 ELSE
  J=INDEXNOCASE(IDFNAME,'.',.TRUE.)-1
  IF(.NOT.IDFWRITE(IDF(1),IDFNAME(:J)//'.IDF',1))THEN; ENDIF
 ENDIF
 
 ASC2IDF_TYPE1=.TRUE.

 END FUNCTION ASC2IDF_TYPE1

 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_TYPE2(IU,IDFNAME)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 CHARACTER(LEN=*),INTENT(IN) :: IDFNAME
 REAL,DIMENSION(0:3) :: X,Y
 REAL,DIMENSION(:),ALLOCATABLE :: Z
 REAL :: CS,A,B,C,D
 INTEGER :: NGRID,N,NODES,I,J,IROW,ICOL,JROW,JCOL

 ASC2IDF_TYPE2=.FALSE.

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
  READ(IU,*,IOSTAT=IOS(1)) X(0),Y(0)!,Z(0)
  IF(IOS(1).NE.0)EXIT
  N=N+1
  X(1)=MIN(X(0),X(1))
  X(2)=MAX(X(0),X(2))
  Y(1)=MIN(Y(0),Y(1))
  Y(2)=MAX(Y(0),Y(2))
!  Z(1)=MIN(Z(0),Z(1))
!  Z(2)=MAX(Z(0),Z(2))
  !## store first result
  IF(N.EQ.1)THEN
   X(3)=X(0)
   Y(3)=Y(0)
!   Z(3)=Z(0)
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

!WRITE(*,*) A,B,C,D

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

 ASC2IDF_TYPE2=.TRUE.

 END FUNCTION ASC2IDF_TYPE2

 !###======================================================================
 LOGICAL FUNCTION ASC2IDF_TYPE3(IFILE,XMIN,YMIN,XMAX,YMAX)
 !###======================================================================
 USE MOD_ASC2IDF_PAR
 USE MOD_KRIGING, ONLY : KRIGING_MAIN,KRIGING_VARIOGRAM,KRIGING_UNITTEST
 USE MOD_SOLID_PCG, ONLY : SOLID_PCGINT,MICNVG,MXITER2,HCLOSE,RCLOSE
 USE MOD_BIVARIATE, ONLY : BIVARIATE_INT
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFILE
 REAL,INTENT(IN) :: XMIN,YMIN,XMAX,YMAX
 REAL :: X,Y,Z
 REAL,DIMENSION(1) :: ZPERC
 CHARACTER(LEN=256) :: LINE
 CHARACTER(LEN=5) :: EXT
 CHARACTER(LEN=52),ALLOCATABLE,DIMENSION(:) :: STRING
 REAL,ALLOCATABLE,DIMENSION(:) :: RC,RR
 REAL,POINTER,DIMENSION(:) :: XA,YA,ZA
 REAL,ALLOCATABLE,DIMENSION(:) :: ZV
 INTEGER :: IU,IROW,ICOL,J,K,M,MM,II,JJ,MX,IIPF,IERROR,NCOL,IEXT
 INTEGER(KIND=2),ALLOCATABLE,DIMENSION(:,:) :: NZ
 LOGICAL :: LEX

 ASC2IDF_TYPE3=.FALSE.

 ALLOCATE(IOS(3))

 IIPF=0
 IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.XYZ',.TRUE.).GT.0)IIPF=1
 IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.IPF',.TRUE.).GT.0)IIPF=2
 IF(INDEX(UTL_CAP(XYZFNAMES(IFILE),'U'),'.IDF',.TRUE.).GT.0)IIPF=3  

 IF(IIPF.EQ.0)THEN; WRITE(*,'(A)') 'Not correct file format specified, choose IPF, IDF or XYZ'; RETURN; ENDIF
 
 IF(IIPF.NE.3)THEN
  IU=UTL_GETUNIT()
  CALL OSD_OPEN(IU,FILE=XYZFNAMES(IFILE),STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS(1))
  IF(IOS(1).NE.0)THEN
   WRITE(*,'(A)') 'Can not open file: ['//TRIM(XYZFNAMES(IFILE))//'] for reading'
   RETURN
  ENDIF
 ENDIF
 
 ALLOCATE(IDF(4)); DO I=1,SIZE(IDF); CALL IDFNULLIFY(IDF(I)); ENDDO

 IF(IIPF.EQ.1.OR.IIPF.EQ.2)THEN

  IDF(1)%DX=CS; IDF(1)%DY=CS
  IDF(1)%XMIN=10.0E10; IDF(1)%YMIN=10.0E10; IDF(1)%XMAX=-10.0E10; IDF(1)%YMAX=-10.0E10; N=0

  WRITE(*,'(A)') 'Get window ...'
  M=0
  IF(IIPF.EQ.2)THEN
   READ(IU,*) N; READ(IU,*) M; DO I=1,M; READ(IU,*); ENDDO; READ(IU,*); NCOL=MAX(IXCOL,IYCOL,IZCOL); M=N
  ELSEIF(IIPF.EQ.1)THEN
   NCOL=3
  ENDIF

  !## read file to find out dimensions
  ALLOCATE(STRING(NCOL)); N=0; DO
   READ(IU,*,IOSTAT=IOS(1)) (STRING(K),K=1,NCOL)
   IF(IOS(1).NE.0)EXIT 
   READ(STRING(IXCOL),*) X; READ(STRING(IYCOL),*) Y !; READ(STRING(IZCOL),*) Z    
   N=N+1
   IDF(1)%XMIN=MIN(IDF(1)%XMIN,X); IDF(1)%XMAX=MAX(IDF(1)%XMAX,X)
   IDF(1)%YMIN=MIN(IDF(1)%YMIN,Y); IDF(1)%YMAX=MAX(IDF(1)%YMAX,Y)
  ENDDO

  WRITE(*,'(A,I20,A)') 'Found ',N,' points'

  IDF(1)%XMIN=IDF(1)%XMIN-CS; IDF(1)%XMAX=IDF(1)%XMAX+CS
  IDF(1)%YMIN=IDF(1)%YMIN-CS; IDF(1)%YMAX=IDF(1)%YMAX+CS
  IF(XMIN.NE.XMAX.AND.YMIN.NE.YMAX)THEN 
   IDF(1)%XMIN=XMIN; IDF(1)%XMAX=XMAX; IDF(1)%YMIN=YMIN; IDF(1)%YMAX=YMAX
  ENDIF
  IDF(1)%IEQ =0; IDF(1)%ITB =0; IDF(1)%IXV =0; IDF(1)%NODATA=NODATA
  IF(IGRIDFUNC.NE.7)CALL UTL_IDFSNAPTOGRID(IDF(1)%XMIN,IDF(1)%XMAX,IDF(1)%YMIN,IDF(1)%YMAX,IDF(1)%DX,IDF(1)%NCOL,IDF(1)%NROW)

 ELSE

  IF(XMIN.NE.XMAX.AND.YMIN.NE.YMAX)THEN
   IDF(1)%DX=CS; IDF(1)%DY=CS
   IDF(1)%XMIN=XMIN; IDF(1)%XMAX=XMAX; IDF(1)%YMIN=YMIN; IDF(1)%YMAX=YMAX
   IDF(1)%IEQ =0; IDF(1)%ITB =0; IDF(1)%IXV =0; IDF(1)%NODATA=NODATA
   CALL UTL_IDFSNAPTOGRID(IDF(1)%XMIN,IDF(1)%XMAX,IDF(1)%YMIN,IDF(1)%YMAX,IDF(1)%DX,IDF(1)%NCOL,IDF(1)%NROW)
   CALL IDFCOPY(IDF(1),IDF(2))
   IF(.NOT.IDFREADSCALE(XYZFNAMES(IFILE),IDF(2),2,1,0.0,0))RETURN !## scale mean
  ELSE
   IF(.NOT.IDFREAD(IDF(1),XYZFNAMES(IFILE),1))RETURN
   CALL IDFCOPY(IDF(1),IDF(2))
  ENDIF

  CALL IDFCOPY(IDF(2),IDF(3)); N=IDF(2)%NCOL*IDF(2)%NROW
  IF(TRIM(XYZFNAMES(2)).NE.'')THEN
   IF(.NOT.IDFREADSCALE(XYZFNAMES(2),IDF(3),7,1,0.0,0))RETURN !## scale most frequent occurence
  ENDIF
 ENDIF
 IF(ABS(IGRIDFUNC).EQ.6)THEN
  CALL IDFCOPY(IDF(1),IDF(4)); IF(.NOT.IDFALLOCATEX(IDF(4)))RETURN
 ENDIF
 
 IF(IGRIDFUNC.NE.7)THEN
  IF(.NOT.IDFALLOCATEX(IDF(1)))RETURN; IDF(1)%X=NODATA
 ENDIF

 SELECT CASE (IGRIDFUNC)
  CASE (4)
   ALLOCATE(RC(N),STAT=IOS(1)); ALLOCATE(RR(N),STAT=IOS(2)); ALLOCATE(ZV(N),STAT=IOS(3))
   IF(SUM(IOS).NE.0)THEN
    WRITE(*,'(A,I10,A1,I10,A)') 'ERROR, can not allocate memory for IC,IR,ZV(',N,')-arrays.'; RETURN
   ENDIF
  CASE (1,2,3)
   ALLOCATE(NZ(IDF(1)%NCOL,IDF(1)%NROW),STAT=IOS(1))
   IF(IOS(1).NE.0)THEN
    WRITE(*,'(A,I10,A1,I10,A)') 'ERROR, can not allocate memory for NZ(',IDF(1)%NCOL,',',IDF(1)%NROW,')-array.'; RETURN
   ENDIF
  !## bivariate/kriging/variogram/pcg
  CASE (5,-6,6,7,8)
   ALLOCATE(XA(N),STAT=IOS(1)); ALLOCATE(YA(N),STAT=IOS(2)); ALLOCATE(ZA(N),STAT=IOS(3))
   IF(SUM(IOS).NE.0)THEN
    WRITE(*,'(A,I10,A1,I10,A)') 'ERROR, can not allocate memory for XA,YA,ZA(',N,')-arrays.'; RETURN
   ENDIF
   IF(ABS(IGRIDFUNC).EQ.6)THEN; CALL IDFCOPY(IDF(2),IDF(3)); IF(.NOT.IDFALLOCATEX(IDF(3)))RETURN; ENDIF
 END SELECT

 WRITE(*,'(A)') 'Fill grid ...'
 IF(IIPF.NE.3)THEN
  REWIND(IU)
  IF(IIPF.EQ.2)THEN
   READ(IU,*) N; READ(IU,*) M; DO I=1,M; READ(IU,*); ENDDO; READ(IU,*) IEXT,EXT
  ENDIF
 ENDIF
 
 J=0
 SELECT CASE (IGRIDFUNC)

  !## min,max,mean
  CASE (1,2,3)
   DO I=1,N
    READ(IU,*) (STRING(K),K=1,NCOL)
    READ(STRING(IXCOL),*) X; READ(STRING(IYCOL),*) Y
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,X,Y)
    IF(IROW.NE.0.AND.ICOL.NE.0)THEN
     LEX=.TRUE.
     IF(ASSF_COLUMN.EQ.0)THEN
      READ(STRING(IZCOL),*) Z
     ELSE
      LEX=PCK2RPWEL(ASSF_COLUMN,ASSF_STARTDATE,ASSF_ENDDATE,ASSF_MTYPE,Z, &
          XYZFNAMES(IFILE)(:INDEX(XYZFNAMES(IFILE),'\',.TRUE.))//TRIM(STRING(IEXT))//'.'//TRIM(EXT)) 
     ENDIF
     IF(LEX)THEN
      IF(IDF(1)%X(ICOL,IROW).EQ.IDF(1)%NODATA)THEN
       IDF(1)%X(ICOL,IROW)=Z
       NZ(ICOL,IROW)=INT(1,2)
      ELSE
       NZ(ICOL,IROW)=NZ(ICOL,IROW)+INT(1,2)
       IF(IGRIDFUNC.EQ.1)THEN
        IDF(1)%X(ICOL,IROW)=MIN(IDF(1)%X(ICOL,IROW),Z)
       ELSEIF(IGRIDFUNC.EQ.2)THEN
        IDF(1)%X(ICOL,IROW)=MAX(IDF(1)%X(ICOL,IROW),Z)
       ELSEIF(IGRIDFUNC.EQ.3)THEN
        IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)+Z
       ELSEIF(IGRIDFUNC.EQ.4)THEN
        J=J+1; RC(J)=REAL(ICOL); RR(J)=REAL(IROW); ZV(J)=Z
       ENDIF
      ENDIF
     ENDIF
    ENDIF
   ENDDO
  !## percentiles
  CASE (4)
   DO I=1,N
    READ(IU,*) (STRING(K),K=1,NCOL)
    READ(STRING(IXCOL),*) X; READ(STRING(IYCOL),*) Y  !; READ(STRING(IZCOL),*) Z    
    CALL IDFIROWICOL(IDF(1),IROW,ICOL,X,Y)
    IF(IROW.NE.0.AND.ICOL.NE.0)THEN
     LEX=.TRUE.
     IF(ASSF_COLUMN.EQ.0)THEN
      READ(STRING(IZCOL),*) Z
     ELSE
      LEX=PCK2RPWEL(ASSF_COLUMN,ASSF_STARTDATE,ASSF_ENDDATE,ASSF_MTYPE,Z, &
           XYZFNAMES(IFILE)(:INDEX(XYZFNAMES(IFILE),'\',.TRUE.))//TRIM(STRING(IEXT))//'.'//TRIM(EXT)) 
     ENDIF
     IF(LEX)THEN
      J=J+1; RC(J)=REAL(ICOL); RR(J)=REAL(IROW); ZV(J)=Z
     ENDIF
    ENDIF
   ENDDO
  !## bivariate,kriging,variogram,pcg
  CASE (5,-6,6,7,8)
   IF(IIPF.EQ.3)THEN
    N=0; DO IROW=1,IDF(2)%NROW; DO ICOL=1,IDF(2)%NCOL
     !## pointer not equal to nodata
     IF(IDF(3)%X(ICOL,IROW).NE.IDF(3)%NODATA)THEN
      !## z value not equal to nodata
      IF(IDF(2)%X(ICOL,IROW).NE.IDF(2)%NODATA)THEN
       N=N+1
       XA(N)=REAL(ICOL); YA(N)=REAL(IROW); ZA(N)=IDF(2)%X(ICOL,IROW)
      ENDIF
     ENDIF
    ENDDO; ENDDO
   ELSE
    J=0; DO I=1,N
     READ(IU,*,IOSTAT=IOS(1)) (STRING(K),K=1,NCOL)
     J=J+1
     READ(STRING(IXCOL),*) XA(J); READ(STRING(IYCOL),*) YA(J)
     LEX=.TRUE.
     IF(ASSF_COLUMN.EQ.0)THEN
      READ(STRING(IZCOL),*) ZA(J)
     ELSE
      IF(.NOT.PCK2RPWEL(ASSF_COLUMN,ASSF_STARTDATE,ASSF_ENDDATE,ASSF_MTYPE,ZA(I), &
          XYZFNAMES(IFILE)(:INDEX(XYZFNAMES(IFILE),'\',.TRUE.))//TRIM(STRING(IEXT))//'.'//TRIM(EXT)))J=J-1
     ENDIF
    ENDDO
    N=J
    !## create irow/icol indices for pcg interpolation
    IF(IGRIDFUNC.EQ.8)THEN
     J=0; DO I=1,N
      CALL IDFIROWICOL(IDF(1),IROW,ICOL,XA(I),YA(I))
      IF(IROW.NE.0.AND.ICOL.NE.0)THEN; J=J+1; XA(J)=REAL(ICOL); YA(J)=REAL(IROW); ZA(J)=ZA(I); ENDIF 
     ENDDO
     N=J
    ENDIF
   ENDIF

   IF(MINP.EQ.0)MINP=N; IF(MAXP.EQ.0)MAXP=N
   IF(IGRIDFUNC.EQ.5)CALL BIVARIATE_INT(XA,YA,ZA,INT(N,4),IERROR,IDF(1))
   IF(ABS(IGRIDFUNC).EQ.6)THEN
    !## each cell need to be interpolated
    IDF(1)%X=IDF(1)%NODATA
    IF(IGRIDFUNC.EQ. 6)CALL KRIGING_MAIN(INT(N,4),XA,YA,ZA,IDF(1),IDF(4),MINP,MAXP,RANGE,SILL,NUGGET, KTYPE,1,SEARCHDISTANCE,NOSEARCH, &
                               IEXPVARIOGRAM,LAGINTERVAL,LAGDISTANCE,IBATCH=1) !## simple kriging
    IF(IGRIDFUNC.EQ.-6)CALL KRIGING_MAIN(INT(N,4),XA,YA,ZA,IDF(1),IDF(4),MINP,MAXP,RANGE,SILL,NUGGET,-KTYPE,1,SEARCHDISTANCE,NOSEARCH, &
                               IEXPVARIOGRAM,LAGINTERVAL,LAGDISTANCE,IBATCH=1) !## ordinary kriging
   ENDIF
   IF(IGRIDFUNC.EQ.7)CALL KRIGING_VARIOGRAM(INT(N,4),XA,YA,ZA,J,IDF(1),LAGINTERVAL,LAGDISTANCE,IBATCH=1,SNAME=XYZFNAMES(IFILE))
   MICNVG=25; 
   IF(IGRIDFUNC.EQ.8)CALL SOLID_PCGINT(XA,YA,ZA,INT(N,4),IERROR,IDF(1),-1)
   
   !## reset nodata values
   IF(IIPF.EQ.3.AND.TRIM(XYZFNAMES(2)).NE.'')THEN
    DO IROW=1,IDF(2)%NROW; DO ICOL=1,IDF(2)%NCOL  
     IF(IDF(3)%X(ICOL,IROW).EQ.IDF(3)%NODATA)IDF(1)%X(ICOL,IROW)=IDF(1)%NODATA
    ENDDO; ENDDO
   ENDIF
   
 END SELECT
 IF(ALLOCATED(STRING))DEALLOCATE(STRING)
 
 IF(IGRIDFUNC.EQ.3)THEN
  DO ICOL=1,IDF(1)%NCOL
   DO IROW=1,IDF(1)%NROW
    IF(IDF(1)%X(ICOL,IROW).NE.IDF(1)%NODATA)THEN
     IDF(1)%X(ICOL,IROW)=IDF(1)%X(ICOL,IROW)/REAL(NZ(ICOL,IROW))
    ENDIF
   ENDDO
  ENDDO
 !## percentiles
 ELSEIF(IGRIDFUNC.EQ.4)THEN
  !## sort array
  WRITE(*,'(A)') 'Sorting grid ...'
  M=INT(N)
  CALL SORTEM(1,M,RR,2,RC,ZV,ZV,ZV,ZV,ZV,ZV)
  WRITE(*,'(A)') 'Finished Sorting grid ...'
  WRITE(*,'(A)') 'Start assigning percentiles to grid ...'

  I=1
  DO
   !## get offset for current row
   IROW=INT(RR(I))

   J=I+1
   DO
    IF(INT(RR(J)).NE.INT(RR(I)))EXIT
    J=J+1
    IF(J.GT.N)EXIT
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
   WRITE(*,'(2I10)') IROW,IDF(1)%NROW
  ENDDO

 ENDIF

 IF(IGRIDFUNC.NE.7)THEN
  IF(IDFWRITE(IDF(1),IDFFILE,1))ASC2IDF_TYPE3=.TRUE.
  IF(ABS(IGRIDFUNC).EQ.6)THEN; IF(IDFWRITE(IDF(4),STDEVIDF,1))THEN; ENDIF; ENDIF
 ENDIF
 CALL ASC2IDF_CLOSE(IU)

 END FUNCTION ASC2IDF_TYPE3

 !###====================================================================
 LOGICAL FUNCTION PCK2RPWEL(ICOL,SDATE,EDATE,MTYPE,Q,FNAME) 
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: SDATE,EDATE,ICOL,MTYPE
 REAL,DIMENSION(:),ALLOCATABLE :: QSORT
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 REAL,INTENT(OUT) :: Q
 INTEGER :: IR,I,I1,I2,IU,NR,NC,IDATE,JDATE,NDATE,NAJ,N,IOS,TTIME
 REAL :: FRAC,Q1,QQ
 CHARACTER(LEN=8) :: ATTRIB
 CHARACTER(LEN=256) :: LINE
 REAL,DIMENSION(:),ALLOCATABLE :: NODATA,QD

 !## transient(2)/steady-state(1)
 TTIME=EDATE-SDATE; ALLOCATE(QSORT(TTIME)); Q=0.0

 !## open textfiles with pump information
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ')

 READ(IU,*) NR
 IF(NR.GT.0.0)THEN
  READ(IU,*) NC
  
  ALLOCATE(NODATA(NC),QD(NC)); QD=0.0
  
  DO I=1,NC; READ(IU,*) ATTRIB,NODATA(I); ENDDO 

  QSORT=NODATA(ICOL)
  I1=1

  DO IR=1,NR

   IF(IR.EQ.1)THEN
    READ(IU,*) IDATE,(QD(I),I=2,NC)
    QQ=QD(ICOL)
   ELSE
    QQ   =Q1
    IDATE=JDATE
   ENDIF

   !## edate=end date of current simulation period
   NDATE=EDATE
   IF(IR.LT.NR)THEN
    READ(IU,*) NDATE,(QD(I),I=2,NC) 
    Q1=QD(ICOL)
    JDATE=NDATE
    NDATE=IDATETOJDATE(NDATE) !## fname=optional for error message
   ENDIF
   !## ndate is min of end date in txt file or simulation period
   NDATE=MIN(NDATE,EDATE)

   !## is begin date read from txt file
   IDATE=IDATETOJDATE(IDATE)  !## fname=optional for error message

   !## stop searching for data, outside modeling window!
   IF(IDATE.GT.EDATE)EXIT

   !## within modeling window
   IF(NDATE.GT.SDATE)THEN

    !### defintions ($ time window current stressperiod)
    !  $        |---------|         $ 
    !sdate    idate     ndate     edate
    
    N=NDATE-SDATE
    !## if startingdate (read from txt file) greater than start date of current stressperiod
    IF(IDATE.GT.SDATE)N=N-(IDATE-SDATE)
    I2=I1+N-1
    
    IF(I2.GE.I1)QSORT(I1:I2)=QQ

    I1=I2+1

   ENDIF
  END DO

  IF(MTYPE.EQ.1)THEN
   Q=SUM(QSORT(1:TTIME))/REAL(TTIME)
  ELSEIF(MTYPE.EQ.2)THEN
   CALL UTL_GETMED(QSORT,TTIME,NODATA(ICOL),(/0.5/),1,NAJ,QD)
   Q=QD(1)
   !## naj becomes zero if no values were found!
   FRAC=REAL(NAJ)/REAL(TTIME)
   Q   =Q*FRAC
  ENDIF
  
 ENDIF
 
 PCK2RPWEL=.TRUE.; IF(Q.EQ.NODATA(ICOL))PCK2RPWEL=.FALSE.
 
 CLOSE(IU); DEALLOCATE(QSORT,NODATA,QD)
 
 END FUNCTION PCK2RPWEL
 
 !###======================================================================
 SUBROUTINE ASC2IDF_CLOSE(IU)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU
 LOGICAL :: LEX

 IF(ALLOCATED(IOS))DEALLOCATE(IOS)
 IF(ALLOCATED(TXT))DEALLOCATE(TXT)
 INQUIRE(UNIT=IU,OPENED=LEX)
 IF(LEX)CLOSE(IU)
 IF(ALLOCATED(IDF))THEN
  CALL IDFDEALLOCATE(IDF,SIZE(IDF))
  DEALLOCATE(IDF)
 ENDIF

 END SUBROUTINE ASC2IDF_CLOSE

 !###======================================================================
 SUBROUTINE ASC2IDF_EXPORTASC(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: IROW,ICOL,JU,I,J,N,IPLOT,ITYPE, &
            IC1,IC2,IR1,IR2,IOS,SNROW,SNCOL,IC,IR,NR,NC,IRAT,IRAT1
 REAL :: XMIN,YMIN,XMAX,YMAX,X,CS
 CHARACTER(LEN=256) :: PATH
 CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: IDFS
 CHARACTER(LEN=15)  :: CH
 LOGICAL :: LEX
 CHARACTER(LEN=500) :: FNAME

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

 !GHN 25-01-2008 to enable save as, in case N > 1, then select dir, else save as dialog.
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

 I=0
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
       IC1=1
       IC2=MP(IPLOT)%IDF%NCOL
       IR1=1
       IR2=MP(IPLOT)%IDF%NROW
      !## current extent
      ELSEIF(ID.EQ.ID_MAPEXPORT2)THEN
       IC1 =INT((MPW%XMIN-MP(IPLOT)%IDF%XMIN)/MP(IPLOT)%IDF%DX)
       IC2 =INT((MPW%XMAX-MP(IPLOT)%IDF%XMIN)/MP(IPLOT)%IDF%DX)
       IR1 =INT((MP(IPLOT)%IDF%YMAX-MPW%YMAX)/MP(IPLOT)%IDF%DY)
       IR2 =INT((MP(IPLOT)%IDF%YMAX-MPW%YMIN)/MP(IPLOT)%IDF%DY)
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
      WRITE(CH,*) MP(IPLOT)%IDF%NODATA
      WRITE(JU,'(A14,A)')     'NODATA_VALUE  ',TRIM(CH)

      IF(MP(IPLOT)%IDF%IEQ.EQ.0)THEN

       IRAT1=0
       DO IROW=IR1,IR2
        CALL WMESSAGEPEEK(ITYPE,MESSAGE)
        DO ICOL=IC1,IC2
         IF(IROW.GE.1.AND.IROW.LE.MP(IPLOT)%IDF%NROW.AND. &
            ICOL.GE.1.AND.ICOL.LE.MP(IPLOT)%IDF%NCOL)THEN
          X=IDFGETVAL(MP(IPLOT)%IDF,IROW,ICOL)
         ELSE
          X=MP(IPLOT)%IDF%NODATA
         ENDIF
         WRITE(CH,*) X
         !## avoid '*****' in idf to be exported - make them nodata values
         IF(INDEX(CH,'*').NE.0)WRITE(CH,*) MP(IPLOT)%IDF%NODATA
         WRITE(JU,*) TRIM(ADJUSTL(CH))
        ENDDO
        CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW-IR1+1,IR2-IR1+1,'Exporting '//TRIM(ITOS(I))//' out of '//TRIM(ITOS(N))//', Progress ')
       ENDDO

      ELSE

       IRAT1=0
       DO IROW=IR1,IR2
        CALL WMESSAGEPEEK(ITYPE,MESSAGE)
        NR=INT((MP(IPLOT)%IDF%SY(IROW-1)-MP(IPLOT)%IDF%SY(IROW))/CS)
        DO IR=1,NR
         DO ICOL=IC1,IC2
          IF(IROW.GE.1.AND.IROW.LE.MP(IPLOT)%IDF%NROW.AND. &
             ICOL.GE.1.AND.ICOL.LE.MP(IPLOT)%IDF%NCOL)THEN
           X=IDFGETVAL(MP(IPLOT)%IDF,IROW,ICOL)
          ELSE
           X=MP(IPLOT)%IDF%NODATA
          ENDIF
          WRITE(CH,*) X
          !## avoid '*****' in idf to be exported - make them nodata values
          IF(INDEX(CH,'*').NE.0)WRITE(CH,*) MP(IPLOT)%IDF%NODATA
          NC=INT((MP(IPLOT)%IDF%SX(ICOL)-MP(IPLOT)%IDF%SX(ICOL-1))/CS)
          DO IC=1,NC
           WRITE(JU,*) TRIM(ADJUSTL(CH))
          ENDDO
         ENDDO
        ENDDO
        CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW-IR1+1,IR2-IR1+1,'Exporting '//TRIM(ITOS(I))//' out of '//TRIM(ITOS(N))//', Progress ')
       ENDDO

      ENDIF

      CLOSE(MP(IPLOT)%IDF%IU)
      CLOSE(JU)
      CALL IDFDEALLOCATEX(MP(IPLOT)%IDF)

     ENDIF
    ENDIF
   ENDIF
  ENDIF

 ENDDO

 IF(ALLOCATED(IDFS))DEALLOCATE(IDFS)

 CALL UTL_MESSAGEHANDLE(1)

 CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully converted selected IDF file(s) to:'//CHAR(13)// &
    TRIM(PATH)//CHAR(13)//' in ESRI-Raster format (*.asc)','Information')

 END SUBROUTINE ASC2IDF_EXPORTASC

END MODULE MOD_ASC2IDF
