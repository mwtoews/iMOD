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
MODULE MOD_POLYGON_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_POLYGON_PAR
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_UTL, ONLY : ITOS,RTOS,UTL_GETUNIT,UTL_WSELECTFILE,UTL_CREATEDIR,UTL_GENLABELSREAD, &
          UTL_GENLABELSDEALLOCATE,VAR,DVAR,CCNST,UTL_GENLABELSWRITE,VAR,NV,NL
USE MOD_IDF, ONLY : IDFDEALLOCATE
USE MOD_SPOINTS_PAR
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_GENPLOT, ONLY : TOPOSHPTOGEN

CONTAINS

 !###======================================================================
 SUBROUTINE POLYGON1FIELDS(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 INTEGER :: I

 SHPNO=MAX(0,SHPNO)

 CALL WDIALOGSELECT(ID)

 IF(SHPNO.GT.0)THEN
  CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)
 ELSE
  SHPIACT=0
 ENDIF

 I=0
 IF(SUM(SHPIACT(1:SHPNO)).GT.0)I=1
 CALL WDIALOGFIELDSTATE(ID_SAVESHAPE,I)
 CALL WDIALOGFIELDSTATE(ID_DELETE,I)
 CALL WDIALOGFIELDSTATE(ID_ZOOMSELECT,I)
 I=0
 IF(SUM(SHPIACT(1:SHPNO)).EQ.1)I=1
 CALL WDIALOGFIELDSTATE(ID_RENAME,I)

 END SUBROUTINE POLYGON1FIELDS

 !###======================================================================
 SUBROUTINE POLYGON1SAVELOADSHAPE(CODE,ID,GENFNAME,IDAT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: GENFNAME
 INTEGER,INTENT(IN) :: CODE,ID
 INTEGER,INTENT(IN),OPTIONAL :: IDAT
 INTEGER :: IU,IREC,I,J,K,IOS,NSHAPE,NCRDS,MXCRDS,NID,DATAOFF !,IUDBF
 REAL :: XC,YC
 REAL,ALLOCATABLE,DIMENSION(:) :: XPOL,YPOL
 DOUBLE PRECISION :: XMINSHP,YMINSHP,XMAXSHP,YMAXSHP
 CHARACTER(LEN=256) :: FNAME,STRING
 CHARACTER(LEN=10) :: CTYPE
 CHARACTER(LEN=50) :: SHAPENAME
 INTEGER :: ISHP,NUMPARTS,NUMPOINTS,INUM,JP,MAXNPARTS,I1,I2,N,M
 DOUBLE PRECISION :: X,Y
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IP

 !## save/load ipf file
 IF(INDEX(GENFNAME,'.IPF').GT.0)THEN
  FNAME=GENFNAME; IF(LEN_TRIM(GENFNAME).EQ.4)FNAME=''
  IF(CODE.EQ.ID_LOADSHAPE)THEN
   CALL POLYGON1LOADFROMIPF(FNAME,ID)
  ELSE
   CALL POLYGON1SAVEASIPF(FNAME)
  ENDIF
  RETURN
 ENDIF

 SHPFILE=''

 FNAME=TRIM(PREFVAL(1))//'\SHAPES'
 IU   =UTL_GETUNIT()

 IF(CODE.EQ.ID_LOADSHAPE)THEN

  IF(GENFNAME.EQ.'')THEN
   IF(.NOT.UTL_WSELECTFILE('All Possible Files (*.gen;*.shp)|*.gen;*.shp|ArcInfo Generate Files (*.gen)|*.gen|'// &
     'ArcGis Shape Files (*.shp)|*.shp|',&
        LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Load iMOD Shape File'))RETURN
  ELSE
   FNAME=GENFNAME
  ENDIF

  CALL IUPPERCASE(FNAME)

  !## reset number of polygons available yet!
  SHPNO=0
  
  !## test file to determine array-dimensions
  NSHAPE=0
  MXCRDS=0

  IF(INDEX(FNAME,'.SHP').GT.0)THEN
   !## transform shp/dbf -> gen/dat
   IF(.NOT.TOPOSHPTOGEN(TRIM(FNAME)))RETURN
   FNAME=FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.GEN' 
  ENDIF

  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ,DENYWRITE',FORM='FORMATTED',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open file for reading:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
   RETURN
  ENDIF  
  SHPFILE=FNAME
  
  DO
   READ(IU,*,IOSTAT=IOS) I
   IF(IOS.NE.0)EXIT
   NSHAPE=NSHAPE+1
   NCRDS =0
   DO
    NCRDS=NCRDS+1
    READ(IU,*,IOSTAT=IOS) XC,YC
    IF(IOS.NE.0)EXIT
   END DO
   MXCRDS=MAX(NCRDS,MXCRDS)
  ENDDO

  IF(NSHAPE+SHPNO.GT.MAXSHAPES)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Total number of shapes read is '// &
             TRIM(ITOS(NSHAPE+SHPNO))//CHAR(13)//'* Maximal allowed is '//TRIM(ITOS(MAXSHAPES))//CHAR(13)//&
             'You can increase these settings in the menu-option: Preferences'//CHAR(13)//CHAR(13)// &
            'Selected file not read!','Error')
   CLOSE(IU)
   RETURN
  ENDIF

  REWIND(IU)

  IF(ALLOCATED(XPOL))DEALLOCATE(XPOL)
  IF(ALLOCATED(YPOL))DEALLOCATE(YPOL)
  ALLOCATE(XPOL(MXCRDS),YPOL(MXCRDS))

  !## maximum is for UTL_INSIDEPOLYGON etc.
  IF(ASSOCIATED(SHPXC))THEN
   IF(SIZE(SHPXC,1).LT.MXCRDS.OR.SIZE(SHPXC,2).LT.SHPNO)THEN
    ALLOCATE(CSHPXC(MXCRDS,MAXSHAPES+1),STAT=IOS)
    N=SIZE(SHPXC,1)
    M=SIZE(SHPXC,2)
    DO I=1,N; DO J=1,M; CSHPXC(I,J)=SHPXC(I,J); ENDDO; ENDDO
    DEALLOCATE(SHPXC)
    SHPXC=>CSHPXC
   ENDIF
  ENDIF
  IF(ASSOCIATED(SHPYC))THEN
   IF(SIZE(SHPYC,1).LT.MXCRDS.OR.SIZE(SHPYC,2).LT.SHPNO)THEN
    ALLOCATE(CSHPYC(MXCRDS,MAXSHAPES+1))
    N=SIZE(SHPYC,1)
    M=SIZE(SHPYC,2)
    DO I=1,N; DO J=1,M; CSHPYC(I,J)=SHPYC(I,J); ENDDO; ENDDO
    DEALLOCATE(SHPYC)
    SHPYC=>CSHPYC
   ENDIF
  ENDIF

  IF(SHPNO.GT.0)SHPIACT(1:SHPNO)=0

  J=SHPNO
  DO

   READ(IU,'(A256)',IOSTAT=IOS) STRING
   IF(IOS.NE.0)EXIT
   
   READ(STRING,*,IOSTAT=IOS) SHPID(MIN(J+1,MAXSHAPES)),SHAPENAME
   IF(IOS.NE.0)THEN
    SHAPENAME=''
    READ(STRING,*,IOSTAT=IOS) SHPID(MIN(J+1,MAXSHAPES))
   ENDIF
   IF(IOS.NE.0)EXIT

   J=MIN(J+1,MAXSHAPES)

   I=0
   DO
    READ(IU,*,IOSTAT=IOS) XC,YC
    IF(IOS.NE.0)EXIT
    I=I+1
    XPOL(I)=XC
    YPOL(I)=YC
   END DO

   SHPNCRD(J)=I
   SHPNO     =J

 !   CALL IMODSIMPLIFYSHAPE(XPOL,YPOL,SHPNCRD(J),MCRD)
 !   SHPNCRD(J)=MCRD
   SHPXC(1:SHPNCRD(J),J)=XPOL(1:SHPNCRD(J))
   SHPYC(1:SHPNCRD(J),J)=YPOL(1:SHPNCRD(J))
   SHPNAME(J)           =SHAPENAME
   SHPWIDTH(J)          =2
   SHPCOLOR(J)          =ICLRPOLG !WRGB(0,0,255)

   !## determine what kind of shape ...
   IF(SHPNCRD(J).EQ.1)THEN
    SHPTYPE(J)=ID_POINT
    CTYPE='POINT'
   ELSE
    IF(SHPXC(1,J).EQ.SHPXC(SHPNCRD(J),J).AND. &
       SHPYC(1,J).EQ.SHPYC(SHPNCRD(J),J))THEN
     !## remove last point
     SHPNCRD(J)  =SHPNCRD(J)-1
     SHPTYPE(J)=ID_POLYGON
     CTYPE='POLYGON'
    ELSE
     SHPTYPE(J)=ID_LINE
     CTYPE='LINE'
    ENDIF
   ENDIF

   SHPCOLOR(SHPNO)=WRGB(255,0,0)
   IF(ALLOCATED(SPNT))THEN
    SPNT(J)%IDX=25
    SPNT(J)%IDY=25
    SPNT(J)%ISX=25
    SPNT(J)%ISY=25
    SPNT(J)%ISZ=1
    SPNT(J)%IRADIUS=100
    SPNT(J)%BOTIDF=''
    SPNT(J)%TOPIDF=''
    SPNT(J)%REFIDF=''
    SPNT(J)%IREF=0
   ENDIF

   IF(SHPNAME(SHPNO).EQ.'')THEN
    I=INDEXNOCASE(FNAME,'\',.TRUE.)+1
    K=INDEXNOCASE(FNAME,'.',.TRUE.)-1
    IF(K-I.LE.0)THEN
     SHPNAME(SHPNO) ='SHAPE'//TRIM(ITOS(J))//'_'//TRIM(CTYPE)
    ELSE
     SHPNAME(SHPNO) =FNAME(I:K)//'_'//TRIM(ITOS(J))//'_'//TRIM(CTYPE)
    ENDIF
   ENDIF

   SHPIACT(SHPNO)=1
  ENDDO

  CLOSE(IU)

  !## load associated file (if exist)
  IF(PRESENT(IDAT))THEN
   IF(IDAT.EQ.1)CALL UTL_GENLABELSREAD(FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.dat' )
  ENDIF
  
  IF(ALLOCATED(IP))DEALLOCATE(IP)
  IF(ALLOCATED(XPOL))DEALLOCATE(XPOL)
  IF(ALLOCATED(YPOL))DEALLOCATE(YPOL)

  IF(ID.NE.0)THEN
   CALL WDIALOGSELECT(ID)
   CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
  ENDIF

 ELSEIF(CODE.EQ.ID_SAVESHAPE)THEN !.OR.CODE.EQ.ID_SAVE)THEN
 
  IF(GENFNAME.EQ.'')THEN
   IF(.NOT.UTL_WSELECTFILE('iMOD Shape Files (*.gen)|*.gen|',     &
        SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Save iMOD Shape File'))RETURN
  ELSE
   FNAME=GENFNAME
   CALL UTL_CREATEDIR(FNAME(:INDEX(FNAME,'\',.TRUE.)-1))
  ENDIF

  IF(ID.NE.0)THEN
   CALL WDIALOGSELECT(ID); CALL WDIALOGGETMENU(IDF_MENU1,SHPIACT)
  ENDIF

  CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open file for writing:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
   RETURN
  ENDIF  
  SHPFILE=FNAME

  CALL IUPPERCASE(FNAME)
  NID=0
  DO I=1,SHPNO
   IF(SHPNCRD(I).GT.0)THEN
    IF(SHPTYPE(I).EQ.ID_POINT)THEN
     DO J=1,SHPNCRD(I)
      NID=NID+1; STRING=TRIM(ITOS(NID))//','//TRIM(SHPNAME(I))
      WRITE(IU,'(A)') TRIM(STRING) 
      STRING=TRIM(RTOS(SHPXC(J,I),'F',7))//','//TRIM(RTOS(SHPYC(J,I),'F',7))
      WRITE(IU,'(A)') TRIM(STRING) 
      WRITE(IU,'(A)') 'END'
     END DO
    ELSEIF(SHPTYPE(I).EQ.ID_RECTANGLE)THEN
     NID=NID+1; IF(SHPID(I).EQ.0)SHPID(I)=NID
     STRING=TRIM(ITOS(SHPID(I)))//','//TRIM(SHPNAME(I))
     WRITE(IU,'(A)') TRIM(STRING) 

     STRING=TRIM(RTOS(SHPXC(1,I),'F',7))//','//TRIM(RTOS(SHPYC(1,I),'F',7))
     WRITE(IU,'(A)') TRIM(STRING) 
     STRING=TRIM(RTOS(SHPXC(1,I),'F',7))//','//TRIM(RTOS(SHPYC(2,I),'F',7))
     WRITE(IU,'(A)') TRIM(STRING) 
     STRING=TRIM(RTOS(SHPXC(2,I),'F',7))//','//TRIM(RTOS(SHPYC(2,I),'F',7))
     WRITE(IU,'(A)') TRIM(STRING) 
     STRING=TRIM(RTOS(SHPXC(2,I),'F',7))//','//TRIM(RTOS(SHPYC(1,I),'F',7))
     WRITE(IU,'(A)') TRIM(STRING) 
     STRING=TRIM(RTOS(SHPXC(1,I),'F',7))//','//TRIM(RTOS(SHPYC(1,I),'F',7))
     WRITE(IU,'(A)') TRIM(STRING) 
     WRITE(IU,'(A)') 'END'

    ELSE
     NID=NID+1;IF(SHPID(I).EQ.0)SHPID(I)=NID
     STRING=TRIM(ITOS(SHPID(I)))//','//TRIM(SHPNAME(I))
     WRITE(IU,'(A)') TRIM(STRING) 
     DO J=1,SHPNCRD(I)
      STRING=TRIM(RTOS(SHPXC(J,I),'F',7))//','//TRIM(RTOS(SHPYC(J,I),'F',7))
      WRITE(IU,'(A)') TRIM(STRING) 
     END DO
     !## close for polygons
     IF(SHPTYPE(I).EQ.ID_POLYGON)THEN
      STRING=TRIM(RTOS(SHPXC(1,I),'F',7))//','//TRIM(RTOS(SHPYC(1,I),'F',7))
      WRITE(IU,'(A)') TRIM(STRING)
     ENDIF
     WRITE(IU,'(A)') 'END'
    ENDIF
   ENDIF
  END DO
  WRITE(IU,'(A)') 'END'

  CLOSE(IU)
  
  !## save associated file (if exist)
  IF(PRESENT(IDAT))THEN
   IF(IDAT.EQ.1)CALL UTL_GENLABELSWRITE(FNAME(:INDEX(FNAME,'.',.TRUE.)-1)//'.dat' )
  ENDIF

 ENDIF

 END SUBROUTINE POLYGON1SAVELOADSHAPE

 !###======================================================================
 SUBROUTINE POLYGON1SAVEASIPF(FNAME)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256),INTENT(INOUT) :: FNAME
 CHARACTER(LEN=256) :: LINE
 INTEGER :: IOS,I,J,IU
  
 IF(LEN_TRIM(FNAME).EQ.0)THEN
  IF(.NOT.UTL_WSELECTFILE('iMOD Point Files (*.ipf)|*.ipf|',     &
       SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Save iMOD Point File'))RETURN
 ENDIF
 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='REPLACE',ACTION='WRITE,DENYREAD',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open file for writing:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
  RETURN
 ENDIF  
 LINE=TRIM(ITOS(SHPNO))
 WRITE(IU,'(A)') TRIM(LINE)
 LINE=TRIM(ITOS(2+MAX(0,NV-2))) 
 WRITE(IU,'(A)') TRIM(LINE)
 WRITE(IU,'(A)') '"X-COORDINATE (UTM)"'   
 WRITE(IU,'(A)') '"Y-COORDINATE (UTM)"'   
 DO I=3,NV; WRITE(IU,'(A)') TRIM(VAR(I,0)); ENDDO
 WRITE(IU,'(A)') '0,TXT'
 DO I=1,MAX(SHPNO,NL)
  LINE=TRIM(RTOS(SHPXC(1,I),'F',2))//','//TRIM(RTOS(SHPYC(1,I),'F',2))
  DO J=3,NV !1,NV
   LINE=TRIM(LINE)//','
   IF(INDEX(TRIM(VAR(J,I)),' ').GT.0)LINE=TRIM(LINE)//'"'
   LINE=TRIM(LINE)//TRIM(VAR(J,I))
   IF(INDEX(TRIM(VAR(J,I)),' ').GT.0)LINE=TRIM(LINE)//'"'
  ENDDO
  WRITE(IU,'(A)') TRIM(LINE)
 ENDDO
 CLOSE(IU)

 END SUBROUTINE POLYGON1SAVEASIPF

 !###======================================================================
 SUBROUTINE POLYGON1LOADFROMIPF(IPFFNAME,ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID
 CHARACTER(LEN=256),INTENT(INOUT) :: IPFFNAME
 INTEGER :: IU,IOS,I,J
 CHARACTER(LEN=256) :: FNAME
  
 IU   =UTL_GETUNIT()
 IF(IPFFNAME.EQ.'')THEN
  IF(.NOT.UTL_WSELECTFILE('iMOD Point Files (*.ipf)|*.ipf|',&
       LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Load iMOD Point File'))RETURN
 ELSE
  FNAME=IPFFNAME
 ENDIF
 CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',ACTION='READ,DENYWRITE',FORM='FORMATTED',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open file for reading:'//CHAR(13)//'['//TRIM(FNAME)//']','Error')
  RETURN
 ENDIF  

 READ(IU,*) NL
 READ(IU,*) NV
 
 IF(ASSOCIATED(VAR))DEALLOCATE(VAR)

 ALLOCATE(VAR(NV,0:NL)); VAR=''
 DO I=1,NV; READ(IU,*) VAR(I,0); ENDDO; READ(IU,*)
 DO I=1,NL; SHPNO=NL
  READ(IU,*) (VAR(J,I),J=1,NV)
  SHPNCRD(I)=1
  READ(VAR(1,I),*) SHPXC(1,I)
  READ(VAR(2,I),*) SHPYC(1,I)
  SHPNAME(I)='Point_'//TRIM(ITOS(I)) !VAR(1,I))
  SHPWIDTH(I)=2
  SHPCOLOR(I)=ICLRPOLG 
  SHPTYPE(I)=ID_POINT
 ENDDO
 CLOSE(IU)

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGPUTMENU(IDF_MENU1,SHPNAME,SHPNO,SHPIACT)
  
 END SUBROUTINE POLYGON1LOADFROMIPF

 !###======================================================================
 SUBROUTINE POLYGON1INIT()
 !###======================================================================
 IMPLICIT NONE

 ALLOCATE(SHPNCRD(MAXSHAPES+1))
 ALLOCATE(SHPXC(MAXSHPCRD,MAXSHAPES+1))
 ALLOCATE(SHPYC(MAXSHPCRD,MAXSHAPES+1))
 ALLOCATE(SHPNAME(MAXSHAPES+1))
 ALLOCATE(SHPCOLOR(MAXSHAPES+1))
 ALLOCATE(SHPIACT(MAXSHAPES+1))
 ALLOCATE(SHPWIDTH(MAXSHAPES+1))
 ALLOCATE(SHPTYPE(MAXSHAPES+1))
 ALLOCATE(SHPID(MAXSHAPES+1))
 SHPNO  =0
 SHPNCRD=0
 NULLIFY(VAR,DVAR,CCNST)

 END SUBROUTINE POLYGON1INIT

 !###======================================================================
 SUBROUTINE POLYGON1DEALLOCATE_SELIDF()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(SELIDF))THEN
  CALL IDFDEALLOCATE(SELIDF,SIZE(SELIDF))
  DEALLOCATE(SELIDF)
 ENDIF

 END SUBROUTINE POLYGON1DEALLOCATE_SELIDF

 !###======================================================================
 SUBROUTINE POLYGON1CLOSE()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(SHPNCRD))DEALLOCATE(SHPNCRD)
 IF(ASSOCIATED(SHPXC))DEALLOCATE(SHPXC)
 IF(ASSOCIATED(SHPYC))DEALLOCATE(SHPYC)
 IF(ALLOCATED(SHPNAME))DEALLOCATE(SHPNAME)
 IF(ALLOCATED(SHPIACT))DEALLOCATE(SHPIACT)
 IF(ALLOCATED(SHPCOLOR))DEALLOCATE(SHPCOLOR)
 IF(ALLOCATED(SHPWIDTH))DEALLOCATE(SHPWIDTH)
 IF(ALLOCATED(SHPTYPE))DEALLOCATE(SHPTYPE)
 IF(ALLOCATED(SHPID))DEALLOCATE(SHPID)
 CALL POLYGON1DEALLOCATE_SELIDF()
 CALL UTL_GENLABELSDEALLOCATE()
 SHPNO  =0

 END SUBROUTINE POLYGON1CLOSE

 !###======================================================================
 SUBROUTINE POLYGON1IMAGES(ID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: ID

 CALL WDIALOGSELECT(ID)
 CALL WDIALOGPUTIMAGE(ID_SAVESHAPE,ID_ICONSAVEAS)
 CALL WDIALOGPUTIMAGE(ID_LOADSHAPE,ID_ICONOPEN)
 CALL WDIALOGPUTIMAGE(ID_DRAW,ID_ICONDRAW)
 CALL WDIALOGPUTIMAGE(ID_DELETE,ID_ICONDELETE)
 CALL WDIALOGPUTIMAGE(ID_RENAME,ID_ICONRENAME)
 CALL WDIALOGPUTIMAGE(ID_ZOOMSELECT,ID_ICONZOOMFULL)

 END SUBROUTINE POLYGON1IMAGES

END MODULE MOD_POLYGON_UTL

