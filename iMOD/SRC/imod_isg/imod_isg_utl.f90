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
MODULE MOD_ISG_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_ISG_PAR 
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_WSELECTFILE,UTL_MESSAGEHANDLE,UTL_CREATEDIR
USE MOD_OSD, ONLY : OSD_OPEN,OSD_IOSTAT_MSG,ICF
USE MOD_IDF, ONLY : IDFALLOCATEX,IDFNULLIFY,IDFIROWICOL,IDFCOPY
USE MODPLOT, ONLY : MP,MXMPLOT
USE MOD_PREF_PAR, ONLY : PREFVAL
USE IMOD, ONLY : IDFINIT

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION ISGATTRIBUTES_2DCROSS_READ(J,IDF,PIDF,ZCHK)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 REAL,INTENT(OUT) :: ZCHK
 INTEGER :: I,IROW,ICOL,ICHK
 TYPE(IDFOBJ),INTENT(OUT) :: IDF,PIDF
 
 ISGATTRIBUTES_2DCROSS_READ=.FALSE.
 
 CALL IDFNULLIFY(IDF); CALL IDFNULLIFY(PIDF)
 IF(.NOT.ISGATTRIBUTES_2DCROSS_IDFDIM(IDF,J))THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot present results in table.'//CHAR(13)// &
    'no cellsizes can be determined','Warning')
  RETURN
 ENDIF
 
 CALL IDFCOPY(IDF,PIDF)
 IF(.NOT.IDFALLOCATEX(IDF)) RETURN
 IF(.NOT.IDFALLOCATEX(PIDF))RETURN

 IDF%NODATA =-9999;  IDF%X=IDF%NODATA
 PIDF%NODATA=-9999; PIDF%X=0.0  !## on default, inundated whenever stage higher than riverbed
 ICHK=0; IF(DATISC2(J,1)%DISTANCE.LT.0.0.AND.DATISC2(J,1)%BOTTOM.LT.0.0)THEN
  ICHK=1; ZCHK=DATISC2(J,1)%MRC
 ENDIF
 
 !## skip first (dx,dy)
 DO I=2,TISC(J)
  CALL IDFIROWICOL(IDF,IROW,ICOL,DATISC2(J,I)%DISTANCE,DATISC2(J,I)%BOTTOM)
  IDF%X(ICOL,IROW) =DATISC2(J,I)%MRC
  !## store inundation pointer
  IF(ICHK.EQ.1)PIDF%X(ICOL,IROW)=DATISC2(J,I)%ZP
 ENDDO 
 
 ISGATTRIBUTES_2DCROSS_READ=.TRUE.
 
 END FUNCTION ISGATTRIBUTES_2DCROSS_READ
   
 !###======================================================================
 LOGICAL FUNCTION ISGATTRIBUTES_2DCROSS_IDFDIM(IDF,J)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: J
 TYPE(IDFOBJ),INTENT(OUT) :: IDF
 
 ISGATTRIBUTES_2DCROSS_IDFDIM=.FALSE. 

 IDF%DX=ABS(DATISC2(J,1)%DISTANCE)
 IDF%DY=ABS(DATISC2(J,1)%BOTTOM)
 
 IF(IDF%DX.LE.0.0.OR.IDF%DY.LE.0.0)RETURN
 
 IDF%XMIN=MINVAL(DATISC2(J,2:TISC(J))%DISTANCE)-0.5*IDF%DX 
 IDF%XMAX=MAXVAL(DATISC2(J,2:TISC(J))%DISTANCE)+0.5*IDF%DX 
 IDF%YMIN=MINVAL(DATISC2(J,2:TISC(J))%BOTTOM)  -0.5*IDF%DY
 IDF%YMAX=MAXVAL(DATISC2(J,2:TISC(J))%BOTTOM)  +0.5*IDF%DY

 IDF%IEQ=0; IDF%ITB=0; IDF%IVF=0
 IDF%NCOL=(IDF%XMAX-IDF%XMIN)/IDF%DX
 IDF%NROW=(IDF%YMAX-IDF%YMIN)/IDF%DY
  
 ISGATTRIBUTES_2DCROSS_IDFDIM=.TRUE.
 
 END FUNCTION ISGATTRIBUTES_2DCROSS_IDFDIM
 
 !##=====================================================================
 SUBROUTINE ISGGETXY(X,Y,N,TDIST,XCRD,YCRD)
 !##=====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL,INTENT(IN) :: TDIST
 REAL,INTENT(IN),DIMENSION(N) :: X,Y   
 REAL,INTENT(OUT) :: XCRD,YCRD
 INTEGER :: I
 REAL :: D,F,TD
 
 TD=0.0; DO I=2,N
  D=(X(I)-X(I-1))**2.0+(Y(I)-Y(I-1))**2.0
  IF(D.GT.0.0)D=SQRT(D); TD=TD+D
  IF(TD.GT.TDIST)THEN
   F=(D-(TD-TDIST))/D
   XCRD=X(I-1)+F*(X(I)-X(I-1))
   YCRD=Y(I-1)+F*(Y(I)-Y(I-1))
   EXIT
  ENDIF
 ENDDO
 
 END SUBROUTINE ISGGETXY

 !###===============================================================================
 SUBROUTINE ISGSTUWEN_INTERSECT(MAXDIST,XC,YC,IISG,DIST)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IISG !## selected isg segment
 REAL,INTENT(IN) :: MAXDIST,XC,YC !## maximum distance allowed and location of cross-section
 REAL,INTENT(OUT) :: DIST !## distance on selected segment
 INTEGER :: I,J,K,ISTATUS
 REAL :: DX,DY,TD,D,MD,X,Y

 !## initialize
 ISGX=XC; ISGY=YC; DIST=-1.0; IISG=0; MD=MAXDIST
 DO I=1,NISG
  TD=0.0; K=ISG(I)%ISEG-1
  DO J=1,ISG(I)%NSEG-1

   K=K+1; DX=ISP(K+1)%X-ISP(K)%X; DY=ISP(K+1)%Y-ISP(K)%Y
   !## perform intersection
   CALL IGRINTERSECTLINE(ISP(K)%X,ISP(K)%Y,ISP(K+1)%X,ISP(K+1)%Y,XC,YC,XC+DY,YC-DX,X,Y,ISTATUS)

   !## intersect isgline and/or intersect (5)
   IF(ISTATUS.EQ.3.OR.ISTATUS.EQ.5)THEN
    !## compute distance
    D=SQRT((X-XC)**2.0+(Y-YC)**2.0)
    !## first time to put results, or replace it whenever new point is closer
    IF(D.LT.MD)THEN
     MD=D; DIST=TD+SQRT((ISP(K)%X-X)**2.0+(ISP(K)%Y-Y)**2.0)
     ISGX=X; ISGY=Y; IISG=I
    ENDIF
   ELSE
    !## include position of nodes
    D=SQRT((XC-ISP(K)%X)**2.0+(YC-ISP(K)%Y)**2.0)
    IF(D.LT.MD)THEN
     MD=D; DIST=0.0
     IF(J.GT.1)DIST=TD+SQRT(DX**2.0+DY**2.0)
     ISGX=ISP(K)%X; ISGY=ISP(K)%Y; IISG=I
    ENDIF
    !## evaluate last point
    IF(J.EQ.ISG(I)%NSEG-1)THEN
     D=SQRT((XC-ISP(K+1)%X)**2.0+(YC-ISP(K+1)%Y)**2.0)
     IF(D.LT.MD)THEN
      MD=D; DIST=TD+SQRT(DX**2.0+DY**2.0)
      ISGX=ISP(K+1)%X; ISGY=ISP(K+1)%Y; IISG=I
     ENDIF
    ENDIF
   ENDIF

   !## get total distance
   TD=TD+SQRT(DX**2.0+DY**2.0)

  ENDDO
 END DO

 END SUBROUTINE ISGSTUWEN_INTERSECT

 !###======================================================================
 SUBROUTINE UTL_GETUNITSISG(IU,ISGNAME,TSTAT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: ISGNAME
 CHARACTER(LEN=*),INTENT(IN) :: TSTAT
 INTEGER,DIMENSION(MAXFILES),INTENT(OUT) :: IU
 LOGICAL :: LEX,LOPEN
 CHARACTER(LEN=10) :: TSTATUS
 CHARACTER(LEN=50) :: MESSAGE,TACT
 CHARACTER(LEN=256) :: FNAME
 INTEGER :: IOS,I,J,IRECL,IBYTE

 TSTATUS=TSTAT; CALL IUPPERCASE(TSTATUS)
 TACT='READWRITE'; IF(TSTATUS(1:3).EQ.'OLD')TACT='READ,DENYWRITE'

 !## initialise all
 IU=0

 DO I=1,MAXFILES

  IRECL=RECLEN(I) !## record length in bytes!

  IF(I.EQ.4)THEN
   !## depends on type of ISG what size of database is applied
   IF(ISFR.EQ.0)IRECL= SIZE(TATTRIB1)*4
   IF(ISFR.EQ.1)IRECL=(SIZE(TATTRIB2)-1)*4+8
  ENDIF
  
  J=INDEX(ISGNAME,'.',.TRUE.)-1
  FNAME=ISGNAME(:J)//'.'//TRIM(EXT(I))

  INQUIRE(FILE=FNAME,OPENED=LOPEN)
  IF(LOPEN)THEN
   INQUIRE(FILE=FNAME,NUMBER=IU(I))
   CLOSE(IU(I))
  ENDIF
  IF(TSTATUS.EQ.'OLD')THEN
   INQUIRE(FILE=FNAME,EXIST=LEX)
   !## skip if file cannot be found
   IF(.NOT.LEX)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot find'//CHAR(13)//TRIM(FNAME),'Error')
    EXIT
   ENDIF

   IF(IRECL.GT.0)THEN
    IF(ICF.EQ.0)THEN !## lahey record length in bytes
     INQUIRE(FILE=FNAME,RECL=IBYTE)
    ELSEIF(ICF.EQ.1)THEN !## intel record lenght in words
     !## open file
     IU(I)=UTL_GETUNIT()
     CALL OSD_OPEN(IU(I),FILE=FNAME,STATUS=TSTATUS,FORM=TFORM(I),ACTION=TACT,ACCESS='TRANSPARENT',IOSTAT=IOS)
     READ(IU(I)) IBYTE
     !## record length
     IBYTE=(IBYTE-247)/256  !## in bytes
     CLOSE(IU(I))
    ENDIF

    IF(IBYTE.NE.IRECL)THEN
     SELECT CASE (EXT(I))
      CASE ('ISD1','ISC1','IST1','ISQ1')
       CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Record length needed: '//TRIM(ITOS(IRECL))//' bytes'//CHAR(13)// &
       'Record length read: '//TRIM(ITOS(IBYTE))//' bytes'//CHAR(13)//'Wrong record length, cannot open'//CHAR(13)// &
       TRIM(FNAME)//CHAR(13)//CHAR(13)//'iMOD can rewrite this file to be consistent with this iMOD version.'//CHAR(13)// &
       'A copy of the original file will be called *_old_imod_version, continue?','Question?')
       IF(WINFODIALOG(4).NE.1)EXIT
       !## rewrite file to be consistent with record length in this iMOD version (44 instead of 42)
       IF(.NOT.ISGREWRITEFILE(FNAME,TFORM(I),(/IBYTE,IRECL/)))EXIT

      CASE DEFAULT
       CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Record length needed: '//TRIM(ITOS(IRECL))//' bytes'//CHAR(13)// &
        'Record length read: '//TRIM(ITOS(IBYTE))//' bytes'//CHAR(13)//'Wrong record length, cannot open'//CHAR(13)//&
        TRIM(FNAME),'Error')
       EXIT
     END SELECT
    ENDIF

   ENDIF
  ENDIF

  IU(I)=UTL_GETUNIT()
  IF(ICF.EQ.1)IRECL=IRECL/4 !## words for INTEL
  CALL UTL_CREATEDIR(FNAME(:INDEX(FNAME,'\',.TRUE.)-1))
  IF(IRECL.EQ.0)CALL OSD_OPEN(IU(I),FILE=FNAME,STATUS=TSTATUS,FORM=TFORM(I),ACTION=TACT,IOSTAT=IOS)
  IF(IRECL.GT.0)CALL OSD_OPEN(IU(I),FILE=FNAME,STATUS=TSTATUS,FORM=TFORM(I),ACTION=TACT,ACCESS='DIRECT',RECL=IRECL,IOSTAT=IOS)

  IF(IOS.NE.0)THEN
   CALL OSD_IOSTAT_MSG(IOS,MESSAGE)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open'//CHAR(13)//TRIM(FNAME)//CHAR(13)// &
   TRIM(MESSAGE),'Error')
   EXIT
  ENDIF
  
  !## read type of ISG
  IF(TSTATUS.EQ.'OLD')THEN
   IF(I.EQ.1)THEN
    READ(IU(1),*,IOSTAT=IOS) J,ISFR
    IF(IOS.NE.0)ISFR=0; REWIND(IU(1))
   ENDIF
  ENDIF
   
 ENDDO

 IF(MINVAL(IU).LE.0)THEN
  DO I=1,MAXFILES
   IF(IU(I).GT.0)THEN
    INQUIRE(UNIT=IU(I),OPENED=LEX)
    IF(LEX)CLOSE(IU(I))
   ENDIF
  END DO
  IU=0
 ENDIF

 END SUBROUTINE UTL_GETUNITSISG

 !###======================================================================
 LOGICAL FUNCTION ISGREWRITEFILE(FNAME,TFORM,IRECL)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME,TFORM
 INTEGER,INTENT(IN),DIMENSION(2) :: IRECL
 INTEGER,DIMENSION(2) :: IU,IOS,TLEN
 CHARACTER(LEN=256) :: STRING
 INTEGER :: IREC,IREF,N,I
 REAL :: DIST

 ISGREWRITEFILE=.FALSE.

 IU(1)=UTL_GETUNIT()
 CALL OSD_OPEN(IU(1),FILE=FNAME,STATUS='OLD',FORM=TFORM,ACTION='READ,DENYWRITE',ACCESS='TRANSPARENT',IOSTAT=IOS(1))
 IF(IOS(1).NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot open file: '//CHAR(13)// &
    TRIM(FNAME),'Error')
  RETURN
 ENDIF

 IU(2)=UTL_GETUNIT()
 IREC=IRECL(2)
 IF(ICF.EQ.1)IREC=IREC/4 !## words for INTEL
 CALL OSD_OPEN(IU(2),FILE=TRIM(PREFVAL(1))//'\tmp\xxx.tmp',STATUS='REPLACE',FORM=TFORM,ACTION='WRITE', &
      ACCESS='DIRECT',RECL=IREC,IOSTAT=IOS(2))  !## record length in words/bytes!
 IF(IOS(2).NE.0)THEN
  CLOSE(IU(1))
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot create temporary file: '//CHAR(13)// &
    TRIM(PREFVAL(1))//'\tmp\xxx.tmp','Error')
  RETURN
 ENDIF
 !## record length in bytes (for Lahey)

 WRITE(IU(2),REC=1) IRECL(2)*256+247
 TLEN=IRECL

 READ(IU(1),IOSTAT=IOS(1)) IREC,STRING(1:(TLEN(1)-4)) !## record length in bytes (according to Lahey)

 TLEN=TLEN-(3*4)

 !## make sure string is empty
 STRING=''

 IREC=0
 DO

  READ(IU(1),IOSTAT=IOS(1))  N,IREF,DIST,STRING(1:TLEN(1))
  IF(IOS(1).NE.0)EXIT

  IREC=IREC+1
  WRITE(IU(2),REC=IREC+ICF,IOSTAT=IOS(2)) N,IREF,DIST,STRING(1:TLEN(2))
  IF(IOS(2).NE.0)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error writing record '//TRIM(ITOS(IREC))//' from file: '//CHAR(13)// &
     TRIM(FNAME),'Error')
   RETURN
  ENDIF
 ENDDO

 CLOSE(IU(1))
 CLOSE(IU(2))

 I=INFOERROR(1)
 CALL IOSCOPYFILE(TRIM(FNAME),TRIM(FNAME)//'_old_imod_version')
 IF(WINFOERROR(1).EQ.13)THEN
  CALL INTEGERTOSTRING(WINFOERROR(3),STRING,'(I5)')
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Failed to back up file. Code='//TRIM(STRING),'Copy Error')
  RETURN
 ENDIF
 I=INFOERROR(1)
 CALL IOSCOPYFILE(TRIM(PREFVAL(1))//'\tmp\xxx.tmp',TRIM(FNAME))
 IF(WINFOERROR(1).EQ.13)THEN
  CALL INTEGERTOSTRING(WINFOERROR(3),STRING,'(I5)')
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Failed to copy new file. Code='//TRIM(STRING),'Copy Error')
  RETURN
 ENDIF

 ISGREWRITEFILE=.TRUE.

 END FUNCTION ISGREWRITEFILE

 !###======================================================================
 LOGICAL FUNCTION ISGOPENFILES(ISGFILE,CSTATUS)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: CSTATUS
 CHARACTER(LEN=*),INTENT(IN) :: ISGFILE
 INTEGER :: NISGFILES
 
 ISGOPENFILES=.FALSE.

 NISGFILES=1

 IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU)
 ALLOCATE(ISGIU(MAXFILES,NISGFILES))

 !## open idf-file - get idfkind
 CALL UTL_GETUNITSISG(ISGIU(:,1),ISGFILE,CSTATUS)
 !## failed to open
 IF(MINVAL(ISGIU(:,1)).LE.0)THEN
  CALL ISGCLOSEFILES()
 ELSE
  ISGOPENFILES=.TRUE.
 ENDIF

 END FUNCTION ISGOPENFILES

 !###======================================================================
 SUBROUTINE ISGCLOSEFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 DO I=1,SIZE(ISGIU,2)  
  DO J=1,SIZE(ISGIU,1) 
   CLOSE(ISGIU(J,I))
  END DO
 END DO
 IF(ALLOCATED(ISGIU))DEALLOCATE(ISGIU)

 END SUBROUTINE ISGCLOSEFILES

 !###===============================================================================
 SUBROUTINE ISGREAD(FNAME,IBATCH)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IBATCH
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: FNAME
 CHARACTER(LEN=256) :: LINE
 INTEGER :: IS,I,J,ISGFILES,IREC,NISGFILES,CFN_N_ELEM,NV
 INTEGER,DIMENSION(5) :: IOS

 !## deallocate memory
 CALL ISGDEAL(1)

 NISGFILES=SIZE(FNAME)
 ALLOCATE(ISGIU(MAXFILES,NISGFILES))

 DO ISGFILES=1,NISGFILES

  CALL UTL_GETUNITSISG(ISGIU(:,1),FNAME(ISGFILES),'OLD')
  IF(MINVAL(ISGIU(:,1)).LE.0)THEN
   DO I=1,NISGFILES; DO J=1,MAXFILES
    IF(ISGIU(J,I).GT.0)CLOSE(ISGIU(J,I))
   END DO; END DO
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Cannot allocate files for reading','Error')
   IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot allocate files for reading ! '; RETURN
  ENDIF
  IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(FNAME(ISGFILES))//' ...')
  IF(IBATCH.EQ.1)WRITE(*,'(/1X,A/)') 'Reading '//TRIM(FNAME(ISGFILES))//' ...'
 ENDDO
 
 ALLOCATE(NISGF(NISGFILES),NISFR(NISGFILES))

 !#----------------*.isg

 DIMISG=0
 DO ISGFILES=1,NISGFILES
  !#read segment-dimensions
  READ(ISGIU(1,ISGFILES),'(A256)',IOSTAT=IOS(1)) LINE
  READ(LINE,*,IOSTAT=IOS(1)) NISGF(ISGFILES),NISFR(ISGFILES)
  IF(IOS(1).NE.0)THEN
   NISFR(ISGFILES)=0
   READ(LINE,*,IOSTAT=IOS(1)) NISGF(ISGFILES)
   IF(IOS(1).NE.0)THEN
    CALL ISGDEAL(1)
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD not read the number: '//TRIM(ITOS(ISGFILES))// &
                   ', of the selected isg-files properly!','Error')
    RETURN
   ENDIF
   ALLOCATE(ISDLABELS(SIZE(TATTRIB1))); DO I=1,SIZE(ISDLABELS); ISDLABELS(I)=TATTRIB1(I); ENDDO
  ELSE
   !## try to read label for the isd-file - use labels from the first isg given only
   IF(ISGFILES.EQ.1)THEN
    IF(NISFR(ISGFILES).EQ.0)THEN
     ALLOCATE(ISDLABELS(SIZE(TATTRIB1))); DO I=1,SIZE(ISDLABELS); ISDLABELS(I)=TATTRIB1(I); ENDDO
    ELSE
     ALLOCATE(ISDLABELS(SIZE(TATTRIB2))); DO I=1,SIZE(ISDLABELS); ISDLABELS(I)=TATTRIB2(I); ENDDO
    ENDIF
    NV=CFN_N_ELEM(',',1,LINE); NV=NV-2
    !## overwrite default labels
    READ(LINE,*) NISGF(ISGFILES),NISFR(ISGFILES),(ISDLABELS(I),I=1,NV)
   ENDIF
  ENDIF
  DIMISG=DIMISG+NISGF(ISGFILES)
 END DO

 ALLOCATE(OFFSD(0:NISGFILES) ,OFFSC(0:NISGFILES) ,OFFST(0:NISGFILES), &
          OFFSQ(0:NISGFILES) ,OFFISG(0:NISGFILES),OFFISD(0:NISGFILES),OFFISC(0:NISGFILES),&
          OFFIST(0:NISGFILES),OFFISQ(0:NISGFILES))

 !## allocate memory for ISG()
 ALLOCATE(ISG(DIMISG))
 NISG=DIMISG
 ISFR=NISFR(1)
 
 OFFISG=0; OFFSD =0; OFFSC =0; OFFST =0; OFFSQ =0

 !## read segment-file
 IS=0
 ISGLOOP: DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1
   READ(ISGIU(1,ISGFILES),*,IOSTAT=IOS(1)) ISG(IS)%SNAME,ISG(IS)%ISEG,ISG(IS)%NSEG,ISG(IS)%ICLC,ISG(IS)%NCLC,ISG(IS)%ICRS, &
                                           ISG(IS)%NCRS, ISG(IS)%ISTW,ISG(IS)%NSTW,ISG(IS)%IQHR,ISG(IS)%NQHR
   IF(IOS(1).NE.0)EXIT ISGLOOP
   IF(ISG(IS)%ISEG+ISG(IS)%NSEG.GT.OFFISG(ISGFILES))OFFISG(ISGFILES)=ISG(IS)%ISEG+ISG(IS)%NSEG
   IF(ISG(IS)%ICLC+ISG(IS)%NCLC.GT.OFFSD(ISGFILES)) OFFSD(ISGFILES) =ISG(IS)%ICLC+ISG(IS)%NCLC
   IF(ISG(IS)%ICRS+ISG(IS)%NCRS.GT.OFFSC(ISGFILES)) OFFSC(ISGFILES) =ISG(IS)%ICRS+ISG(IS)%NCRS
   IF(ISG(IS)%ISTW+ISG(IS)%NSTW.GT.OFFST(ISGFILES)) OFFST(ISGFILES) =ISG(IS)%ISTW+ISG(IS)%NSTW
   IF(ISG(IS)%IQHR+ISG(IS)%NQHR.GT.OFFSQ(ISGFILES)) OFFSQ(ISGFILES) =ISG(IS)%IQHR+ISG(IS)%NQHR
  ENDDO
  OFFISG(ISGFILES)=MAX(-1,OFFISG(ISGFILES-1)+OFFISG(ISGFILES)-1)
  OFFSD(ISGFILES) =MAX(-1,OFFSD(ISGFILES-1) +OFFSD(ISGFILES)-1)
  OFFSC(ISGFILES) =MAX(-1,OFFSC(ISGFILES-1) +OFFSC(ISGFILES)-1)
  OFFST(ISGFILES) =MAX(-1,OFFST(ISGFILES-1) +OFFST(ISGFILES)-1)
  OFFSQ(ISGFILES) =MAX(-1,OFFSQ(ISGFILES-1) +OFFSQ(ISGFILES)-1)
 ENDDO ISGLOOP

 IF(IOS(1).NE.0)THEN
  CALL ISGDEAL(1); NISG=-1
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD not read the number '//TRIM(ITOS(ISGFILES))// &
                 ' of the selected isg-files properly!','Error')
  RETURN
 ENDIF

 !## determine amount of memory consumed by *.isp
 DIMISP=SUM(ISG(1:NISG)%NSEG)
 ALLOCATE(ISP(DIMISP),STAT=IOS(1))

 !## determine amount of memory consumed by *.isd part I
 DIMISD=SUM(ISG(1:NISG)%NCLC)
 ALLOCATE(ISD(DIMISD),STAT=IOS(2))

 !## determine amount of memory consumed by *.isc part I
 DIMISC=SUM(ISG(1:NISG)%NCRS)
 ALLOCATE(ISC(DIMISC),STAT=IOS(3))

 !## determine amount of memory consumed by *.ist part I
 DIMIST=SUM(ISG(1:NISG)%NSTW)
 ALLOCATE(IST(DIMIST),STAT=IOS(4))

 !## determine amount of memory consumed by *.isq part I
 DIMISQ=SUM(ISG(1:NISG)%NQHR)
 ALLOCATE(ISQ(DIMISQ),STAT=IOS(5))

 IF(SUM(IOS).NE.0)THEN
  CALL ISGDEAL(1); NISG=-1
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD not able to allocate memory to read entire isg(s)!','Error')
  RETURN
 ENDIF

 !## determine total number of datapoints
 NDISD =0
 OFFISD=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%ICLC-1
   DO J=1,ISG(IS)%NCLC
    NISD=IREC+J+OFFSD(ISGFILES-1)
    READ(ISGIU(3,ISGFILES),REC=IREC+J+ICF) ISD(NISD)%N,ISD(NISD)%IREF,ISD(NISD)%DIST,ISD(NISD)%CNAME
    IF(ISD(NISD)%IREF+ISD(NISD)%N.GT.OFFISD(ISGFILES))OFFISD(ISGFILES)=ISD(NISD)%IREF+ISD(NISD)%N
    NDISD=NDISD+ISD(NISD)%N
   ENDDO
   !## correct in case of multiply isg files
   ISG(IS)%ICLC=MAX(0,ISG(IS)%ICLC+OFFSD(ISGFILES-1))

  ENDDO
  OFFISD(ISGFILES)=MAX(-1,OFFISD(ISGFILES-1)+OFFISD(ISGFILES)-1)
 ENDDO

 !## determine total number of cross-section points
 NDISC =0
 OFFISC=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%ICRS-1
   DO J=1,ISG(IS)%NCRS
    NISC=IREC+J+OFFSC(ISGFILES-1)
    READ(ISGIU(5,ISGFILES),REC=IREC+J+ICF) ISC(NISC)%N,ISC(NISC)%IREF,ISC(NISC)%DIST,ISC(NISC)%CNAME
    IF(ISC(NISC)%IREF+ABS(ISC(NISC)%N).GT.OFFISC(ISGFILES))OFFISC(ISGFILES)=ISC(NISC)%IREF+ABS(ISC(NISC)%N)
    NDISC=NDISC+ABS(ISC(NISC)%N)
   ENDDO

   !## correct in case of multiply isg files
   ISG(IS)%ICRS=MAX(0,ISG(IS)%ICRS+OFFSC(ISGFILES-1))

  ENDDO
  OFFISC(ISGFILES)=MAX(-1,OFFISC(ISGFILES-1)+OFFISC(ISGFILES)-1)
 ENDDO

 !## determine total number of struct points
 NDIST =0
 OFFIST=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%ISTW-1
   DO J=1,ISG(IS)%NSTW
    NIST=IREC+J+OFFST(ISGFILES-1)
    READ(ISGIU(7,ISGFILES),REC=IREC+J+ICF) IST(NIST)%N,IST(NIST)%IREF,IST(NIST)%DIST,IST(NIST)%CNAME
    IF(IST(NIST)%IREF+IST(NIST)%N.GT.OFFIST(ISGFILES))OFFIST(ISGFILES)=IST(NIST)%IREF+IST(NIST)%N
    NDIST=NDIST+IST(NIST)%N
   ENDDO
   !## correct in case of multiply isg files
   ISG(IS)%ISTW=MAX(0,ISG(IS)%ISTW+OFFST(ISGFILES-1))

  ENDDO
  OFFIST(ISGFILES)=MAX(-1,OFFIST(ISGFILES-1)+OFFIST(ISGFILES)-1)
 ENDDO

 !## determine total number of qh relationships
 NDISQ =0
 OFFISQ=0
 IS    =0
 DO ISGFILES=1,NISGFILES
  DO I=1,NISGF(ISGFILES)
   IS=IS+1

   IREC=ISG(IS)%IQHR-1
   DO J=1,ISG(IS)%NQHR
    NISQ=IREC+J+OFFSQ(ISGFILES-1)
    READ(ISGIU(9,ISGFILES),REC=IREC+J+ICF) ISQ(NISQ)%N,ISQ(NISQ)%IREF,ISQ(NISQ)%DIST,ISQ(NISQ)%CNAME
    IF(ISQ(NISQ)%IREF+ISQ(NISQ)%N.GT.OFFISQ(ISGFILES))OFFISQ(ISGFILES)=ISQ(NISQ)%IREF+ISQ(NISQ)%N
    NDISQ=NDISQ+ISQ(NISQ)%N
   ENDDO
   !## correct in case of multiply isg files
   ISG(IS)%IQHR=MAX(0,ISG(IS)%IQHR+OFFSQ(ISGFILES-1))

  ENDDO
  OFFISQ(ISGFILES)=MAX(-1,OFFISQ(ISGFILES-1)+OFFISQ(ISGFILES)-1)
 ENDDO

 DIMDATISD=NDISD; DIMDATISC=NDISC; DIMDATIST=NDIST; DIMDATISQ=NDISQ
 ALLOCATE(DATISD(DIMDATISD)); ALLOCATE(DATISC(DIMDATISC))
 ALLOCATE(DATIST(DIMDATIST)); ALLOCATE(DATISQ(DIMDATISQ))

 !## read *.isp,*.isd,*.isc

 NISD=0; NISC=0; NIST=0; NISP=0; NISQ=0

 IS=0
 DO ISGFILES=1,NISGFILES
  DO J=1,NISGF(ISGFILES)

   IS=IS+1

   CALL ISGREADISP(IS,ISGFILES,OFFISG(ISGFILES-1))
   CALL ISGREADISD(IS,ISGFILES,OFFISD(ISGFILES-1))
   CALL ISGREADISC(IS,ISGFILES,OFFISC(ISGFILES-1))
   CALL ISGREADIST(IS,ISGFILES,OFFIST(ISGFILES-1))
   CALL ISGREADISQ(IS,ISGFILES,OFFISQ(ISGFILES-1))

  END DO
 ENDDO

 CALL ISGDEAL(0)
 
 !## isg files name will be first in line, or empty if isg files were merged
 IF(NISGFILES.EQ.1)THEN
  ISGFNAME=FNAME(1)
 ELSE
  ISGFNAME=''
 ENDIF
 
 END SUBROUTINE ISGREAD

 !###===============================================================================
 SUBROUTINE ISGREADISP(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC

 !## read segments
 DO IREC=ISG(IS)%ISEG,ISG(IS)%ISEG+ISG(IS)%NSEG-1
  NISP=NISP+1
  JREC=IREC+IOFF
  READ(ISGIU(2,ISGFILES),REC=IREC+ICF) ISP(JREC)%X,ISP(JREC)%Y
 END DO
 ISG(IS)%ISEG=MAX(0,ISG(IS)%ISEG+IOFF)

 END SUBROUTINE ISGREADISP

 !###===============================================================================
 SUBROUTINE ISGREADISD(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J

 !## read *.isd

 JREC=ISG(IS)%ICLC-1
 DO I=1,ISG(IS)%NCLC
  NISD=NISD+1
  JREC=JREC+1
  IREC=ISD(JREC)%IREF-1
  DO J=1,ISD(JREC)%N
   IREC =IREC+1
   KREC =IREC+IOFF
   IF(ISFR.EQ.0)THEN
    READ(ISGIU(4,ISGFILES),REC=IREC+ICF) DATISD(KREC)%IDATE,DATISD(KREC)%WLVL,DATISD(KREC)%BTML, &
                                         DATISD(KREC)%RESIS,DATISD(KREC)%INFF
   ELSEIF(ISFR.EQ.1)THEN
    READ(ISGIU(4,ISGFILES),REC=IREC+ICF) DATISD(KREC)%IDATE,DATISD(KREC)%CTIME,DATISD(KREC)%WLVL, &   
                                         DATISD(KREC)%BTML, DATISD(KREC)%WIDTH, &
                                         DATISD(KREC)%THCK ,DATISD(KREC)%HCND, &
                                         DATISD(KREC)%UPSG, DATISD(KREC)%DWNS, DATISD(KREC)%ICLC, &
                                         DATISD(KREC)%IPRI ,DATISD(KREC)%QFLW, DATISD(KREC)%QROF
   ENDIF
  ENDDO
  ISD(JREC)%IREF=MAX(0,ISD(JREC)%IREF+IOFF)
 ENDDO

 END SUBROUTINE ISGREADISD

 !###===============================================================================
 SUBROUTINE ISGREADISC(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J,ICSTYPE
 INTEGER(KIND=2) :: ZM
 INTEGER(KIND=1) :: ZC,ZP
 
 !## read *.isc
 JREC=ISG(IS)%ICRS-1
 DO I=1,ISG(IS)%NCRS
  
  NISC=NISC+1
  JREC=JREC+1
  IREC=ISC(JREC)%IREF-1
  ICSTYPE=0
  
  DO J=1,ABS(ISC(JREC)%N)
   IREC =IREC+1
   KREC =IREC+IOFF
   IF(J.EQ.1.OR.ICSTYPE.EQ.0)THEN
    READ(ISGIU(6,ISGFILES),REC=IREC+ICF) DATISC(KREC)%DISTANCE,DATISC(KREC)%BOTTOM,DATISC(KREC)%MRC
    !## read bathemetry with thressholds
    IF(DATISC(KREC)%DISTANCE.LT.0.AND.DATISC(KREC)%BOTTOM.LT.0.0)ICSTYPE=1
    DATISC(KREC)%ZP=0.0 
   ELSE
    READ(ISGIU(6,ISGFILES),REC=IREC+ICF) DATISC(KREC)%DISTANCE,DATISC(KREC)%BOTTOM,ZM,ZC,ZP 
    DATISC(KREC)%MRC=REAL(ZM)+REAL(ZC)/100.0
    DATISC(KREC)%ZP=REAL(ZP) 
   ENDIF
  END DO
  ISC(JREC)%IREF=MAX(0,ISC(JREC)%IREF+IOFF)
 END DO

 END SUBROUTINE ISGREADISC

 !###===============================================================================
 SUBROUTINE ISGREADIST(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J

 !## read *.ist

 JREC=ISG(IS)%ISTW-1
 DO I=1,ISG(IS)%NSTW
  NIST=NIST+1
  JREC=JREC+1
  IREC=IST(JREC)%IREF-1
  DO J=1,IST(JREC)%N
   IREC =IREC+1
   KREC =IREC+IOFF
   READ(ISGIU(8,ISGFILES),REC=IREC+ICF) DATIST(KREC)%IDATE,DATIST(KREC)%WLVL_UP,DATIST(KREC)%WLVL_DOWN
  END DO
  IST(JREC)%IREF=MAX(0,IST(JREC)%IREF+IOFF)
 END DO

 END SUBROUTINE ISGREADIST

 !###===============================================================================
 SUBROUTINE ISGREADISQ(IS,ISGFILES,IOFF)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IS,ISGFILES,IOFF
 INTEGER :: IREC,JREC,KREC,I,J

 !## read *.isq

 JREC=ISG(IS)%IQHR-1
 DO I=1,ISG(IS)%NQHR
  NISQ=NISQ+1
  JREC=JREC+1
  IREC=ISQ(JREC)%IREF-1
  DO J=1,ISQ(JREC)%N
   IREC =IREC+1
   KREC =IREC+IOFF
   READ(ISGIU(10,ISGFILES),REC=IREC+ICF) DATISQ(KREC)%Q,DATISQ(KREC)%W,DATISQ(KREC)%D,DATISQ(KREC)%F
  END DO
  ISQ(JREC)%IREF=MAX(0,ISQ(JREC)%IREF+IOFF)
 END DO

 END SUBROUTINE ISGREADISQ


 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISDVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISDVALUE=.FALSE.

 IF(ALLOCATED(DATISD2))DEALLOCATE(DATISD2)
 IF(ALLOCATED(TISD))DEALLOCATE(TISD)
 ALLOCATE(DATISD2(ISG(ISELISG)%NCLC,ISDMAXROW))
 ALLOCATE(TISD(ISG(ISELISG)%NCLC))

 K=ISG(ISELISG)%ICLC-1

 I=K+1
 J=K+ISG(ISELISG)%NCLC
 CALL WDIALOGSELECT(ID_DISGATTRIBUTESTAB1)
 CALL WDIALOGPUTMENU(IDF_MENU1,ISD(I:J)%CNAME,ISG(ISELISG)%NCLC,1)

 DO I=1,ISG(ISELISG)%NCLC

  K=K+1

  IF(ISDMAXROW.LT.ISD(K)%N)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records to be read is '//TRIM(ITOS(ISDMAXROW))//CHAR(13)// &
                   'iMOD is now reading '//TRIM(ITOS(ISD(K)%N))//' records, rest will be left out!','Error')
  ENDIF

  TISD(I)=0
  L      =ISD(K)%IREF-1

  DO J=1,MIN(ISDMAXROW,ISD(K)%N)

   L           =L+1
   TISD(I)     =TISD(I)+1
   DATISD2(I,J)=DATISD(L)

  ENDDO

 ENDDO

 ISGATTRIBUTESREADISDVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISDVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISCVALUE(IDIALOG)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDIALOG
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISCVALUE=.FALSE.

 IF(ALLOCATED(DATISC2))DEALLOCATE(DATISC2)
 IF(ALLOCATED(TISC))DEALLOCATE(TISC)
 IF(ALLOCATED(ISCN))DEALLOCATE(ISCN)
 
 IF(IDIALOG.EQ.0)THEN
  K=ISG(ISELISG)%ICRS; L=K+ISG(ISELISG)%NCRS-1
  ISCMAXROW=MAXVAL(ABS(ISC(K:L)%N))
 ENDIF
 ALLOCATE(DATISC2(ISG(ISELISG)%NCRS,ISCMAXROW))
 ALLOCATE(TISC(ISG(ISELISG)%NCRS),ISCN(ISG(ISELISG)%NCRS))

 K=ISG(ISELISG)%ICRS-1

 I=K+1
 J=K+ISG(ISELISG)%NCRS
 
 IF(IDIALOG.GT.0)THEN
  CALL WDIALOGSELECT(IDIALOG)
  CALL WDIALOGPUTMENU(IDF_MENU1,ISC(I:J)%CNAME,ISG(ISELISG)%NCRS,1)
 ENDIF
 
 DO I=1,ISG(ISELISG)%NCRS

  K=K+1

  IF(IDIALOG.GT.0)THEN
   IF(ISCMAXROW.LT.ABS(ISC(K)%N))THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records to be read is '//TRIM(ITOS(ISCMAXROW))//CHAR(13)// &
                    'iMOD is now reading '//TRIM(ITOS(ABS(ISC(K)%N)))//' records, rest will be left out!','Error')
   ENDIF
  ENDIF
  
  TISC(I)=0; ISCN(I)=SIGN(1,ISC(K)%N)
  
  L      =ISC(K)%IREF-1

  DO J=1,MIN(ISCMAXROW,ABS(ISC(K)%N))

   L           =L+1
   TISC(I)     =TISC(I)+1
   DATISC2(I,J)=DATISC(L)

  ENDDO
  
 ENDDO

 ISGATTRIBUTESREADISCVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISCVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISTVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISTVALUE=.TRUE.
 
 IF(ALLOCATED(DATIST2))DEALLOCATE(DATIST2)
 IF(ALLOCATED(TIST))DEALLOCATE(TIST)

 I=1
 IF(ISG(ISELISG)%NSTW.EQ.0)I=0
 CALL WDIALOGSELECT(ID_DISGATTRIBUTES)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DISGATTRIBUTESTAB4,I)
 IF(I.EQ.0)RETURN

 ISGATTRIBUTESREADISTVALUE=.FALSE.
 
 ALLOCATE(DATIST2(ISG(ISELISG)%NSTW,ISTMAXROW))
 ALLOCATE(TIST(ISG(ISELISG)%NSTW))

 K=ISG(ISELISG)%ISTW-1

 I=K+1
 J=K+ISG(ISELISG)%NSTW
 CALL WDIALOGSELECT(ID_DISGATTRIBUTESTAB4)
 CALL WDIALOGPUTMENU(IDF_MENU1,IST(I:J)%CNAME,ISG(ISELISG)%NSTW,1)

 DO I=1,ISG(ISELISG)%NSTW

  K=K+1

  IF(ISTMAXROW.LT.IST(K)%N)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records to be read is '//TRIM(ITOS(ISTMAXROW))//CHAR(13)// &
                   'iMOD is now reading '//TRIM(ITOS(IST(K)%N))//' records, rest will be left out!','Error')
  ENDIF

  TIST(I)=0
  L      =IST(K)%IREF-1

  DO J=1,MIN(ISTMAXROW,IST(K)%N)

   L           =L+1
   TIST(I)     =TIST(I)+1
   DATIST2(I,J)=DATIST(L)

  ENDDO

 ENDDO

 ISGATTRIBUTESREADISTVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISTVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISQVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,L

 ISGATTRIBUTESREADISQVALUE=.TRUE.

 IF(ALLOCATED(DATISQ2))DEALLOCATE(DATISQ2)
 IF(ALLOCATED(TISQ))DEALLOCATE(TISQ)

 I=1
 IF(ISG(ISELISG)%NQHR.EQ.0)I=0
 CALL WDIALOGSELECT(ID_DISGATTRIBUTES)
 CALL WDIALOGTABSTATE(IDF_TAB1,ID_DISGATTRIBUTESTAB5,I)
 IF(I.EQ.0)RETURN

 ISGATTRIBUTESREADISQVALUE=.FALSE.
 
 ALLOCATE(DATISQ2(ISG(ISELISG)%NQHR,ISQMAXROW))
 ALLOCATE(TISQ(ISG(ISELISG)%NQHR))

 K=ISG(ISELISG)%IQHR-1

 I=K+1
 J=K+ISG(ISELISG)%NQHR
 CALL WDIALOGSELECT(ID_DISGATTRIBUTESTAB5)
 CALL WDIALOGPUTMENU(IDF_MENU1,ISQ(I:J)%CNAME,ISG(ISELISG)%NQHR,1)

 DO I=1,ISG(ISELISG)%NQHR

  K=K+1

  IF(ISQMAXROW.LT.ISQ(K)%N)THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records to be read is '//TRIM(ITOS(ISQMAXROW))//CHAR(13)// &
                   'iMOD is now reading '//TRIM(ITOS(ISQ(K)%N))//' records, rest will be left out!','Error')
  ENDIF

  TISQ(I)=0
  L      =ISQ(K)%IREF-1

  DO J=1,MIN(ISQMAXROW,ISQ(K)%N)

   L           =L+1
   TISQ(I)     =TISQ(I)+1
   DATISQ2(I,J)=DATISQ(L)

  ENDDO

 ENDDO

 ISGATTRIBUTESREADISQVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISQVALUE

 !###====================================================================
 LOGICAL FUNCTION ISGATTRIBUTESREADISPVALUE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: ISEG,NSEG,I

 ISGATTRIBUTESREADISPVALUE=.FALSE.

 IF(ALLOCATED(ISP2))DEALLOCATE(ISP2)
 ALLOCATE(ISP2(ISPMAXROW))

 NSEG=ISG(ISELISG)%NSEG

 IF(ISPMAXROW.LT.NSEG)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Maximum number of records to be read is '//TRIM(ITOS(ISPMAXROW))//CHAR(13)// &
                  'iMOD wants to read '//TRIM(ITOS(NSEG))//' records, rest will be left out!'//CHAR(13)// &
                  'If you continue and save you will be loose data!','Error')
 ENDIF

 ISEG=ISG(ISELISG)%ISEG-1
 TISP=0
 DO I=1,MIN(ISPMAXROW,NSEG)

  TISP   =TISP+1
  ISP2(I)=ISP(ISEG+I)

 ENDDO

 ISGATTRIBUTESREADISPVALUE=.TRUE.

 END FUNCTION ISGATTRIBUTESREADISPVALUE
 
 !###====================================================================
 SUBROUTINE ISGDELISD(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =ISD(IPOS)%IREF
 N               =ISD(IPOS)%N

 !## adjust isd variable
 ISD(IPOS:NISD-1)=ISD(IPOS+1:NISD)
 NISD            =NISD-1
 ISG(IISG)%NCLC  =ISG(IISG)%NCLC-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ICLC.GT.ISG(IISG)%ICLC)ISG(I)%ICLC=ISG(I)%ICLC-1
 END DO

 IF(ISG(IISG)%NCLC.EQ.0)ISG(IISG)%ICLC=0

 !## find other references to selected calc.pnt definition
 DO J=1,NISD
  IF(ISD(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to calc.pnt
 IF(J.GT.NISD.AND.N.GT.0)THEN
  DATISD(IREF:NDISD-N)=DATISD(IREF+N:NDISD)
  NDISD               =NDISD-N
  !## adjust other references to selected calc.pnt definition
  DO I=1,NISD
   IF(ISD(I)%IREF.GT.IREF)ISD(I)%IREF=ISD(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELISD

 !###====================================================================
 SUBROUTINE ISGDELISC(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =ISC(IPOS)%IREF
 N               =ABS(ISC(IPOS)%N)

 !## adjust isc variable
 ISC(IPOS:NISC-1)=ISC(IPOS+1:NISC)
 NISC            =NISC-1
 ISG(IISG)%NCRS  =ISG(IISG)%NCRS-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ICRS.GT.ISG(IISG)%ICRS)ISG(I)%ICRS=ISG(I)%ICRS-1
 END DO

 IF(ISG(IISG)%NCRS.EQ.0)ISG(IISG)%ICRS=0

 !## find other references to selected cross-section definition
 DO J=1,NISC
  IF(ISC(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to cross-section
 IF(J.GT.NISC.AND.N.GT.0)THEN
  DATISC(IREF:NDISC-N)=DATISC(IREF+N:NDISC)
  NDISC               =NDISC-N
  !## adjust other references to selected cross-section definition
  DO I=1,NISC
   IF(ISC(I)%IREF.GT.IREF)ISC(I)%IREF=ISC(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELISC

 !###====================================================================
 SUBROUTINE ISGDELIST(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =IST(IPOS)%IREF
 N               =IST(IPOS)%N

 !## adjust isd variable
 IST(IPOS:NIST-1)=IST(IPOS+1:NIST)
 NIST            =NIST-1
 ISG(IISG)%NSTW  =ISG(IISG)%NSTW-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ISTW.GT.ISG(IISG)%ISTW)ISG(I)%ISTW=ISG(I)%ISTW-1
 END DO

 IF(ISG(IISG)%NSTW.EQ.0)ISG(IISG)%ISTW=0

 !## find other references to selected cross-section definition
 DO J=1,NIST
  IF(IST(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to cross-section
 IF(J.GT.NIST.AND.N.GT.0)THEN
  DATIST(IREF:NDIST-N)=DATIST(IREF+N:NDIST)
  NDIST               =NDIST-N
 !## adjust other references to selected cross-section definition
  DO I=1,NIST
   IF(IST(I)%IREF.GT.IREF)IST(I)%IREF=IST(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELIST

 !###====================================================================
 SUBROUTINE ISGDELISQ(IISG,IPOS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG,IPOS
 INTEGER :: IREF,N,I,J

 !## remove from current ipos
 IREF            =ISQ(IPOS)%IREF
 N               =ISQ(IPOS)%N

 !## adjust isq variable
 ISQ(IPOS:NISQ-1)=ISQ(IPOS+1:NISQ)
 NISQ            =NISQ-1
 ISG(IISG)%NQHR  =ISG(IISG)%NQHR-1

 !## adjust other references to selected calc.pnt definition from ISG
 DO I=1,NISG
  IF(ISG(I)%IQHR.GT.ISG(IISG)%IQHR)ISG(I)%IQHR=ISG(I)%IQHR-1
 END DO

 IF(ISG(IISG)%NQHR.EQ.0)ISG(IISG)%IQHR=0

 !## find other references to selected cross-section definition
 DO J=1,NISQ
  IF(ISQ(J)%IREF.EQ.IREF)EXIT
 ENDDO

 !## remove from iscdat = only whenever no other refers to cross-section
 IF(J.GT.NISQ.AND.N.GT.0)THEN
  DATISQ(IREF:NDISQ-N)=DATISQ(IREF+N:NDISQ)
  NDISQ               =NDISQ-N
 !## adjust other references to selected cross-section definition
  DO I=1,NISQ
   IF(ISQ(I)%IREF.GT.IREF)ISQ(I)%IREF=ISQ(I)%IREF-N
  ENDDO
 ENDIF

 END SUBROUTINE ISGDELISQ

 !###====================================================================
 SUBROUTINE ISGDELISP(IISG)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IISG
 INTEGER :: ISEG,NSEG,I

 ISEG=ISG(IISG)%ISEG
 NSEG=ISG(IISG)%NSEG

 !##adjust isp variable
 ISP(ISEG:NISP-NSEG)=ISP(ISEG+NSEG:NISP)
 NISP               =NISP-NSEG
 ISG(IISG)%NSEG     =ISG(IISG)%NSEG-NSEG

 !##adjust other references to selected nodes definition from ISG
 DO I=1,NISG
  IF(ISG(I)%ISEG.GT.ISG(IISG)%ISEG)ISG(I)%ISEG=ISG(I)%ISEG-NSEG
 END DO

 END SUBROUTINE ISGDELISP

 !###====================================================================
 SUBROUTINE ISGGETPOSID(TDIST,IPOS,ITYPE)
 !###====================================================================
 !determine id of nearest feature (crosssection/calculationpoint)
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: IPOS
 INTEGER,INTENT(IN) :: ITYPE
 REAL,INTENT(IN) :: TDIST
 REAL :: DIST,MINDIST
 INTEGER :: IREC,NREC,I

 IF(ITYPE.EQ.1)THEN
  IREC=ISG(ISELISG)%ICLC-1
  NREC=ISG(ISELISG)%NCLC
 ELSEIF(ITYPE.EQ.2)THEN
  IREC=ISG(ISELISG)%ICRS-1
  NREC=ISG(ISELISG)%NCRS
 ELSEIF(ITYPE.EQ.3)THEN
  IREC=ISG(ISELISG)%ISTW-1
  NREC=ISG(ISELISG)%NSTW
 ELSEIF(ITYPE.EQ.4)THEN
  IREC=ISG(ISELISG)%IQHR-1
  NREC=ISG(ISELISG)%NQHR
 ENDIF

 !## initial value
 IPOS=1
 DO I=1,NREC
  IREC=IREC+1
  IF(ITYPE.EQ.1)DIST=ABS(ISD(IREC)%DIST-TDIST)
  IF(ITYPE.EQ.2)DIST=ABS(ISC(IREC)%DIST-TDIST)
  IF(ITYPE.EQ.3)DIST=ABS(IST(IREC)%DIST-TDIST)
  IF(ITYPE.EQ.4)DIST=ABS(ISQ(IREC)%DIST-TDIST)
  IF(I.EQ.1)THEN
   MINDIST=DIST
   IPOS   =IREC
  ELSE
   IF(DIST.LE.MINDIST)THEN
    MINDIST=DIST
    IPOS   =IREC
   ENDIF
  ENDIF
 ENDDO

 END SUBROUTINE ISGGETPOSID

 !###====================================================================
 SUBROUTINE ISGGETPOSDISTANCE(XINTER,YINTER,TDIST,IDIST)
 !###====================================================================
 !determine cumulative distance on segment for current location xinter/yinter
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDIST
 REAL,INTENT(OUT) :: TDIST
 REAL,INTENT(IN) :: XINTER,YINTER
 REAL :: DIST
 INTEGER :: ISEG,NSEG,I

 !## enough cross-sections both side of split-point?
 ISEG =ISG(ISELISG)%ISEG
 NSEG =ISG(ISELISG)%NSEG
 TDIST=0.0
 DO I=2,NSEG
  IF(ISEG+1.EQ.IDIST)THEN
   ISEG=ISEG+1
   DIST=(XINTER-ISP(ISEG-1)%X)**2.0+(YINTER-ISP(ISEG-1)%Y)**2.0
   IF(DIST.GT.0.0)DIST=SQRT(DIST)
   TDIST=TDIST+DIST
   EXIT
  ENDIF
  ISEG=ISEG+1
  DIST=(ISP(ISEG)%X-ISP(ISEG-1)%X)**2.0+(ISP(ISEG)%Y-ISP(ISEG-1)%Y)**2.0
  IF(DIST.GT.0.0)DIST=SQRT(DIST)
  TDIST=TDIST+DIST
 END DO

 END SUBROUTINE ISGGETPOSDISTANCE

 !###===============================================================================
 SUBROUTINE ISGSAVE(ISGFILE,ISAVE) !- saving ONLY *.ISG, *.isp, *.isd, *.isc
 !###===============================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(INOUT) :: ISGFILE
 INTEGER,INTENT(IN) :: ISAVE

 IF(ISAVE.EQ.0)THEN
  CALL WMESSAGEBOX(YESNOCANCEL,QUESTIONICON,COMMONNO,'Do you want to SAVE adjustments to: '//CHAR(13)// &
      TRIM(ISGFILE)//' ?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ELSEIF(ISAVE.EQ.1)THEN
  IF(.NOT.UTL_WSELECTFILE('iMOD segment-River File (*.isg)|*.isg|',&
                   SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,ISGFILE,&
                   'Save iMOD segment-River File (*.isg)'))RETURN
 ENDIF

 IF(.NOT.ISGOPENFILES(ISGFILE,'REPLACE'))THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD cannot (re)write ISG file:'//CHAR(13)//TRIM(ISGFILE),'Error')
  RETURN
 ENDIF

 CALL UTL_MESSAGEHANDLE(0)
 CALL WINDOWSELECT(0); CALL WINDOWOUTSTATUSBAR(4,'Saving '//TRIM(ISGFILE)//'...')

 CALL ISGSAVEIT()

 CALL UTL_MESSAGEHANDLE(1)

 CALL ISGCLOSEFILES()
 
 END SUBROUTINE ISGSAVE

 !###===============================================================================
 SUBROUTINE ISGSAVEIT() !- saving ONLY *.ISG, *.isp, *.isd, *.isc
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: IS,I,J,IREC,JREC
 INTEGER(KIND=2) :: ZM
 INTEGER(KIND=1) :: ZC,ZP
 CHARACTER(LEN=256) :: LINE

 !## write header *.ISG file
 LINE=TRIM(ITOS(NISG))//','//TRIM(ITOS(ISFR))
 IF(ALLOCATED(ISDLABELS))THEN
  DO I=1,SIZE(ISDLABELS)
   LINE=TRIM(LINE)//','//TRIM(ISDLABELS(I)) 
  ENDDO
 ENDIF
 WRITE(ISGIU(1,1),'(A)') TRIM(LINE)

 !## write header *.ISG file
 CALL ISGSAVEHEADERS()

 DO IS=1,NISG

  LINE='"'//TRIM(ISG(IS)%SNAME)//'",'//TRIM(ITOS(ISG(IS)%ISEG))//','//TRIM(ITOS(ISG(IS)%NSEG))//','// &
                                       TRIM(ITOS(ISG(IS)%ICLC))//','//TRIM(ITOS(ISG(IS)%NCLC))//','// &
                                       TRIM(ITOS(ISG(IS)%ICRS))//','//TRIM(ITOS(ISG(IS)%NCRS))//','// &
                                       TRIM(ITOS(ISG(IS)%ISTW))//','//TRIM(ITOS(ISG(IS)%NSTW))//','// &
                                       TRIM(ITOS(ISG(IS)%IQHR))//','//TRIM(ITOS(ISG(IS)%NQHR))
  WRITE(ISGIU(1,1),*) TRIM(LINE)

  !## write segments
  IREC=ISG(IS)%ISEG-1
  DO I=1,ISG(IS)%NSEG
   IREC=IREC+1
   WRITE(ISGIU(2,1),REC=IREC+ICF) ISP(IREC)%X,ISP(IREC)%Y
  END DO

  !## write isd1
  IREC=ISG(IS)%ICLC-1
  DO I=1,ISG(IS)%NCLC
   IREC=IREC+1
   WRITE(ISGIU(3,1),REC=IREC+ICF) ISD(IREC)%N,ISD(IREC)%IREF,ISD(IREC)%DIST,ISD(IREC)%CNAME
   !## write isd2
   JREC=ISD(IREC)%IREF-1
   DO J=1,ISD(IREC)%N
    JREC=JREC+1
    IF(ISFR.EQ.0)THEN
     WRITE(ISGIU(4,1),REC=JREC+ICF) DATISD(JREC)%IDATE,DATISD(JREC)%WLVL, &
                                    DATISD(JREC)%BTML,DATISD(JREC)%RESIS,DATISD(JREC)%INFF
    ELSEIF(ISFR.EQ.1)THEN
     WRITE(ISGIU(4,1),REC=JREC+ICF) DATISD(JREC)%IDATE,DATISD(JREC)%CTIME,DATISD(JREC)%WLVL, &
                                    DATISD(JREC)%BTML, DATISD(JREC)%WIDTH, &
                                    DATISD(JREC)%THCK ,DATISD(JREC)%HCND, &
                                    DATISD(JREC)%UPSG, DATISD(JREC)%DWNS ,DATISD(JREC)%ICLC, &
                                    DATISD(JREC)%IPRI, DATISD(JREC)%QFLW ,DATISD(JREC)%QROF
    ENDIF
   ENDDO
  ENDDO

  !## write isc1
  IREC=ISG(IS)%ICRS-1
  DO I=1,ISG(IS)%NCRS
   IREC=IREC+1
   WRITE(ISGIU(5,1),REC=IREC+ICF) ISC(IREC)%N,ISC(IREC)%IREF,ISC(IREC)%DIST,ISC(IREC)%CNAME
   !## write isc2
   JREC=ISC(IREC)%IREF-1

   JREC=JREC+1
   WRITE(ISGIU(6,1),REC=JREC+ICF) DATISC(JREC)%DISTANCE,DATISC(JREC)%BOTTOM,DATISC(JREC)%MRC   
   IF(DATISC(JREC)%DISTANCE.LT.0.0.AND.DATISC(JREC)%BOTTOM.LT.0.0)THEN
    DO J=2,ABS(ISC(IREC)%N)
     JREC=JREC+1
     ZM=INT(DATISC(JREC)%MRC,2)
     ZC=INT(100.0*(DATISC(JREC)%MRC-REAL(ZM)),1)
     ZP=INT(DATISC(JREC)%ZP,1)
     WRITE(ISGIU(6,1),REC=JREC+ICF) DATISC(JREC)%DISTANCE,DATISC(JREC)%BOTTOM,ZM,ZC,ZP
    ENDDO 
   ELSE   
    DO J=2,ABS(ISC(IREC)%N)
     JREC=JREC+1
     WRITE(ISGIU(6,1),REC=JREC+ICF) DATISC(JREC)%DISTANCE,DATISC(JREC)%BOTTOM,DATISC(JREC)%MRC
    ENDDO
   ENDIF
   
  ENDDO

  !## write ist1
  IREC=ISG(IS)%ISTW-1
  DO I=1,ISG(IS)%NSTW
   IREC=IREC+1
   WRITE(ISGIU(7,1),REC=IREC+ICF) IST(IREC)%N,IST(IREC)%IREF,IST(IREC)%DIST,IST(IREC)%CNAME
   !## write isc2
   JREC=IST(IREC)%IREF-1
   DO J=1,IST(IREC)%N
    JREC=JREC+1
    WRITE(ISGIU(8,1),REC=JREC+ICF) DATIST(JREC)%IDATE,DATIST(JREC)%WLVL_UP,DATIST(JREC)%WLVL_DOWN
   ENDDO
  ENDDO

  !## write isq1
  IREC=ISG(IS)%IQHR-1
  DO I=1,ISG(IS)%NQHR
   IREC=IREC+1
   WRITE(ISGIU(9,1),REC=IREC+ICF) ISQ(IREC)%N,ISQ(IREC)%IREF,ISQ(IREC)%DIST,ISQ(IREC)%CNAME
   !## write isc2
   JREC=ISQ(IREC)%IREF-1
   DO J=1,ISQ(IREC)%N
    JREC=JREC+1
    WRITE(ISGIU(10,1),REC=JREC+ICF) DATISQ(JREC)%Q,DATISQ(JREC)%W,DATISQ(JREC)%D,DATISQ(JREC)%F
   ENDDO
  ENDDO

 ENDDO

 DO I=1,MAXFILES; CLOSE(ISGIU(I,1)); END DO

 END SUBROUTINE ISGSAVEIT

 !###===============================================================================
 SUBROUTINE ISGSAVEHEADERS()
 !###===============================================================================
 IMPLICIT NONE
 INTEGER :: IREC

 !## write header (recordlength) in case IVF is used
 IF(ICF.EQ.1)THEN
  WRITE(ISGIU(2,1),REC=1)  (RECLEN(2) *256)+247  !## segments
  WRITE(ISGIU(3,1),REC=1)  (RECLEN(3) *256)+247  !## isd1
  !## depends on type of ISG what size of database is applied
  IF(ISFR.EQ.0)IREC= SIZE(TATTRIB1)*4
  IF(ISFR.EQ.1)IREC=(SIZE(TATTRIB2)-1)*4+8
  WRITE(ISGIU(4,1),REC=1)  (IREC      *256)+247  !## isd2
  WRITE(ISGIU(5,1),REC=1)  (RECLEN(5) *256)+247  !## isc1
  WRITE(ISGIU(6,1),REC=1)  (RECLEN(6) *256)+247  !## isc2
  WRITE(ISGIU(7,1),REC=1)  (RECLEN(7) *256)+247  !## ist1
  WRITE(ISGIU(8,1),REC=1)  (RECLEN(8) *256)+247  !## ist2
  WRITE(ISGIU(9,1),REC=1)  (RECLEN(9) *256)+247  !## isq1
  WRITE(ISGIU(10,1),REC=1) (RECLEN(10)*256)+247  !## isq2
 ENDIF

 END SUBROUTINE ISGSAVEHEADERS

 !###===============================================================================
 SUBROUTINE ISGMEMORYISG(DN)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN

 NISG=NISG+DN

 IF(NISG.GT.DIMISG)THEN

  IF(ALLOCATED(DUMISG))DEALLOCATE(DUMISG)
  ALLOCATE(DUMISG(DIMISG))
  DUMISG=ISG

  IF(ALLOCATED(ISG))DEALLOCATE(ISG)
  ALLOCATE(ISG(NISG))
  ISG(1:DIMISG)=DUMISG(1:DIMISG)
  IF(ALLOCATED(DUMISG))DEALLOCATE(DUMISG)

  DIMISG=NISG

 ENDIF

 END SUBROUTINE ISGMEMORYISG

 !###===============================================================================
 SUBROUTINE ISGMEMORYISD(DN,K,ICLC)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICLC
 INTEGER :: I,N

 NISD=NISD+DN

 IF(NISD.GT.DIMISD)THEN

  IF(ALLOCATED(DUMISD))DEALLOCATE(DUMISD)
  ALLOCATE(DUMISD(DIMISD))
  DUMISD=ISD

  IF(ALLOCATED(ISD))DEALLOCATE(ISD)
  ALLOCATE(ISD(NISD))
  ISD(1:DIMISD)=DUMISD(1:DIMISD)
  IF(ALLOCATED(DUMISD))DEALLOCATE(DUMISD)

  DIMISD=NISD

 ENDIF

 !## copy data to create appropriate space
 ICLC=MAX(1,ISG(K)%ICLC)
 IF(DN.GT.0)THEN
  IF(ICLC+DN.LE.NISD)ISD(ICLC+DN:NISD)=ISD(ICLC:NISD-DN)
 ELSEIF(DN.LT.0)THEN
  N=ISG(K)%NCLC
  ISD(ICLC+N+DN:NISD+DN)=ISD(ICLC+N:NISD)
 ENDIF

 ISG(K)%NCLC=ISG(K)%NCLC+DN

 !## change all citations greater than isd(iseg)%iref
 ICLC=ISG(K)%ICLC
 DO I=1,NISG
  IF(ISG(I)%ICLC.GT.ICLC)ISG(I)%ICLC=ISG(I)%ICLC+DN
 END DO

 ISG(K)%ICLC=MAX(1,ISG(K)%ICLC)
 ICLC       =ISG(K)%ICLC

 END SUBROUTINE ISGMEMORYISD

 !###===============================================================================
 SUBROUTINE ISGMEMORYISC(DN,K,ICRS)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICRS
 INTEGER :: I,N

 NISC=NISC+DN

 IF(NISC.GT.DIMISC)THEN

  IF(ALLOCATED(DUMISC))DEALLOCATE(DUMISC)
  ALLOCATE(DUMISC(DIMISC))
  DUMISC=ISC

  IF(ALLOCATED(ISC))DEALLOCATE(ISC)
  ALLOCATE(ISC(NISC))
  ISC(1:DIMISC)=DUMISC(1:DIMISC)
  IF(ALLOCATED(DUMISC))DEALLOCATE(DUMISC)

  DIMISC=NISC

 ENDIF  

 !## copy data to create appropriate space
 ICRS=MAX(1,ISG(K)%ICRS)
 IF(DN.GT.0)THEN
  IF(ICRS+DN.LE.NISC)ISC(ICRS+DN:NISC)=ISC(ICRS:NISC-DN)
 ELSEIF(DN.LT.0)THEN
  N=ISG(K)%NCRS
  ISC(ICRS+N+DN:NISC+DN)=ISC(ICRS+N:NISC)
 ENDIF
 
 ISG(K)%NCRS=ISG(K)%NCRS+DN

 !## change all citations greater than isd(iseg)%iref
 ICRS=ISG(K)%ICRS
 DO I=1,NISG
  IF(ISG(I)%ICRS.GT.ICRS)ISG(I)%ICRS=ISG(I)%ICRS+DN
 END DO

 ISG(K)%ICRS=MAX(1,ISG(K)%ICRS)
 ICRS       =ISG(K)%ICRS

 END SUBROUTINE ISGMEMORYISC

 !###===============================================================================
 SUBROUTINE ISGMEMORYIST(DN,K,ISTW)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ISTW
 INTEGER :: I,N

 NIST=NIST+DN

 IF(NIST.GT.DIMIST)THEN

  IF(ALLOCATED(DUMIST))DEALLOCATE(DUMIST)
  ALLOCATE(DUMIST(DIMIST))
  DUMIST=IST

  IF(ALLOCATED(IST))DEALLOCATE(IST)
  ALLOCATE(IST(NIST))
  IST(1:DIMIST)=DUMIST(1:DIMIST)
  IF(ALLOCATED(DUMIST))DEALLOCATE(DUMIST)

  DIMIST=NIST

 ENDIF

 !## copy data to create appropriate space
 ISTW=MAX(1,ISG(K)%ISTW)
 IF(DN.GT.0)THEN
  IF(ISTW+DN.LE.NIST)IST(ISTW+DN:NIST)=IST(ISTW:NIST-DN)
 ELSEIF(DN.LT.0)THEN
  N=ISG(K)%NSTW
  IST(ISTW+N+DN:NIST+DN)=IST(ISTW+N:NIST)
 ENDIF

 ISG(K)%NSTW=ISG(K)%NSTW+DN

 !## change all citations greater than isd(iseg)%iref
 ISTW=ISG(K)%ISTW
 DO I=1,NISG
  IF(ISG(I)%ISTW.GT.ISTW)ISG(I)%ISTW=ISG(I)%ISTW+DN
 END DO

 ISG(K)%ISTW=MAX(1,ISG(K)%ISTW)
 ISTW       =ISG(K)%ISTW

 END SUBROUTINE ISGMEMORYIST

 !###===============================================================================
 SUBROUTINE ISGMEMORYISQ(DN,K,IQHR)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: IQHR
 INTEGER :: I,N

 NISQ=NISQ+DN

 IF(NISQ.GT.DIMISQ)THEN

  IF(ALLOCATED(DUMISQ))DEALLOCATE(DUMISQ)
  ALLOCATE(DUMISQ(DIMISQ))
  DUMISQ=ISQ

  IF(ALLOCATED(ISQ))DEALLOCATE(ISQ)
  ALLOCATE(ISQ(NISQ))
  ISQ(1:DIMISQ)=DUMISQ(1:DIMISQ)
  IF(ALLOCATED(DUMISQ))DEALLOCATE(DUMISQ)

  DIMISQ=NISQ

 ENDIF

 !## copy data to create appropriate space
 IQHR=MAX(1,ISG(K)%IQHR)
 IF(DN.GT.0)THEN
  IF(IQHR+DN.LT.NISQ)ISQ(IQHR+DN:NISQ)=ISQ(IQHR:NISQ-DN)
 ELSEIF(DN.LT.0)THEN
  N=ISG(K)%NQHR
  ISQ(IQHR+N+DN:NISQ+DN)=ISQ(IQHR+N:NISQ)
 ENDIF

 ISG(K)%NQHR=ISG(K)%NQHR+DN

 !## change all citations greater than isd(iseg)%iref
 IQHR=ISG(K)%IQHR
 DO I=1,NISG
  IF(ISG(I)%IQHR.GT.IQHR)ISG(I)%IQHR=ISG(I)%IQHR+DN
 END DO

 ISG(K)%IQHR=MAX(1,ISG(K)%IQHR)
 IQHR       =ISG(K)%IQHR

 END SUBROUTINE ISGMEMORYISQ

 !###===============================================================================
 SUBROUTINE ISGMEMORYISP(DN,K,ISEG)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(OUT) :: ISEG
 INTEGER,INTENT(IN) :: DN,K
 INTEGER :: I,N

 NISP=NISP+DN

 IF(NISP.GT.DIMISP)THEN

  IF(ALLOCATED(DUMISP))DEALLOCATE(DUMISP)
  ALLOCATE(DUMISP(DIMISP))
  DUMISP=ISP

  IF(ALLOCATED(ISP))DEALLOCATE(ISP)
  ALLOCATE(ISP(NISP))
  ISP(1:DIMISP)=DUMISP(1:DIMISP)
  IF(ALLOCATED(DUMISP))DEALLOCATE(DUMISP)

  DIMISP=NISP

 ENDIF

 !## copy data to create appropriate space
 ISEG=MAX(1,ISG(K)%ISEG)
 IF(DN.GT.0)THEN
  IF(ISEG+DN.LE.NISP)ISP(ISEG+DN:NISP)=ISP(ISEG:NISP-DN)
 ELSEIF(DN.LT.0)THEN
  N=ISG(K)%NSEG
  ISP(ISEG+N+DN:NISP+DN)=ISP(ISEG+N:NISP)
 ENDIF

 ISG(K)%NSEG=ISG(K)%NSEG+DN

 !## change all citations greater than isg(k)%iseg
 ISEG=ISG(K)%ISEG
 DO I=1,NISG
  IF(ISG(I)%ISEG.GT.ISEG)ISG(I)%ISEG=ISG(I)%ISEG+DN
 END DO

 ISG(K)%ISEG=MAX(1,ISG(K)%ISEG)
 ISEG       =ISG(K)%ISEG

 END SUBROUTINE ISGMEMORYISP

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATISD(DN,K,ICLC)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICLC
 INTEGER :: I,N

 NDISD=NDISD+DN

 IF(NDISD.GT.DIMDATISD)THEN

  IF(ALLOCATED(DUMDATISD))DEALLOCATE(DUMDATISD)
  ALLOCATE(DUMDATISD(DIMDATISD))
  DUMDATISD=DATISD

  IF(ALLOCATED(DATISD))DEALLOCATE(DATISD)
  ALLOCATE(DATISD(NDISD))
  DATISD(1:DIMDATISD)=DUMDATISD(1:DIMDATISD)
  IF(ALLOCATED(DUMDATISD))DEALLOCATE(DUMDATISD)

  DIMDATISD=NDISD

 ENDIF

 !## copy data to create appropriate space
 ICLC =ISD(K)%IREF

 IF(ICLC.GT.0)THEN
  IF(DN.GT.0)THEN
   IF(ICLC+DN.LE.NDISD)DATISD(ICLC+DN:NDISD)=DATISD(ICLC:NDISD-DN)
  ELSEIF(DN.LT.0)THEN
   N=ISD(K)%N
   DATISD(ICLC+N+DN:NDISD+DN)=DATISD(ICLC+N:NDISD)
  ENDIF
 ENDIF

 ISD(K)%N=ISD(K)%N+DN

 !## change all citations greater than isd(k)%iref
 DO I=1,NISD
  IF(ISD(I)%IREF.GT.ICLC)ISD(I)%IREF=ISD(I)%IREF+DN
 END DO

 ISD(K)%IREF=MAX(1,ISD(K)%IREF)
 ICLC       =ISD(K)%IREF

 END SUBROUTINE ISGMEMORYDATISD

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATISC(DN,K,ICRS)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ICRS
 INTEGER :: I,N,J

 NDISC=NDISC+DN
 ICRS =ISC(K)%IREF

 IF(NDISC.GT.DIMDATISC)THEN

  IF(ALLOCATED(DUMDATISC))DEALLOCATE(DUMDATISC)
  ALLOCATE(DUMDATISC(DIMDATISC))
  DUMDATISC=DATISC

  IF(ALLOCATED(DATISC))DEALLOCATE(DATISC)
  ALLOCATE(DATISC(NDISC))
  DATISC(1:DIMDATISC)=DUMDATISC(1:DIMDATISC)
  IF(ALLOCATED(DUMDATISC))DEALLOCATE(DUMDATISC)

  DIMDATISC=NDISC

 ENDIF

 !## copy data to create appropriate space
 IF(ICRS.GT.0)THEN
  IF(DN.GT.0)THEN
   IF(ICRS+DN.LE.NDISC)DATISC(ICRS+DN:NDISC)=DATISC(ICRS:NDISC-DN)
  ELSEIF(DN.LT.0)THEN
   N=ABS(ISC(K)%N)
   DATISC(ICRS+N+DN:NDISC+DN)=DATISC(ICRS+N:NDISC)
  ENDIF
 ENDIF

 J=1; IF(ISC(K)%N.LT.0)J=-1
 ISC(K)%N=J*(ABS(ISC(K)%N)+DN)
 
 !## change all citations greater than isc(k)%iref
 DO I=1,NISC
  IF(ISC(I)%IREF.GT.ICRS)ISC(I)%IREF=ISC(I)%IREF+DN
 END DO

 ISC(K)%IREF=MAX(1,ISC(K)%IREF)
 ICRS       =ISC(K)%IREF

 END SUBROUTINE ISGMEMORYDATISC

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATIST(DN,K,ISTW)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: ISTW
 INTEGER :: I,N

 NDIST=NDIST+DN
 ISTW =IST(K)%IREF

 IF(NDIST.GT.DIMDATIST)THEN

  IF(ALLOCATED(DUMDATIST))DEALLOCATE(DUMDATIST)
  ALLOCATE(DUMDATIST(DIMDATIST))
  DUMDATIST=DATIST

  IF(ALLOCATED(DATIST))DEALLOCATE(DATIST)
  ALLOCATE(DATIST(NDIST))
  DATIST(1:DIMDATIST)=DUMDATIST(1:DIMDATIST)
  IF(ALLOCATED(DUMDATIST))DEALLOCATE(DUMDATIST)

  DIMDATIST=NDIST

 ENDIF

 !## copy data to create appropriate space
 IF(ISTW.GT.0)THEN
  IF(DN.GT.0)THEN
   IF(ISTW+DN.LE.NDIST)DATIST(ISTW+DN:NDIST)=DATIST(ISTW:NDIST-DN)
  ELSEIF(DN.LT.0)THEN
   N=IST(K)%N
   DATIST(ISTW+N+DN:NDIST+DN)=DATIST(ISTW+N:NDIST)
  ENDIF
 ENDIF

 IST(K)%N=IST(K)%N+DN

 !## change all citations greater than ist(k)%iref
 DO I=1,NIST
  IF(IST(I)%IREF.GT.ISTW)IST(I)%IREF=IST(I)%IREF+DN
 END DO

 IST(K)%IREF=MAX(1,IST(K)%IREF)
 ISTW       =IST(K)%IREF

 END SUBROUTINE ISGMEMORYDATIST

 !###===============================================================================
 SUBROUTINE ISGMEMORYDATISQ(DN,K,IQHR)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DN,K
 INTEGER,INTENT(OUT) :: IQHR
 INTEGER :: I,N

 NDISQ=NDISQ+DN
 IQHR =ISQ(K)%IREF

 IF(NDISQ.GT.DIMDATISQ)THEN

  IF(ALLOCATED(DUMDATISQ))DEALLOCATE(DUMDATISQ)
  ALLOCATE(DUMDATISQ(DIMDATISQ))
  DUMDATISQ=DATISQ

  IF(ALLOCATED(DATISQ))DEALLOCATE(DATISQ)
  ALLOCATE(DATISQ(NDISQ))
  DATISQ(1:DIMDATISQ)=DUMDATISQ(1:DIMDATISQ)
  IF(ALLOCATED(DUMDATISQ))DEALLOCATE(DUMDATISQ)

  DIMDATISQ=NDISQ

 ENDIF

 !## copy data to create appropriate space
 IF(IQHR.GT.0)THEN
  IF(DN.GT.0)THEN
   IF(IQHR+DN.LE.NDISQ)DATISQ(IQHR+DN:NDISQ)=DATISQ(IQHR:NDISQ-DN)
  ELSEIF(DN.LT.0)THEN
   N=ISQ(K)%N
   DATISQ(IQHR+N+DN:NDISQ+DN)=DATISQ(IQHR+N:NDISQ)
  ENDIF
 ENDIF

 ISQ(K)%N=ISQ(K)%N+DN

 !## change all citations greater than ist(k)%iref
 DO I=1,NISQ
  IF(ISQ(I)%IREF.GT.IQHR)ISQ(I)%IREF=ISQ(I)%IREF+DN
 END DO

 ISQ(K)%IREF=MAX(1,ISQ(K)%IREF)
 IQHR       =ISQ(K)%IREF

 END SUBROUTINE ISGMEMORYDATISQ

 !###===============================================================================
 SUBROUTINE ISGDEAL(IOPTION)
 !###===============================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPTION

 IF(ALLOCATED(NISGF)) DEALLOCATE(NISGF);  IF(ALLOCATED(OFFSD)) DEALLOCATE(OFFSD)
 IF(ALLOCATED(OFFSC)) DEALLOCATE(OFFSC);  IF(ALLOCATED(OFFST)) DEALLOCATE(OFFST)
 IF(ALLOCATED(OFFSQ)) DEALLOCATE(OFFSQ);  IF(ALLOCATED(NISFR)) DEALLOCATE(NISFR)
 IF(ALLOCATED(OFFISG))DEALLOCATE(OFFISG); IF(ALLOCATED(OFFISD))DEALLOCATE(OFFISD)
 IF(ALLOCATED(OFFISC))DEALLOCATE(OFFISC); IF(ALLOCATED(OFFIST))DEALLOCATE(OFFIST)
 IF(ALLOCATED(OFFISQ))DEALLOCATE(OFFISQ); IF(ALLOCATED(ISGLEG))DEALLOCATE(ISGLEG)
 
 !## only remove temporary arrays
 IF(IOPTION.EQ.0)RETURN

 IF(ALLOCATED(ISDLABELS))DEALLOCATE(ISDLABELS)
 
 IF(ALLOCATED(TISC))DEALLOCATE(TISC)
 IF(ALLOCATED(ISCN))DEALLOCATE(ISCN)
 IF(ALLOCATED(TISD))DEALLOCATE(TISD)
 IF(ALLOCATED(TIST))DEALLOCATE(TIST)
 IF(ALLOCATED(TISQ))DEALLOCATE(TISQ)

 IF(ALLOCATED(ISG)) DEALLOCATE(ISG)
 IF(ALLOCATED(ISP)) DEALLOCATE(ISP)
 IF(ALLOCATED(ISP2))DEALLOCATE(ISP2)
 IF(ALLOCATED(ISC)) DEALLOCATE(ISC)
 IF(ALLOCATED(ISD)) DEALLOCATE(ISD)
 IF(ALLOCATED(IST)) DEALLOCATE(IST)
 IF(ALLOCATED(ISQ)) DEALLOCATE(ISQ)

 IF(ALLOCATED(DUMISG))DEALLOCATE(DUMISG)
 IF(ALLOCATED(DUMISP))DEALLOCATE(DUMISP)
 IF(ALLOCATED(DUMISC))DEALLOCATE(DUMISC)
 IF(ALLOCATED(DUMISD))DEALLOCATE(DUMISD)
 IF(ALLOCATED(DUMIST))DEALLOCATE(DUMIST)
 IF(ALLOCATED(DUMISQ))DEALLOCATE(DUMISQ)

 IF(ALLOCATED(DATISD))DEALLOCATE(DATISD)
 IF(ALLOCATED(DATISC))DEALLOCATE(DATISC)
 IF(ALLOCATED(DATIST))DEALLOCATE(DATIST)
 IF(ALLOCATED(DATISQ))DEALLOCATE(DATISQ)

 IF(ALLOCATED(DATISD2))DEALLOCATE(DATISD2)
 IF(ALLOCATED(DATISC2))DEALLOCATE(DATISC2)
 IF(ALLOCATED(DATIST2))DEALLOCATE(DATIST2)
 IF(ALLOCATED(DATISQ2))DEALLOCATE(DATISQ2)

 IF(ALLOCATED(DUMDATISD))DEALLOCATE(DUMDATISD)
 IF(ALLOCATED(DUMDATISC))DEALLOCATE(DUMDATISC)
 IF(ALLOCATED(DUMDATIST))DEALLOCATE(DUMDATIST)
 IF(ALLOCATED(DUMDATISQ))DEALLOCATE(DUMDATISQ)

 IF(ALLOCATED(ISGADJ))DEALLOCATE(ISGADJ)

 IF(ALLOCATED(ISGEDITISD))DEALLOCATE(ISGEDITISD)
 IF(ALLOCATED(ISGEDITISC))DEALLOCATE(ISGEDITISC)
 IF(ALLOCATED(ISGEDITIST))DEALLOCATE(ISGEDITIST)
 IF(ALLOCATED(ISGEDITISQ))DEALLOCATE(ISGEDITISQ)

 IF(ALLOCATED(ISEGMENTS))DEALLOCATE(ISEGMENTS)
 IF(ALLOCATED(IDATES))DEALLOCATE(IDATES)
 IF(ALLOCATED(CDATES))DEALLOCATE(CDATES)
 IF(ALLOCATED(XPOL))DEALLOCATE(XPOL)
 IF(ALLOCATED(YPOL))DEALLOCATE(YPOL)

 END SUBROUTINE ISGDEAL

END MODULE MOD_ISG_UTL