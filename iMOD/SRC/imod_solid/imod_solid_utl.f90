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
MODULE MOD_SOLID_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,RTOS,UTL_READINITFILE,UTL_CAP,CLOSEUNITS,UTL_MESSAGEHANDLE,UTL_IMODFILLMENU
USE MOD_IDF, ONLY : IDFNULLIFY,IDFDEALLOCATE,IDFDEALLOCATEX
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE
USE MOD_POLYGON_PAR
USE MOD_SOLID_PAR

CONTAINS

 !###======================================================================
 FUNCTION GETSOLNAME()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=256) :: GETSOLNAME
 INTEGER :: I
 
 CALL WDIALOGSELECT(ID_DSOLIDTAB1)
 CALL WDIALOGGETMENU(IDF_MENU1,I,GETSOLNAME)
 !## read entire sol, incl. spf-files
 GETSOLNAME=TRIM(PREFVAL(1))//'\SOLIDS\'//TRIM(GETSOLNAME)//'\'//TRIM(GETSOLNAME)//'.SOL'

 END FUNCTION GETSOLNAME

 !###======================================================================
 LOGICAL FUNCTION SOLIDINITFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,IOS,IU,NSOLID,ICLR,IR,IG,IB
 LOGICAL :: LEX
 CHARACTER(LEN=256) :: LINE

 SOLIDINITFILES=.FALSE.

 CALL UTL_MESSAGEHANDLE(0)

 !## no consensus solid building
 IF(TRIM(PREFVAL(12)).EQ.'')RETURN
 INQUIRE(FILE=PREFVAL(12),EXIST=LEX)
 IF(.NOT.LEX)RETURN

! SOLIDINITFILES=.FALSE.

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=TRIM(PREFVAL(12)),ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ,DENYWRITE',IOSTAT=IOS)
 IF(IOS.NE.0)THEN
  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not open '//CHAR(13)// &
       TRIM(PREFVAL(12))//CHAR(13)//'for reading!','Error')
  RETURN
 ENDIF

 !## number of solids
 IF(.NOT.UTL_READINITFILE(UTL_CAP('NSOLID','U'),LINE,IU,0))RETURN
 READ(LINE,*) NSOLID
 !## number of solids
 IF(.NOT.UTL_READINITFILE(UTL_CAP('NBOREHOLES','U'),LINE,IU,0))RETURN
 READ(LINE,*) NBOREHOLES
 IF(ALLOCATED(FBOREHOLES))DEALLOCATE(FBOREHOLES)
 ALLOCATE(FBOREHOLES(NBOREHOLES))
 DO I=1,NBOREHOLES
  IF(.NOT.UTL_READINITFILE(UTL_CAP('FBOREHOLE'//TRIM(ITOS(I)),'U'),LINE,IU,0))RETURN
  READ(LINE,*) FBOREHOLES(I)
 ENDDO

 CALL SOLIDINITSLD(NSOLID)

 DO I=1,NSOLID

  IF(.NOT.UTL_READINITFILE(UTL_CAP('NAME_S'//TRIM(ITOS(I)),'U'),LINE,IU,0))RETURN
  READ(LINE,*) SLD(I)%SNAME
  IF(.NOT.UTL_READINITFILE(UTL_CAP('NLAY_S'//TRIM(ITOS(I)),'U'),LINE,IU,0))RETURN
  READ(LINE,*) SLD(I)%NLAY
  CALL SOLIDINITSLDPOINTER(I,SLD(I)%NLAY)
  ICLR=0
  DO K=1,2
   DO J=1,SLD(I)%NLAY
    IF(K.EQ.1)THEN
     IF(.NOT.UTL_READINITFILE(UTL_CAP('TOP_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),'U'),LINE,IU,0))RETURN
    ELSE
     IF(.NOT.UTL_READINITFILE(UTL_CAP('BOT_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),'U'),LINE,IU,0))RETURN
    ENDIF
    READ(LINE,*,IOSTAT=IOS) SLD(I)%TBNAME(J,K),IR,IG,IB
    IF(IOS.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading line:'//CHAR(13)//TRIM(LINE),'Error'); CLOSE(IU); RETURN
    ENDIF
    SLD(I)%TBCLR(J,K)=WRGB(IR,IG,IB)
   ENDDO
  ENDDO
  DO J=1,SLD(I)%NLAY  
   !## optional kd-values
   SLD(I)%KDCLR(J)=0
   IF(UTL_READINITFILE(UTL_CAP('KD_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),'U'),LINE,IU,1))THEN
    READ(LINE,*,IOSTAT=IOS) SLD(I)%KDNAME(J),IR,IG,IB
    IF(IOS.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading line:'//CHAR(13)//TRIM(LINE),'Error'); CLOSE(IU); RETURN
    ENDIF
    SLD(I)%KDCLR(J)=WRGB(IR,IG,IB)
   ENDIF
  ENDDO
  DO J=1,SLD(I)%NLAY-1
   !## optional c-values
   SLD(I)%CCLR(J)=0
   IF(UTL_READINITFILE(UTL_CAP('C_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),'U'),LINE,IU,1))THEN
    READ(LINE,*,IOSTAT=IOS) SLD(I)%CNAME(J),IR,IG,IB
    IF(IOS.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading line:'//CHAR(13)//TRIM(LINE),'Error'); CLOSE(IU); RETURN
    ENDIF
    SLD(I)%CCLR(J)=WRGB(IR,IG,IB)
   ENDIF
  ENDDO
  CALL WINDOWOUTSTATUSBAR(4,'Reading '//TRIM(PREFVAL(12))//'['//TRIM(SLD(I)%SNAME)//'], progress '// &
           TRIM(RTOS(REAL(I*100)/REAL(NSOLID),'F',2))//' %')
 END DO

 CLOSE(IU)

 SOLIDINITFILES=.TRUE.

 END FUNCTION SOLIDINITFILES

 !###======================================================================
 LOGICAL FUNCTION SOLIDOPENSOL(RW,SOLNAME,IQ,TXT)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RW,SOLNAME
 INTEGER,INTENT(IN),OPTIONAL :: IQ
 CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: TXT
 INTEGER :: IOS,I,J,K,IU,N,IR,IG,IB
 CHARACTER(LEN=256) :: LINE,DIR

 I=0; IF(PRESENT(IQ))I=IQ

 IF(I.EQ.1)THEN
  SOLIDOPENSOL=.TRUE.
  LINE='>>> add keyword <<<'
  IF(PRESENT(TXT))LINE=TXT
  !## nothing to save/adjust
  CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Do you want to save the (adjusted/added) '//TRIM(LINE)// &
   ' to the Solid Project?','Question')
  IF(WINFODIALOG(4).NE.1)RETURN
 ENDIF

 SOLIDOPENSOL=.FALSE.

 IU=UTL_GETUNIT()
 IF(RW.EQ.'R'.OR.RW.EQ.'r')THEN
  CALL OSD_OPEN(IU,FILE=SOLNAME,ACTION='READ' ,FORM='FORMATTED',STATUS='OLD'    ,IOSTAT=IOS)
 ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN
  CALL OSD_OPEN(IU,FILE=SOLNAME,ACTION='WRITE',FORM='FORMATTED',STATUS='UNKNOWN',IOSTAT=IOS)
 ENDIF

 IF(IOS.NE.0)THEN
  IF(RW.EQ.'R'.OR.RW.EQ.'r')THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open *.sol for reading.'//CHAR(13)// &
     '['//TRIM(SOLNAME)//']','Warning')
  ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open *.sol for writing.'//CHAR(13)// &
     '['//TRIM(SOLNAME)//']','Warning')
  ENDIF
  RETURN
 ENDIF

 DIR=SOLNAME(:INDEX(SOLNAME,'\',.TRUE.)-1)

 IF(RW.EQ.'R'.OR.RW.EQ.'r')THEN

  NMASK=0
  NBOREHOLES=0
  NSPF=0

  !## clean solid
  CALL SOLID_DEALLOCATE()

  !## clean polygon
  SHPNO=0

  !## number of solids
  IF(.NOT.UTL_READINITFILE('NSOLID',LINE,IU,0))RETURN
  READ(LINE,*) N
  IF(N.LE.0)THEN
   CLOSE(IU)
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading NSOLIDS (<= 0)','Error')
   RETURN
  ENDIF

  ALLOCATE(JSEL_SOLID(N))
  JSEL_SOLID=1      !## support models
  JSEL_SOLID(1)=-1  !## source  model

  CALL SOLIDINITSLD(N)

  DO I=1,N

   IF(.NOT.UTL_READINITFILE(UTL_CAP('NAME_S'//TRIM(ITOS(I)),'U'),LINE,IU,0))RETURN
   READ(LINE,*) SLD(I)%SNAME

   IF(.NOT.UTL_READINITFILE('NLAY_S'//TRIM(ITOS(I)),LINE,IU,0))RETURN
   !## number of modellayers
   READ(LINE,*) SLD(I)%NLAY

   CALL SOLIDINITSLDPOINTER(I,SLD(I)%NLAY)

   SLD(I)%KDCLR=0
   SLD(I)%CCLR =0
   !## try to read all idf's
   DO K=1,2
    DO J=1,SLD(I)%NLAY
     IF(K.EQ.1)THEN
      IF(.NOT.UTL_READINITFILE('TOP_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),LINE,IU,0))RETURN
     ELSE
      IF(.NOT.UTL_READINITFILE('BOT_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),LINE,IU,0))RETURN
     ENDIF
     IF(I.EQ.1)THEN
      READ(LINE,*,IOSTAT=IOS) SLD(I)%TBNAME(J,K),IR,IG,IB,SLD(I)%ICLC(J,K),SLD(I)%ICHECK(J,K) !,SLD(I)%ACC(J,K)
      IF(IOS.NE.0)THEN
       SLD(I)%ICHECK(J,K)=1
       READ(LINE,*,IOSTAT=IOS) SLD(I)%TBNAME(J,K),IR,IG,IB,SLD(I)%ICLC(J,K) !,SLD(I)%ACC(J,K)
      ENDIF
     ENDIF
     IF(I.GT.1)READ(LINE,*,IOSTAT=IOS) SLD(I)%TBNAME(J,K),IR,IG,IB
     IF(IOS.NE.0)THEN
      CLOSE(IU)
      IF(K.EQ.1)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading TOP_S'//TRIM(ITOS(I))// &
                '_L'//TRIM(ITOS(J))//' for solid '// &
          TRIM(ITOS(I)),'Error')
      IF(K.EQ.2)CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading BOT_S'//TRIM(ITOS(I))// &
                '_L'//TRIM(ITOS(J))//' for solid '// &
          TRIM(ITOS(I)),'Error')
      RETURN
     ENDIF
     SLD(I)%TBCLR(J,K)=WRGB(IR,IG,IB)
     IF(I.EQ.1)SLD(I)%TBNAME(J,K)=TRIM(DIR)//'\'//TRIM(SLD(I)%TBNAME(J,K))
    ENDDO
   ENDDO
   DO J=1,SLD(I)%NLAY 
    IF(UTL_READINITFILE('KD_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),LINE,IU,1))THEN
     READ(LINE,*,IOSTAT=IOS) SLD(I)%KDNAME(J),IR,IG,IB
     SLD(I)%KDCLR(J)=WRGB(IR,IG,IB)
     SLD(I)%KDNAME(J)=TRIM(DIR)//'\'//TRIM(SLD(I)%KDNAME(J))
    ENDIF
   ENDDO
   DO J=1,SLD(I)%NLAY
    IF(UTL_READINITFILE('C_S'//TRIM(ITOS(I))//'_L'//TRIM(ITOS(J)),LINE,IU,1))THEN
     READ(LINE,*,IOSTAT=IOS) SLD(I)%CNAME(J),IR,IG,IB
     SLD(I)%CCLR(J)=WRGB(IR,IG,IB)
     SLD(I)%CNAME(J)=TRIM(DIR)//'\'//TRIM(SLD(I)%CNAME(J))
    ENDIF
   ENDDO

  ENDDO

!  !## see whether a new version is active
!  CALL SOLIDOPENSOL_GETVERSION(DIR)

  !## number of idf in solid is number of first solid in sol file
  NTBSOL=SLD(1)%NLAY*2

  !## number of cross-sections
  IF(UTL_READINITFILE('NCROSS',LINE,IU,1))READ(LINE,*) NSPF

  !## allocate memory for cross-sections: maximum = 100?
  CALL SOLIDINITSPF(MAX(200,NSPF))

  !## read cross-section information
  DO I=1,NSPF
   IF(.NOT.UTL_READINITFILE('FCROSS'//TRIM(ITOS(I)),LINE,IU,0))RETURN
   READ(LINE,*) SPF(I)%FNAME
   IF(.NOT.SOLIDOPENSPF(I,'R',DIR))THEN
    CLOSE(IU)
    RETURN
   ENDIF
  END DO

  !## number of masks to be used for the interpolation
  IF(UTL_READINITFILE('NMASK',LINE,IU,1))THEN
   READ(LINE,*) NMASK
   IF(NMASK.GT.0)THEN
    ALLOCATE(MASK(NMASK))
    DO I=1,SIZE(MASK); CALL IDFNULLIFY(MASK(I)%IDF); ENDDO
    DO I=1,NMASK
     IF(.NOT.UTL_READINITFILE('FMASK'//TRIM(ITOS(I)),LINE,IU,0))RETURN
     READ(LINE,*,IOSTAT=IOS) MASK(I)%ALIAS
     IF(IOS.NE.0)THEN
      CLOSE(IU)
      CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading FMASK'//TRIM(ITOS(I)),'Error')
      RETURN
     ENDIF
     MASK(I)%FNAME=TRIM(DIR)//'\'//TRIM(MASK(I)%ALIAS)
    ENDDO
   ENDIF
  ENDIF

  !## number of shapes to be used for the interpolation
  IF(UTL_READINITFILE('GENFILE',LINE,IU,1))THEN
   READ(LINE,*) FNAME
   CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,0,TRIM(DIR)//'\'//TRIM(FNAME))
  ENDIF

  !## number of boreholes data
  IF(UTL_READINITFILE('NBOREHOLES',LINE,IU,1))READ(LINE,*) NBOREHOLES
  IF(NBOREHOLES.GT.0)THEN
   ALLOCATE(FBOREHOLES(NBOREHOLES))
   DO I=1,NBOREHOLES
    IF(.NOT.UTL_READINITFILE('FBOREHOLE'//TRIM(ITOS(I)),LINE,IU,0))RETURN
    READ(LINE,*) FBOREHOLES(I)
    FBOREHOLES(I)=TRIM(DIR)//'\'//TRIM(FBOREHOLES(I))
   ENDDO
  ENDIF

  !## write legend(s)
  KLEGEND=TRIM(PREFVAL(1))//'\TMP\KLEGEND.LEG'
  CALL SOLIDLEGWRITE()

 ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN

  !## number of solids
  NTBSOL=SUM(ABS(JSEL_SOLID))

  !## write number of solids
  LINE=TRIM(ITOS(NTBSOL))
  WRITE(IU,'(A)',IOSTAT=IOS) 'NSOLID='//TRIM(LINE)

  !## write source  model
  DO I=1,SIZE(SLD)
   IF(JSEL_SOLID(I).EQ.-1)CALL SOLIDOPENSOL_WRITESOLID(IU,I,1)
  ENDDO

  !## write support models
  J=1
  DO I=1,SIZE(SLD)
   IF(JSEL_SOLID(I).EQ.1)THEN
    J=J+1
    CALL SOLIDOPENSOL_WRITESOLID(IU,I,J)
   ENDIF
  ENDDO

  IF(NSPF.GT.0)THEN
   WRITE(IU,*)
   LINE=TRIM(ITOS(NSPF))
   WRITE(IU,'(A)',IOSTAT=IOS) 'NCROSS='//TRIM(LINE)
   NTBSOL=SLD(1)%NLAY*2
   !## write cross-section information
   DO I=1,NSPF
    LINE='FCROSS'//TRIM(ITOS(I))//'='
    WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)//'"'//TRIM(SPF(I)%FNAME)//'"'
    IF(.NOT.SOLIDOPENSPF(I,'W',DIR))RETURN
   END DO
  ENDIF

  IF(NMASK.GT.0)THEN
   WRITE(IU,*)
   LINE='NMASK='//TRIM(ITOS(NMASK))
   WRITE(IU,'(A)') TRIM(LINE)
    DO I=1,NMASK
     LINE='FMASK'//TRIM(ITOS(I))//'="'//TRIM(MASK(I)%ALIAS)//'"' !//','//TRIM(ITOS(MASK(I)%ILAY))
     WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
    ENDDO
   ENDIF

  !## number of shapes to be used for the interpolation
  IF(SHPNO.GT.0)THEN
   WRITE(IU,*)
   WRITE(IU,'(A)',IOSTAT=IOS) 'GENFILE="SHAPES\SHAPE.GEN"'
   CALL POLYGON1SAVELOADSHAPE(ID_SAVESHAPE,0,TRIM(DIR)//'\SHAPES\SHAPE.GEN')
  ENDIF

  !## number of consensus data
  IF(NBOREHOLES.GT.0)THEN
   WRITE(IU,*)
   LINE=TRIM(ITOS(NBOREHOLES))
   WRITE(IU,'(A)',IOSTAT=IOS) 'NBOREHOLES='//TRIM(LINE)
   DO I=1,NBOREHOLES
    LINE='FBOREHOLE'//TRIM(ITOS(I))//'='
!    WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)//'"boreholes\'//TRIM(FBOREHOLES(I)(INDEX(FBOREHOLES(I),'\',.TRUE.)+1:))//'"'
    J=LEN_TRIM(DIR)+2
!    WRITE(*,*) LEN_TRIM(DIR)+1,J,DIR(J:J) 
    LINE=TRIM(LINE)//'"'//TRIM(FBOREHOLES(I)(J:))//'"'
    WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE) !//'"'//TRIM(FBOREHOLES(I)(LEN_TRIM(DIR)+1:))//'"'
   ENDDO
  ENDIF

 ENDIF

 CLOSE(IU)
 SOLIDOPENSOL=.TRUE.

 END FUNCTION SOLIDOPENSOL

 !###====================================================================
 SUBROUTINE SOLIDOPENSOL_GETVERSION(DIR,IDD,IDM)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: DIR
 INTEGER,INTENT(IN) :: IDD,IDM
 INTEGER :: N

 CALL WDIALOGSELECT(IDD)
 CALL UTL_IMODFILLMENU(IDM,DIR(:INDEX(DIR,'\',.TRUE.)-1),'VERSION[*','D',N,0,0)
 IF(N.GT.0)RETURN

 CALL WDIALOGFIELDSTATE(IDF_MENU1,1)
 CALL WDIALOGPUTMENU(IDF_MENU1,(/'1'/),1,1)

 END SUBROUTINE SOLIDOPENSOL_GETVERSION

 !###====================================================================
 SUBROUTINE SOLIDOPENSOL_WRITESOLID(IU,IS,JS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IS,JS
 CHARACTER(LEN=256) :: LINE
 INTEGER :: I,J,K,IOS,N,IR,IG,IB

 WRITE(IU,*) 
 
 !## write
 LINE='NAME_S'//TRIM(ITOS(JS))//'="'//TRIM(SLD(IS)%SNAME)//'"'
 WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
 LINE='NLAY_S'//TRIM(ITOS(JS))//'='//TRIM(ITOS(SLD(IS)%NLAY))
 WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)

 DO K=1,2
  DO J=1,SLD(IS)%NLAY
   !## source solid --- only local path!
   IF(JSEL_SOLID(IS).EQ.-1)THEN
    I=INDEX(SLD(IS)%TBNAME(J,K),'\',.TRUE.)+1
    LINE=SLD(IS)%TBNAME(J,K)(I:)
    IF(K.EQ.1)LINE='TOP_S'//TRIM(ITOS(JS))//'_L'//TRIM(ITOS(J))//'="'//TRIM(LINE)//'"'
    IF(K.EQ.2)LINE='BOT_S'//TRIM(ITOS(JS))//'_L'//TRIM(ITOS(J))//'="'//TRIM(LINE)//'"'
    CALL WRGBSPLIT(SLD(IS)%TBCLR(J,K),IR,IG,IB)
    LINE=TRIM(LINE)//','//TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','//TRIM(ITOS(IB))//','// &
         TRIM(ITOS(SLD(IS)%ICLC(J,K)))//','//TRIM(ITOS(SLD(IS)%ICHECK(J,K))) 
   ELSE
    IF(K.EQ.1)LINE='TOP_S'//TRIM(ITOS(JS))//'_L'//TRIM(ITOS(J))//'="'//TRIM(SLD(IS)%TBNAME(J,K))//'"'
    IF(K.EQ.2)LINE='BOT_S'//TRIM(ITOS(JS))//'_L'//TRIM(ITOS(J))//'="'//TRIM(SLD(IS)%TBNAME(J,K))//'"'
    CALL WRGBSPLIT(SLD(IS)%TBCLR(J,K),IR,IG,IB)
    LINE=TRIM(LINE)//','//TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','//TRIM(ITOS(IB))
   ENDIF
   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  END DO
 ENDDO

 DO J=1,SLD(IS)%NLAY
  IF(SLD(IS)%KDCLR(J).NE.0)THEN
   I=INDEX(SLD(IS)%KDNAME(J),'\',.TRUE.)+1
   LINE=SLD(IS)%KDNAME(J)(I:)
   CALL WRGBSPLIT(SLD(IS)%KDCLR(J),IR,IG,IB)
   LINE='KD_S'//TRIM(ITOS(JS))//'_L'//TRIM(ITOS(J))//'="'//TRIM(LINE)//'",'// &
        TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','//TRIM(ITOS(IB))
   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  ENDIF
 ENDDO
 DO J=1,SLD(IS)%NLAY-1
  IF(SLD(IS)%CCLR(J).NE.0)THEN
   I=INDEX(SLD(IS)%CNAME(J),'\',.TRUE.)+1
   LINE=SLD(IS)%CNAME(J)(I:)
   CALL WRGBSPLIT(SLD(IS)%CCLR(J),IR,IG,IB)
   LINE='C_S'//TRIM(ITOS(JS))//'_L'//TRIM(ITOS(J))//'="'//TRIM(LINE)//'",'// &
       TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','//TRIM(ITOS(IB))
   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  ENDIF
 END DO

 END SUBROUTINE SOLIDOPENSOL_WRITESOLID

 !###====================================================================
 LOGICAL FUNCTION SOLIDOPENSPF(ISPF,RW,DIR)
 !###====================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RW,DIR
 INTEGER,INTENT(IN) :: ISPF
 INTEGER :: IU,IOS,I,IL
 CHARACTER(LEN=256) :: LINE
 REAL :: DX,DY,DXY

 SOLIDOPENSPF=.FALSE.

 !## read spf-file, put in object spf()%
 IU=UTL_GETUNIT()
 IF(RW.EQ.'R'.OR.RW.EQ.'r')THEN
  CALL OSD_OPEN(IU,FILE=TRIM(DIR)//'\'//TRIM(SPF(ISPF)%FNAME),ACTION='READ', &
       FORM='FORMATTED',IOSTAT=IOS,STATUS='OLD')
 ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN
  CALL OSD_OPEN(IU,FILE=TRIM(DIR)//'\'//TRIM(SPF(ISPF)%FNAME),ACTION='WRITE', &
       FORM='FORMATTED',IOSTAT=IOS,STATUS='UNKNOWN')
 ENDIF
 IF(IOS.NE.0)THEN
  IF(RW.EQ.'R'.OR.RW.EQ.'r')THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open Solid Cross-Section File (*.scf) for reading.'//CHAR(13)// &
    '['//TRIM(DIR)//'\'//TRIM(SPF(ISPF)%FNAME)//']','Warning')
  ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open Solid Cross-Section File (*.scf) for writing.'//CHAR(13)// &
    '['//TRIM(DIR)//'\'//TRIM(SPF(ISPF)%FNAME)//']','Warning')
  ENDIF
  RETURN
 ENDIF

 !## read spf-file

 IF(RW.EQ.'R'.OR.RW.EQ.'r')THEN

  !## number of coordinates
  READ(IU,*) SPF(ISPF)%NXY
  ALLOCATE(SPF(ISPF)%X(SPF(ISPF)%NXY),SPF(ISPF)%Y(SPF(ISPF)%NXY))
  !## read coordinates
  DO I=1,SPF(ISPF)%NXY; READ(IU,*) SPF(ISPF)%X(I),SPF(ISPF)%Y(I); ENDDO
  !##read length of cross-section
  READ(IU,*) SPF(ISPF)%TX
  !## recompute tx, could be inaccurat or edited?
  SPF(ISPF)%TX=0.0
  DO I=2,SPF(ISPF)%NXY
   DX=SPF(ISPF)%X(I)-SPF(ISPF)%X(I-1); DY=SPF(ISPF)%Y(I)-SPF(ISPF)%Y(I-1); DXY=0.0
   IF(DX.NE.0.0.OR.DY.NE.0.0)DXY=SQRT(DX**2.0+DY**2.0)
   SPF(ISPF)%TX=SPF(ISPF)%TX+DXY
  ENDDO
  !## number of layers to interpolate (NTBSOL)
  NULLIFY(SPF(ISPF)%PROF)
  ALLOCATE(SPF(ISPF)%PROF(NTBSOL))
  DO IL=1,NTBSOL
   !## default colours (red)
   SPF(ISPF)%PROF(IL)%ICLR=WRGB(255,0,0)
   !## default linewidth=1
   SPF(ISPF)%PROF(IL)%IWIDTH=1
   !## read number of position/ilay for il-th elevation
   READ(IU,'(A)') LINE
   READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS,SPF(ISPF)%PROF(IL)%ICLR,SPF(ISPF)%PROF(IL)%IWIDTH
   IF(IOS.NE.0)THEN
    READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS,SPF(ISPF)%PROF(IL)%ICLR
    IF(IOS.NE.0)READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS
   ENDIF
   NULLIFY(SPF(ISPF)%PROF(IL)%PX,SPF(ISPF)%PROF(IL)%PZ)
   ALLOCATE(SPF(ISPF)%PROF(IL)%PX(MXPX),SPF(ISPF)%PROF(IL)%PZ(MXPX))
   !## read all dist/zvalue on the segments
   DO I=1,SPF(ISPF)%PROF(IL)%NPOS
    READ(IU,*) SPF(ISPF)%PROF(IL)%PX(I),SPF(ISPF)%PROF(IL)%PZ(I)
   ENDDO
  ENDDO

 !## write spf-file (correct for lines that are below/beyond the extent of the profile)

 ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN

  !## number of coordinates
  LINE=TRIM(ITOS(SPF(ISPF)%NXY))
  WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  !## read coordinates
  DO I=1,SPF(ISPF)%NXY
   LINE=TRIM(RTOS(SPF(ISPF)%X(I),'F',2))//','//TRIM(RTOS(SPF(ISPF)%Y(I),'F',2))
   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  ENDDO
  LINE=TRIM(RTOS(SPF(ISPF)%TX,'F',2))//' !## length of the cross-section'
  WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  !## number of layers to interpolate
  DO IL=1,NTBSOL
   LINE=TRIM(ITOS(SPF(ISPF)%PROF(IL)%NPOS))//','// &
        TRIM(ITOS(SPF(ISPF)%PROF(IL)%ICLR))//','// &
        TRIM(ITOS(SPF(ISPF)%PROF(IL)%IWIDTH))
   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
   !## read all dist/zvalue on the segments
   DO I=1,SPF(ISPF)%PROF(IL)%NPOS
    LINE=TRIM(RTOS(SPF(ISPF)%PROF(IL)%PX(I),'F',2))//','//TRIM(RTOS(SPF(ISPF)%PROF(IL)%PZ(I),'F',2))
    WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
   ENDDO
  ENDDO

 ENDIF
 CLOSE(IU)

 SOLIDOPENSPF=.TRUE.

 END FUNCTION SOLIDOPENSPF

 !###====================================================================
 SUBROUTINE SOLIDINITSLD(N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I

 CALL SOLIDDEALLOCATESLD()
 !## nothing to allocate - return
 IF(N.EQ.0)RETURN

 ALLOCATE(SLD(N))
 DO I=1,SIZE(SLD)
  NULLIFY(SLD(I)%TBNAME)
  NULLIFY(SLD(I)%TBCLR)
  NULLIFY(SLD(I)%ICLC)
!  NULLIFY(SLD(I)%ACC)
  NULLIFY(SLD(I)%KDNAME)
  NULLIFY(SLD(I)%CNAME)
  NULLIFY(SLD(I)%KDCLR)
  NULLIFY(SLD(I)%CCLR)
 END DO

 END SUBROUTINE SOLIDINITSLD

 !###====================================================================
 SUBROUTINE SOLIDINITSLDPOINTER(I,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,I

 ALLOCATE(SLD(I)%TBNAME(N,2))
 ALLOCATE(SLD(I)%TBCLR (N,2))
 ALLOCATE(SLD(I)%ICLC  (N,2))
 ALLOCATE(SLD(I)%ICHECK(N,2))
 ALLOCATE(SLD(I)%KDNAME(N))
 ALLOCATE(SLD(I)%CNAME (N-1))
 ALLOCATE(SLD(I)%KDCLR (N))
 ALLOCATE(SLD(I)%CCLR  (N-1))
 SLD(I)%KDNAME =''
 SLD(I)%CNAME =''
 SLD(I)%KDCLR =0
 SLD(I)%CCLR  =0
 SLD(I)%ICLC  =0
 SLD(I)%ICHECK=1

 END SUBROUTINE SOLIDINITSLDPOINTER

 !###====================================================================
 SUBROUTINE SOLIDINITSPF(N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I

 CALL SOLIDDEALLOCATESPF()
 !## nothing to allocate - return
 IF(N.EQ.0)RETURN

 ALLOCATE(SPF(N))
 DO I=1,SIZE(SPF)
  NULLIFY(SPF(I)%X)
  NULLIFY(SPF(I)%Y)
  NULLIFY(SPF(I)%PROF)
 END DO

 END SUBROUTINE SOLIDINITSPF

 !###======================================================================
 SUBROUTINE SOLID_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(DZ))DEALLOCATE(DZ)
 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
 IF(ALLOCATED(JSEL_SOLID))DEALLOCATE(JSEL_SOLID)
 IF(ALLOCATED(FBOREHOLES))DEALLOCATE(FBOREHOLES)
 IF(ALLOCATED(MASK))THEN
  DO I=1,SIZE(MASK); CALL IDFDEALLOCATEX(MASK(I)%IDF); ENDDO
  DEALLOCATE(MASK)
 ENDIF
 IF(ALLOCATED(SOLIDF))THEN
  CALL IDFDEALLOCATE(SOLIDF,SIZE(SOLIDF))
  DEALLOCATE(SOLIDF)
 ENDIF
 IF(ALLOCATED(TB))THEN
  CALL IDFDEALLOCATE(TB,SIZE(TB))
  DEALLOCATE(TB)
 ENDIF
 CALL IDFDEALLOCATEX(MDLIDF)
 CALL SOLIDDEALLOCATESLD()
 CALL SOLIDDEALLOCATESPF()

 END SUBROUTINE SOLID_DEALLOCATE

 !###====================================================================
 SUBROUTINE SOLIDDEALLOCATESLD()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(.NOT.ALLOCATED(SLD))RETURN

 DO I=1,SIZE(SLD)
  IF(ASSOCIATED(SLD(I)%TBNAME))DEALLOCATE(SLD(I)%TBNAME)
  IF(ASSOCIATED(SLD(I)%KDNAME))DEALLOCATE(SLD(I)%KDNAME)
  IF(ASSOCIATED(SLD(I)%CNAME)) DEALLOCATE(SLD(I)%CNAME)
  IF(ASSOCIATED(SLD(I)%TBCLR)) DEALLOCATE(SLD(I)%TBCLR)
  IF(ASSOCIATED(SLD(I)%KDCLR)) DEALLOCATE(SLD(I)%KDCLR)
  IF(ASSOCIATED(SLD(I)%CCLR))  DEALLOCATE(SLD(I)%CCLR)
  IF(ASSOCIATED(SLD(I)%ICLC))  DEALLOCATE(SLD(I)%ICLC)
!  IF(ASSOCIATED(SLD(I)%ACC))   DEALLOCATE(SLD(I)%ACC)
 END DO
 DEALLOCATE(SLD)

 END SUBROUTINE SOLIDDEALLOCATESLD

 !###====================================================================
 SUBROUTINE SOLIDDEALLOCATESPF()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 IF(.NOT.ALLOCATED(SPF))RETURN

 DO I=1,NSPF
  IF(ASSOCIATED(SPF(I)%X))DEALLOCATE(SPF(I)%X)
  IF(ASSOCIATED(SPF(I)%Y))DEALLOCATE(SPF(I)%Y)
  DO J=1,SIZE(SPF(I)%PROF)
   IF(SPF(I)%PROF(J)%NPOS.GT.0)THEN
    IF(ASSOCIATED(SPF(I)%PROF(J)%PX))DEALLOCATE(SPF(I)%PROF(J)%PX)
    IF(ASSOCIATED(SPF(I)%PROF(J)%PZ))DEALLOCATE(SPF(I)%PROF(J)%PZ)
   ENDIF
  ENDDO
  IF(ASSOCIATED(SPF(I)%PROF))DEALLOCATE(SPF(I)%PROF)
 END DO
 DEALLOCATE(SPF)
 NSPF=0

 END SUBROUTINE SOLIDDEALLOCATESPF

 !###======================================================================
 SUBROUTINE SOLIDLEGWRITE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IU,IOS

 IU=UTL_GETUNIT()
 CALL OSD_OPEN(IU,FILE=KLEGEND,IOSTAT=IOS)
 IF(IOS.EQ.0) THEN
  WRITE(IU,'(A)') '16,1,1,1,1,1,1,1'
  WRITE(IU,'(A)') 'UPPERBND,LOWERBND,IRED,IGREEN,IBLUE,DOMAIN'
  WRITE(IU,'(A)') '500.0000,100.0000,173,0,0,">=100. - <= 500."'
  WRITE(IU,'(A)') '100.0000,50.00000,255,0,42,">=50. - <  100."'
  WRITE(IU,'(A)') '50.00000,25.00000,255,0,194,">=25. - <  50."'
  WRITE(IU,'(A)') '25.00000,20.00000,164,0,255,">=20. - <  25."'
  WRITE(IU,'(A)') '20.00000,15.00000,13,0,255,">=15. - <  20."'
  WRITE(IU,'(A)') '15.00000,10.00000,0,142,113,">=10. - <  15."'
  WRITE(IU,'(A)') '10.00000,5.000000,37,254,1,">=5. - <  10."'
  WRITE(IU,'(A)') '5.000000,2.500000,198,254,1,">=2.5 - <  5."'
  WRITE(IU,'(A)') '2.500000,1.000000,208,255,92,">=1. - <  2.5"'
  WRITE(IU,'(A)') '1.000000,0.5000000,206,255,94,">=0.5 - <  1."'
  WRITE(IU,'(A)') '0.5000000,0.1000000,205,255,97,">=0.1 - <  0.5"'
  WRITE(IU,'(A)') '0.1000000,9.9999998E-03,203,255,102,">=0.1000E-01 - <  0.1"'
  WRITE(IU,'(A)') '9.9999998E-03,1.0000000E-03,198,255,112,">=0.1000E-02 - <  0.1000E-01"'
  WRITE(IU,'(A)') '1.0000000E-03,9.9999997E-05,188,255,132,">=0.1000E-03 - <  0.1000E-02"'
  WRITE(IU,'(A)') '9.9999997E-05,9.9999997E-06,168,255,173,">=0.1000E-04 - <  0.1000E-03"'
  WRITE(IU,'(A)') '9.9999997E-06,0.0000000E+00,128,255,254,">=8.320709 - <  0.1000E-04"'
  CLOSE(IU)
 ENDIF

 END SUBROUTINE SOLIDLEGWRITE

END MODULE MOD_SOLID_UTL

