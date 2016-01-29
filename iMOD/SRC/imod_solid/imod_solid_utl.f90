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
MODULE MOD_SOLID_UTL

USE WINTERACTER
USE RESOURCE
USE MOD_COLOURS
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,RTOS,UTL_READINITFILE,UTL_CAP,UTL_CLOSEUNITS,UTL_MESSAGEHANDLE,UTL_IMODFILLMENU
USE MOD_IDF, ONLY : IDFNULLIFY,IDFDEALLOCATE,IDFDEALLOCATEX
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE
USE MOD_POLYGON_PAR
USE MOD_PROF_PAR, ONLY : PBITMAP
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
  NSPF=0
  NGEN=0
  
  !## clean solid
  CALL SOLID_DEALLOCATE()

  !## clean polygon
  SHPNO=0

  CALL SOLID_INITSLD(1)

  DO I=1,SIZE(SLD)

   SLD(I)%SNAME=SOLNAME(INDEX(SOLNAME,'\',.TRUE.)+1:INDEX(SOLNAME,'.',.TRUE.)-1)
   
   IF(.NOT.UTL_READINITFILE('NINT',LINE,IU,0))RETURN
   !## number of modellayers (interfaces)
   READ(LINE,*) SLD(I)%NINT

   !## number of cross-sections
   IF(UTL_READINITFILE('NCROSS',LINE,IU,1))READ(LINE,*) NSPF

   CALL SOLID_INITSLDPOINTER(I,SLD(I)%NINT)

   !## try to read all idf's
   DO J=1,SLD(I)%NINT
    IF(.NOT.UTL_READINITFILE('INT_L'//TRIM(ITOS(J)),LINE,IU,0))RETURN
    IF(I.EQ.1)THEN
     READ(LINE,*,IOSTAT=IOS) SLD(I)%INTNAME(J),IR,IG,IB,SLD(I)%ICLC(J),SLD(I)%ICHECK(J),SLD(I)%XRESOLUTION(J) 
     IF(IOS.NE.0)THEN
      !## initiate with null, convert to dx from idf later on
      SLD(I)%XRESOLUTION(J)=0.0
      READ(LINE,*,IOSTAT=IOS) SLD(I)%INTNAME(J),IR,IG,IB,SLD(I)%ICLC(J),SLD(I)%ICHECK(J)
      IF(IOS.NE.0)THEN
       SLD(I)%ICHECK(J)=1
       READ(LINE,*,IOSTAT=IOS) SLD(I)%INTNAME(J),IR,IG,IB,SLD(I)%ICLC(J) 
      ENDIF
     ENDIF
    ENDIF
    IF(I.GT.1)READ(LINE,*,IOSTAT=IOS) SLD(I)%INTNAME(J),IR,IG,IB
    IF(IOS.NE.0)THEN
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading INT_L'//TRIM(ITOS(J))//' for solid '//TRIM(ITOS(I)),'Error')
     CLOSE(IU); RETURN
    ENDIF
    SLD(I)%INTCLR(J)=WRGB(IR,IG,IB)
    IF(I.EQ.1)SLD(I)%INTNAME(J)=TRIM(DIR)//'\'//TRIM(SLD(I)%INTNAME(J))
   ENDDO

  ENDDO

  !## number of idf in solid is number of first solid in sol file
  NTBSOL=SLD(1)%NINT

  !## allocate memory for cross-sections: maximum = 200
  CALL SOLID_INITSPF(MAX(200,NSPF))

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

  IF(UTL_READINITFILE('NGEN',LINE,IU,1))READ(LINE,*) NGEN
  IF(NGEN.GT.0)THEN
   IF(ALLOCATED(GEN))DEALLOCATE(GEN); ALLOCATE(GEN(NGEN))
   DO J=1,NGEN
    IF(.NOT.UTL_READINITFILE('GEN_'//TRIM(ITOS(J)),LINE,IU,0))RETURN; READ(LINE,*) GEN(J)%ILAY,GEN(J)%FNAME
   ENDDO
  ENDIF

  ALLOCATE(KSETTINGS(SLD(1)%NINT))

  KSETTINGS%RANGE=5000.0
  KSETTINGS%SILL=30.0
  KSETTINGS%NUGGET=0.0  
  KSETTINGS%MINP=20
  KSETTINGS%KTYPE=-2          !## default ordinary kriging
  KSETTINGS%PNTSEARCH=0
  KSETTINGS%IADJRANGE=0
  KSETTINGS%IQUADRANT=0

  DO I=1,SIZE(KSETTINGS)
   IF(UTL_READINITFILE('RANGE_'//TRIM(ITOS(I)),LINE,IU,1))     READ(LINE,*) KSETTINGS(I)%RANGE
   IF(UTL_READINITFILE('SILL_'//TRIM(ITOS(I)),LINE,IU,1))      READ(LINE,*) KSETTINGS(I)%SILL
   IF(UTL_READINITFILE('NUGGET_'//TRIM(ITOS(I)),LINE,IU,1))    READ(LINE,*) KSETTINGS(I)%NUGGET    
   IF(UTL_READINITFILE('MINP_'//TRIM(ITOS(I)),LINE,IU,1))      READ(LINE,*) KSETTINGS(I)%MINP
   IF(UTL_READINITFILE('KTYPE_'//TRIM(ITOS(I)),LINE,IU,1))     READ(LINE,*) KSETTINGS(I)%KTYPE
   IF(UTL_READINITFILE('PNTSEARCH_'//TRIM(ITOS(I)),LINE,IU,1)) READ(LINE,*) KSETTINGS(I)%PNTSEARCH
   IF(UTL_READINITFILE('IADJRANGE_'//TRIM(ITOS(I)),LINE,IU,1)) READ(LINE,*) KSETTINGS(I)%IADJRANGE
   IF(UTL_READINITFILE('IQUADRANT_'//TRIM(ITOS(I)),LINE,IU,1)) READ(LINE,*) KSETTINGS(I)%IQUADRANT
  ENDDO
  
 ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN

  !## write source  model
  CALL SOLIDOPENSOL_WRITESOLID(IU,1)

  IF(NSPF.GT.0)THEN
   WRITE(IU,*)
   LINE=TRIM(ITOS(NSPF))
   WRITE(IU,'(A)',IOSTAT=IOS) 'NCROSS='//TRIM(LINE)
   NTBSOL=SLD(1)%NINT 
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
     LINE='FMASK'//TRIM(ITOS(I))//'="'//TRIM(MASK(I)%ALIAS)//'"' 
     WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
    ENDDO
   ENDIF

  !## number of shapes to be used for the interpolation
  IF(SHPNO.GT.0)THEN
   WRITE(IU,*)
   WRITE(IU,'(A)',IOSTAT=IOS) 'GENFILE="SHAPES\SHAPE.GEN"'
   CALL POLYGON1SAVELOADSHAPE(ID_SAVESHAPE,0,TRIM(DIR)//'\SHAPES\SHAPE.GEN')
  ENDIF

  !## get number of genfiles in grid
  CALL WDIALOGSELECT(ID_DSOLIDTAB4)
  DO K=1,2
   NGEN=0
   DO I=1,WINFOGRID(IDF_GRID1,GRIDROWSMAX)
    CALL WGRIDGETCELLINTEGER(IDF_GRID1,1,I,J)
    CALL WGRIDGETCELLSTRING(IDF_GRID1,2,I,LINE)
    IF(J.GT.0.AND.J.LE.SLD(1)%NINT)THEN
     IF(TRIM(LINE).NE.'')THEN
      NGEN=NGEN+1
      IF(K.EQ.2)THEN; GEN(NGEN)%ILAY=J; GEN(NGEN)%FNAME=LINE; ENDIF
     ENDIF
    ENDIF
   ENDDO
   IF(K.EQ.1)THEN
    IF(ALLOCATED(GEN))DEALLOCATE(GEN); ALLOCATE(GEN(NGEN))
   ENDIF
  ENDDO
  
  IF(NGEN.GT.0)THEN
   WRITE(IU,*)
   LINE='NGEN='//TRIM(ITOS(NGEN))
   WRITE(IU,'(A)') TRIM(LINE)
   DO J=1,NGEN
    LINE='GEN_'//TRIM(ITOS(J))//'='//TRIM(ITOS(GEN(J)%ILAY))//','//TRIM(GEN(J)%FNAME)
    WRITE(IU,'(A)') TRIM(LINE)
   ENDDO
  ENDIF
  
  IF(ALLOCATED(KSETTINGS))THEN
   DO I=1,SIZE(KSETTINGS)
    WRITE(IU,*)
    LINE='RANGE_'//TRIM(ITOS(I))//'='//TRIM(RTOS(KSETTINGS(I)%RANGE,'F',2));   WRITE(IU,'(A)') TRIM(LINE)
    LINE='SILL_'//TRIM(ITOS(I))//'='//TRIM(RTOS(KSETTINGS(I)%SILL,'F',2));     WRITE(IU,'(A)') TRIM(LINE)
    LINE='NUGGET_'//TRIM(ITOS(I))//'='//TRIM(RTOS(KSETTINGS(I)%NUGGET,'F',2)); WRITE(IU,'(A)') TRIM(LINE)
    LINE='MINP_'//TRIM(ITOS(I))//'='//TRIM(ITOS(KSETTINGS(I)%MINP));           WRITE(IU,'(A)') TRIM(LINE)
    LINE='KTYPE_'//TRIM(ITOS(I))//'='//TRIM(ITOS(KSETTINGS(I)%KTYPE));         WRITE(IU,'(A)') TRIM(LINE)
    LINE='PNTSEARCH_'//TRIM(ITOS(I))//'='//TRIM(ITOS(KSETTINGS(I)%PNTSEARCH)); WRITE(IU,'(A)') TRIM(LINE)
    LINE='IADJRANGE_'//TRIM(ITOS(I))//'='//TRIM(ITOS(KSETTINGS(I)%IADJRANGE)); WRITE(IU,'(A)') TRIM(LINE)
    LINE='IQUADRANT_'//TRIM(ITOS(I))//'='//TRIM(ITOS(KSETTINGS(I)%IQUADRANT)); WRITE(IU,'(A)') TRIM(LINE)
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
 SUBROUTINE SOLIDOPENSOL_WRITESOLID(IU,IS)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IU,IS
 CHARACTER(LEN=256) :: LINE
 INTEGER :: I,J,K,IOS,N,IR,IG,IB

 WRITE(IU,*) 
 
 !## write
 LINE='NINT='//TRIM(ITOS(SLD(IS)%NINT))
 WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)

 DO J=1,SLD(IS)%NINT
  I=INDEX(SLD(IS)%INTNAME(J),'\',.TRUE.)+1
  LINE=SLD(IS)%INTNAME(J)(I:)
  LINE='INT_L'//TRIM(ITOS(J))//'="'//TRIM(LINE)//'"'
  CALL WRGBSPLIT(SLD(IS)%INTCLR(J),IR,IG,IB)
  LINE=TRIM(LINE)//','//TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','//TRIM(ITOS(IB))//','// &
       TRIM(ITOS(SLD(IS)%ICLC(J)))//','//TRIM(ITOS(SLD(IS)%ICHECK(J)))//','//TRIM(RTOS(SLD(IS)%XRESOLUTION(J),'F',2))
  WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
 ENDDO

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
 INTEGER,ALLOCATABLE,DIMENSION(:) :: INFO
 
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
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open Solid Cross-Section File (*.spf) for reading.'//CHAR(13)// &
    '['//TRIM(DIR)//'\'//TRIM(SPF(ISPF)%FNAME)//']','Warning')
  ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN
   CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Can not open Solid Cross-Section File (*.spf) for writing.'//CHAR(13)// &
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
  !## read length of cross-section
  READ(IU,*) SPF(ISPF)%TX
  !## recompute tx, could be inaccurate or edited?
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
   !## default labelname
   SPF(ISPF)%PROF(IL)%LNAME=SLD(1)%INTNAME(IL)(INDEX(SLD(1)%INTNAME(IL),'\',.TRUE.)+1:INDEX(SLD(1)%INTNAME(IL),'.',.TRUE.)-1)
   !## default lines are active
   SPF(ISPF)%PROF(IL)%IACTIVE=1
   !## read number of position/ilay for il-th elevation
   READ(IU,'(A)') LINE
   READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS,SPF(ISPF)%PROF(IL)%ICLR,SPF(ISPF)%PROF(IL)%IWIDTH,SPF(ISPF)%PROF(IL)%LNAME,SPF(ISPF)%PROF(IL)%IACTIVE
   IF(IOS.NE.0)THEN
    READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS,SPF(ISPF)%PROF(IL)%ICLR,SPF(ISPF)%PROF(IL)%IWIDTH,SPF(ISPF)%PROF(IL)%LNAME
    IF(IOS.NE.0)THEN
     READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS,SPF(ISPF)%PROF(IL)%ICLR,SPF(ISPF)%PROF(IL)%IWIDTH
     IF(IOS.NE.0)THEN
      READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS,SPF(ISPF)%PROF(IL)%ICLR
      IF(IOS.NE.0)READ(LINE,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%NPOS
     ENDIF
    ENDIF
   ENDIF
   NULLIFY(SPF(ISPF)%PROF(IL)%PX,SPF(ISPF)%PROF(IL)%PZ)
   ALLOCATE(SPF(ISPF)%PROF(IL)%PX(MXPX),SPF(ISPF)%PROF(IL)%PZ(MXPX))
   !## read all dist/zvalue on the segments
   DO I=1,SPF(ISPF)%PROF(IL)%NPOS
    READ(IU,*) SPF(ISPF)%PROF(IL)%PX(I),SPF(ISPF)%PROF(IL)%PZ(I)
   ENDDO
  ENDDO
  SPF(ISPF)%PBITMAP%IACT=0
  READ(IU,*,IOSTAT=IOS) SPF(ISPF)%PBITMAP%GX1,SPF(ISPF)%PBITMAP%GY1,SPF(ISPF)%PBITMAP%GX2,SPF(ISPF)%PBITMAP%GY2,SPF(ISPF)%PBITMAP%FNAME
  IF(IOS.EQ.0) SPF(ISPF)%PBITMAP%IACT=1

  IF(SPF(ISPF)%PBITMAP%IACT.GT.0)THEN
   IF(ALLOCATED(INFO))DEALLOCATE(INFO); ALLOCATE(INFO(6))
   CALL IGRFILEINFO(SPF(ISPF)%PBITMAP%FNAME,INFO,6)
   SPF(ISPF)%PBITMAP%ITYPE=INFO(1)
   SPF(ISPF)%PBITMAP%NCOL =INFO(2) !## Image width in pixels.
   SPF(ISPF)%PBITMAP%NROW =INFO(3) !## Image height in pixels.
   SPF(ISPF)%PBITMAP%NCLR =INFO(4) !## Number of colours.
   SPF(ISPF)%PBITMAP%COMPR=INFO(5) !## Is file compressed ? 0 = no , 1 = yes.
   SPF(ISPF)%PBITMAP%CDEPT=INFO(6) !## Colour depth in bits-per-pixel (1-32)
  ENDIF
  
 !## write spf-file (correct for lines that are below/beyond the extent of the profile)

 ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN

  !## number of coordinates
  LINE=TRIM(ITOS(SPF(ISPF)%NXY))
  WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  !## read coordinates
  DO I=1,SPF(ISPF)%NXY
!   LINE=TRIM(RTOS(SPF(ISPF)%X(I),'F',2))//','//TRIM(RTOS(SPF(ISPF)%Y(I),'F',2))
!   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
   WRITE(IU,*,IOSTAT=IOS) SPF(ISPF)%X(I),',',SPF(ISPF)%Y(I)
  ENDDO
  LINE=TRIM(RTOS(SPF(ISPF)%TX,'F',2))//' !## length of the cross-section'
  WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  !## number of layers to interpolate
  DO IL=1,NTBSOL
   LINE=TRIM(ITOS(SPF(ISPF)%PROF(IL)%NPOS))//','// &
        TRIM(ITOS(SPF(ISPF)%PROF(IL)%ICLR))//','// &
        TRIM(ITOS(SPF(ISPF)%PROF(IL)%IWIDTH))//',"'// &
        TRIM(SPF(ISPF)%PROF(IL)%LNAME)//'",'// &
        TRIM(ITOS(SPF(ISPF)%PROF(IL)%IACTIVE))
   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
   !## read all dist/zvalue on the segments
   DO I=1,SPF(ISPF)%PROF(IL)%NPOS
!    LINE=TRIM(RTOS(SPF(ISPF)%PROF(IL)%PX(I),'F',2))//','//TRIM(RTOS(SPF(ISPF)%PROF(IL)%PZ(I),'F',2))
!    WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
    WRITE(IU,*,IOSTAT=IOS) SPF(ISPF)%PROF(IL)%PX(I),',',SPF(ISPF)%PROF(IL)%PZ(I)
   ENDDO
  ENDDO
  IF(SPF(ISPF)%PBITMAP%IACT.GT.0)THEN
   LINE=TRIM(RTOS(SPF(ISPF)%PBITMAP%GX1,'F',2))//','//TRIM(RTOS(SPF(ISPF)%PBITMAP%GY1,'F',2))//','// &
        TRIM(RTOS(SPF(ISPF)%PBITMAP%GX2,'F',2))//','//TRIM(RTOS(SPF(ISPF)%PBITMAP%GY2,'F',2))//',"'// &
        TRIM(SPF(ISPF)%PBITMAP%FNAME)//'"'
   WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)
  ENDIF
  
 ENDIF
 CLOSE(IU)

 SOLIDOPENSPF=.TRUE.

 END FUNCTION SOLIDOPENSPF

 !###====================================================================
 SUBROUTINE SOLID_INITSLD(N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 INTEGER :: I

 CALL SOLIDDEALLOCATESLD()
 !## nothing to allocate - return
 IF(N.EQ.0)RETURN

 ALLOCATE(SLD(N))
 DO I=1,SIZE(SLD)
  NULLIFY(SLD(I)%INTNAME)
  NULLIFY(SLD(I)%INTCLR)
  NULLIFY(SLD(I)%ICLC)
  NULLIFY(SLD(I)%ICHECK)
  NULLIFY(SLD(I)%XRESOLUTION)
 END DO

 END SUBROUTINE SOLID_INITSLD

 !###====================================================================
 SUBROUTINE SOLID_INITSLDPOINTER(I,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,I

 ALLOCATE(SLD(I)%INTNAME(N))
 ALLOCATE(SLD(I)%INTCLR(N))
 ALLOCATE(SLD(I)%ICLC(N))
 ALLOCATE(SLD(I)%ICHECK(N))
 ALLOCATE(SLD(I)%XRESOLUTION(N))
 SLD(I)%INTNAME=''
 SLD(I)%INTCLR=0
 SLD(I)%ICLC=0
 SLD(I)%ICHECK=1
 SLD(I)%XRESOLUTION=0.0

 END SUBROUTINE SOLID_INITSLDPOINTER

 !###====================================================================
 SUBROUTINE SOLID_INITSPF(N)
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
  SPF(I)%PBITMAP%IACT=0
 END DO

 END SUBROUTINE SOLID_INITSPF

 !###======================================================================
 SUBROUTINE SOLID_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(DZ))DEALLOCATE(DZ)
 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
 IF(ALLOCATED(FBOREHOLES))DEALLOCATE(FBOREHOLES)
 IF(ALLOCATED(MASK))THEN
  DO I=1,SIZE(MASK); CALL IDFDEALLOCATEX(MASK(I)%IDF); ENDDO; DEALLOCATE(MASK)
 ENDIF
 IF(ALLOCATED(SOLIDF))THEN
  CALL IDFDEALLOCATE(SOLIDF,SIZE(SOLIDF)); DEALLOCATE(SOLIDF)
 ENDIF
 IF(ALLOCATED(TB))THEN
  CALL IDFDEALLOCATE(TB,SIZE(TB)); DEALLOCATE(TB)
 ENDIF
 IF(ALLOCATED(KSETTINGS))DEALLOCATE(KSETTINGS)
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
  IF(ASSOCIATED(SLD(I)%INTNAME))DEALLOCATE(SLD(I)%INTNAME)
  IF(ASSOCIATED(SLD(I)%INTCLR)) DEALLOCATE(SLD(I)%INTCLR)
  IF(ASSOCIATED(SLD(I)%ICLC))   DEALLOCATE(SLD(I)%ICLC)
  IF(ASSOCIATED(SLD(I)%ICHECK)) DEALLOCATE(SLD(I)%ICHECK)
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

END MODULE MOD_SOLID_UTL

