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
!!
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

  !## clean solid
  CALL SOLID_DEALLOCATE()

  !## clean polygon
  SHPNO=0

  CALL SOLIDINITSLD(1)

  DO I=1,SIZE(SLD)

   SLD(I)%SNAME=SOLNAME(INDEX(SOLNAME,'\',.TRUE.)+1:INDEX(SOLNAME,'.',.TRUE.)-1)
   
   IF(.NOT.UTL_READINITFILE('NINT',LINE,IU,0))RETURN
   !## number of modellayers (interfaces)
   READ(LINE,*) SLD(I)%NINT

   CALL SOLIDINITSLDPOINTER(I,SLD(I)%NINT)

   !## try to read all idf's
   DO J=1,SLD(I)%NINT
    IF(.NOT.UTL_READINITFILE('INT_L'//TRIM(ITOS(J)),LINE,IU,0))RETURN
    IF(I.EQ.1)THEN
     READ(LINE,*,IOSTAT=IOS) SLD(I)%INTNAME(J),IR,IG,IB,SLD(I)%ICLC(J),SLD(I)%ICHECK(J) 
     IF(IOS.NE.0)THEN
      SLD(I)%ICHECK(J)=1
      READ(LINE,*,IOSTAT=IOS) SLD(I)%INTNAME(J),IR,IG,IB,SLD(I)%ICLC(J) 
     ENDIF
    ENDIF
    IF(I.GT.1)READ(LINE,*,IOSTAT=IOS) SLD(I)%INTNAME(J),IR,IG,IB
    IF(IOS.NE.0)THEN
     CLOSE(IU)
     CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'Error reading INT_S'//TRIM(ITOS(I))// &
               '_L'//TRIM(ITOS(J))//' for solid '//TRIM(ITOS(I)),'Error')
     RETURN
    ENDIF
    SLD(I)%INTCLR(J)=WRGB(IR,IG,IB)
    IF(I.EQ.1)SLD(I)%INTNAME(J)=TRIM(DIR)//'\'//TRIM(SLD(I)%INTNAME(J))
   ENDDO

  ENDDO

  !## number of idf in solid is number of first solid in sol file
  NTBSOL=SLD(1)%NINT

  !## number of cross-sections
  IF(UTL_READINITFILE('NCROSS',LINE,IU,1))READ(LINE,*) NSPF

  !## allocate memory for cross-sections: maximum = 200
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

 ELSEIF(RW.EQ.'W'.OR.RW.EQ.'w')THEN

  !## write source  model
  CALL SOLIDOPENSOL_WRITESOLID(IU,I,1)

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
 LINE='NINT='//TRIM(ITOS(SLD(IS)%NINT))
 WRITE(IU,'(A)',IOSTAT=IOS) TRIM(LINE)

 DO J=1,SLD(IS)%NINT
  I=INDEX(SLD(IS)%INTNAME(J),'\',.TRUE.)+1
  LINE=SLD(IS)%INTNAME(J)(I:)
  LINE='INT_L'//TRIM(ITOS(J))//'="'//TRIM(LINE)//'"'
  CALL WRGBSPLIT(SLD(IS)%INTCLR(J),IR,IG,IB)
  LINE=TRIM(LINE)//','//TRIM(ITOS(IR))//','//TRIM(ITOS(IG))//','//TRIM(ITOS(IB))//','// &
       TRIM(ITOS(SLD(IS)%ICLC(J)))//','//TRIM(ITOS(SLD(IS)%ICHECK(J))) 
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
  SPF(ISPF)%PBITMAP%IACT=0
  READ(IU,*,IOSTAT=IOS) SPF(ISPF)%PBITMAP%GX1,SPF(ISPF)%PBITMAP%GY1,SPF(ISPF)%PBITMAP%GX2,SPF(ISPF)%PBITMAP%GY2,SPF(ISPF)%PBITMAP%FNAME
  IF(IOS.EQ.0) SPF(ISPF)%PBITMAP%IACT=1

  IF(SPF(ISPF)%PBITMAP%IACT.EQ.1)THEN
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
  IF(SPF(ISPF)%PBITMAP%IACT.EQ.1)THEN
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
  NULLIFY(SLD(I)%INTNAME)
  NULLIFY(SLD(I)%INTCLR)
  NULLIFY(SLD(I)%ICLC)
  NULLIFY(SLD(I)%ICHECK)
 END DO

 END SUBROUTINE SOLIDINITSLD

 !###====================================================================
 SUBROUTINE SOLIDINITSLDPOINTER(I,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N,I

 ALLOCATE(SLD(I)%INTNAME(N))
 ALLOCATE(SLD(I)%INTCLR (N))
 ALLOCATE(SLD(I)%ICLC   (N))
 ALLOCATE(SLD(I)%ICHECK (N))
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
  SPF(I)%PBITMAP%IACT=0
 END DO

 END SUBROUTINE SOLIDINITSPF

 !###======================================================================
 SUBROUTINE SOLID_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(DZ))DEALLOCATE(DZ)
 IF(ALLOCATED(ILIST))DEALLOCATE(ILIST)
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

