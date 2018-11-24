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
MODULE MOD_GXG_CLC

USE WINTERACTER
USE RESOURCE
USE DATEVAR
USE MOD_UTL, ONLY : UTL_GETUNIT,ITOS,UTL_MESSAGEHANDLE,UTL_CLOSEUNITS,UTL_IDFSNAPTOGRID,UTL_WAITMESSAGE,NEWLINE,UTL_GDATE,JD
USE MOD_PREF_PAR, ONLY : PREFVAL
USE MOD_IDF, ONLY : IDFOPEN,IDFREADDIM,IDFWRITE,IDFALLOCATEX,IDFALLOCATESXY,IDFDEALLOCATEX,IDFNULLIFY, &
                    IDFREAD,IDFDEALLOCATE,IDFGETVAL,IDFGETLOC,IDFIROWICOL,IDFCOPY,IDFREADPART,IDFFILLCOMMENT
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_TOOLS_UTL, ONLY : TOOLS_UTL_FILLPOINTER
USE MOD_OSD, ONLY : OSD_OPEN,OSD_GETENV
USE MOD_POLYGON_PAR
USE MOD_POLYGON_UTL, ONLY : POLYGON1SAVELOADSHAPE
USE MOD_GXG_PAR

CHARACTER(LEN=256) :: FGLG,FGHG,FGVG,FGT,FNLEG,FHG3,FLG3
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:,:) :: FGXGLH
REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: LG,HG
INTEGER,ALLOCATABLE,DIMENSION(:) :: NOYEAR
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: IDF,GXG
TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:,:) :: GXGLH
TYPE(IDFOBJ) :: MVIDF
TYPE(IDFOBJ),PRIVATE :: IDFCP !## pointer idf to be computed
TYPE(IDFOBJ),PRIVATE :: IDFRP !## pointer idf to be read

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION GXG1_COMPUTEGXG()
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,SHPI
 CHARACTER(LEN=256) :: FN,LINE,LINEP
 CHARACTER(LEN=256),DIMENSION(4) :: FGXG
 CHARACTER(LEN=4) :: CY
 CHARACTER(LEN=2) :: CD,CM
 REAL(KIND=DP_KIND) :: RS,SUMLG,SUMHG,SUMLGX,SUMHGX,NGXG,MV,XC,YC,NODATA,XMIN,YMIN,XMAX,YMAX
 INTEGER :: MVICOL,MVIROW,IRAT,IRAT1,II,YY,MM,IYEAR,DN,IUSEMV,ILAY,JLAY, &
            I,J,N,IDN,IROW,ICOL,IOS,JU,NLOC,NIP,MINMEASUREMENT
 INTEGER :: FYR,TYR,NYR,YCNT,IYR ! FromYear, ToYear, Number of Years, YearCounter,yearnumber
 INTEGER :: IIDF                 ! nr of first IDF that exists in IDFarray
 LOGICAL :: LEX,LEXMV
 CHARACTER(LEN=52) :: IDFFILE

 GXG1_COMPUTEGXG=.FALSE.

 MINMEASUREMENT=24; IF(SUM(GXG_IPERIOD).NE.24)MINMEASUREMENT=3
  
 !## nodata value for new created idf's: glg,ghg,gvg,gt
 NODATA=-999.99D0

 CALL GXG1_ABORT()
 IF(IBATCH.EQ.0)GXG_STARTMONTH=4
 
 !## allocate number of FGXG files, based on HG3/LG3 option
 IF(GXG_HGLG3.EQ.1)THEN
  ALLOCATE(FGXGLH(2,SIZE(GXG_IYEAR)))
 ENDIF

 !## entire area
 IF(ISEL.EQ.1)THEN
  SHP%POL%IACT=0
 !## select all polygons
 ELSEIF(ISEL.EQ.2)THEN
  CALL POLYGON1SAVELOADSHAPE(ID_LOADSHAPE,GXG_GENFNAME)
  SHP%POL(1:SHP%NPOL)%IACT=1
 !## usage of idf
 ELSEIF(ISEL.EQ.3)THEN
  IF(.NOT.IDFREAD(IDFRP,GXG_IDFNAME,0))RETURN
 ENDIF

 FYR=MINVAL(GXG_IYEAR)
 TYR=MAXVAL(GXG_IYEAR)
 !## determine no of years
 NYR=TYR-FYR; IF(GXG_STARTMONTH.EQ.1)NYR=NYR+1
  
 IDFFILE=GXG_RESDIR(INDEX(GXG_RESDIR,'\',.TRUE.)+1:)
 IF(IBATCH.EQ.1)GXG_RESDIR=GXG_RESDIR(:INDEX(GXG_RESDIR,'\',.TRUE.)-1)
 
 DO JLAY=1,GXG_NLAYER 
  
  ILAY=GXG_ILAYER(JLAY)
  
  !## Concatenate years to layer files
  FGLG   ='GLG_'//TRIM(ITOS(FYR))//'-'//TRIM(ITOS(TYR))//'_L'//TRIM(ITOS(ILAY))//'.IDF'
  FGXG(1)=FGLG
  FGHG   ='GHG_'//TRIM(ITOS(FYR))//'-'//TRIM(ITOS(TYR))//'_L'//TRIM(ITOS(ILAY))//'.IDF'
  FGXG(2)=FGHG
  FGVG   ='GVG_'//TRIM(ITOS(FYR))//'-'//TRIM(ITOS(TYR))//'_L'//TRIM(ITOS(ILAY))//'.IDF'
  FGXG(3)=FGVG
  FGT    ='GT_'// TRIM(ITOS(FYR))//'-'//TRIM(ITOS(TYR))//'_L'//TRIM(ITOS(ILAY))//'.IDF'
  FGXG(4)=FGT
  !## Concatenate years to layer files for 3 days with highest and 3 days with lowest stages
  IF(GXG_HGLG3.EQ.1)THEN
   DO IYR=1,NYR  
    FLG3   ='LG3_'//TRIM(ITOS(GXG_IYEAR(IYR)))//'0101_L'//TRIM(ITOS(ILAY))//'.IDF'
    FGXGLH(1,IYR)=FLG3
    FHG3   ='HG3_'//TRIM(ITOS(GXG_IYEAR(IYR)))//'0101_L'//TRIM(ITOS(ILAY))//'.IDF'
    FGXGLH(2,IYR)=FHG3       
   ENDDO
  ENDIF
  
  !## check existence GXG files
  IF(IBATCH.EQ.0)THEN
   INQUIRE(FILE=FGLG,EXIST=LEX)
   IF(LEX)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
     TRIM(FGLG)//CHAR(13)//'already exists overwrite it and continue?','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF
   INQUIRE(FILE=FGHG,EXIST=LEX)
   IF(LEX)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
     TRIM(FGHG)//CHAR(13)//'already exists overwrite it and continue?','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF
   INQUIRE(FILE=FGHG,EXIST=LEX)
   IF(LEX)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
     TRIM(FGHG)//CHAR(13)//'already exists overwrite it and continue?','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF
   INQUIRE(FILE=FGVG,EXIST=LEX)
   IF(LEX)THEN
    CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
     TRIM(FGVG)//CHAR(13)//'already exists overwrite it and continue?','Question')
    IF(WINFODIALOG(4).NE.1)RETURN
   ENDIF
  !## in case of HG3 and LG3
   IF(GXG_HGLG3.EQ.1)THEN
    DO I=1,6 
     DO IYR=1,NYR  
      INQUIRE(FILE=FGXGLH(I,IYR),EXIST=LEX)
      IF(LEX)THEN
       CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONNO,'Current file:'//CHAR(13)// &
        TRIM(FGXGLH(I,IYR))//CHAR(13)//'already exists overwrite it and continue?','Question')
       IF(WINFODIALOG(4).NE.1)RETURN
      ENDIF
     ENDDO
    ENDDO
   ENDIF
  ENDIF
  
  IF(NYR.LE.0)THEN
   IF(IBATCH.EQ.0)CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'You need to select one year at least'//CHAR(13)// &
     'to form a hydrological year','Information')
   IF(IBATCH.EQ.1)WRITE(*,'(A)') 'You need to select one year at least to form a hydrological year.'
   CALL GXG1_ABORT()
   RETURN
  ENDIF

  !## allocate various arrays
  ALLOCATE(IDF(24*NYR),NOYEAR(NYR),LG(3,NYR),HG(3,NYR))
  DO I=1,24*NYR; CALL IDFNULLIFY(IDF(I)); ENDDO
  CALL IDFNULLIFY(MVIDF)

  IUSEMV=0
  IF(LEN_TRIM(GXG_MVIDFNAME).GT.0)THEN
   !## initialise dimensions for elevation data
   IF(.NOT.IDFREAD(MVIDF,GXG_MVIDFNAME,0))THEN
    IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot open '//TRIM(GXG_MVIDFNAME)
    CALL GXG1_ABORT()
    RETURN
   ENDIF
   IUSEMV=1
  ENDIF

  IF(IBATCH.EQ.0)THEN
   CALL UTL_MESSAGEHANDLE(0)
   CALL WINDOWSELECT(0)
  ENDIF

  !## initiate variables
  YY    =FYR
  NOYEAR=0
  IDF%IU=0
  !## number of years
  N=0
  
  !## starting with april = 4 (start with march = 3)
  MM=GXG_STARTMONTH-1           
  DO YCNT=1,NYR

   !## twelve months
   DO I=1,12
    MM=MM+1
    IF(MM.GT.12)THEN
     YY=YY+1        
     MM=1
    END IF

    !## lies within selected year
    DO J=1,GXG_NYEAR; IF(YY.EQ.GXG_IYEAR(J))EXIT; ENDDO
    IF(J.LE.GXG_NYEAR)THEN

     WRITE(CY,'(I4.4)') YY
     WRITE(CM,'(I2.2)') MM

     !## collect results from 14/28 solely
     IDN=0
     DO DN=14,28,14
      IDN=IDN+1
      N  =N+1

      !## check iperiods first, whether we're interested at all!##
      IF(GXG_IPERIOD(MM,IDN).EQ.1)THEN
       WRITE(CD,'(I2.2)') DN

       FN=TRIM(GXG_RESDIR)//'\'//TRIM(IDFFILE)//'_'//CY//CM//CD//'_L'//TRIM(ITOS(ILAY))//'.IDF'

       INQUIRE(FILE=FN,EXIST=LEX)
       IF(LEX)THEN
        IIDF=N
        IF(IDFREAD(IDF(N),FN,0))THEN
         NOYEAR(YCNT)=NOYEAR(YCNT)+1
         LINE=TRIM(IDFFILE)//'_'//CY//CM//CD//'_L'//TRIM(ITOS(ILAY))//'.IDF'
         IF(IBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Opening '//TRIM(LINE))
         IF(IBATCH.EQ.1)WRITE(*,*) 'Opening '//TRIM(LINE)
        ELSE
         CALL GXG1_ABORT()
         RETURN
        ENDIF
       ENDIF
      ENDIF
     ENDDO

    ENDIF
   ENDDO
  ENDDO

  NGXG=0
  DO IYEAR=1,NYR
   IF(NOYEAR(IYEAR).GE.MINMEASUREMENT)NGXG=NGXG+1
  ENDDO

  !## check if files to compute GxG for are found
  IF(SUM(IDF%IU).LE.0)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'No '//TRIM(IDFFILE)//' Files found for specified period'//CHAR(13)// &
     'Head files should be saved for the 14/28e of each month', 'Error')
    CALL UTL_MESSAGEHANDLE(1)
   ELSEIF(IBATCH.EQ.1)THEN
    WRITE(*,'(A)') 'No '//TRIM(IDFFILE)//' Files found for specified period, should be save for the 14/28e of each month'
    LINE='iMOD checked the folder: '//TRIM(GXG_RESDIR)//'\'//TRIM(IDFFILE)//'_yyyymmdd_L'//TRIM(ITOS(ILAY))//'.IDF'
    WRITE(*,'(A)') TRIM(LINE)
   ENDIF
   CALL GXG1_ABORT()
   RETURN
  ENDIF

  JU=UTL_GETUNIT()
  CALL OSD_OPEN(JU,FILE=TRIM(GXG_RESDIR)//'\summary_l'//TRIM(ITOS(ILAY))//'_'//TRIM(OSD_GETENV('USERNAME'))//'.txt',&
     STATUS='UNKNOWN',IOSTAT=IOS)
  IF(IOS.NE.0)THEN
   LINE=TRIM(GXG_RESDIR)//'\summary_l'//TRIM(ITOS(ILAY))//'_'//TRIM(OSD_GETENV('USERNAME'))//'.txt'
   IF(IBATCH.EQ.1)WRITE(*,*) 'Cannot open '//TRIM(LINE)
   CALL GXG1_ABORT()
   RETURN
  ENDIF
  WRITE(JU,*) 'Following files were INCLUDED:'
  N =0
  YY=FYR
  !## starting with april = 4 (start with march = 3)
  MM=GXG_STARTMONTH-1
  DO YCNT=1,NYR
   WRITE(JU,'(50A1)') ('-',I=1,50)
   WRITE(JU,*) 'Hydrological Year ',YCNT+FYR-1,YCNT+FYR
   IF(NOYEAR(YCNT).LT.MINMEASUREMENT) &
    WRITE(JU,*) 'NOT INCLUDED in GxG, due to ',NOYEAR(YCNT),' measurements (should be >=',MINMEASUREMENT,')'
   IF(NOYEAR(YCNT).GE.MINMEASUREMENT) &
    WRITE(JU,*) 'GxG based upon ',NOYEAR(YCNT),' measurements'
   WRITE(JU,'(50A1)') ('-',I=1,50)
   II=0
   DO I=1,12
    MM=MM+1
    IF(MM.GT.12)THEN
     YY=YY+1         
     MM=1
    END IF
    !## lies within selected year
    DO J=1,GXG_NYEAR; IF(YY.EQ.GXG_IYEAR(J))EXIT; ENDDO
    IF(J.LE.GXG_NYEAR)THEN

     WRITE(CY,'(I4.4)') YY
     WRITE(CM,'(I2.2)') MM
     DO DN=14,28,14
      N=N+1
      IF(IDF(N)%IU.GT.0)THEN
       II=II+1
       WRITE(CD,'(I2.2)') DN
       FN=TRIM(IDFFILE)//'_'//CY//CM//CD//'_L'//TRIM(ITOS(ILAY))//'.IDF'
       IF(NOYEAR(YCNT).GE.MINMEASUREMENT)WRITE(JU,*) II,' '//TRIM(FN)
      ENDIF
     ENDDO
    ENDIF
   ENDDO
  ENDDO
  CLOSE(JU)

  ALLOCATE(GXG(4))
  DO I=1,4; CALL IDFNULLIFY(GXG(I)); ENDDO

  !## computational window is equal to idf's
  DO II=1,24*NYR
   IF(IDF(II)%IU.GT.0)THEN
    DO I=1,4
     GXG(I)%NCOL  =IDF(II)%NCOL
     GXG(I)%NROW  =IDF(II)%NROW
     GXG(I)%XMIN  =IDF(II)%XMIN
     GXG(I)%XMAX  =IDF(II)%XMAX
     GXG(I)%YMIN  =IDF(II)%YMIN
     GXG(I)%YMAX  =IDF(II)%YMAX
     GXG(I)%DX    =IDF(II)%DX
     GXG(I)%DY    =IDF(II)%DY
     GXG(I)%IEQ   =IDF(II)%IEQ
     GXG(I)%NODATA=NODATA
    END DO
    EXIT
   ENDIF
  ENDDO
  N=0
  DO II=1,24*NYR
   IF(IDF(II)%IU.GT.0)THEN
    IF(GXG(1)%NCOL.NE.IDF(II)%NCOL)N=N+1
    IF(GXG(1)%NROW.NE.IDF(II)%NROW)N=N+1
    IF(GXG(1)%XMIN.NE.IDF(II)%XMIN)N=N+1
    IF(GXG(1)%XMAX.NE.IDF(II)%XMAX)N=N+1
    IF(GXG(1)%YMIN.NE.IDF(II)%YMIN)N=N+1
    IF(GXG(1)%YMAX.NE.IDF(II)%YMAX)N=N+1
    IF(GXG(1)%DX.NE.IDF(II)%DX)    N=N+1
    IF(GXG(1)%DY.NE.IDF(II)%DY)    N=N+1
    IF(GXG(1)%IEQ.NE.IDF(II)%IEQ)  N=N+1
   ENDIF
  ENDDO

  IF(N.NE.0)THEN
   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'IDF files are not identical in dimension, they should!', 'Error')
    CALL UTL_MESSAGEHANDLE(1)
   ELSEIF(IBATCH.EQ.1)THEN
    WRITE(*,*) 'IDF files are not identical in dimension, they should!## '
   ENDIF
   CALL GXG1_ABORT()
   RETURN
  ENDIF

  !## find proper idf for idf settings
  DO II=1,24*NYR
   IF(IDF(II)%IU.GT.0)EXIT
  ENDDO

  DO I=1,4
   IF(.NOT.IDFALLOCATEX(GXG(I)))THEN
    CALL GXG1_ABORT()
    RETURN
   ENDIF
   IF(GXG(I)%IEQ.EQ.1)THEN
    IF(.NOT.IDFALLOCATESXY(GXG(I)))THEN
     CALL GXG1_ABORT()
     RETURN
    ENDIF
    GXG(I)%SX=IDF(II)%SX
    GXG(I)%SY=IDF(II)%SY
   ENDIF
  END DO

  !## create ghg file
  FGHG=TRIM(GXG_RESDIR)//'\'//TRIM(FGHG)
  !## create glg file
  FGLG=TRIM(GXG_RESDIR)//'\'//TRIM(FGLG)
  !## create gvg file
  FGVG=TRIM(GXG_RESDIR)//'\'//TRIM(FGVG)
  !## create gt file
  FGT =TRIM(GXG_RESDIR)//'\'//TRIM(FGT)

  IF(GXG_HGLG3.EQ.1)THEN
   ALLOCATE(GXGLH(2,NYR))
   DO I=1,2
    DO IYR=1,NYR; CALL IDFNULLIFY(GXGLH(I,IYR)); ENDDO
   ENDDO
   DO IYR=1,NYR
    DO I=1,2
     GXGLH(I,IYR)%NCOL  =GXG(1)%NCOL
     GXGLH(I,IYR)%NROW  =GXG(1)%NROW
     GXGLH(I,IYR)%XMIN  =GXG(1)%XMIN
     GXGLH(I,IYR)%XMAX  =GXG(1)%XMAX
     GXGLH(I,IYR)%YMIN  =GXG(1)%YMIN
     GXGLH(I,IYR)%YMAX  =GXG(1)%YMAX
     GXGLH(I,IYR)%DX    =GXG(1)%DX
     GXGLH(I,IYR)%DY    =GXG(1)%DY
     GXGLH(I,IYR)%IEQ   =GXG(1)%IEQ
     GXGLH(I,IYR)%NODATA=NODATA
    END DO

    DO I=1,2
     IF(.NOT.IDFALLOCATEX(GXGLH(I,IYR)))THEN
      CALL GXG1_ABORT()
      RETURN
     ENDIF
     IF(GXGLH(I,IYR)%IEQ.EQ.1)THEN
      IF(.NOT.IDFALLOCATESXY(GXGLH(I,IYR)))THEN
       CALL GXG1_ABORT()
       RETURN
      ENDIF
      GXGLH(I,IYR)%SX=GXG(1)%SX
      GXGLH(I,IYR)%SY=GXG(1)%SY
     ENDIF
    END DO
   
    !## create HG3 en LG3 files 
    FGXGLH(1,IYR)=TRIM(GXG_RESDIR)//'\'//TRIM(FGXGLH(1,IYR))
    FGXGLH(2,IYR)=TRIM(GXG_RESDIR)//'\'//TRIM(FGXGLH(2,IYR))  
   ENDDO
  ENDIF

  !## Check for existence of legend, if not, then create one in tmp directory
  FNLEG=TRIM(PREFVAL(1))//'\TMP\GT.LEG'
  CALL GXG1_GTLEG_WRITE(FNLEG)

  !## copy settings --- USE FIRST ONE
  CALL IDFCOPY(GXG(1),IDFCP)
  !## create pointer in ipidf - only once to be created
  CALL TOOLS_UTL_FILLPOINTER(ISEL,IDFRP,IDFCP,NIP)
  !## deallocate pointer idf read
  IF(ISEL.EQ.3)CALL IDFDEALLOCATEX(IDFRP)

  IRAT =0; IRAT1=IRAT

  !## for every hydrological year collect 3 highest and 3 largest values for each cel
  !## for GHG and GLG and calculate a GVG using elevation data
  DO IROW=1,GXG(1)%NROW

   IF(IBATCH.EQ.0)THEN
    CALL WMESSAGEPEEK(ITYPE,MESSAGE)
    IF(ITYPE.EQ.KEYDOWN.AND.MESSAGE%VALUE1.EQ.KEYESCAPE)THEN
     CALL GXG1_ABORT()
     RETURN
    ENDIF
   ENDIF

   DO ICOL=1,GXG(1)%NCOL

    IF(IDFCP%X(ICOL,IROW).NE.IDFCP%NODATA)THEN

     !## create array's to hold lowest and largest values
     HG=-999999.0
     LG= 999999.0

     !## startyear
     YY=FYR
     !## starting with april = 4 (start with march = 3)
     MM=GXG_STARTMONTH-1
     !## number of years
     N=0
     !## number of values for current location ne nodata
     NLOC=0

     DO YCNT=1,NYR

      !## twelve months
      DO I=1,12
       !## starting with april = 4 (start with march = 3)
       MM=MM+1 
       IF(MM.GT.12)THEN
        YY=YY+1 
        MM=1
       END IF

       !## lies within selected year
       DO J=1,GXG_NYEAR; IF(YY.EQ.GXG_IYEAR(J))EXIT; ENDDO
       IF(J.LE.GXG_NYEAR)THEN

        !## collect results from 14/28 solely
        DO DN=14,28,14
         N=N+1
         IF(IDF(N)%IU.GT.0)THEN

          !## compute irow/icol indices for each IDF
          RS=IDFGETVAL(IDF(N),IROW,ICOL)

          IF(RS.NE.IDF(N)%NODATA)THEN

           NLOC=NLOC+1

           !## collect 3 largest values
           IF(RS.GT.HG(1,YCNT)) THEN
            DO II=3,2,-1; HG(II,YCNT)=HG(II-1,YCNT); ENDDO
            HG(1,YCNT) = RS
           ELSEIF(RS.GT.HG(2,YCNT)) THEN
            HG(3,YCNT) = HG(2,YCNT)
            HG(2,YCNT) = RS
           ELSEIF(RS.GT.HG(3,YCNT)) THEN
            HG(3,YCNT) = RS
           ENDIF

           !## collect 3 lowest values
           IF(RS.LT.LG(1,YCNT)) THEN
            DO II=3,2,-1; LG(II,YCNT)=LG(II-1,YCNT); ENDDO
            LG(1,YCNT) = RS
           ELSEIF(RS.LT.LG(2,YCNT)) THEN
            LG(3,YCNT) = LG(2,YCNT)
            LG(2,YCNT) = RS
           ELSEIF(RS.LT.LG(3,YCNT)) THEN
            LG(3,YCNT) = RS
           ENDIF

          ENDIF !## rs.ne.idf(n)%nodata
         ENDIF !## idf(n)%iu.gt.0
        ENDDO !## day loop
       ENDIF
      ENDDO !## month loop
     ENDDO !## year loop

     !## mid coordinates of cell
     
     CALL IDFGETLOC(IDF(IIDF),IROW,ICOL,XC,YC)   
     LEXMV=.TRUE.
     MV   =0.0D0
     IF(IUSEMV.EQ.1)THEN
      CALL IDFIROWICOL(MVIDF,MVIROW,MVICOL,XC,YC)
      LEXMV=MVICOL.GE.1.AND.MVICOL.LE.MVIDF%NCOL.AND. &
            MVIROW.GE.1.AND.MVIROW.LE.MVIDF%NROW
      IF(LEXMV)THEN
       MV=IDFGETVAL(MVIDF,MVIROW,MVICOL)
       LEXMV=MV.NE.MVIDF%NODATA
      ENDIF
     ENDIF

     !## minimal of three points found (incl. nodata)
     IF(NLOC.GE.3.AND.LEXMV)THEN

      SUMHG=0.0D0; SUMLG=0.0D0
      DO IYEAR=1,NYR
       IF(NOYEAR(IYEAR).GE.MINMEASUREMENT)THEN
        DO II=1,3
         SUMHG=SUMHG+HG(II,IYEAR)
         SUMLG=SUMLG+LG(II,IYEAR)
        ENDDO
       ENDIF
       !## option write HG3 en LG3 (2 IDF's in total per year)
       IF(GXG_HGLG3.EQ.1)THEN
        SUMLGX=LG(1,IYEAR)+LG(2,IYEAR)+LG(3,IYEAR)
        SUMHGX=HG(1,IYEAR)+HG(2,IYEAR)+HG(3,IYEAR)
        GXGLH(1,IYEAR)%X(ICOL,IROW)=SUMLGX/REAL(3)
        GXGLH(2,IYEAR)%X(ICOL,IROW)=SUMHGX/REAL(3)
        IF(IUSEMV.EQ.1)THEN
         GXGLH(1,IYEAR)%X(ICOL,IROW)=MV-(SUMLGX/REAL(3))
         GXGLH(2,IYEAR)%X(ICOL,IROW)=MV-(SUMHGX/REAL(3))
        ENDIF
       ENDIF
      ENDDO

      GXG(1)%X(ICOL,IROW)=SUMHG/REAL(NGXG*3)  !## ghg
      GXG(2)%X(ICOL,IROW)=SUMLG/REAL(NGXG*3)  !## glg       
      IF(IUSEMV.EQ.1)THEN
       !## ghg
       GXG(1)%X(ICOL,IROW)=MV-GXG(1)%X(ICOL,IROW)
       !## glg
       GXG(2)%X(ICOL,IROW)=MV-GXG(2)%X(ICOL,IROW)
      ENDIF
      !## gvg (VANDER SLUIJS 1990)
      GXG(3)%X(ICOL,IROW)=0.05D0+(0.8*GXG(1)%X(ICOL,IROW))+(0.2*GXG(2)%X(ICOL,IROW))
      !## gt
      GXG(4)%X(ICOL,IROW)=GXG1_GETGT(GXG(2)%X(ICOL,IROW),GXG(1)%X(ICOL,IROW),NODATA)
     ELSE
      DO II=1,4; GXG(II)%X(ICOL,IROW)=NODATA; ENDDO
      IF(GXG_HGLG3.EQ.1)THEN
       DO II=1,2
        DO IYR=1,NYR; GXGLH(II,IYR)%X(ICOL,IROW)=NODATA; ENDDO
       ENDDO
      ENDIF
     ENDIF
    ELSE
     DO II=1,4; GXG(II)%X(ICOL,IROW)=NODATA; ENDDO
      IF(GXG_HGLG3.EQ.1)THEN
       DO II=1,2
        DO IYR=1,NYR; GXGLH(II,IYR)%X(ICOL,IROW)=NODATA; ENDDO
       ENDDO
      ENDIF     
    ENDIF
   ENDDO
   IF(IBATCH.EQ.0)THEN
    CALL WINDOWSELECT(0)
    CALL UTL_WAITMESSAGE(IRAT,IRAT1,IROW,GXG(1)%NROW,'Progress GxG: ')
   ELSEIF(IBATCH.EQ.1)THEN
    WRITE(6,'(A,F10.2,A)') '+Progress GxG: ',REAL(100*IROW)/REAL(GXG(1)%NROW),'%'    ! FR 20131007
   ENDIF
  ENDDO

  LINEP=''
  DO I=1,12
   IF(GXG_IPERIOD(I,1).EQ.1)THEN
    WRITE(LINE,'(A)') TRIM(LINEP)//'14-'//CDATE(I)(1:3)//'. '
    LINEP=LINE
   ENDIF
   IF(GXG_IPERIOD(I,2).EQ.1)THEN
    WRITE(LINE,'(A)') TRIM(LINEP)//'28-'//CDATE(I)(1:3)//'. '
    LINEP=LINE
   ENDIF
  ENDDO

  WRITE(LINE,'(99(I4,1X))') (GXG_IYEAR(I),I=1,GXG_NYEAR)

  CALL IDFFILLCOMMENT(GXG(1),'Units: GHG (m+sl)'//NEWLINE// &
                             'Ilay: '//TRIM(ITOS(ILAY))//NEWLINE// &
                             'From Date: '//TRIM(ITOS(FYR))//NEWLINE// &
                             'To Date: '//TRIM(ITOS(TYR))//NEWLINE// &
                             'Including Years: '//TRIM(LINE)//NEWLINE//&
                             'Including Periods: '//TRIM(LINEP))
  CALL IDFFILLCOMMENT(GXG(2),'Units: GLG (m+sl)'//NEWLINE// &
                             'Ilay: '//TRIM(ITOS(ILAY))//NEWLINE// &
                             'From Date: '//TRIM(ITOS(FYR))//NEWLINE// &
                             'To Date: '//TRIM(ITOS(TYR))//NEWLINE// &
                             'Including Years: '//TRIM(LINE)//NEWLINE//&
                             'Including Periods: '//TRIM(LINEP))
  CALL IDFFILLCOMMENT(GXG(3),'Units: GVG (m+sl)'//NEWLINE// &
                             'Ilay: '//TRIM(ITOS(ILAY))//NEWLINE// &
                             'From Date: '//TRIM(ITOS(FYR))//NEWLINE// &
                             'To Date: '//TRIM(ITOS(TYR))//NEWLINE// &
                             'Including Years: '//TRIM(LINE)//NEWLINE//&
                             'Including Periods: '//TRIM(LINEP))
  CALL IDFFILLCOMMENT(GXG(4),'Units: GT'//NEWLINE// &
                             'Ilay: '//TRIM(ITOS(ILAY))//NEWLINE// &
                             'From Date: '//TRIM(ITOS(FYR))//NEWLINE// &
                             'To Date: '//TRIM(ITOS(TYR))//NEWLINE// &
                             'Including Years: '//TRIM(LINE)//NEWLINE//&
                             'Including Periods: '//TRIM(LINEP))

  IF(.NOT.IDFWRITE(GXG(1),FGHG,1).OR..NOT.IDFWRITE(GXG(2),FGLG,1).OR. &
     .NOT.IDFWRITE(GXG(3),FGVG,1).OR..NOT.IDFWRITE(GXG(4),FGT,1))THEN
  !## error occured
  ENDIF
  IF(GXG_HGLG3.EQ.1)THEN
   DO I=1,2; 
    DO IYEAR=1,NYR
     IF(.NOT.IDFWRITE(GXGLH(I,IYEAR),FGXGLH(I,IYEAR),1))THEN
     !## error occured
     ENDIF 
    ENDDO
   ENDDO
  ENDIF

  !## make cut to fit polygon only
  IF(ISEL.EQ.2)THEN
   XMIN= 10.0D10
   YMIN= 10.0D10
   XMAX=-10.0D10
   YMAX=-10.0D10
   DO SHPI=1,SHP%NPOL
    IF(SHP%POL(SHPI)%IACT.EQ.1.AND.SHP%POL(SHPI)%N.GT.0)THEN
     XMIN=MIN(XMIN,MINVAL(SHP%POL(SHPI)%X(1:SHP%POL(SHPI)%N)))
     XMAX=MAX(XMAX,MAXVAL(SHP%POL(SHPI)%X(1:SHP%POL(SHPI)%N)))
     YMIN=MIN(YMIN,MINVAL(SHP%POL(SHPI)%Y(1:SHP%POL(SHPI)%N)))
     YMAX=MAX(YMAX,MAXVAL(SHP%POL(SHPI)%Y(1:SHP%POL(SHPI)%N)))
    ENDIF
   ENDDO
   DO I=1,4
    IF(IDFREAD(GXG(I),FGXG(I),0))THEN
     IF(.NOT.IDFREADPART(GXG(I),XMIN,YMIN,XMAX,YMAX))THEN
     ENDIF
     CLOSE(GXG(I)%IU)
     IF(.NOT.IDFWRITE(GXG(I),FGXG(I),1))THEN
     ENDIF
    ENDIF
   ENDDO
  ENDIF

  CALL GXG1_ABORT()
 ENDDO

 IF(IBATCH.EQ.0)THEN
  CALL UTL_MESSAGEHANDLE(1)

  CALL WINDOWOPENCHILD(I,FLAGS=HIDEWINDOW,TITLE='Summary_l'//TRIM(ITOS(ILAY))//'_'//TRIM(OSD_GETENV('USERNAME'))//'.txt')
  CALL WEDITFILE(TRIM(GXG_RESDIR)//'\summary_l'//TRIM(ITOS(ILAY))//'_'//TRIM(OSD_GETENV('USERNAME'))//'.txt',MODAL,0,0,COURIERNEW,ISIZE=8)

 ENDIF
 
 GXG1_COMPUTEGXG=.TRUE.

 END FUNCTION GXG1_COMPUTEGXG

 !###======================================================================
 SUBROUTINE GXG1_ABORT()
 !###======================================================================
 IMPLICIT NONE

 IF(ALLOCATED(NOYEAR))DEALLOCATE(NOYEAR)
 IF(ALLOCATED(LG))DEALLOCATE(LG)
 IF(ALLOCATED(HG))DEALLOCATE(HG)

 IF(ALLOCATED(IDF))THEN
  CALL IDFDEALLOCATE(IDF,SIZE(IDF))
  DEALLOCATE(IDF)
 ENDIF

 CALL IDFDEALLOCATEX(MVIDF)
 IF(MVIDF%IU.GT.0)CLOSE(MVIDF%IU)
 MVIDF%IU=0

 IF(ALLOCATED(GXG))THEN
  CALL IDFDEALLOCATE(GXG,SIZE(GXG))
  DEALLOCATE(GXG)
 ENDIF
 
 IF(ALLOCATED(GXGLH))THEN
  CALL IDFDEALLOCATE(GXGLH,SIZE(GXGLH))
  DEALLOCATE(GXGLH)
 ENDIF

 IF(ALLOCATED(FGXGLH))THEN
  DEALLOCATE(FGXGLH)
 ENDIF

 CALL IDFDEALLOCATEX(IDFRP)
 CALL IDFDEALLOCATEX(IDFCP)

 END SUBROUTINE GXG1_ABORT

 !###======================================================================
 SUBROUTINE GXG1_GTLEG_WRITE(FNLEG)
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNLEG
 INTEGER :: IU,IOS

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNLEG,IOSTAT=IOS)
 IF(IOS.EQ.0) THEN
  WRITE(IU,'(A)') '   20    1    0    0    0    0    0    1'
  WRITE(IU,'(A)') 'UPPER BND LOWER BND      IRED    IGREEN     IBLUE     DOMAIN'
  WRITE(IU,'(A)') '  802.0      801.0    254        32         0    "GT VIIId"'
  WRITE(IU,'(A)') '  801.0      800.0    254        32         0    "GT VIIIo"'
  WRITE(IU,'(A)') '  800.0      700.0    254        32         0    "GT VIII"'
  WRITE(IU,'(A)') '  700.0      600.0    254       211         0    "GT VII"'
  WRITE(IU,'(A)') '  600.0      502.0    254       254       124    "GT VI"'
  WRITE(IU,'(A)') '  502.0      501.0    170       170        73    "GT Vb"'
  WRITE(IU,'(A)') '  501.0      500.0     88       127        73    "GT Va"'
  WRITE(IU,'(A)') '  500.0      401.0     78       137        63    "GT V"'
  WRITE(IU,'(A)') '  401.0      400.0    170       254        73    "GT IVc"'
  WRITE(IU,'(A)') '  400.0      302.0    170       254        73    "GT IVu"'
  WRITE(IU,'(A)') '  302.0      301.0     25       254        30    "GT IIIb"'
  WRITE(IU,'(A)') '  301.0      300.0     25       170        30    "GT IIIa"'
  WRITE(IU,'(A)') '  300.0      203.0     25       170        30    "GT III"'
  WRITE(IU,'(A)') '  203.0      202.0     50       254       254    "GT IIc"'
  WRITE(IU,'(A)') '  202.0      201.0     50       254       254    "GT IIb"'
  WRITE(IU,'(A)') '  201.0      200.0     25       170       254    "GT IIa"'
  WRITE(IU,'(A)') '  200.0      103.0     25       170       254    "GT II"'
  WRITE(IU,'(A)') '  103.0      101.0     88        76       208    "GT Ic"'
  WRITE(IU,'(A)') '  101.0      100.0     88        76       208    "GT Ia"'
  WRITE(IU,'(A)') '  100.0       99.0     88        76       208    "GT I"'
 ENDIF
 CLOSE(IU)

 END SUBROUTINE GXG1_GTLEG_WRITE

 !###======================================================================
 REAL(KIND=DP_KIND) FUNCTION GXG1_GETGT(GLG ,GHG ,NODATA)
 !###======================================================================
 IMPLICIT NONE
 REAL(KIND=DP_KIND),INTENT(IN) :: GLG ,GHG ,NODATA
 REAL :: GT

 IF ((GLG.EQ.NODATA)    .AND. (GHG.EQ.NODATA)) THEN      !## GT NODATA
   GT = NODATA
 ELSEIF (GHG.LE.0.0D0    .AND. GLG.LT.0.50) THEN
   GT = 100     !## GTI
 ELSEIF ((GLG.LT.0.50)  .AND. (GHG.LE.0.25   .AND. GHG.GT.0.0D0)) THEN
   GT = 101     !## GT Ia
 ELSEIF ((GLG.LT.0.50)  .AND. (GHG.GT.0.25))  THEN
   GT = 103     !## GT Ic
 ELSEIF (((GLG.GE.0.50) .AND. (GLG.LT.0.80)) .AND. (GHG.LE.0.0D0)) THEN
   GT = 200     !## GT II
 ELSEIF (((GLG.GE.0.50) .AND. (GLG.LT.0.80)) .AND. ((GHG.LT.0.25).AND.(GHG.GT.0.0D0))) THEN
   GT = 201     !## GT IIa
 ELSEIF (((GLG.GE.0.50) .AND. (GLG.LT.0.80)) .AND. ((GHG.GE.0.25).AND.(GHG.LT.0.40))) THEN
   GT = 202     !## GT IIb
 ELSEIF (((GLG.GE.0.50) .AND. (GLG.LT.0.80)) .AND. (GHG.GE.0.40)) THEN
   GT = 203     !## GT IIc
 ELSEIF (((GLG.GE.0.80) .AND. (GLG.LT.1.20)) .AND. (GHG.LE.0.0D0)) THEN
   GT = 300     !## GT III
 ELSEIF (((GLG.GE.0.80) .AND. (GLG.LT.1.20)) .AND. ((GHG.LT.0.25).AND.(GHG.GT.0.0D0))) THEN
   GT = 301     !## GT IIIa
 ELSEIF (((GLG.GE.0.80) .AND. (GLG.LT.1.20)) .AND. ((GHG.GE.0.25).AND.(GHG.LT.0.40))) THEN
   GT = 302     !## GT IIIb
 ELSEIF (((GLG.GE.0.80) .AND. (GLG.LT.1.20)) .AND. ((GHG.GE.0.40).AND.(GHG.LT.0.80))) THEN
   GT = 400     !## GT IVu
 ELSEIF (((GLG.GE.0.80) .AND. (GLG.LT.1.20)) .AND. (GHG.GE.0.80)) THEN
   GT = 401     !## GT IVc
 ELSEIF ((GLG.GE.1.20)  .AND. (GHG.LT.0.40)) THEN
   GT = 500     !GT V
 ELSEIF (((GLG.GE.1.20) .AND. (GLG.LT.1.80)) .AND. (GHG.LT.0.25)) THEN
   GT = 501     !## GT Va
 ELSEIF ((GLG.GE.1.20)  .AND. ((GHG.GE.0.25) .AND. (GHG.LT.0.40))) THEN
   GT = 502     !## GT Vb
 ELSEIF ((GLG.GE.1.20)  .AND. ((GHG.GE.0.40) .AND. (GHG.LT.0.80)))  THEN
   GT = 600     !## GT VI
 ELSEIF ((GLG.GE.1.20)  .AND. ((GHG.GE.0.80) .AND.(GHG.LT.1.40))) THEN
   GT = 700     !## GT VII
 ELSEIF ((GLG.LT.1.60)  .AND. (GHG.GE.1.40)) THEN
   GT = 800     !## GT VIII
 ELSEIF (((GLG.GE.1.60) .AND. (GLG.LT.1.80)) .AND. (GHG.GE.1.40)) THEN
   GT = 801     !## GT VIIIo
 ELSEIF ((GLG.GE.1.80)  .AND. (GHG.GE.1.40)) THEN
   GT = 802     !## GT VIIId
 ELSE
   GT = NODATA
 END IF
 GXG1_GETGT = GT

 END FUNCTION GXG1_GETGT

 !###======================================================================
 SUBROUTINE GXG_COMPUTE_SERIE(JDT,MSR,GHG,GLG,MINMEASUREMENT,DIFFDAY,NODATA)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: MINMEASUREMENT,DIFFDAY
 INTEGER,INTENT(IN),DIMENSION(:) :: JDT
 REAL(KIND=DP_KIND),INTENT(IN),DIMENSION(:) :: MSR
 REAL(KIND=DP_KIND),INTENT(IN) :: NODATA
 REAL(KIND=DP_KIND),INTENT(OUT) :: GHG,GLG
 REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:,:) :: LG,HG
 INTEGER,ALLOCATABLE,DIMENSION(:) :: NY,IL
 INTEGER :: NYR,IY,IM,ID,MNY,MXY,I,II,IYP,NGXG,IIND,MIND,JDD,DD,SM
 REAL(KIND=DP_KIND) :: SUMHG,SUMLG
 INTEGER,DIMENSION(2) :: DGXG
 
 !## fix intervals for gxg
 DGXG(1)=14; DGXG(2)=28
 
 !## find min- and max year
 MNY= 10E6; MXY=-10E6
 DO I=1,SIZE(JDT)
  CALL UTL_GDATE(JDT(I),IY,IM,ID); MNY=MIN(MNY,IY); MXY=MAX(MXY,IY)
 ENDDO
 NYR=(MXY-MNY)+1 

 ALLOCATE(NY(NYR),LG(3,NYR),HG(3,NYR)); NY=0; LG=HUGE(1.0D0); HG=-1.0D0*HUGE(1.0D0)
 ALLOCATE(IL(SIZE(JDT))); IL=0

 !## start month
 SM=4
 
 !## pick potential time steps
 DO IY=MNY,MXY
  DO IM=1,12
   DO ID=14,28,14
    JDD=JD(IY,IM,ID)
    !## find nearest entry
    MIND=10E8; IIND=0
    DO I=1,SIZE(JDT)
     DD=ABS(JDT(I)-JDD)
     IF(DD.LT.MIND.AND.DD.LE.DIFFDAY)THEN
      MIND=DD
      IIND=I
     ENDIF
    ENDDO
    !## set optimal timestep
    IF(IIND.GT.0)IL(IIND)=1
   ENDDO
  ENDDO
 ENDDO

 DO I=1,SIZE(JDT)
  !## skip this time step
  IF(IL(I).EQ.0)CYCLE
  
  CALL UTL_GDATE(JDT(I),IY,IM,ID)
  IYP=(IY-MNY)+1
  IF(IM.LT.SM)IYP=IYP-1
  !## skip this as it belongs to the previous hydrological year
  IF(IYP.LE.0)CYCLE
  
  NY(IYP)=NY(IYP)+1

  !## collect 3 largest values
  IF(MSR(I).GT.HG(1,IYP))THEN
   DO II=3,2,-1; HG(II,IYP)=HG(II-1,IYP); ENDDO
   HG(1,IYP)=MSR(I)
  ELSEIF(MSR(I).GT.HG(2,IYP)) THEN
   HG(3,IYP)=HG(2,IYP)
   HG(2,IYP)=MSR(I)
  ELSEIF(MSR(I).GT.HG(3,IYP)) THEN
   HG(3,IYP)=MSR(I)
  ENDIF

  !## collect 3 lowest values
  IF(MSR(I).LT.LG(1,IYP)) THEN
   DO II=3,2,-1; LG(II,IYP)=LG(II-1,IYP); ENDDO
   LG(1,IYP)=MSR(I)
  ELSEIF(MSR(I).LT.LG(2,IYP)) THEN
   LG(3,IYP)=LG(2,IYP)
   LG(2,IYP)=MSR(I)
  ELSEIF(MSR(I).LT.LG(3,IYP)) THEN
   LG(3,IYP)=MSR(I)
  ENDIF
  
 ENDDO
  
 NGXG=0; SUMHG=0.0D0; SUMLG=0.0D0
 DO I=1,NYR
  IF(NY(I).LT.MINMEASUREMENT)CYCLE
  DO II=1,3
   SUMHG=SUMHG+HG(II,I); SUMLG=SUMLG+LG(II,I)
  ENDDO
  NGXG=NGXG+1
 ENDDO
 
 IF(NGXG.GT.0)THEN
  !## ghg
  GHG=SUMHG/REAL(NGXG*3,8)
  !## glg
  GLG=SUMLG/REAL(NGXG*3,8)
 ELSE
  GHG=NODATA; GLG=NODATA
 ENDIF
 
 DEALLOCATE(LG,HG,IL,NY)

 END SUBROUTINE GXG_COMPUTE_SERIE

END MODULE MOD_GXG_CLC

