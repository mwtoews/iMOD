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
MODULE MOD_GEOCONNECT

USE WINTERACTER
USE RESOURCE
USE IMODVAR, ONLY : IDIAGERROR
USE MODPLOT, ONLY : MPW
USE MOD_GEOCONNECT_PAR
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_SUBST,ITOS,RTOS,UTL_DIRINFO_POINTER,UTL_CAP,UTL_WSELECTFILE,UTL_MESSAGEHANDLE, &
    INVERSECOLOUR,UTL_PLOTLOCATIONIDF,UTL_GETUNIQUE_INT
USE MOD_IDF, ONLY : IDFGETXYVAL,IDFREADSCALE,IDFWRITE,IDFREAD,IDFIROWICOL,IDFGETXYVAL,IDFGETLOC
USE MOD_OSD, ONLY : OSD_OPEN
USE MOD_PREF_PAR, ONLY : PREFVAL

CONTAINS

 !###======================================================================
 SUBROUTINE GC_MAIN(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 CHARACTER(LEN=256) :: FNAME
 INTEGER :: IU
 
 CALL WDIALOGSELECT(MESSAGE%WIN)

 SELECT CASE (MESSAGE%WIN)

  CASE (ID_DGEOCONNECT)
   CALL GC_MAIN_DIALOG(ITYPE,MESSAGE)
   
  CASE (ID_DGEOCONNECT_TAB1)
   CALL GC_MAIN_TAB1(ITYPE,MESSAGE)
 
  CASE (ID_DGEOCONNECT_TAB2)
   CALL GC_MAIN_TAB2(ITYPE,MESSAGE)

  CASE (ID_DGEOCONNECT_TAB3)
   CALL GC_MAIN_TAB3(ITYPE,MESSAGE)

  CASE (ID_DGEOCONNECT_TAB4)
   CALL GC_MAIN_TAB4(ITYPE,MESSAGE)
  
 END SELECT
  
 END SUBROUTINE GC_MAIN

 !###======================================================================
 SUBROUTINE GC_MAIN_DIALOG(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 INTEGER :: ITAB,IU,IOS
 CHARACTER(LEN=256) :: FNAME
 
 SELECT CASE (ITYPE)
    
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (IDCANCEL)
     CALL GC_CLOSE()
    CASE (IDHELP)
      
    CASE (IDOK) 
     CALL WDIALOGGETTAB(ID_GCTAB,ITAB)
     SELECT CASE (ITAB)
      !## call preprocessing routines  
      CASE (ID_DGEOCONNECT_TAB2)
       CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'Are you sure to compute the preprocessing variables?','Question')
       IF(WINFODIALOG(4).EQ.1)THEN
        !## read initial settings from settings-tab
        CALL GC_INIT_GET()               
        !## read initial settings from preprocessing-tab
        IF(GC_INIT_PREPROCESSING_GET())THEN
         !## compute preprocessing 
         CALL GC_COMPUTE_MAIN(0)           
        ENDIF
       ENDIF
      !## call postprocessing routines
      CASE (ID_DGEOCONNECT_TAB3)
       CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'Are you sure to compute the postprocessing files?','Question')              
       IF(WINFODIALOG(4).EQ.1)THEN
        !## read initial settings from settings-tab
        CALL GC_INIT_GET()               
        !## read initial settings from preprocessing-tab
        IF(GC_INIT_POSTPROCESSING_GET())THEN
         !## compute preprocessing 
         CALL GC_COMPUTE_MAIN(0)           
        ENDIF
       ENDIF
     END SELECT
    CASE (ID_SAVEAS)
     CALL WDIALOGGETTAB(ID_GCTAB,ITAB)
     SELECT CASE (ITAB)
      CASE (ID_DGEOCONNECT_TAB1)
      !## call settings routines
       TXTFILE=TRIM(PREFVAL(1))//'\SETTINGS\Geoconnect.txt'
       IF(.NOT.UTL_WSELECTFILE('Textfile (*.txt)|*.txt|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,TXTFILE,&
                   'Save Current Settings into a textfile (*.txt)'))RETURN  
       CALL GC_INIT_WRITE(TXTFILE) 
      CASE (ID_DGEOCONNECT_TAB2)
       FNAME=TRIM(PREFVAL(1))//'\SETTINGS\Geoconnect_pre.ini'
       IF(.NOT.UTL_WSELECTFILE('Initialization file (*.ini)|*.ini|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
                   'Save Current Settings into an initialization file (*.ini)'))RETURN
       !## write *.ini-file
       CALL GC_INIT_PREPROCESSING_WRITE(FNAME)
      CASE (ID_DGEOCONNECT_TAB3)
!       !## call postprocessing routines
!       FNAME=TRIM(PREFVAL(1))//'\IMOD_USER\SETTINGS\Geoconnect_post.ini'
!       IF(.NOT.UTL_WSELECTFILE('Initialization file (*.ini)|*.ini|',SAVEDIALOG+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,&
!                    'Save Current Settings into an initialization file (*.ini)'))RETURN
!       !## Write *.ini-file
!       CALL GC_INIT_POSTPROCESSING_WRITE(FNAME)      
      CASE (ID_DGEOCONNECT_TAB4)
     END SELECT
    CASE (ID_OPEN)
     CALL WDIALOGGETTAB(ID_GCTAB,ITAB)
     SELECT CASE (ITAB)
      CASE (ID_DGEOCONNECT_TAB1)
      !## open general settings
      CASE (ID_DGEOCONNECT_TAB2)
      !## open preprocessing settings
       IF(.NOT.UTL_WSELECTFILE('Initialization file (*.ini)|*.ini|',&
         LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Load initialization file (*.ini)'))RETURN
       IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(FNAME),STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
       IF(.NOT.GC_INIT_PREPROCESSING(IU))RETURN
       CALL GC_INIT_PREPROCESSING_PUT()
      CASE (ID_DGEOCONNECT_TAB3)
      !## open postprocessing settings
     
      CASE (ID_DGEOCONNECT_TAB4)
     END SELECT
   END SELECT
  CASE (TABCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    CASE (ID_DGEOCONNECT_TAB1)
     CALL WDIALOGFIELDSTATE(IDOK,0)
     CALL WDIALOGFIELDSTATE(ID_SAVEAS,0)
     CALL WDIALOGFIELDSTATE(ID_OPEN,0)
     CALL GC_IDENTIFY_INITGRID()
    CASE (ID_DGEOCONNECT_TAB2,ID_DGEOCONNECT_TAB3)
     CALL WDIALOGFIELDSTATE(IDOK,1)
     CALL WDIALOGFIELDSTATE(ID_SAVEAS,1)
     CALL WDIALOGFIELDSTATE(ID_OPEN,1)     
    CASE (ID_DGEOCONNECT_TAB4)
     CALL WDIALOGFIELDSTATE(IDOK,0)
     CALL WDIALOGFIELDSTATE(ID_SAVEAS,1)
     CALL WDIALOGFIELDSTATE(ID_OPEN,1)     
   END SELECT
 END SELECT

 END SUBROUTINE GC_MAIN_DIALOG

 !###======================================================================
 SUBROUTINE GC_MAIN_TAB1(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 
 SELECT CASE (ITYPE)
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    !## identify
    CASE (IDOK)
     !## compute preprocessing 
     GC_IFLAG=1; CALL GC_COMPUTE_MAIN(0)     
   END SELECT
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT

 END SUBROUTINE GC_MAIN_TAB1

 !###======================================================================
 SUBROUTINE GC_MAIN_TAB2(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 CHARACTER(LEN=256) :: FNAME
 
 SELECT CASE (ITYPE)
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_OPEN2)
     CALL WSELECTDIR(DIRCHANGE+DIRCREATE,OUTPUTFOLDER,'Load output folder')
     !## put fname on window
     CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(OUTPUTFOLDER))
    CASE (ID_OPEN1)
     IF(.NOT.UTL_WSELECTFILE('Text file (*.txt)|*.txt|',&
        LOADDIALOG+MUSTEXIST+PROMPTON+DIRCHANGE+APPENDEXT,FNAME,'Load Textfile (*.txt)'))RETURN 
     !## call to subroutine to read iPEST file into variable and put on grid ... 
!     CALL 
    CASE (ID_REFRESH)
     !## call to subroutine to reset all formation factors to 1 in grid... 
     CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'Are you sure to refresh all formation factors'//CHAR(13)// &
          'based upon the given Regis formations?','Question')
     IF(WINFODIALOG(4).EQ.1)THEN
      !## reset factors
      IPFAC%FACT=1.0; IF(.NOT.GC_REGISFILES_PUT(ID_DGEOCONNECT_TAB2))RETURN
     ENDIF
   END SELECT
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
    CASE (IDF_CHECK1,IDF_CHECK2)
     CALL WDIALOGGETCHECKBOX(IDF_CHECK1,ISAVEK)
     CALL WDIALOGGETCHECKBOX(IDF_CHECK2,ISAVEC)
     CALL WDIALOGSELECT(ID_DGEOCONNECT)
     IF(ISAVEK+ISAVEC.EQ.0)THEN
      CALL WDIALOGFIELDSTATE(IDOK,0)
     ELSE
      CALL WDIALOGFIELDSTATE(IDOK,1)
     ENDIF
   END SELECT
 END SELECT

 END SUBROUTINE GC_MAIN_TAB2
 
 !###======================================================================
 SUBROUTINE GC_MAIN_TAB3(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 
 SELECT CASE (ITYPE)
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
   END SELECT
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT

 END SUBROUTINE GC_MAIN_TAB3

 !###======================================================================
 SUBROUTINE GC_MAIN_TAB4(ITYPE,MESSAGE)
 !###======================================================================
 IMPLICIT NONE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 INTEGER,INTENT(IN) :: ITYPE
 
 SELECT CASE (ITYPE)
  CASE (PUSHBUTTON)
   SELECT CASE (MESSAGE%VALUE1)
    CASE (ID_OPEN1)
     CALL WSELECTDIR(DIRCHANGE+DIRCREATE,REGISFOLDER,'Load Regis folder')
      !## put fname on window
      CALL WDIALOGPUTSTRING(IDF_STRING1,TRIM(REGISFOLDER))
    CASE (ID_OPEN2)
     CALL WSELECTDIR(DIRCHANGE+DIRCREATE,TOPFOLDER,'Load Top folder')
      !## put fname on window
      CALL WDIALOGPUTSTRING(IDF_STRING2,TRIM(TOPFOLDER))
    CASE (ID_OPEN3)
     CALL WSELECTDIR(DIRCHANGE+DIRCREATE,BOTFOLDER,'Load Bot folder')
      !## put fname on window
      CALL WDIALOGPUTSTRING(IDF_STRING3,TRIM(BOTFOLDER))   
   END SELECT
  CASE (FIELDCHANGED)
   SELECT CASE (MESSAGE%VALUE2)
   END SELECT
 END SELECT

 END SUBROUTINE GC_MAIN_TAB4

 !###======================================================================
 SUBROUTINE GC_MAIN_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WINDOWSELECT(0)
 IF(WMENUGETSTATE(ID_GEOCONNECT,2).EQ.1)THEN
  CALL GC_CLOSE(); RETURN
 ENDIF

 CALL MAIN1INACTMODULE(ID_GEOCONNECT)

 !## other module no closed, no approvement given
 IF(IDIAGERROR.EQ.1)RETURN

 CALL WMENUSETSTATE(ID_GEOCONNECT,2,1)

 !## open geoconnect dialog 
 CALL WDIALOGLOAD(ID_DGEOCONNECT,ID_DGEOCONNECT)
 
 !## identify
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB1) 
 
 !## preprocessing
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB2)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN) !## open folder where output files need to be stored
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPEN) !## open folder where output files need to be stored
 CALL WDIALOGPUTIMAGE(ID_REFRESH,ID_ICONREDRAW) !## open folder where output files need to be stored
 CALL WDIALOGPUTCHECKBOX(IDF_CHECK1,1) !# default=> save KHV,KVV,KVA (ISAVEK=1)
 
 !## postprocessing
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB3)
 CALL WDIALOGPUTIMAGE(ID_IDENTIFY,ID_ICONPIPET)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS) !## saveas
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN)     !## open
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPEN)     !## open
 CALL WDIALOGPUTIMAGE(ID_OPEN9,ID_ICONOPEN)     !## open
 CALL WDIALOGPUTREAL(IDF_REAL1,MPW%XMIN,'(F10.2)')
 CALL WDIALOGPUTREAL(IDF_REAL2,MPW%YMIN,'(F10.2)')
 CALL WDIALOGPUTREAL(IDF_REAL3,MPW%XMAX,'(F10.2)')
 CALL WDIALOGPUTREAL(IDF_REAL4,MPW%YMAX,'(F10.2)')

 !## settings
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB4)
 CALL WDIALOGPUTIMAGE(ID_OPEN1,ID_ICONOPEN) !## open folder with Regis files
 CALL WDIALOGPUTIMAGE(ID_OPEN2,ID_ICONOPEN) !## open folder with TOP.idf files
 CALL WDIALOGPUTIMAGE(ID_OPEN3,ID_ICONOPEN) !## open folder with BOT.idf files

 TXTFILE=TRIM(PREFVAL(1))//'\SETTINGS\Geoconnect.txt'
 IF(.NOT.GC_INIT())THEN; CALL GC_CLOSE(); RETURN; ENDIF
 !## put settings on tab4
 CALL GC_INIT_PUT()
 
 !## call to routine that 1. reads REGIS files, 2. fills IPFAC%FORM with Regis formation names, 
 !## 3. fills IPFAC%FACT with 1.0 and 4. put this in grid on tab2
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB2)
 IF(.NOT.GC_REGISFILES_GETLIST())RETURN
  
 IF(.NOT.GC_REGISFILES_PUT(ID_DGEOCONNECT_TAB2))RETURN
 IF(.NOT.GC_REGISFILES_PUT(ID_DGEOCONNECT_TAB3))RETURN
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS) !## saveas
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN)     !## open

 CALL WDIALOGSETTAB(ID_GCTAB,ID_DGEOCONNECT_TAB4)
 CALL WDIALOGFIELDSTATE(IDOK,0)
 CALL WDIALOGSHOW(-1,-1,0,2)

 END SUBROUTINE GC_MAIN_INIT

 !###======================================================================
 SUBROUTINE GC_COMPUTE_MAIN(IMODBATCH)
 !###======================================================================
 !# subroutine to manage all preprocessing steps
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH
 
 !## call to read settings-variables from ini-file and allocate memory
 SELECT CASE (GC_IFLAG)
  !## call to identify routine
  CASE (1)
   CALL GC_IDENTIFY(IMODBATCH)
  !## call to calculation-subroutine in MOD_GEOCONNECT 
  CASE (2)
   !## turn message off
   CALL UTL_MESSAGEHANDLE(0)
   !## call to compute
   CALL GC_PRE_COMPUTE(IMODBATCH)
   !## write variables to file depending on checkbox options
   CALL GC_PRE_COMPUTE_WRITE(IMODBATCH) 
   !## turn message on again
   CALL UTL_MESSAGEHANDLE(1)
  !## call to read postprocessing variables from ini-file
  CASE (3) 
   !## call to calculation-subroutine
   CALL GC_POST(IMODBATCH)
 END SELECT
 
 END SUBROUTINE GC_COMPUTE_MAIN

 !###======================================================================
 SUBROUTINE GC_IDENTIFY_INITGRID()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,N
 INTEGER,DIMENSION(:),ALLOCATABLE :: ICOLS
 
 !## refresh settings
 CALL GC_INIT_GET()
 
 !## get how many layers and fill grid accordingly
 N=SUM(IACTM); ALLOCATE(ICOLS(N)); ICOLS=1
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB1)
 CALL WGRIDCLEAR(IDF_GRID1); CALL WGRIDCOLUMNS(IDF_GRID1,N,ICOLS)
 DEALLOCATE(ICOLS)
 J=0; DO I=1,NLAYM
  IF(IACTM(I).EQ.1)THEN
   J=J+1
   CALL WGRIDLABELCOLUMN(IDF_GRID1,J,'Layer '//TRIM(ITOS(I)))
  ENDIF
 ENDDO
  
 END SUBROUTINE GC_IDENTIFY_INITGRID

 !###======================================================================
 SUBROUTINE GC_IDENTIFY(IMODBATCH)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH
 INTEGER :: I,J,K,N,ITYPE,IROW,ICOL
 REAL :: XC,YC
 REAL,ALLOCATABLE,DIMENSION(:) :: TM,BM
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 !## read all idf's with model top and bot values opening files only
 IF(.NOT.GC_READ_MODELDATA(0))RETURN
 ALLOCATE(TM(NLAYM),BM(NLAYM))
  
 CALL WGRIDROWS(IDF_GRID1,N) 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB1)

 CALL WCURSORSHAPE(ID_CURSORIDFVALUE)
 CALL IGRCOLOURN(INVERSECOLOUR(WRGB(255,0,0))) 
 IROW=0; ICOL=0
  
 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (MOUSEMOVE)
   
    CALL WINDOWSELECT(0)
    CALL WINDOWOUTSTATUSBAR(1,'X:'//TRIM(ITOS(INT(MESSAGE%GX)))//' m, Y:'//TRIM(ITOS(INT(MESSAGE%GY)))//' m')
    XC=MESSAGE%GX; YC=MESSAGE%GY
    
    !## remove previous rectangle
    CALL UTL_PLOTLOCATIONIDF(TOPM(1),IROW,ICOL)
    CALL IDFIROWICOL(TOPM(1),IROW,ICOL,XC,YC)
    !## draw new rectangle
    CALL UTL_PLOTLOCATIONIDF(TOPM(1),IROW,ICOL)
   
    CALL GC_IDENTIFY_COMPUTE(MESSAGE%GX,MESSAGE%GY,TM,BM)

    !## fill results in grid
    CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB1) 
    N=0; DO I=1,NLAYR
     !## none of the layers contains current formation, skip it
     IF(SUM(IPFAC(I)%FVAL).LE.0.0)CYCLE
     !## current formation contains results, fractions gt 0.0
      N=N+1; CALL WGRIDLABELROW(IDF_GRID1,N,TRIM(IPFAC(I)%FORM))
      K=0; DO J=1,NLAYM
      !## skip deselected layer
      IF(IACTM(J).EQ.0)CYCLE
      K=K+1; CALL WGRIDPUTCELLREAL(IDF_GRID1,K,N,IPFAC(I)%FVAL(J))
     ENDDO
    ENDDO

   CASE (MOUSEBUTDOWN)
    EXIT

    !## bitmap scrolled, renew top-left pixel coordinates
   CASE (BITMAPSCROLLED)
    MPW%IX=MESSAGE%VALUE1
    MPW%IY=MESSAGE%VALUE2

   CASE (EXPOSE)
!    CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)
!    CALL WDIALOGSELECT(ID_DIDFINFO)

  END SELECT

 ENDDO
 
 !## remove previous rectangle
 CALL UTL_PLOTLOCATIONIDF(TOPM(1),IROW,ICOL)

 CALL WCURSORSHAPE(CURARROW)
 DEALLOCATE(TM,BM)
  
 END SUBROUTINE GC_IDENTIFY

 !###======================================================================
 SUBROUTINE GC_IDENTIFY_COMPUTE(XC,YC,TM,BM)
 !###======================================================================
 IMPLICIT NONE
 REAL,INTENT(IN) :: XC,YC
 REAL,DIMENSION(:),INTENT(INOUT) :: TM,BM
 INTEGER :: I,J,IKHR,IKVR
 REAL :: TR,BR,Z1,Z2
 CHARACTER(LEN=52) :: FTYPE
 
 !## get top/bottom of current location in model grid
 DO I=1,NLAYM
  TM(I)=IDFGETXYVAL(TOPM(I),XC,YC) 
  BM(I)=IDFGETXYVAL(BOTM(I),XC,YC) 
 ENDDO
 
 !## scan all files for current location
 DO I=1,NLAYR
  
  !## initiate fractions
  IPFAC(I)%FVAL=0.0

  !## take the next if current regisfiles - not concerning kh values
  IKHR=0; IKVR=0; IF(.NOT.GC_READ_REGISDATA(I,IKHR,IKVR,FTYPE,0))CYCLE

  TR=IDFGETXYVAL(TOPR,XC,YC)
  BR=IDFGETXYVAL(BOTR,XC,YC)

  !## skip nodata
  IF(TR.EQ.TOPR%NODATA.OR.BR.EQ.BOTR%NODATA)CYCLE

  !## assign fraction to different modellayers
  DO J=1,NLAYM
   !## model contains nodata - skip it
   IF(TM(J).EQ.TOPM(J)%NODATA.OR.BM(J).EQ.BOTM(J)%NODATA)CYCLE

   Z1=MIN(TR,TM(J)); Z2=MAX(BR,BM(J))
   IF(Z1.GT.Z2.AND.TM(J).GT.BM(J))THEN
    IPFAC(I)%FVAL(J)=(Z1-Z2)/(TM(J)-BM(J)) 
   ENDIF     
  ENDDO
    
 ENDDO
 
 END SUBROUTINE GC_IDENTIFY_COMPUTE

 !###======================================================================
 SUBROUTINE GC_PRE_COMPUTE(IMODBATCH)
 !###======================================================================
 !# subroutine to compute K-values for preprocessing purposes 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH !# =1 Only write to dos-window if started in batch modus
 INTEGER :: I,J,IROW,ICOL,ILAY,IKHR,IKVR
 REAL :: TR,BR,Z1,Z2,F,KVAL,XTOP,XBOT,KV
 CHARACTER(LEN=52) :: FTYPE

 !## read all idf's with model top and bot values 
 IF(.NOT.GC_READ_MODELDATA(1))RETURN
 
 !## get factors "regis"-files
 IF(.NOT.GC_REGISFILES_GET(IMODBATCH))RETURN
 
 DO I=1,NLAYM
  KDHIDF(I)%X=0.0
  KDVIDF(I)%X=0.0
  KVAIDF(I)%X=0.0
  KHVIDF(I)%X=0.0
 ENDDO
 DO I=1,NLAYM-1
  CIDF(I)%X=0.0; KVVIDF(I)%X=0.0
 ENDDO  

 !## read/process
 DO I=1,NLAYR
  
  !## take the next if current regisfiles does not have khv of kvv
  IKHR=1; IKVR=1; IF(.NOT.GC_READ_REGISDATA(I,IKHR,IKVR,FTYPE,1))CYCLE
     
  !## process data
  IF(IMODBATCH.EQ.1)WRITE(*,'(A,I)') 'Process data for layer: ',I
  IF(IMODBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Process Formation: '//TRIM(FTYPE)//' '//TRIM(RTOS(REAL(I*100)/REAL(NLAYR),'F',2))//'%')

  DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL
   
   TR=TOPR%X(ICOL,IROW); BR=BOTR%X(ICOL,IROW)   
   IF(TR.EQ.TOPR%NODATA.OR.BR.EQ.BOTR%NODATA)CYCLE
      
   DO ILAY=1,NLAYM

    !## compute KDH (horizontal transmissivity) 
    XTOP=TOPM(ILAY)%X(ICOL,IROW); XBOT=BOTM(ILAY)%X(ICOL,IROW)
    Z1=MIN(TR,XTOP); Z2=MAX(BR,XBOT)
    IF(Z1.GT.Z2.AND.XTOP.GT.XBOT)THEN
     F=(Z1-Z2)/(XTOP-XBOT) 
     !## assign maximum k values for aquifers
     KVAL=0.0
     !## found horizontal permeability
     IF(IKHR.EQ.1)THEN
      KVAL=MAX(KVAL,KHR%X(ICOL,IROW))
     !## if not, try vertical permeability
     ELSE
      IF(IKVR.EQ.1)KVAL=MAX(KVAL,(3.0*KVR%X(ICOL,IROW)))
     ENDIF
     !## sum horizontal transmissivity for each model layer by using fraction grids and formationfactors of model
     !## and select the correct formation factor per Regislayer/modellayer, if IPFAC is filled, else IPFAC(J)%FACT=1.0
     DO J=1,SIZE(IPFAC%FORM)
      !## stored in IPFAC by reading in iPEST-files
      IF(TRIM(FTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN 
       KDHIDF(ILAY)%X(ICOL,IROW)=KDHIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*IPFAC(J)%FACT)
      ENDIF
     ENDDO
    
     !## compute KDV (vertical transmissivity) model-values for aquifers
     KVAL=0.0
     !## found vertical permeability
     IF(IKVR.EQ.1)THEN
      KVAL=MAX(KVAL,KVR%X(ICOL,IROW))
     !## if not, try the horizontal permeability
     ELSE
      IF(IKHR.EQ.1)KVAL=MAX(KVAL,(0.3*KHR%X(ICOL,IROW)))
     ENDIF
     !## sum vertical transmissivity for each model layer
     !## if IPFAC is filled, else IPFAC(J)%FACT=1.0
     DO J=1,SIZE(IPFAC%FORM)
      !## stored in IPFAC by reading in iPEST-files
      IF(TRIM(FTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN 
       KDVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*IPFAC(J)%FACT)
      ENDIF
     ENDDO
        
    ENDIF
   ENDDO

   !## compute fractions for aquitards, resistance of aquitard
   DO ILAY=1,NLAYM-1    
    XTOP=BOTM(ILAY)%X(ICOL,IROW)
    XBOT=TOPM(ILAY+1)%X(ICOL,IROW)
    Z1=MIN(TR,XTOP); Z2=MAX(BR,XBOT)
    !## fraction in aquitards
    IF(Z1.GT.Z2)THEN
     F=(Z1-Z2)/(XTOP-XBOT)
     !## assign minimum values for aquitards
     KVAL=10.0E10
     !## found vertical permeability
     IF(IKVR.EQ.1)THEN
      KVAL=MIN(KVAL,KVR%X(ICOL,IROW))
     !## if not, try the horizontal permeability
     ELSE
      IF(IKHR.EQ.1)KVAL=MIN(KVAL,(0.3*KHR%X(ICOL,IROW)))
     ENDIF
     IF(KVAL.GT.0.0)THEN
      !## sum up the total resistance
      DO J=1,SIZE(IPFAC%FORM)
       !## stored in IPFAC by reading in iPEST-files
       IF(TRIM(FTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN
        CIDF(ILAY)%X(ICOL,IROW)=CIDF(ILAY)%X(ICOL,IROW)+(((Z1-Z2)/KVAL)*IPFAC(J)%FACT)
       ENDIF
      ENDDO
     ENDIF
        
    ENDIF
   ENDDO
   
  ENDDO; ENDDO
    
 ENDDO 
 
 !## compute KH,KV,KVA,KVV 
 DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL; DO ILAY=1,NLAYM
  TR=TOPM(ILAY)%X(ICOL,IROW); BR=BOTM(ILAY)%X(ICOL,IROW)   
  IF(TR.EQ.TOPM(ILAY)%NODATA.OR.BR.EQ.BOTM(ILAY)%NODATA)CYCLE
  IF(TR-BR.GT.0.0)THEN
   !## KH-value modellayer
   KHVIDF(ILAY)%X(ICOL,IROW)=KDHIDF(ILAY)%X(ICOL,IROW)/ABS(TR-BR) 
   IF(KHVIDF(ILAY)%X(ICOL,IROW).NE.0.0)THEN 
    !## KV-value modellayer
    KV=KDVIDF(ILAY)%X(ICOL,IROW)/ABS(TR-BR) 
    !## KVA-value modellayer (vertical anisotropy)
    KVAIDF(ILAY)%X(ICOL,IROW)=KV/KHVIDF(ILAY)%X(ICOL,IROW)
   ENDIF
  ENDIF
  !## compute vertical permeability
  IF(ILAY.LT.NLAYM)THEN
   TR=BOTM(ILAY)%X(ICOL,IROW); BR=TOPM(ILAY+1)%X(ICOL,IROW)   
   IF(TR.EQ.TOPM(ILAY)%NODATA.OR.BR.EQ.BOTM(ILAY)%NODATA)CYCLE
   IF(CIDF(ILAY)%X(ICOL,IROW).LE.0.0)THEN
    CIDF(ILAY)%X(ICOL,IROW)= 0.0
   ELSE
    KVVIDF(ILAY)%X(ICOL,IROW)=ABS(TR-BR)/CIDF(ILAY)%X(ICOL,IROW) !## KVV-value modellayer
   ENDIF
  ENDIF
 ENDDO; ENDDO; ENDDO

 END SUBROUTINE GC_PRE_COMPUTE

 !###======================================================================
 SUBROUTINE GC_PRE_COMPUTE_WRITE(IMODBATCH)
 !###======================================================================
 !# subroutine to calculate KH,KV,KVA and write K- and C-values to file (KHV, KVV, KVA, KDW and VCW)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH !# =1 Only write to dos-window if started in batch modus
 INTEGER :: I
 CHARACTER(LEN=256) :: LINE
  
 IF(IMODBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Write data ...')
 IF(IMODBATCH.EQ.1)WRITE(*,'(A)') 'Write data ...'

 !## option only write KHV, KVV and KVA
 IF(ISAVEK.EQ.1)THEN 
  DO I=1,SIZE(KHVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_khv_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KHVIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(KVVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kvv_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KVVIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(KVAIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kva_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KVAIDF(I),TRIM(LINE),1))RETURN
  ENDDO
 ENDIF 
 
 !## option only write KDW and VCW
 IF(ISAVEC.EQ.1)THEN 
  DO I=1,SIZE(KDHIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kd_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KDHIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(CIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_vc_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(CIDF(I),TRIM(LINE),1))RETURN
  ENDDO
 ENDIF
 
 IF(IMODBATCH.EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(4,'')
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully saved the files in'//CHAR(13)//TRIM(OUTPUTFOLDER),'Information')
 ENDIF

 IF(IMODBATCH.EQ.1)WRITE(*,'(A)') 'Finished writing data to file(s).'
 
 END SUBROUTINE GC_PRE_COMPUTE_WRITE

 !###======================================================================
 LOGICAL FUNCTION GC_IPEST_READ(FNAME,NR)
 !###======================================================================
 !# subroutine to read IPEST factors from file
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: NR
 INTEGER :: I,IU,IOS
 
 GC_IPEST_READ=.FALSE.
 IPFAC%FORM=''
 IPFAC%FACT=1.0
 
 !#Read variables from file into IPFAC-array
 IF(TRIM(FNAME).NE.'')THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)THEN; WRITE(*,'(A)') 'Can not open file: ['//TRIM(FNAME)//'] for reading'; RETURN; ENDIF
  DO I=1,NLAYM !# loop over total amount of modellayers
 !  IPFAC(I)%FORM=
 !  IPFAC(I)%FACT=
  ENDDO
 ENDIF
 
 GC_IPEST_READ=.TRUE.
 
 END FUNCTION GC_IPEST_READ

! !###======================================================================
! LOGICAL FUNCTION GC_IPEST_RESET()
! !###====================================================================== 
! IMPLICIT NONE
! INTEGER :: I
! 
! DO I=1,NLAYR
!  IPFAC(I)%FACT=1.0
!  CALL WGRIDPUTCELLREAL(IDF_GRID1,2,I,IPFAC(I)%FACT,'(F10.3)')
! ENDDO
! 
! END FUNCTION GC_IPEST_RESET
 
 !###======================================================================
 LOGICAL FUNCTION GC_REGISFILES_GETLIST()
 !###======================================================================
 !# function to get REGIS files onto the dialog and used for Refresh option
 IMPLICIT NONE
 INTEGER :: I,J,K
 
 GC_REGISFILES_GETLIST=.FALSE.
 
 IF(ASSOCIATED(REGISFILES))DEALLOCATE(REGISFILES)
 
 !## define subdirections for needed REGIS-files
 IF(.NOT.UTL_DIRINFO_POINTER(REGISFOLDER,'*-T-CK.IDF',REGISFILES,'F'))RETURN
 
 IF(.NOT.ASSOCIATED(REGISFILES))RETURN
 
 !## number of regis files - based upon the top-files
 NLAYR=SIZE(REGISFILES)
 IF(.NOT.ALLOCATED(IPFAC))ALLOCATE(IPFAC(NLAYR))
 DO I=1,NLAYR
  NULLIFY(IPFAC(I)%FVAL); ALLOCATE(IPFAC(I)%FVAL(NLAYM))
  IPFAC(I)%FVAL=0.0; IPFAC(I)%FACT=1.0; IPFAC(I)%IGRP=I
 ENDDO
  
 GC_REGISFILES_GETLIST=.TRUE.
 
 END FUNCTION GC_REGISFILES_GETLIST

 !###======================================================================
 LOGICAL FUNCTION GC_REGISFILES_PUT(DID)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: DID
 INTEGER :: I,J,K
 
 GC_REGISFILES_PUT=.FALSE.
 
 IF(.NOT.ASSOCIATED(REGISFILES))RETURN
 
 CALL WDIALOGSELECT(DID)
 CALL WGRIDROWS(IDF_GRID1,NLAYR)

 DO I=1,NLAYR

  CALL WGRIDLABELROW(IDF_GRID1,I,TRIM(ITOS(I)))

  J=INDEX(REGISFILES(I),'\',.TRUE.)+1; K=INDEX(REGISFILES(I)(J:),'-')
  IPFAC(I)%FORM=TRIM(REGISFILES(I)(J:K-1))
  IPFAC(I)%FACT=1.0  
  CALL WGRIDPUTCELLSTRING(IDF_GRID1,1,I,TRIM(IPFAC(I)%FORM))
  CALL WGRIDSTATECELL    (IDF_GRID1,1,I,2)
  IF(DID.EQ.ID_DGEOCONNECT_TAB2)THEN
   CALL WGRIDPUTCELLREAL  (IDF_GRID1,2,I,IPFAC(I)%FACT,'(F10.3)')
  ELSEIF(DID.EQ.ID_DGEOCONNECT_TAB3)THEN
   CALL WGRIDPUTCELLINTEGER(IDF_GRID1,2,I,IPFAC(I)%IGRP)
  ENDIF
 ENDDO
 
 GC_REGISFILES_PUT=.TRUE.
 
 END FUNCTION GC_REGISFILES_PUT

 !###======================================================================
 LOGICAL FUNCTION GC_REGISFILES_GET(IMODBATCH)
 !###======================================================================
 !# function to get REGIS files from the dialog
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH
 INTEGER :: I
 
 GC_REGISFILES_GET=.FALSE.

 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB2)
 DO I=1,NLAYR
 ! CALL WGRIDGETCELLSTRING(IDF_GRID1,1,I,TRIM(IPFAC(I)%FORM))
 !  IF(.NOT.ASSOCIATED(REGISFILES))RETURN
  CALL WGRIDGETCELLREAL(IDF_GRID1,2,I,IPFAC(I)%FACT)
 ENDDO
 
 GC_REGISFILES_GET=.TRUE.
 
 END FUNCTION GC_REGISFILES_GET

 !###======================================================================
 LOGICAL FUNCTION GC_READ_MODELDATA(IREAD)
 !###======================================================================
 !## function to read model top and bot files
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IREAD
 INTEGER :: ILAY,I
 
 GC_READ_MODELDATA=.FALSE.
 
 DO ILAY=1,NLAYM

  !## get TOP filename and add layer number
  TOPM(ILAY)%FNAME=TRIM(TOPFOLDER)//'\TOP_L'//TRIM(ITOS(ILAY))//'.IDF'

  IF(IDF%NROW.EQ.0.OR.IDF%NCOL.EQ.0)THEN
   IF(.NOT.IDFREAD(TOPM(ILAY),TOPM(ILAY)%FNAME,IREAD))RETURN
  ELSE
   !## scale mean
   CALL IDFCOPY(IDF,TOPM(ILAY))
   IF(.NOT.IDFREADSCALE(TOPM(ILAY)%FNAME,TOPM(ILAY),2,0,0.0,0))RETURN 
  ENDIF

  !## get BOTTOM filename and add layer number
  BOTM(ILAY)%FNAME=TRIM(BOTFOLDER)//'\BOT_L'//TRIM(ITOS(ILAY))//'.IDF'
  IF(IDF%NROW.EQ.0.OR.IDF%NCOL.EQ.0)THEN
   IF(.NOT.IDFREAD(BOTM(ILAY),BOTM(ILAY)%FNAME,IREAD))RETURN
  ELSE
   !## scale mean
   CALL IDFCOPY(IDF,BOTM(ILAY))
   IF(.NOT.IDFREADSCALE(BOTM(ILAY)%FNAME,BOTM(ILAY),2,0,0.0,0))RETURN 
  ENDIF

 ENDDO
 
 !## copy IDF settings
 DO I=1,SIZE(CIDF);   CALL IDFCOPY(TOPM(1),CIDF(I)); ENDDO 
 DO I=1,SIZE(KDHIDF); CALL IDFCOPY(TOPM(1),KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFCOPY(TOPM(1),KDVIDF(I)); ENDDO 
 DO I=1,SIZE(KHVIDF); CALL IDFCOPY(TOPM(1),KHVIDF(I)); ENDDO 
 DO I=1,SIZE(KVVIDF); CALL IDFCOPY(TOPM(1),KVVIDF(I)); ENDDO 
 DO I=1,SIZE(KVAIDF); CALL IDFCOPY(TOPM(1),KVAIDF(I)); ENDDO 
 CALL IDFCOPY(TOPM(1),TOPR)
 CALL IDFCOPY(TOPM(1),BOTR)
 CALL IDFCOPY(TOPM(1),KHR)
 CALL IDFCOPY(TOPM(1),KVR) 
 
 GC_READ_MODELDATA=.TRUE.
 
 END FUNCTION GC_READ_MODELDATA
  
 !###======================================================================
 LOGICAL FUNCTION GC_READ_REGISDATA(IFILE,IKHR,IKVR,FTYPE,IREAD)
 !###======================================================================
 !## function to collect the data from the REGIS files.
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFILE,IREAD
 INTEGER,INTENT(INOUT) :: IKHR,IKVR
 INTEGER :: I,J,K,JKHR,JKVR
 CHARACTER(LEN=52),INTENT(OUT) :: FTYPE
 CHARACTER(LEN=256) :: FNAME
 
 GC_READ_REGISDATA=.FALSE.

 !## copy request parameters
 JKHR=IKHR; JKVR=IKVR

 !## we assume that formation is before first "-"-sign and after "\"-sign, so
 !## apz1-t-ck.idf - formation becomes apz1
 J=INDEX(REGISFILES(IFILE),'\',.TRUE.)+1; K=INDEX(REGISFILES(IFILE)(J:),'-')-1
 FTYPE=REGISFILES(IFILE)(J:K) 

 !## top= formationname-t-ck,  thus  apz1-t-ck.idf
 !## bot= formationname-b-ck,  thus  apz1-b-ck.idf
 !## khv= formationname-kh-sk, thus apz1-ks-sk.idf
 !## kvv= formationname-kv-ck, thus apz1-kv-sk.idf

 !## try top
 TOPR%FNAME=TRIM(REGISFOLDER)//'\'//TRIM(FTYPE)//'-T-CK.IDF'
 IF(IREAD.EQ.0)THEN
  !## open file only
  IF(.NOT.IDFREAD(TOPR,TOPR%FNAME,IREAD))RETURN 
 ELSE
  !## scale mean
  IF(.NOT.IDFREADSCALE(TOPR%FNAME,TOPR,2,0,0.0,0))RETURN 
 ENDIF
 
 !## try bot
 BOTR%FNAME=TRIM(REGISFOLDER)//'\'//TRIM(FTYPE)//'-B-CK.IDF'
 IF(IREAD.EQ.0)THEN
  !## open file only
  IF(.NOT.IDFREAD(BOTR,BOTR%FNAME,IREAD))RETURN 
 ELSE
  !## scale mean
  IF(.NOT.IDFREADSCALE(BOTR%FNAME,BOTR,2,0,0.0,0))RETURN 
 ENDIF

 !## try kh - if requested ikhr.eq.1
 IF(IKHR.EQ.1)THEN
  KHR%FNAME=TRIM(REGISFOLDER)//'\'//TRIM(FTYPE)//'-KH-SK.IDF'
  IKHR=1
  IF(IREAD.EQ.0)THEN
   IF(.NOT.IDFREAD(KHR,KHR%FNAME,0,IQ=1))IKHR=0
  ELSE
   IF(.NOT.IDFREADSCALE(KHR%FNAME,KHR,3,0,0.0,IOPTIONAL=1))IKHR=0
  ENDIF
 ENDIF
 
 !## try kv - if requested ikvr.eq.1
 IF(IKVR.EQ.1)THEN
  KVR%FNAME=TRIM(REGISFOLDER)//'\'//TRIM(FTYPE)//'-KV-SK.IDF'
  IKVR=1
  IF(IREAD.EQ.0)THEN
   IF(.NOT.IDFREAD(KVR,KVR%FNAME,0,IQ=1))IKVR=0
  ELSE
   IF(.NOT.IDFREADSCALE(KVR%FNAME,KVR,3,0,0.0,IOPTIONAL=1))IKVR=0
  ENDIF
 ENDIF
 
 !## No horizontal/vertical permeabilities found, formation will be skipped - if requested
 IF(JKHR.EQ.1.AND.JKVR.EQ.1)THEN
  IF(IKHR.EQ.0.AND.IKVR.EQ.0)RETURN
 ENDIF
 
 GC_READ_REGISDATA=.TRUE.
  
 END FUNCTION GC_READ_REGISDATA
 
 !###======================================================================
 SUBROUTINE GC_POST(IMODBATCH)
 !###======================================================================
 !# subroutine to compute K-values for postprocessing purposes
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH
 REAL :: XC,YC
 INTEGER :: IROW,ICOL,NU,I
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IGRP
  
 !## read/scale idf's with model top and bot values
 IF(.NOT.GC_READ_MODELDATA(1))RETURN
 
 ALLOCATE(IGRP(NLAYR)); IGRP=IPFAC%IGRP

 !## get number of unique groups
 CALL UTL_GETUNIQUE_INT(IGRP,NLAYR,NU)
 
 !## do for each group
 DO I=1,NU

  CALL GC_POST_COMPUTE(IGRP(I))
 
 ENDDO
 
 !## save results
! CALL GC_POST_COMPUTE_WRITE(IMODBATCH)

 !## deallocate
 DEALLOCATE(IGRP)
 
 END SUBROUTINE GC_POST

 !###======================================================================
 SUBROUTINE GC_POST_COMPUTE(IGRP)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IGRP
 INTEGER :: I,J,IKHR,IKVR,IROW,ICOL
 REAL :: TR,BR,TM,BM,FM,FP,Z1,Z2
 CHARACTER(LEN=52) :: FTYPE
  
 !## scan all files for current location
 DO I=1,NLAYR
  
  !# skip this formation, not in current group
  IF(IPFAC(I)%IGRP.NE.IGRP)CYCLE
  
  !## take the next if current regisfiles - not concerning kh values
  IKHR=1; IKVR=1; IF(.NOT.GC_READ_REGISDATA(I,IKHR,IKVR,FTYPE,1))CYCLE

  DO IROW=1,IDF%NROW; DO ICOL=1,IDF%NCOL
  
   !## skip nodata
   TR=TOPR%X(ICOL,IROW); BR=BOTR%X(ICOL,IROW)
   IF(TR.EQ.TOPR%NODATA.OR.BR.EQ.BOTR%NODATA)CYCLE

   !## assign fraction to different modellayers
   DO J=1,NLAYM

    !## model contains nodata - skip it
    TM=TOPM(J)%X(ICOL,IROW); BM=BOTM(J)%X(ICOL,IROW)
    IF(TM.EQ.TOPM(J)%NODATA.OR.BM.EQ.BOTM(J)%NODATA)CYCLE

    Z1=MIN(TR,TM); Z2=MAX(BR,BM)
    IF(Z1.GT.Z2.AND.TM.GT.BM)THEN
     !## fraction
     FM=(Z1-Z2)/(TM-BM)
     !## ipest factor
     FP= IPFAC(I)%FACT
    ENDIF

   ENDDO
    
  ENDDO; ENDDO
  
 ENDDO
 
 END SUBROUTINE GC_POST_COMPUTE
 
  !###======================================================================
 SUBROUTINE GC_POST_COMPUTE_WRITE(IMODBATCH)
 !###======================================================================
 !# subroutine to calculate KH,KV,KVA and write K- and C-values to file (KHV, KVV, KVA, KDW and VCW)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH !# =1 Only write to dos-window if started in batch modus
 INTEGER :: I
 CHARACTER(LEN=256) :: LINE
  
 IF(IMODBATCH.EQ.0)CALL WINDOWOUTSTATUSBAR(4,'Write data ...')
 IF(IMODBATCH.EQ.1)WRITE(*,'(A)') 'Write data ...'

! !## option only write KHV, KVV and KVA
! IF(ISAVEK.EQ.1)THEN 
!  DO I=1,SIZE(KHVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_khv_l'//TRIM(ITOS(I))//'.idf'
!   IF(.NOT.IDFWRITE(KHVIDF(I),TRIM(LINE),1))RETURN
!  ENDDO
!  DO I=1,SIZE(KVVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kvv_l'//TRIM(ITOS(I))//'.idf'
!   IF(.NOT.IDFWRITE(KVVIDF(I),TRIM(LINE),1))RETURN
!  ENDDO
!  DO I=1,SIZE(KVAIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kva_l'//TRIM(ITOS(I))//'.idf'
!   IF(.NOT.IDFWRITE(KVAIDF(I),TRIM(LINE),1))RETURN
!  ENDDO
! ENDIF 
! 
! !## option only write KDW and VCW
! IF(ISAVEC.EQ.1)THEN 
!  DO I=1,SIZE(KDHIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kd_l'//TRIM(ITOS(I))//'.idf'
!   IF(.NOT.IDFWRITE(KDHIDF(I),TRIM(LINE),1))RETURN
!  ENDDO
!  DO I=1,SIZE(CIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_vc_l'//TRIM(ITOS(I))//'.idf'
!   IF(.NOT.IDFWRITE(CIDF(I),TRIM(LINE),1))RETURN
!  ENDDO
! ENDIF
 
 IF(IMODBATCH.EQ.0)THEN
  CALL WINDOWOUTSTATUSBAR(4,'')
  CALL WMESSAGEBOX(OKONLY,INFORMATIONICON,COMMONOK,'Succesfully saved the files in'//CHAR(13)//TRIM(OUTPUTFOLDER),'Information')
 ENDIF

 IF(IMODBATCH.EQ.1)WRITE(*,'(A)') 'Finished writing data to file(s).'
 
 END SUBROUTINE GC_POST_COMPUTE_WRITE
 
 !###======================================================================
 SUBROUTINE GC_CLOSE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CALL WINDOWSELECT(0); CALL WMENUSETSTATE(ID_GEOCONNECT,2,0)

 CALL WDIALOGSELECT(ID_DGEOCONNECT); CALL WDIALOGUNLOAD()
 
 CALL GC_DEALLOCATE()
 IF(ALLOCATED(IACTM))THEN; DEALLOCATE(IACTM); ENDIF
 IF(ALLOCATED(IPFAC))THEN
  DO I=1,SIZE(IPFAC)
   IF(ASSOCIATED(IPFAC(I)%FVAL))DEALLOCATE(IPFAC(I)%FVAL)
  ENDDO
  DEALLOCATE(IPFAC)
 ENDIF
 IF(ASSOCIATED(REGISFILES))DEALLOCATE(REGISFILES)
 
 END SUBROUTINE GC_CLOSE

END MODULE MOD_GEOCONNECT