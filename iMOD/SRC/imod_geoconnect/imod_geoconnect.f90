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

MODULE MOD_GEOCONNECT

USE WINTERACTER
USE RESOURCE
USE MOD_GEOCONNECT_PAR
USE MOD_UTL, ONLY : UTL_GETUNIT,UTL_SUBST,ITOS,UTL_DIRINFO_POINTER,UTL_CAP
USE MOD_IDF, ONLY : IDFGETXYVAL,IDFREADSCALE,IDFWRITE,IDFREAD
USE MOD_OSD, ONLY : OSD_OPEN

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
   SELECT CASE (ITYPE)
    
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
      CASE (IDCANCEL)
       CALL GC_CLOSE()
      CASE (IDHELP)
      CASE (IDOK) !#UITGRIJZEN INDIEN NIET ALLE BENODIGDE VARIABELEN ZIJN INGEVULD OP SETTINGSTAB+PROCESSINGTAB(S)!!
       IF(ITAB.EQ.3)THEN
        CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'Are you sure to compute the preprocessing variables?','Question')
        IF(WINFODIALOG(4).EQ.1)THEN
         CALL GC_INIT_READ() !# Read initial settings from Settings-tab
         CALL GC_INIT_PREPROCESSING_READ() !# Read initial settings from Preprocessing-tab
         CALL GC_COMPUTE_MAIN(0) !# Compute preprocessing 
        ENDIF
       ELSEIF(ITAB.EQ.4)THEN
        !# call postprocessing routines  
        CALL WMESSAGEBOX(YESNO,QUESTIONICON,COMMONYES,'Are you sure to compute the postprocessing files?','Question')              
       ENDIF
     END SELECT
     CASE (ID_SAVEAS_INI)
      !# ...include subroutine to open save-window and put FNAME in memory
      CALL GC_INIT_PREPROCESSING_WRITE(FNAME) !# Write *.ini-file
     CASE (ID_OPEN_INI)
      !# ...include subroutine to open open-window and put IU in memory + settings on window
      IF(.NOT.GC_INIT_PREPROCESSING(IU))RETURN !# read *.ini-file

    CASE (FIELDCHANGED)
     !# UITGRIJZGEN OPTIES
    CASE (TABCHANGED)
     SELECT CASE (MESSAGE%VALUE2)
      CASE (ID_DGEOCONNECT_TAB3)
       ITAB=3
      CASE (ID_DGEOCONNECT_TAB4)
       ITAB=4 
     END SELECT
   END SELECT
 
 ! CASE (ID_DGEOCONNECT_TAB1)
 
 ! CASE (ID_DGEOCONNECT_TAB2)  
 
  CASE (ID_DGEOCONNECT_TAB3)
   SELECT CASE (ITYPE)
    CASE (PUSHBUTTON)
     SELECT CASE (MESSAGE%VALUE1)
     END SELECT
    CASE (FIELDCHANGED)
    !# uitgrijzen...
   END SELECT
   
!  CASE (ID_DGEOCONNECT_TAB4)
  
 END SELECT

! clean memory???
  
 END SUBROUTINE GC_MAIN

 !###======================================================================
 SUBROUTINE GC_MAIN_INIT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I

 !## clean everything
 !CALL GC_DEALLOCATE()
 
 !##Open Geoconnect dialog 
 CALL WDIALOGLOAD(ID_DGEOCONNECT,ID_DGEOCONNECT)
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB1) 
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB2)
 CALL WDIALOGPUTIMAGE(ID_OPEN_REGIS,ID_ICONOPEN) !# open folder with Regis files
 CALL WDIALOGPUTIMAGE(ID_OPEN_TOP,ID_ICONOPEN) !# open folder with TOP.idf files
 CALL WDIALOGPUTIMAGE(ID_OPEN_BOT,ID_ICONOPEN) !# open folder with BOT.idf files
 CALL WDIALOGPUTIMAGE(ID_SAVEAS,ID_ICONSAVEAS) !# save settings as *.txt file
 CALL WDIALOGPUTIMAGE(ID_OPEN,ID_ICONOPEN) !# open settings *.txt file

 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB3)
 CALL WDIALOGPUTIMAGE(ID_OPEN_OUTFOLDER,ID_ICONOPEN) !# open folder where output files need to be stored
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT_TAB4)
 CALL WDIALOGPUTIMAGE(ID_IDENTIFY,ID_ICONPIPET)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS_CSV,ID_ICONSAVEAS)
 CALL WDIALOGPUTIMAGE(ID_OPEN_CSV,ID_ICONOPEN)
 
 CALL WDIALOGSELECT(ID_DGEOCONNECT)
 CALL WDIALOGPUTIMAGE(ID_SAVEAS_INI,ID_ICONSAVEAS) !# save settings as *.ini file
 CALL WDIALOGPUTIMAGE(ID_OPEN_INI,ID_ICONOPEN) !# open settings *.ini file

 CALL WDIALOGSETTAB(ID_GCTAB,ID_DGEOCONNECT_TAB2)

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
!    CALL GC_IDENTIFY()
   !## Call to calculation-subroutine in MOD_GEOCONNECT 
   CASE (2) 
    !## call to read preprocessing variables from ini-file
    CALL GC_PRE_COMPUTE(IMODBATCH)
    !## write variables to file depending on checkbox options
    CALL GC_PRE_COMPUTE_WRITE(IMODBATCH) 
   !## call to read postprocessing variables from ini-file
   CASE (3) 
!  CALL GC_POST()!# Call to calculation-subroutine in MOD_GEOCONNECT
  END SELECT
 
 END SUBROUTINE GC_COMPUTE_MAIN

! !###======================================================================
! SUBROUTINE GC_ID_COMPUTE(IMODBATCH)
! !###======================================================================
! !# subroutine to identify fractions per layer AND per x,y-location 
! IMPLICIT NONE
! INTEGER,INTENT(IN) :: IMODBATCH !# =1 Only write to dos-window if started in batch modus
! INTEGER :: ILAY,J
! REAL :: X_ID,Y_ID,TMVAL
! 
! !# Get specific coordinates for calculating fractions
! X_ID=
! Y_ID=
! 
! !## Read in names of needed files
! !# Model Top/Bot
! DO ILAY=1,NLAYM
!  FNAME=TRIM(TOPFOLDER(INDEX(TOPFOLDER,'\',.TRUE.)+1:));  FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
!  TOPM(ILAY)%FNAME=TRIM(TOPFOLDER(:INDEX(TOPFOLDER,'\',.TRUE.)-1))//'\'//TRIM(FNAME)
!  
!  FNAME=TRIM(BOTFOLDER(INDEX(BOTFOLDER,'\',.TRUE.)+1:));  FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
!  BOTM(ILAY)%FNAME=TRIM(BOTFOLDER(:INDEX(BOTFOLDER,'\',.TRUE.)-1))//'\'//TRIM(FNAME)
! ENDDO
! !# Regis Top/Bot
! J=INDEX(REGISFILES(IFILE),'\',.TRUE.)+1; K=INDEX(REGISFILES(IFILE)(J:),'-')
! FTYPE=REGISFILES(IFILE)(J:K) 
! TOPR%FNAME=TRIM(REGISFOLDER)//TRIM(FTYPE)//'T-CK.IDF'
! BOTR%FNAME=TRIM(REGISFOLDER)//TRIM(FTYPE)//'B-CK.IDF'
!
! !# Open TOPM,BOTM,TOPR,BOTR --> in functie, omdat dit 4x herhaald moet worden??
! I= !# ilay to compute fractions for
! TOPM(I)%IU=UTL_GETUNIT()
! IF(ICF.EQ.0)THEN
!  CALL OSD_OPEN(TOPM(I)%IU,FILE=TOPM%FNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
!                RECL=4,ACTION='READWRITE',IOSTAT=IOS)
! ELSEIF(ICF.EQ.1)THEN
!  CALL OSD_OPEN(TOPM(I)%IU,FILE=TOPM%FNAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT', &
!                RECL=1,ACTION='READWRITE',IOSTAT=IOS)
! ENDIF
! IF(IOS.NE.0)THEN
!  CALL WMESSAGEBOX(OKONLY,EXCLAMATIONICON,COMMONOK,'iMOD can not adjust current file'//CHAR(13)// &
!    TRIM(TOPM%FNAME)//CHAR(13)//'IDF has been marked probably as READ-ONLY','Error')
!  IF(TOPM(I)%IU.GT.0)CLOSE(IU)
!  RETURN
! ENDIF
! 
! TMVAL=IDFGETXYVAL(TOPM(I),X_ID,Y_ID) !# --> als output variabele teruggeven uit bovenstaande functie??
! BMVAL=IDFGETXYVAL(BOTM(I),X_ID,Y_ID) !# --> ook in bovenstaande functie??
! TRVAL=IDFGETXYVAL(TOPR(I),X_ID,Y_ID) !# --> ook in bovenstaande functie??
! BRVAL=IDFGETXYVAL(BOTR(I),X_ID,Y_ID) !# --> ook in bovenstaande functie??
! 
! !# loop over Regis layers
! !# loop over model layers
! !# compute fractions per model layer depending on Regis layers
!  FRAC=
!
!! INTEGER,INTENT(IN) :: IOPT
!! INTEGER :: I,IU,ILAY,NLAYR,ICOL,IROW
!! REAL :: F,Z1,Z2,XTOP,XBOT,TR,BR
!! CHARACTER(LEN=52) :: FTYPE
!!  
!! !## try to read all idf's with model Top and Bot values 
!! IF(.NOT.GC_READ_MODELDATA())RETURN
!!
!! !## get list of "regis"-files 
!! IF(.NOT.GC_GET_REGISFILES())RETURN
!! 
!! !# Open txt-file in case of iMODbatch
!! IF(IOPT.EQ.1)THEN
!!  IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTPUTFOLDER)//'identify.txt',STATUS='UNKNOWN',ACTION='WRITE')
!! ENDIF
!!  
!! DO I=1,NLAYR
!! 
!!  IF(.NOT.GC_GET_REGISDATA(I,IKHR,IKVR,FTYPE))RETURN
!! 
!!  DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL 
!!   TR=TOPR%X(ICOL,IROW); BR=BOTR%X(ICOL,IROW)   
!!   IF(TR.EQ.TOPR%NODATA.OR.BR.EQ.BOTR%NODATA)CYCLE
!!   DO ILAY=1,NLAYM
!!    XTOP=TOPM(ILAY)%X(ICOL,IROW); XBOT=BOTM(ILAY)%X(ICOL,IROW)
!!    Z1=MIN(TR,XTOP); Z2=MAX(BR,XBOT)
!!    IF(Z1.GT.Z2.AND.XTOP.GT.XBOT)THEN
!!     F=(Z1-Z2)/(XTOP-XBOT) 
!!    ENDIF     
!!   ENDDO
!!  ENDDO; ENDDO
!!  
!!!  DO ILAY=1,NLAYM
!!!   FVAL(ILAY)=IDFGETXYVAL(?,X_ID,Y_ID) !#fraction value related to layer and formation
!!!  ENDDO
!!
!!  !##Write FVAL to screen (GUI) or to txt-file (iMODBATCH)
!!  IF(IOPT.EQ.1)THEN
!!  !#Write grid to txt-file organized as columns=layers, rows=fractions per formation
!!   WRITE(IU,*) CTYPE,FVAL
!!  ELSEIF(IOPT.EQ.0)THEN
!!   !# Write grid to screen see figure in documentation
!!  ENDIF
!!
!! ENDDO
!! 
!! CLOSE(IU)
!! 
! END SUBROUTINE GC_ID_COMPUTE

 !###======================================================================
 SUBROUTINE GC_PRE_COMPUTE(IMODBATCH)
 !###======================================================================
 !# subroutine to compute K-values for preprocessing purposes 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IMODBATCH !# =1 Only write to dos-window if started in batch modus
 INTEGER :: I,J,IROW,ICOL,ILAY,IKHR,IKVR
 REAL :: TR,BR,Z1,Z2,F,KVAL,XTOP,XBOT
 CHARACTER(LEN=52) :: FTYPE

 !## read all idf's with model top and bot values  (detail: geen capitals in comments)
 IF(.NOT.GC_READ_MODELDATA())RETURN
 
 !## get list of "regis"-files
 IF(.NOT.GC_GET_REGISFILES())RETURN
 
 !## get iPEST variables
 !IF(.NOT.GC_READ_IPEST(IPEST,IPESTNR))RETURN
  
 !## read/process
 DO I=1,NLAYR
  
  !## take the next if current regisfiles does not have khv of kvv
  IF(.NOT.GC_GET_REGISDATA(I,IKHR,IKVR,FTYPE))CYCLE
     
  !## process data
  IF(IMODBATCH.EQ.1)WRITE(*,'(A,I)') 'Process data for layer: ',I
  
   DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL
   
   TR=TOPR%X(ICOL,IROW); BR=BOTR%X(ICOL,IROW)   
   IF(TR.EQ.TOPR%NODATA.OR.BR.EQ.BOTR%NODATA)CYCLE
      
   !## compute KDH and KDV model-values for aquifers
   DO ILAY=1,NLAYM
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
     IF(LEN_TRIM(IPEST).NE.0)THEN
      DO J=1,SIZE(IPFAC%FORM)
       IF(TRIM(FTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN !#stored in IPFAC by reading in iPEST-files
        KDHIDF(ILAY)%X(ICOL,IROW)=KDHIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*IPFAC(J)%FACT)
       ENDIF
      ENDDO
     ELSE
      KDHIDF(ILAY)%X(ICOL,IROW)=KDHIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*1.0)
     ENDIF
    
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
     IF(TRIM(IPEST).NE.'')THEN
      DO J=1,SIZE(IPFAC%FORM)
       IF(TRIM(FTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN !#stored in IPFAC by reading in iPEST-files
        KDVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*IPFAC(J)%FACT)
       ENDIF
      ENDDO
     ELSE
      KDVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*1.0)
     ENDIF
        
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
     !## sum up the total resistance
     IF(TRIM(IPEST).NE.'')THEN
      DO J=1,SIZE(IPFAC%FORM)
       IF(TRIM(FTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN !#stored in IPFAC by reading in iPEST-files
        IF(KVAL.GT.0.0)THEN
         CIDF(ILAY)%X(ICOL,IROW)=CIDF(ILAY)%X(ICOL,IROW)+(((Z1-Z2)/KVAL)*IPFAC(J)%FACT)
        ENDIF
       ENDIF
      ENDDO
     ELSE
      IF(KVAL.GT.0.0)THEN
       CIDF(ILAY)%X(ICOL,IROW)=CIDF(ILAY)%X(ICOL,IROW)+(((Z1-Z2)/KVAL)*1.0)
      ENDIF
     ENDIF
         
    ENDIF
   ENDDO
   
  ENDDO; ENDDO
    
 ENDDO 
 
 !## compute KH,KV,KVA,KVV 
 DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL; DO ILAY=1,NLAYM
  TR=TOPM(ILAY)%X(ICOL,IROW); BR=BOTM(ILAY)%X(ICOL,IROW)   
  IF(TR.EQ.TOPM(ILAY)%NODATA.OR.BR.EQ.BOTM(ILAY)%NODATA)CYCLE
  IF(TR-BR.LE.0.0)THEN
   KHIDF(ILAY)%X(ICOL,IROW)=0.0
   KVIDF(ILAY)%X(ICOL,IROW)=1.0
  ELSE
   KHIDF(ILAY)%X(ICOL,IROW)=KDHIDF(ILAY)%X(ICOL,IROW)/(TR-BR) !#KH-value modellayer
   KVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)/(TR-BR) !#KV-value modellayer
   IF(KHIDF(ILAY)%X(ICOL,IROW).EQ.0.0)THEN
    KVIDF(ILAY)%X(ICOL,IROW)=1.0
   ELSE
    KVAIDF(ILAY)%X(ICOL,IROW)=KVIDF(ILAY)%X(ICOL,IROW)/KHIDF(ILAY)%X(ICOL,IROW) !#KVA-value modellayer (vertical anisotropy)
   ENDIF
  ENDIF
  !## compute vertical permeability
  IF(ILAY.LT.NLAYM)THEN
   TR=BOTM(ILAY)%X(ICOL,IROW); BR=TOPM(ILAY+1)%X(ICOL,IROW)   
   IF(TR.EQ.TOPM(ILAY)%NODATA.OR.BR.EQ.BOTM(ILAY)%NODATA)CYCLE
   IF(CIDF(ILAY)%X(ICOL,IROW).LE.0.0)THEN
    CIDF(ILAY)%X(ICOL,IROW)= 0.0
   ELSE
    KVVIDF(ILAY)%X(ICOL,IROW)=(TR-BR)/CIDF(ILAY)%X(ICOL,IROW) !#KVV-value modellayer
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
  
 IF(IMODBATCH.EQ.1)WRITE(*,'(A)') 'Write data ...'

 IF(ISAVEK.EQ.1.AND.ISAVEC.EQ.0)THEN !# Option only write KHV, KVV and KVA
  DO I=1,SIZE(KDVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_khv_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KDVIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(KVVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kvv_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KVVIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(KVAIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kva_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KVAIDF(I),TRIM(LINE),1))RETURN
  ENDDO
 
 ELSEIF(ISAVEK.EQ.0.AND.ISAVEC.EQ.1)THEN !# Option only write KDW and VCW
  DO I=1,SIZE(KDHIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kd_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KDHIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(CIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_vc_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(CIDF(I),TRIM(LINE),1))RETURN
  ENDDO
 
 ELSEIF(ISAVEK.EQ.1.AND.ISAVEC.EQ.1)THEN !# Option to write all variables (KHV, KVV, KVA, KDW and VCW)
  DO I=1,SIZE(KDVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_khv_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KDVIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(KVVIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kvv_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KVVIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(KVAIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kva_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KVAIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(KDHIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_kd_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(KDHIDF(I),TRIM(LINE),1))RETURN
  ENDDO
  DO I=1,SIZE(CIDF); LINE=TRIM(OUTPUTFOLDER)//'\mdl_vc_l'//TRIM(ITOS(I))//'.idf'
   IF(.NOT.IDFWRITE(CIDF(I),TRIM(LINE),1))RETURN
  ENDDO
 ENDIF
 
 IF(IMODBATCH.EQ.1)WRITE(*,'(A)') 'Finished writing data to file(s).'
 
 END SUBROUTINE GC_PRE_COMPUTE_WRITE

 !###======================================================================
 LOGICAL FUNCTION GC_READ_IPEST(FNAME,NR)
 !###======================================================================
 !# subroutine to read IPEST factors from file
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: NR
 INTEGER :: I,IU,IOS
 
 GC_READ_IPEST=.FALSE.
 IPFAC%FORM=''
 IPFAC%FACT=1.0
 
 !#Read variables from file into IPFAC-array
 IF(TRIM(FNAME).NE.'')THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=FNAME,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=IOS)
  IF(IOS.NE.0)THEN; WRITE(*,'(A)') 'Can not open file: ['//TRIM(FNAME)//'] for reading'; RETURN; ENDIF
  DO I=1,NLAYM !# loop over total amount of modellayers
 !  IPFAC(I)%FORM=
 !  IPFAC(I)%FAC=
  ENDDO
 ENDIF
 
 GC_READ_IPEST=.TRUE.
 
 END FUNCTION GC_READ_IPEST

 !###======================================================================
 LOGICAL FUNCTION GC_READ_MODELDATA()
 !###======================================================================
 !## function to read model top and bot files
 IMPLICIT NONE
 INTEGER :: ILAY,I
 CHARACTER(LEN=52) :: FNAME
 
 GC_READ_MODELDATA=.FALSE.
 
 DO ILAY=1,NLAYM

  !## get TOP filename and add layer number
  FNAME=TRIM(TOPFOLDER(INDEX(TOPFOLDER,'\',.TRUE.)+1:));  FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
  TOPM(ILAY)%FNAME=TRIM(TOPFOLDER(:INDEX(TOPFOLDER,'\',.TRUE.)-1))//'\'//TRIM(FNAME)
  IF(.NOT.IDFREAD(TOPM(ILAY),TOPM(ILAY)%FNAME,1))RETURN
!  IF(.NOT.IDFREADSCALE(TOPM(ILAY)%FNAME,TOPM(ILAY),2,1,0.0,0))RETURN

  !## get BOTTOM filename and add layer number
  FNAME=TRIM(BOTFOLDER(INDEX(BOTFOLDER,'\',.TRUE.)+1:));  FNAME=TRIM(FNAME)//'_L'//TRIM(ITOS(ILAY))//'.IDF'
  BOTM(ILAY)%FNAME=TRIM(BOTFOLDER(:INDEX(BOTFOLDER,'\',.TRUE.)-1))//'\'//TRIM(FNAME)
  IF(.NOT.IDFREAD(BOTM(ILAY),BOTM(ILAY)%FNAME,1))RETURN
!  IF(.NOT.IDFREADSCALE(BOTM(ILAY)%FNAME,BOTM(ILAY),2,1,0.0,0))RETURN

 ENDDO
 
 !## copy IDF settings
 DO I=1,SIZE(CIDF);   CALL IDFCOPY(TOPM(1),CIDF(I)); ENDDO 
 DO I=1,SIZE(KDHIDF); CALL IDFCOPY(TOPM(1),KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFCOPY(TOPM(1),KDVIDF(I)); ENDDO 
 DO I=1,SIZE(KHIDF);  CALL IDFCOPY(TOPM(1),KHIDF(I)); ENDDO 
 DO I=1,SIZE(KVIDF);  CALL IDFCOPY(TOPM(1),KVIDF(I)); ENDDO
 DO I=1,SIZE(KVVIDF); CALL IDFCOPY(TOPM(1),KVVIDF(I)); ENDDO 
 DO I=1,SIZE(KVAIDF); CALL IDFCOPY(TOPM(1),KVAIDF(I)); ENDDO 
 CALL IDFCOPY(TOPM(1),TOPR)
 CALL IDFCOPY(TOPM(1),BOTR)
 CALL IDFCOPY(TOPM(1),KHR)
 CALL IDFCOPY(TOPM(1),KVR) 
 
 GC_READ_MODELDATA=.TRUE.
 
 END FUNCTION GC_READ_MODELDATA

 !###======================================================================
 LOGICAL FUNCTION GC_GET_REGISFILES()
 !###======================================================================
 !# function to get REGIS files
 IMPLICIT NONE
 INTEGER :: I
 
 GC_GET_REGISFILES=.FALSE.
 
 !## define subdirections for needed REGIS-files
 IF(.NOT.UTL_DIRINFO_POINTER(REGISFOLDER,'*.IDF',REGISFILES,'F'))RETURN
 
 IF(.NOT.ASSOCIATED(REGISFILES))RETURN
 
 !## number of regis files - based upon the top-files
 NLAYR=SIZE(REGISFILES)

 GC_GET_REGISFILES=.TRUE.
 
 END FUNCTION GC_GET_REGISFILES

 !###======================================================================
 LOGICAL FUNCTION GC_GET_REGISDATA(IFILE,IKHR,IKVR,FTYPE)
 !###======================================================================
 !## function to collect the data from the REGIS files.
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFILE
 INTEGER,INTENT(OUT) :: IKHR,IKVR
 INTEGER :: I,J,K
 CHARACTER(LEN=52),INTENT(OUT) :: FTYPE
 CHARACTER(LEN=256) :: FNAME
 
 GC_GET_REGISDATA=.FALSE.
   
 !## we assume that formation is before first "-"-sign and after "\"-sign, so
 !## apz1-t-ck.idf - formation becomes apz1
 J=INDEX(REGISFILES(IFILE),'\',.TRUE.)+1; K=INDEX(REGISFILES(IFILE)(J:),'-')
 FTYPE=REGISFILES(IFILE)(J:K) 

!BALNGRIJKE WIJZIGING - we gaan ervan uit dat alle regis data in de regisfolder staan !!!

 !## top= formationname-t-ck, thus  apz1-t-ck.idf
 !## bot= formationname-b-ck, thus  apz1-b-ck.idf
 !## khv= formationname-kh-sk, thus apz1-ks-sk.idf
 !## kvv= formationname-t-ck,  thus apz1-kv-sk.idf

 !## try top
 TOPR%FNAME=TRIM(REGISFOLDER)//TRIM(FTYPE)//'T-CK.IDF'
 IF(.NOT.IDFREADSCALE(TOPR%FNAME,TOPR,2,0,0.0,0))RETURN !## scale mean
 !## try bot
 BOTR%FNAME=TRIM(REGISFOLDER)//TRIM(FTYPE)//'B-CK.IDF'
 IF(.NOT.IDFREADSCALE(BOTR%FNAME,BOTR,2,0,0.0,0))RETURN !## scale mean
 !## try kh
 KHR%FNAME=TRIM(REGISFOLDER)//TRIM(FTYPE)//'KH-SK.IDF'
 IKHR=1; IF(.NOT.IDFREADSCALE(KHR%FNAME,KHR,3,0,0.0,IOPTIONAL=1))IKHR=0
 !## try kv
 KVR%FNAME=TRIM(REGISFOLDER)//TRIM(FTYPE)//'KV-SK.IDF'
 IKVR=1; IF(.NOT.IDFREADSCALE(KVR%FNAME,KVR,3,0,0.0,IOPTIONAL=1))IKVR=0
 
 !## No horizontal/vertical permeabilities found, formation will be skipped
 IF(IKHR.EQ.0.AND.IKVR.EQ.0)RETURN
 
 GC_GET_REGISDATA=.TRUE.
  
 END FUNCTION GC_GET_REGISDATA
 
 !###======================================================================
 SUBROUTINE GC_POST_COMPUTE()
 !###======================================================================
 !# subroutine to compute K-values for postprocessing purposes
 IMPLICIT NONE

 !## read all idf's with model top and bot values  (detail: geen capitals in comments)
 IF(.NOT.GC_READ_MODELDATA())RETURN
 
 !## get list of "regis"-files
 IF(.NOT.GC_GET_REGISFILES())RETURN
 
 !## etc... 

 END SUBROUTINE GC_POST_COMPUTE

 !###======================================================================
 SUBROUTINE GC_CLOSE()
 !###======================================================================
 IMPLICIT NONE

 CALL WDIALOGSELECT(ID_DGEOCONNECT); CALL WDIALOGUNLOAD()

 END SUBROUTINE GC_CLOSE


END MODULE MOD_GEOCONNECT