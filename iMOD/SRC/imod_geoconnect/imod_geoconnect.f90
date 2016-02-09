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

CONTAINS

 !###======================================================================
 SUBROUTINE GC_IDENTIFY(IOPT)
 !###======================================================================
 !# subroutine to manage all preprocessing steps
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 
 IF(IOPT.EQ.1)THEN !# iMODbatch
  CALL GC_ID_COMPUTE(IOPT)
 ELSEIF(IOPT.EQ.0)THEN !# iMOD GUI
  !# use CALL GC_ID_FIND() related to button 'identify' on dialog
 ENDIF
 
 END SUBROUTINE GC_IDENTIFY

 !###======================================================================
 SUBROUTINE GC_PRE(IOPT)
 !###======================================================================
 !# subroutine to manage all preprocessing steps
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 
 IF(IOPT.EQ.1)THEN !# iMODbatch
  CALL GC_READ_IPEST(IPEST,IPESTNR) !#FNAME=iPEST-CSV file (stored in "IPEST"-variable and read in GC_READ_PRE), #NR= number of block to be used from iPEST-file (stored in "IPESTNO"-variable and read in GC_READ_PRE).
  CALL GC_PRE_COMPUTE(IOPT)
 ELSEIF(IOPT.EQ.0)THEN !# iMOD GUI
  !# use CALL GC_READ_IPEST(FNAME,NR) related to button 'iPEST' on dialog
  !# use CALL GC_PRE_COMPUTE(IOPT) related to button 'Apply' on dialog
 ENDIF
 
 END SUBROUTINE GC_PRE

 !###======================================================================
 SUBROUTINE GC_POST(IOPT)
 !###======================================================================
 !# subroutine to manage all postprocessing steps
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 
 IF(IOPT.EQ.1)THEN !# iMODbatch
  CALL GC_POST_COMPUTE()
 ELSEIF(IOPT.EQ.0)THEN !# iMOD GUI
  !# use CALL GC_POST_COMPUTE() related to button 'Apply' on dialog
 ENDIF
 
 END SUBROUTINE GC_POST

 !###======================================================================
 SUBROUTINE GC_ID_COMPUTE(IOPT)
 !###======================================================================
 !# subroutine to identify fractions per layer AND per x,y-location 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 INTEGER :: I,IU,ILAY,NLAYR,ICOL,IROW
 REAL :: F,Z1,Z2,XTOP,XBOT,TR,BR
 CHARACTER(LEN=52) :: CTYPE
 
 IF(ALLOCATED(FVAL))THEN; DEALLOCATE(FVAL); ENDIF
 ALLOCATE(FVAL(NLAYM))
  
 !## initialize needed variables
 IF(.NOT.GC_PRE_COMPUTE_INIT(NLAYM))RETURN
  
 !## try to read all idf's with model Top and Bot values 
 IF(.NOT.GC_READ_MODELTB())RETURN

 !## get list of "regis"-files 
 IF(.NOT.GC_READ_REGIS())RETURN
 
 !# Open txt-file in case of iMODbatch
 IF(IOPT.EQ.1)THEN
  IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTPUTFOLDER)//'identify.txt',STATUS='UNKNOWN',ACTION='WRITE')
 ENDIF
 
 !## calculate fractions and find values related to given x,y-coordinates
 NLAYR=SIZE(TOPR)
 
 DO I=1,NLAYR
 
  IF(.NOT.GC_PRE_PUT_REGIS(CTYPE))RETURN
 
  DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL 
   TR=TBR(1)%X(ICOL,IROW); BR=TBR(2)%X(ICOL,IROW)   
   IF(TR.EQ.TBR(1)%NODATA.OR.BR.EQ.TBR(2)%NODATA)CYCLE
   DO ILAY=1,NLAYM
    XTOP=TOPM(ILAY)%X(ICOL,IROW); XBOT=BOTM(ILAY)%X(ICOL,IROW)
    Z1=MIN(TR,XTOP); Z2=MAX(BR,XBOT)
    IF(Z1.GT.Z2.AND.XTOP.GT.XBOT)THEN
     F=(Z1-Z2)/(XTOP-XBOT) 
     !## sum up the total fractions
     FFRAC(ILAY)%X(ICOL,IROW)=F
    ENDIF     
   ENDDO
  ENDDO; ENDDO
  
  DO ILAY=1,NLAYM
   FVAL(ILAY)=IDFGETXYVAL(FFRAC(ILAY)%X,X_ID,Y_ID) !#fraction value related to layer and formation
  ENDDO

  !##Write FVAL to screen (GUI) or to txt-file (iMODBATCH)
  IF(IOPT.EQ.1)THEN
  !#Write grid to txt-file organized as columns=layers, rows=fractions per formation
   WRITE(IU,*) CTYPE,FVAL
  ELSEIF(IOPT.EQ.0)THEN
   !# Write grid to screen see figure in documentation
  ENDIF

 ENDDO
 
 CLOSE(IU)
 
 END SUBROUTINE GC_ID_COMPUTE

 !###======================================================================
 SUBROUTINE GC_PRE_COMPUTE(IOPT)
 !###======================================================================
 !# subroutine to compute K-values for preprocessing purposes 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT !# =1 Only write to dos-window if started in batch modus
 INTEGER :: I,J,IROW,ICOL,ILAY,IKHR,IKVR
 REAL :: TR,BR,Z1,Z2,F,KVAL,XTOP,XBOT
 CHARACTER(LEN=52) :: CTYPE
 
 !## initialize needed variables
 IF(.NOT.GC_PRE_COMPUTE_INIT(NLAYM))RETURN

 !## try to read all idf's with model Top and Bot values 
 IF(.NOT.GC_READ_MODELTB())RETURN
 
 !## get list of "regis"-files 
 IF(.NOT.GC_READ_REGIS())RETURN
  
 !## calculate KDH,KDV,VCW,FFRAC,CFRAC
 NLAYR=SIZE(TOPR)
 
 !## read/process
 DO I=1,NLAYR
  
  IF(.NOT.GC_PRE_PUT_REGIS(CTYPE))RETURN
     
  !## process data
  IF(IOPT.EQ.1)WRITE(*,'(A)') 'Process data ...'
  DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL
   
   TR=TBR(1)%X(ICOL,IROW); BR=TBR(2)%X(ICOL,IROW)   
   IF(TR.EQ.TBR(1)%NODATA.OR.BR.EQ.TBR(2)%NODATA)CYCLE
      
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
      KVAL=MAX(KVAL,KHR(1)%X(ICOL,IROW))
     !## if not, try vertical permeability
     ELSE
      IF(IKVR.EQ.1)KVAL=MAX(KVAL,(3.0*KVR(1)%X(ICOL,IROW)))
     ENDIF
     !## sum horizontal transmissivity for each model layer by using fraction grids and formationfactors of model
     !## and select the correct formation factor per Regislayer/modellayer
     DO J=1,SIZE(IPFAC%FORM)
      IF(TRIM(CTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN !#stored in IPFAC by reading in iPEST-files
       KDHIDF(ILAY)%X(ICOL,IROW)=KDHIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*IPFAC(J)%FACT)
      ENDIF
     ENDDO
     
     KVAL=0.0
     !## found vertical permeability
     IF(IKVR.EQ.1)THEN
      KVAL=MAX(KVAL,KVR(1)%X(ICOL,IROW))
     !## if not, try the horizontal permeability
     ELSE
      IF(IKHR.EQ.1)KVAL=MAX(KVAL,(0.3*KHR(1)%X(ICOL,IROW)))
     ENDIF
     !## sum vertical transmissivity for each model layer
     DO J=1,SIZE(IPFAC%FORM)
      IF(TRIM(CTYPE).EQ.TRIM(IPFAC(J)%FORM))THEN !#stored in IPFAC by reading in iPEST-files
       KDVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*IPFAC(J)%FACT)
      ENDIF
     ENDDO

     !## sum up the total fractions
     FFRAC(ILAY)%X(ICOL,IROW)=F 
         
    ENDIF
   ENDDO

   !## compute fractions for aquitards, resistance between mid aquifer1 and aquifer2
   DO ILAY=1,NLAYM-1  
    XTOP=BOTM(ILAY  )%X(ICOL,IROW)
    XBOT=TOPM(ILAY+1)%X(ICOL,IROW)
    Z1=MIN(TR,XTOP); Z2=MAX(BR,XBOT)
    !## fraction in aquitards
    IF(Z1.GT.Z2)THEN
     F=(Z1-Z2)/(XTOP-XBOT)
     !## assign minimum values for aquitards
     KVAL=10.0E10
     !## found vertical permeability
     IF(IKVR.EQ.1)THEN
      KVAL=MIN(KVAL,KVR(1)%X(ICOL,IROW))
     !## if not, try the horizontal permeability
     ELSE
      IF(IKHR.EQ.1)KVAL=MIN(KVAL,(0.3*KHR(1)%X(ICOL,IROW)))
     ENDIF
     !## sum up the total resistance
     IF(KVAL.GT.0.0)THEN
      CIDF(ILAY)%X(ICOL,IROW)=CIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)/KVAL)
     ENDIF
     
     !## sum up the total fractions
     CFRAC(ILAY)%X(ICOL,IROW)=F 
    
    ENDIF
   ENDDO
   
  ENDDO ; ENDDO
    
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
 
 IF(IOPT.EQ.1)WRITE(*,'(A)') 'Write data ...'
 CALL GC_PRE_COMPUTE_WRITE() !# Write variables to file depending on checkbox options
 
 END SUBROUTINE GC_PRE_COMPUTE

 !###======================================================================
 LOGICAL FUNCTION GC_PRE_COMPUTE_INIT(NLAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY
 INTEGER :: I
 
 GC_PRE_COMPUTE_INIT=.FALSE.
 
 ALLOCATE(CIDF(NLAY-1),KDHIDF(NLAY),KDVIDF(NLAY),TOPM(NLAY), &
          BOTM(NLAY),TBR(2),KHR(1),KVR(1),FFRAC(NLAY),CFRAC(NLAY), &
          KVIDF(NLAY),KHIDF(NLAY),KVAIDF(NLAY),KVVIDF(NLAY))
 
 !## nullify idf-objects
 DO I=1,SIZE(CIDF); CALL IDFNULLIFY(CIDF(I)); ENDDO 
 DO I=1,SIZE(KDHIDF); CALL IDFNULLIFY(KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFNULLIFY(KDVIDF(I)); ENDDO
 DO I=1,SIZE(KHIDF); CALL IDFNULLIFY(KHIDF(I)); ENDDO 
 DO I=1,SIZE(KVIDF); CALL IDFNULLIFY(KVIDF(I)); ENDDO
 DO I=1,SIZE(KVVIDF); CALL IDFNULLIFY(KVVIDF(I)); ENDDO 
 DO I=1,SIZE(KVAIDF); CALL IDFNULLIFY(KVAIDF(I)); ENDDO 
 DO I=1,SIZE(KHR); CALL IDFNULLIFY(KHR(I)); ENDDO 
 DO I=1,SIZE(KVR); CALL IDFNULLIFY(KVR(I)); ENDDO 
 DO I=1,SIZE(TBR); CALL IDFNULLIFY(TBR(I)); ENDDO
 DO I=1,SIZE(FFRAC); CALL IDFNULLIFY(FFRAC(I)); ENDDO
 DO I=1,SIZE(CFRAC); CALL IDFNULLIFY(CFRAC(I)); ENDDO
 
 !## copy settings0
 DO I=1,SIZE(CIDF); CALL IDFCOPY(TOPM(1),CIDF(I)); ENDDO 
 DO I=1,SIZE(KDHIDF); CALL IDFCOPY(TOPM(1),KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFCOPY(TOPM(1),KDVIDF(I)); ENDDO 
 DO I=1,SIZE(KHIDF); CALL IDFCOPY(TOPM(1),KHIDF(I)); ENDDO 
 DO I=1,SIZE(KVIDF); CALL IDFCOPY(TOPM(1),KVIDF(I)); ENDDO
 DO I=1,SIZE(KVVIDF); CALL IDFCOPY(TOPM(1),KVVIDF(I)); ENDDO 
 DO I=1,SIZE(KVAIDF); CALL IDFCOPY(TOPM(1),KVAIDF(I)); ENDDO 
 DO I=1,SIZE(TBR); CALL IDFCOPY(TOPM(1),TBR(I)); ENDDO 
 DO I=1,SIZE(FFRAC); CALL IDFCOPY(TOPM(1),FFRAC(I)); ENDDO 
 DO I=1,SIZE(CFRAC); CALL IDFCOPY(TOPM(1),CFRAC(I)); ENDDO  
 
 CALL IDFCOPY(TOPM(1),KHR(1)); CALL IDFCOPY(TOPM(1),KVR(1)) 
   
 GC_PRE_COMPUTE_INIT=.TRUE.
 
 END FUNCTION GC_PRE_COMPUTE_INIT

 !###======================================================================
 LOGICAL FUNCTION GC_PRE_PUT_REGIS(CTYPE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,NLAYR,IKHR,IKVR
 REAL :: TR,BR,Z1,Z2,F,KVAL,XTOP,XBOT
 CHARACTER(LEN=52),INTENT(OUT) :: CTYPE
 CHARACTER(LEN=256) :: FNAME
 
 GC_PRE_PUT_REGIS=.FALSE.
 
 !IF(IOPT.EQ.1)WRITE(*,'(2(I10,1X),F10.2)') I,NLAYR,REAL(I*100)/REAL(NLAYR)
  
 J=INDEX(TOPR(I),'\',.TRUE.)+1; K=INDEX(TOPR(I),TRIM(WC(2:)))-1
 CTYPE=TOPR(I)(J:K) !#=Formation name related part filename

 !IF(IOPT.EQ.1)WRITE(* ,'(A)') 'Reading:'
 !## try top
 FNAME=UTL_SUBST(TOPR(I),'*',TRIM(CTYPE))
 !IF(IOPT.EQ.1)WRITE(* ,'(A)') '-'//TRIM(FNAME)
 IF(.NOT.IDFREADSCALE(FNAME,TBR(1),2,1,0.0,0))RETURN !## scale mean
 TBR(1)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
 !## try bot
 FNAME=UTL_SUBST(BOTR(I),'*',TRIM(CTYPE))
 !IF(IOPT.EQ.1)WRITE(* ,'(A)') '-'//TRIM(FNAME)
 IF(.NOT.IDFREADSCALE(FNAME,TBR(2),2,1,0.0,0))RETURN !## scale mean
 TBR(2)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
 !## try kh
 FNAME=UTL_SUBST(KHVR(I),'*',TRIM(CTYPE))
 IKHR=1; IF(.NOT.IDFREADSCALE(FNAME,KHR(1),3,1,0.0,1))IKHR=0
 IF(IKHR.EQ.1)THEN; !IF(IOPT.EQ.1)WRITE(*,'(A)') '-'//TRIM(FNAME)
 KHR(1)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
 !## try kv
 FNAME=UTL_SUBST(KVVR(I),'*',TRIM(CTYPE))
 IKVR=1; IF(.NOT.IDFREADSCALE(FNAME,KVR(1),3,1,0.0,1))IKVR=0
 IF(IKVR.EQ.1)THEN; !IF(IOPT.EQ.1)WRITE(*,'(A)') '-'//TRIM(FNAME)
 KVR(1)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
 
 IF(IKHR.EQ.0.AND.IKVR.EQ.0)THEN
  !IF(IOPT.EQ.1)WRITE(*,'(/A/)')  'No horizontal/vertical permeabilities found, formation will be skipped!'
  CYCLE
 ENDIF
 
 GC_PRE_PUT_REGIS=.TRUE.
  
 END FUNCTION GC_PRE_PUT_REGIS

 !###======================================================================
 SUBROUTINE GC_PRE_COMPUTE_WRITE()
 !###======================================================================
 !# subroutine to calculate KH,KV,KVA and write K- and C-values to file (KHV, KVV, KVA, KDW and VCW)
 IMPLICIT NONE
 !INTEGER,INTENT(IN) :: IOPT !# Write options related to checkbox options
 INTEGER :: I
 CHARACTER(LEN=256) :: LINE
  
 !## write K- and C-values to file
 IF(ISAVEK.EQ.1.AND.ISAVEC.EQ.0)THEN !# Option only write KHV, KVV and KVA
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_khv_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KDVIDF);  IF(.NOT.IDFWRITE(KDVIDF(I),TRIM(LINE),1))RETURN; ENDDO
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_kvv_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KVVIDF);  IF(.NOT.IDFWRITE(KVVIDF(I),TRIM(LINE),1))RETURN; ENDDO
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_kva_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KVAIDF); IF(.NOT.IDFWRITE(KVAIDF(I),TRIM(LINE),1))RETURN; ENDDO
 
 ELSEIF(ISAVEK.EQ.0.AND.ISAVEC.EQ.1)THEN !# Option only write KDW and VCW
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_kd_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KDHIDF); IF(.NOT.IDFWRITE(KDHIDF(I),TRIM(LINE),1))RETURN; ENDDO
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_vc_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(CIDF);   IF(.NOT.IDFWRITE(CIDF(I),TRIM(LINE),1))RETURN; ENDDO
 
 ELSEIF(ISAVEK.EQ.1.AND.ISAVEC.EQ.1)THEN !# Option to write all variables (KHV, KVV, KVA, KDW and VCW)
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_khv_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KDVIDF);  IF(.NOT.IDFWRITE(KDVIDF(I),TRIM(LINE),1))RETURN; ENDDO
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_kvv_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KVVIDF);  IF(.NOT.IDFWRITE(KVVIDF(I),TRIM(LINE),1))RETURN; ENDDO
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_kva_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KVAIDF); IF(.NOT.IDFWRITE(KVAIDF(I),TRIM(LINE),1))RETURN; ENDDO
   LINE=TRIM(OUTPUTFOLDER)//'\mdl_kd_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(KDHIDF); IF(.NOT.IDFWRITE(KDHIDF(I),TRIM(LINE),1))RETURN; ENDDO
  LINE=TRIM(OUTPUTFOLDER)//'\mdl_vc_l'//TRIM(ITOS(I))//'.idf'
  DO I=1,SIZE(CIDF);   IF(.NOT.IDFWRITE(CIDF(I),TRIM(LINE),1))RETURN; ENDDO
 ENDIF

 END SUBROUTINE GC_PRE_COMPUTE_WRITE

 !###======================================================================
 SUBROUTINE GC_READ_IPEST(FNAME,NR)
 !###======================================================================
 !# subroutine to read IPEST factors from file
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FNAME
 INTEGER,INTENT(IN) :: NR
 INTEGER :: I
  
 ALLOCATE(IPFAC(NLAYM))
 !#Read variables from file into IPFAC-array
 DO I=1,NLAYM !# loop over total amount of modellayers
!  IPFAC(I)%FORM=
!  IPFAC(I)%FAC=
 ENDDO
 
 END SUBROUTINE GC_READ_IPEST

 !###======================================================================
 LOGICAL FUNCTION GC_READ_MODELTB()
 !###======================================================================
 !# function to read model top and bot files
 IMPLICIT NONE
 INTEGER :: I,J
 CHARACTER(LEN=256) :: FNAME
 
 GC_READ_MODELTB=.FALSE.
 
 J=1
 DO I=1,NLAYM
  FNAME=TRIM(OUTPUTFOLDER)//'\'//TRIM(TOPM(1)%FNAME(I)(INDEX(TOPM(1)%FNAME(I),'\',.TRUE.)+1:))  
  IF(MOD(I,2).NE.0)THEN; IF(.NOT.IDFREAD(TOPM(J),FNAME,1))RETURN; ENDIF
  FNAME=TRIM(OUTPUTFOLDER)//'\'//TRIM(BOTM(1)%FNAME(I)(INDEX(BOTM(1)%FNAME(I),'\',.TRUE.)+1:))
  IF(MOD(I,2).EQ.0)THEN; IF(.NOT.IDFREAD(BOTM(J),FNAME,1))RETURN; J=J+1; ENDIF
 ENDDO
 
 GC_READ_MODELTB=.TRUE.
 
 END FUNCTION GC_READ_MODELTB

 !###======================================================================
 LOGICAL FUNCTION GC_READ_REGIS()
 !###======================================================================
 !# function to read model top and bot files
 IMPLICIT NONE
 INTEGER :: I
 CHARACTER(LEN=256) :: ROOT
 
 GC_READ_REGIS=.FALSE.
 
 !#define subdirections for needed REGIS-files
 I=INDEX(REGISFOLDER,'\',.TRUE.); ROOT=REGISFOLDER(:I-1); WC='*.IDF'
 IF(.NOT.UTL_DIRINFO_POINTER(TRIM(ROOT)//'\TOP',WC,TOPR,'F'))RETURN
 IF(.NOT.UTL_DIRINFO_POINTER(TRIM(ROOT)//'\BOT',WC,BOTR,'F'))RETURN
 IF(.NOT.UTL_DIRINFO_POINTER(TRIM(ROOT)//'\KHV',WC,KHVR,'F'))RETURN
 IF(.NOT.UTL_DIRINFO_POINTER(TRIM(ROOT)//'\KVV',WC,KVVR,'F'))RETURN
 DO I=1,SIZE(TOPR); TOPR(I)=UTL_CAP(TRIM(ROOT)//'\TOP\'//TRIM(TOPR(I)),'U'); ENDDO
 DO I=1,SIZE(BOTR); BOTR(I)=UTL_CAP(TRIM(ROOT)//'\BOT\'//TRIM(BOTR(I)),'U'); ENDDO
 DO I=1,SIZE(KHVR); KHVR(I)=UTL_CAP(TRIM(ROOT)//'\KHV\'//TRIM(KHVR(I)),'U'); ENDDO
 DO I=1,SIZE(KVVR); KVVR(I)=UTL_CAP(TRIM(ROOT)//'\KVV\'//TRIM(KVVR(I)),'U'); ENDDO
 
 GC_READ_REGIS=.TRUE.
 
 END FUNCTION GC_READ_REGIS

 !###======================================================================
 SUBROUTINE GC_POST_COMPUTE()
 !###======================================================================
 !# subroutine to compute K-values for postprocessing purposes
 IMPLICIT NONE

 

 END SUBROUTINE GC_POST_COMPUTE


END MODULE MOD_GEOCONNECT