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
 SUBROUTINE GC_PRE(IOPT)
 !###======================================================================
 !# subroutine to manage all preprocessing steps
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 
 IF(IOPT.EQ.1)THEN !# iMODbatch
  CALL GC_READ_IPEST(FNAME,NR) !#FNAME=iPEST-CSV file (stored in "IPEST"-variable and read in GC_READ_PRE), #NR= number of block to be used from iPEST-file (stored in "IPESTNO"-variable and read in GC_READ_PRE).
  CALL GC_PRE_COMPUTE(IOPTW)
 ELSEIF(IOPT.EQ.0)THEN !# iMOD GUI
  !# use CALL GC_READ_IPEST(FNAME,NR) related to button 'iPEST' on dialog
  !# use CALL GC_PRE_COMPUTE() related to button 'Apply' on dialog
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
  IPFAC(I)%FORM=
  IPFAC(I)%FAC=
 ENDDO
 
 END SUBROUTINE GC_READ_IPEST

 !###======================================================================
 SUBROUTINE GC_PRE_COMPUTE(IOPT)
 !###======================================================================
 !# subroutine to compute K-values for preprocessing purposes 
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IOPT
 INTEGER :: I,J,K,IKHR,IKVR,IROW,ICOL,ILAY,IU,IOS
 REAL :: TR,BR,Z1,Z2,F,KVAL,XTOP,XBOT
 CHARACTER(LEN=256) :: FNAME,ROOT,LINE
 CHARACTER(LEN=52) :: WC,CTYPE
 
 !## try to read all idf's
 IF(.NOT.GC_READ_MODELTB())RETURN
 
 !## get list of "regis"-files 
 IF(.NOT.GC_PRE_COMPUTE_INIT(NLAYM))RETURN
 IF(.NOT.GC_READ_REGIS(WC))RETURN
  
 NLAYR=SIZE(TOPR)
    
 !## read/process
 DO I=1,NLAYR
  
  WRITE(*,'(2(I10,1X),F10.2)') I,NLAYR,REAL(I*100)/REAL(NLAYR)
  
  J=INDEX(TOPR(I),'\',.TRUE.)+1; K=INDEX(TOPR(I),TRIM(WC(2:)))-1
  CTYPE=TOPR(I)(J:K) !#=Formation name related part filename

  WRITE(* ,'(A)') 'Reading:'
  !## try top
  FNAME=UTL_SUBST(TOPR(I),'*',TRIM(CTYPE))
  WRITE(* ,'(A)') '-'//TRIM(FNAME)
  IF(.NOT.IDFREADSCALE(FNAME,TOPR(I),2,1,0.0,0))RETURN !## scale mean
  TBR(1)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
  !## try bot
  FNAME=UTL_SUBST(BOTR(I),'*',TRIM(CTYPE))
  WRITE(* ,'(A)') '-'//TRIM(FNAME)
  IF(.NOT.IDFREADSCALE(FNAME,BOTR(I),2,1,0.0,0))RETURN !## scale mean
  TBR(2)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
  !## try kh
  FNAME=UTL_SUBST(KHVR(I),'*',TRIM(CTYPE))
  IKHR=1; IF(.NOT.IDFREADSCALE(FNAME,KHVR(I),3,1,0.0,1))IKHR=0
  IF(IKHR.EQ.1)THEN; WRITE(*,'(A)') '-'//TRIM(FNAME)
  KHR(1)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
  !## try kv
  FNAME=UTL_SUBST(KVVR(I),'*',TRIM(CTYPE))
  IKVR=1; IF(.NOT.IDFREADSCALE(FNAME,KVVR(I),3,1,0.0,1))IKVR=0
  IF(IKVR.EQ.1)THEN; WRITE(*,'(A)') '-'//TRIM(FNAME)
  KVR(1)%FNAME=TRIM(OUTPUTFOLDER)//'\REGIS\'//TRIM(FNAME(INDEX(FNAME,'\',.TRUE.):))
  
  IF(IKHR.EQ.0.AND.IKVR.EQ.0)THEN
   WRITE(*,'(/A/)')  'No horizontal/vertical permeabilities found, formation will be skipped!'
   CYCLE
  ENDIF

  !CALL GC_READ_FRACTIONS() !#Read fraction files and content into memory (FFRAC(ILAY)%X(ICOL,IROW))
    
  !## process data
  WRITE(*,'(A)') 'Process data ...'
  DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL
   
   TR=TBR(1)%X(ICOL,IROW); BR=TBR(2)%X(ICOL,IROW)   
   IF(TR.EQ.TBR(1)%NODATA.OR.BR.EQ.TBR(2)%NODATA)CYCLE
      
   !## compute KDH and KDV model-values for aquifers
   DO ILAY=1,NLAYM
    XTOP=TOPM(ILAY)%X(ICOL,IROW); XBOT=BOTM(ILAY)%X(ICOL,IROW)
    Z1=MIN(TR,XTOP); Z2=MAX(BR,XBOT)
    IF(Z1.GT.Z2.AND.XTOP.GT.XBOT)THEN  
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
      IF(TRIM(CTYPE).EQ.TRIM(IPFAC(J)%FORM)THEN !#stored in IPFAC by reading in iPEST-files
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
      IF(TRIM(CTYPE).EQ.TRIM(IPFAC(J)%FORM)THEN !#stored in IPFAC by reading in iPEST-files
       KDVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)+((Z1-Z2)*KVAL*IPFAC(J)%FACT)
      ENDIF
     ENDDO
         
    ENDIF
   ENDDO
   
  ENDDO ; ENDDO
    
 ENDDO
 
 !## write transmissivities/vertical resistances
 LINE=TRIM(OUTPUTFOLDER)//'\mdl_kd_l'//TRIM(ITOS(I))//'.idf'
 DO I=1,SIZE(KDHIDF); IF(.NOT.IDFWRITE(KDHIDF(I),TRIM(LINE),1))RETURN; ENDDO
 LINE=TRIM(OUTPUTFOLDER)//'\mdl_vc_l'//TRIM(ITOS(I))//'.idf'
 DO I=1,SIZE(CIDF);   IF(.NOT.IDFWRITE(CIDF(I)  ,TRIM(LINE),1))RETURN; ENDDO

 DO IROW=1,TOPM(1)%NROW; DO ICOL=1,TOPM(1)%NCOL; DO ILAY=1,NLAYM
  TR=TOPM(ILAY)%X(ICOL,IROW); BR=BOTM(ILAY)%X(ICOL,IROW)   
  IF(TR.EQ.TOPM(ILAY)%NODATA.OR.BR.EQ.BOTM(ILAY)%NODATA)CYCLE
  IF(TR-BR.LE.0.0)THEN
   KDHIDF(ILAY)%X(ICOL,IROW)=0.0
   KDVIDF(ILAY)%X(ICOL,IROW)=1.0
  ELSE
   KDHIDF(ILAY)%X(ICOL,IROW)=KDHIDF(ILAY)%X(ICOL,IROW)/(TR-BR) !#KH-value modellayer
   KDVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)/(TR-BR) !#KV-value modellayer
   IF(KDHIDF(ILAY)%X(ICOL,IROW).EQ.0.0)THEN
    KDVIDF(ILAY)%X(ICOL,IROW)=1.0
   ELSE
    KDVIDF(ILAY)%X(ICOL,IROW)=KDVIDF(ILAY)%X(ICOL,IROW)/KDHIDF(ILAY)%X(ICOL,IROW)
   ENDIF
  ENDIF
  !## compute vertical permeability
  IF(ILAY.LT.NLAY)THEN
   TR=BOTM(ILAY)%X(ICOL,IROW); BR=TOPM(ILAY+1)%X(ICOL,IROW)   
   IF(TR.EQ.TOPM(ILAY)%NODATA.OR.BR.EQ.BOTM(ILAY)%NODATA)CYCLE
   IF(CIDF(ILAY)%X(ICOL,IROW).LE.0.0)THEN
    CIDF(ILAY)%X(ICOL,IROW)= 0.0
   ELSE
    CIDF(ILAY)%X(ICOL,IROW)=(TR-BR)/CIDF(ILAY)%X(ICOL,IROW)
   ENDIF
  ENDIF
 ENDDO; ENDDO; ENDDO
 
 IF(IOPTW.EQ.0)THEN !# Option only write KHV and KVV
  DO I=1,SIZE(KDHIDF);    IF(.NOT.IDFWRITE(KDHIDF(I),KDHIDF(I)%FNAME,1))RETURN; ENDDO
  DO I=1,SIZE(KDVIDF);    IF(.NOT.IDFWRITE(KDVIDF(I),KDVIDF(I)%FNAME,1))RETURN; ENDDO
 ELSEIF(IOPTW.EQ.1)THEN !# Option only write KDW and VCW
  DO I=1,SIZE(CIDF);      IF(.NOT.IDFWRITE(CIDF(I),CIDF(I)%FNAME,1))RETURN; ENDDO
 ELSEIF(IOPTW.EQ.2)THEN !# Option to write all variables (KHV, KVV, KDW and VCW)
  DO I=1,SIZE(KDHIDF);    IF(.NOT.IDFWRITE(KDHIDF(I),KDHIDF(I)%FNAME,1))RETURN; ENDDO
  DO I=1,SIZE(KDVIDF);    IF(.NOT.IDFWRITE(KDVIDF(I),KDVIDF(I)%FNAME,1))RETURN; ENDDO
  DO I=1,SIZE(CIDF);      IF(.NOT.IDFWRITE(CIDF(I),CIDF(I)%FNAME,1))RETURN; ENDDO  
 ENDIF
 
 END SUBROUTINE GC_PRE_COMPUTE

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
  FNAME=TRIM(OUTPUTFOLDER)//'\'//TRIM(TOPM(1)%FNAME(I)(TOPM(1)%FNAME(I),'\',.TRUE.)+1:))
  IF(MOD(I,2).NE.0)THEN; IF(.NOT.IDFREAD(TOPM(J),FNAME,1))RETURN; ENDIF
  FNAME=TRIM(OUTPUTFOLDER)//'\'//TRIM(BOTM(1)%FNAME(I)(BOTM(1)%FNAME(I),'\',.TRUE.)+1:))
  IF(MOD(I,2).EQ.0)THEN; IF(.NOT.IDFREAD(BOTM(J),FNAME,1))RETURN; J=J+1; ENDIF
 ENDDO
 
 GC_READ_MODELTB=.TRUE.
 
 END LOGICAL FUNCTION GC_READ_MODELTB

 !###======================================================================
 LOGICAL FUNCTION GC_READ_REGIS(WC)
 !###======================================================================
 !# function to read model top and bot files
 IMPLICIT NONE
 INTEGER :: I
 CHARACTER(LEN=52),INTENT(OUT) :: WC
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
 
 END LOGICAL FUNCTION GC_READ_REGIS

 !###======================================================================
 LOGICAL FUNCTION GC_PRE_COMPUTE_INIT(NLAY)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NLAY
 INTEGER :: I
 
 GC_PRE_COMPUTE_INIT=.FALSE.
 
 ALLOCATE(CIDF(NLAY-1),KDHIDF(NLAY),KDVIDF(NLAY),TOPM(NLAY), &
          BOTM(NLAY),TBR(2),KHR(1),KVR(1),FFRAC(NLAY)
 !## nullify idf-objects
 DO I=1,SIZE(KDHIDF);    CALL IDFNULLIFY(KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF);    CALL IDFNULLIFY(KDVIDF(I)); ENDDO 
 DO I=1,SIZE(KHR);        CALL IDFNULLIFY(KHR(I));    ENDDO 
 DO I=1,SIZE(KVR);        CALL IDFNULLIFY(KVR(I));    ENDDO 
 DO I=1,SIZE(TBR);        CALL IDFNULLIFY(TBR(I));    ENDDO
 !DO I=1,SIZE(FFRAC);     CALL IDFNULLIFY(FFRAC(I));    ENDDO
 !## copy settings0
 DO I=1,SIZE(KDHIDF); CALL IDFCOPY(TOPM(1),KDHIDF(I)); ENDDO 
 DO I=1,SIZE(KDVIDF); CALL IDFCOPY(TOPM(1),KDVIDF(I)); ENDDO 
 DO I=1,SIZE(TBR)   ; CALL IDFCOPY(TOPM(1),TBR(I))   ; ENDDO 
 !DO I=1,SIZE(FFRAC)   ; CALL IDFCOPY(TOPM(1),FFRAC(I))   ; ENDDO 
 CALL IDFCOPY(TOPM(1),KHR(1)); CALL IDFCOPY(TOPM(1),KVR(1))
 
 !## write header information kdidf / cidf
 DO I=1,SIZE(KDHIDF)
  KDHIDF(I)%X=0.0; KDHIDF(I)%FNAME=TRIM(OUTPUTFOLDER)//'\mdl_khv_l'//TRIM(ITOS(I))//'.idf'
 ENDDO
 DO I=1,SIZE(KDHIDF)
  KDVIDF(I)%X=0.0; KDVIDF(I)%FNAME=TRIM(OUTPUTFOLDER)//'\mdl_kva_l'//TRIM(ITOS(I))//'.idf'
 ENDDO
 DO I=1,SIZE(CIDF)
  CIDF(I)%X=0.0;   CIDF(I)%FNAME=TRIM(OUTPUTFOLDER)//'\mdl_kvv_l'//TRIM(ITOS(I))//'.idf'
 ENDDO
  
 GC_PRE_COMPUTE_INIT=.TRUE.
 
 END FUNCTION GC_PRE_COMPUTE_INIT

 !###======================================================================
 SUBROUTINE GC_POST_COMPUTE()
 !###======================================================================
 !# subroutine to compute K-values for postprocessing purposes
 IMPLICIT NONE

 

 END SUBROUTINE GC_POST_COMPUTE

END MODULE MOD_GEOCONNECT