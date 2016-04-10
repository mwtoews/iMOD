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
MODULE MOD_CUS

USE MOD_CUS_PAR
USE MOD_IDF, ONLY : IDFNULLIFY,IDFREAD,IDFDEALLOCATE,IDF_EXTENT,IDFREADSCALE,IDFWRITE,IDFCOPY, &
                    IDFDEALLOCATEX,IDFALLOCATEX
USE MOD_UTL, ONLY : UTL_CAP,UTL_DIRINFO_POINTER,UTL_SUBST,UTL_CREATEDIR,UTL_IDFSNAPTOGRID, &
                    UTL_GETMED,UTL_GETUNIT,ITOS
USE MOD_IDFEDIT_TRACE, ONLY : IDFEDITTRACE,IDFEDITGETDIR
USE MOD_SMPLX, ONLY : SMPLX_MAIN
USE MOD_SMPLX_PAR, ONLY : IN_CON,NVAR,NCON,IN_OBJ,ZOBJ,XVAR,LPSTATUS,A,B,C,CONSTR_TYPE,NUMLE,NUMGE,ICNVG
USE MOD_QKSORT

INTEGER,PRIVATE :: IU

CONTAINS

 !###======================================================================
 LOGICAL FUNCTION CUS_MAIN()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IOS
 
 CUS_MAIN=.FALSE.
 
 !## compute pointers, otherwise use existing ones
 IF(ICPOINTERS.EQ.1)THEN
  CALL UTL_CREATEDIR(OUTPUTFOLDER)
  CALL UTL_CREATEDIR(TRIM(OUTPUTFOLDER)//'\pointers')

  IF(TRIM(FDISTANCES).EQ.'')THEN
   CALL UTL_CREATEDIR(TRIM(OUTPUTFOLDER)//'\objects')
   CALL UTL_CREATEDIR(TRIM(OUTPUTFOLDER)//'\topbottom')
   CALL UTL_CREATEDIR(TRIM(OUTPUTFOLDER)//'\thickness')
   IU=UTL_GETUNIT(); OPEN(IU,FILE=TRIM(OUTPUTFOLDER)//'\distances.txt',STATUS='UNKNOWN',IOSTAT=IOS)
   IF(IOS.NE.0)THEN; WRITE(*,'(A)') 'Can not open '//TRIM(OUTPUTFOLDER)//'\distances.txt'; RETURN; ENDIF

   IF(.NOT.CUS_GETFILES())RETURN

   CUS_NLAY=SIZE(TOPIDF)
   WRITE(IU,'(I4)') CUS_NLAY 

   IF(.NOT.CUS_READFILES())RETURN

   IF(NCLIP.EQ.1)THEN
    WRITE(*,'(/A)') 'Reading clip file:'
    WRITE(*,'(A)') '[CLIP] - '//TRIM(CLIPIDF%FNAME)
    CALL IDFCOPY(MDLIDF(1),CLIPIDF)
    !## take data most frequent number
    IF(.NOT.IDFREADSCALE(CLIPIDF%FNAME,CLIPIDF,10,0,0.0,0))RETURN
   ENDIF
   
   DO I=1,SIZE(TOPIDF)
    IF(.NOT.CUS_CALCTHICKNESS(I))RETURN
    IF(.NOT.CUS_TRACE(I))RETURN
   ENDDO
   IF(.NOT.CUS_DISTANCES())RETURN
   CLOSE(IU); FDISTANCES=TRIM(OUTPUTFOLDER)//'\distances.txt'
   CALL CUS_DEALLOCATE()
  ENDIF
 
  IF(.NOT.CUS_READDISTANCES())RETURN
 
  !## apply for given percentage
  WRITE(*,'(/A/)') ' Solving Simplex Method for minimization of modellayers ...'
  IF(.NOT.CUS_SMPLX(0))RETURN   
 
 ELSE
  DO I=1,CUS_NLAY
   IF(.NOT.IDFREAD(TOPIDF(I),TOPIDF(I)%FNAME,0))RETURN
   IF(.NOT.IDFREAD(BOTIDF(I),BOTIDF(I)%FNAME,0))RETURN
   IF(.NOT.IDFREAD(ZIDF(I),ZIDF(I)%FNAME,0))RETURN
   IF(ZIDF(I)%DMAX.LT.ZIDF(I)%DMIN)ZIDF(I)%DMAX=0.0
   ZINFO(I)%NZ=ZIDF(I)%DMAX; NULLIFY(ZINFO(I)%NP)
  ENDDO
 ENDIF
 
 I=2; IF(.NOT.CUS_CREATETOPBOT())RETURN
 
 IF(ICPOINTERS.EQ.1)THEN
  DO I=1,NVAR; WRITE(*,'(99I2)') (ILP(I,J),J=0,NPERC); ENDDO; DEALLOCATE(ILP)
 ENDIF
 
 CUS_MAIN=.TRUE.
 
 END FUNCTION CUS_MAIN
 
 !###======================================================================
 LOGICAL FUNCTION CUS_CREATETOPBOT()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: IROW,ICOL,I,J,ILAY,IIDF
 REAL :: T,B,D
 
 CUS_CREATETOPBOT=.FALSE.

 IF(ICPOINTERS.EQ.1)THEN
  NLAY=0; DO I=1,NVAR; IF(ILP(I,0).GE.0)NLAY=MAX(NLAY,ILP(I,0)+1); ENDDO; NLAY=NLAY+1
 ENDIF
 
 CALL UTL_CREATEDIR(TRIM(OUTPUTFOLDER)//'\model')
 CALL UTL_CREATEDIR(TRIM(OUTPUTFOLDER)//'\occupy')

 ALLOCATE(MDLTOP(NLAY),MDLBOT(NLAY))
 DO I=1,NLAY; CALL IDFNULLIFY(MDLTOP(I)); CALL IDFNULLIFY(MDLBOT(I)); ENDDO
 DO I=1,NLAY; CALL IDFCOPY(MDLIDF(1),MDLTOP(I)); MDLTOP(I)%NODATA=-999.99; ENDDO
 DO I=1,NLAY; CALL IDFCOPY(MDLIDF(1),MDLBOT(I)); MDLBOT(I)%NODATA=-999.99; ENDDO
 
 WRITE(*,'(/A)') 'Reading data from:'
 WRITE(*,'(A)') '[TOPSYSTEM] - '//TRIM(TOPSYSTEM)
 IF(.NOT.IDFREADSCALE(TOPSYSTEM,MDLTOP(1),10,0,0.0,0))RETURN
 WRITE(*,'(A)') '[BOTSYSTEM] - '//TRIM(BOTSYSTEM)
 IF(.NOT.IDFREADSCALE(BOTSYSTEM,MDLBOT(NLAY),10,0,0.0,0))RETURN
 DO I=1,NLAY; MDLTOP(I)%FNAME=TRIM(OUTPUTFOLDER)//'\model\mdl_top_l'//TRIM(ITOS(I))//'.idf'; ENDDO
 DO I=1,NLAY; MDLBOT(I)%FNAME=TRIM(OUTPUTFOLDER)//'\model\mdl_bot_l'//TRIM(ITOS(I))//'.idf'; ENDDO

 DO I=2,NLAY  ; IF(.NOT.IDFALLOCATEX(MDLTOP(I)))RETURN; MDLTOP(I)%X=MDLTOP(I)%NODATA; ENDDO
 DO I=1,NLAY-1; IF(.NOT.IDFALLOCATEX(MDLBOT(I)))RETURN; MDLBOT(I)%X=MDLBOT(I)%NODATA;  ENDDO
 
 !## check consistency (thickness<0.0), create base model
 DO IROW=1,MDLTOP(1)%NROW; DO ICOL=1,MDLTOP(1)%NCOL
  T=MDLTOP(1)%X(ICOL,IROW); B=MDLBOT(NLAY)%X(ICOL,IROW); D=0.0
  IF(T.NE.MDLTOP(1)%NODATA.AND.B.NE.MDLBOT(NLAY)%NODATA)THEN
   IF(T.LE.B)THEN
    MDLTOP(1)%X(ICOL,IROW)=MDLTOP(1)%NODATA; MDLBOT(NLAY)%X(ICOL,IROW)=MDLBOT(NLAY)%NODATA
   ENDIF
  ELSE
   DO I=1,NLAY; MDLTOP(I)%X(ICOL,IROW)=MDLTOP(I)%NODATA; MDLBOT(I)%X(ICOL,IROW)=MDLBOT(I)%NODATA; ENDDO
  ENDIF
 ENDDO; ENDDO

 !## insert all layers in the appropriate modellayer
 DO IIDF=1,CUS_NLAY

  IF(ICPOINTERS.EQ.1)THEN
   I=INDEX(ZIDF(IIDF)%FNAME,'\',.TRUE.); J=INDEX(ZIDF(IIDF)%FNAME,'_objects.idf',.TRUE.)
   ZIDF(IIDF)%FNAME=TRIM(OUTPUTFOLDER)//'\pointers\'//ZIDF(IIDF)%FNAME(I+1:J-1)//'_pointers.idf'
  ENDIF
  
  WRITE(*,'(2(I3.3,A))') IIDF,'-',CUS_NLAY,' Reading Pointer Files for '//TRIM(ZIDF(IIDF)%FNAME)//' ...'
  CALL IDFDEALLOCATEX(ZIDF(IIDF)); CALL IDFCOPY(MDLIDF(1),ZIDF(IIDF))
  IF(.NOT.IDFREADSCALE(ZIDF(IIDF)%FNAME,ZIDF(IIDF),7,0,0.0,0))RETURN
  
  WRITE(*,'(2(I3.3,A))') IIDF,'-',CUS_NLAY,' Reading Top Files for '//TRIM(TOPIDF(IIDF)%FNAME)//' ...'
  CALL IDFDEALLOCATEX(TOPIDF(IIDF)); CALL IDFCOPY(MDLIDF(1),TOPIDF(IIDF))
  IF(.NOT.IDFREADSCALE(TOPIDF(IIDF)%FNAME,TOPIDF(IIDF),10,0,0.0,0))RETURN

  WRITE(*,'(2(I3.3,A))') IIDF,'-',CUS_NLAY,' Reading Bot Files for '//TRIM(BOTIDF(IIDF)%FNAME)//' ...'
  CALL IDFDEALLOCATEX(BOTIDF(IIDF)); CALL IDFCOPY(MDLIDF(1),BOTIDF(IIDF))
  IF(.NOT.IDFREADSCALE(BOTIDF(IIDF)%FNAME,BOTIDF(IIDF),10,0,0.0,0))RETURN

  DO IROW=1,MDLTOP(1)%NROW; DO ICOL=1,MDLTOP(1)%NCOL
   IF(MDLTOP(1)%X(ICOL,IROW).NE.MDLTOP(1)%NODATA)THEN
    ILAY=INT(ZIDF(IIDF)%X(ICOL,IROW))
    
    !## assign not assignable distinctive layer in modellayer 1
    IF(ILAY.EQ.-1)ILAY=1

    !## corrected ilay identification
    ZIDF(IIDF)%X(ICOL,IROW)=ILAY
    
    IF(ILAY.GE.1.AND.ILAY.LE.NLAY)THEN
     !## check whether top/bot have data
     IF(TOPIDF(IIDF)%X(ICOL,IROW).NE.TOPIDF(IIDF)%NODATA.AND. &
        BOTIDF(IIDF)%X(ICOL,IROW).NE.BOTIDF(IIDF)%NODATA)THEN
      IF(MDLBOT(ILAY)%X(ICOL,IROW).NE.MDLBOT(ILAY)%NODATA)THEN
       MDLBOT(ILAY)  %X(ICOL,IROW)=MAX(MDLBOT(ILAY  )%X(ICOL,IROW),TOPIDF(IIDF)%X(ICOL,IROW))
       MDLTOP(ILAY+1)%X(ICOL,IROW)=MIN(MDLTOP(ILAY+1)%X(ICOL,IROW),BOTIDF(IIDF)%X(ICOL,IROW))
      ELSE
       MDLBOT(ILAY  )%X(ICOL,IROW)=TOPIDF(IIDF)%X(ICOL,IROW)    
       MDLTOP(ILAY+1)%X(ICOL,IROW)=BOTIDF(IIDF)%X(ICOL,IROW)
      ENDIF
     ENDIF
    ENDIF
   ENDIF
  ENDDO; ENDDO

 ENDDO
 
 DO I=1,NLAY; IF(.NOT.IDFWRITE(MDLTOP(I),MDLTOP(I)%FNAME,1))STOP 'ERROR WRITING TOP'; ENDDO
 DO I=1,NLAY; IF(.NOT.IDFWRITE(MDLBOT(I),MDLBOT(I)%FNAME,1))STOP 'ERROR WRITING BOT'; ENDDO
 
 !## get occupy per modellayer
 DO I=1,NLAY; MDLTOP(I)%X=MDLTOP(I)%NODATA; ENDDO
 DO IIDF=1,CUS_NLAY

  WRITE(*,'(2(I3.3,A))') IIDF,'-',CUS_NLAY,' Reading Pointer Files for '//TRIM(ZIDF(IIDF)%FNAME)//' ...'

  DO IROW=1,MDLTOP(1)%NROW; DO ICOL=1,MDLTOP(1)%NCOL
   ILAY=INT(ZIDF(IIDF)%X(ICOL,IROW)) 
   !## assign not assignable distinctive layer in modellayer 1
   IF(ILAY.EQ.-1)ILAY=1
   IF(ILAY.GE.1.AND.ILAY.LE.NLAY)MDLTOP(ILAY)%X(ICOL,IROW)=REAL(IIDF)
  ENDDO; ENDDO
 
 ENDDO

 DO I=1,NLAY; 
  MDLTOP(I)%FNAME=TRIM(OUTPUTFOLDER)//'\occupy\sdl_l'//TRIM(ITOS(I))//'.idf'
  IF(.NOT.IDFWRITE(MDLTOP(I),MDLTOP(I)%FNAME,1))STOP 'ERROR WRITING OCCUPY'
 ENDDO

 CUS_CREATETOPBOT=.TRUE.
 
 END FUNCTION CUS_CREATETOPBOT
 
 !###======================================================================
 LOGICAL FUNCTION CUS_SMPLX(IPERC)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPERC
 INTEGER :: I,J,IVAR,JVAR 
 CHARACTER(LEN=2),DIMENSION(0:3) :: CTYPE
 CHARACTER(LEN=52) :: FORM
 DATA CTYPE/' ','<=','>=','=='/
 REAL :: F,ZZPERC
 
 CUS_SMPLX=.FALSE.

 ALLOCATE(A(MAX(NVAR,NCON),NVAR),B(MAX(NVAR,NCON)),C(NVAR),CONSTR_TYPE(MAX(NVAR,NCON)))

 CONSTR_TYPE=3; A=0.0; B=0.0

 !## add constraints from distance table for current percentile (iperc)
 IVAR=1
 DO I=1,NCON
!  !## not for removed duplicate definitions
!  IF(SMPLX(I)%IAREA.LE.0)CYCLE

  IVAR=SMPLX(I)%IVAR1 
  JVAR=SMPLX(I)%IVAR2 
  
  !## to be connected whenever distance less than given zcrit
  A(I,IVAR)=1.0; A(I,JVAR)=-1.0
  
  !## get appropriate distance from distance.txt file for appropriate percentile
  IF(IPERC.GT.0)THEN
   ZZPERC=SMPLX(I)%PERC(IPERC)
  ELSEIF(IPERC.EQ.0)THEN
   DO J=SIZE(PERC),1,-1; IF(PERCENTAGE.GE.PERC(J))EXIT; ENDDO
   F=(PERCENTAGE-PERC(J))/(PERC(J+1)-PERC(J))
   ZZPERC=SMPLX(I)%PERC(J)+F*(SMPLX(I)%PERC(J+1)-SMPLX(I)%PERC(J))
  ENDIF 
  
  IF(ZZPERC.LE.ZCRIT)THEN
   B(I)=0.0
   !## variable ivar should be equal or above   variable jvar
   IF(SMPLX(I)%IZ2.GT.0)CONSTR_TYPE(I)=2
   !## variable ivar should be equal or beneath variable jvar
   IF(SMPLX(I)%IZ2.LT.0)CONSTR_TYPE(I)=1
  ELSE
   !## variable ivar should be above   variable jvar
   IF(SMPLX(I)%IZ2.GT.0)THEN
    B(I)= 1.0; CONSTR_TYPE(I)=2
   !## variable ivar should be beneath variable jvar
   ELSEIF(SMPLX(I)%IZ2.LT.0)THEN
    B(I)=-1.0; CONSTR_TYPE(I)=1
   ENDIF
  ENDIF
 ENDDO
  
 !## number of conditions minimal number of variables
 NCON=MAX(NVAR,NCON)

 IF(NVAR.LE.20)THEN
  WRITE(FORM,'(A,I2.2,A)') '(I5,',NVAR,'I2,1X,A2,1X,I2)'; DO I=1,NCON
   WRITE(*,FORM) I,(INT(A(I,IVAR)),IVAR=1,NVAR),CTYPE(CONSTR_TYPE(I)),INT(B(I))
  ENDDO
 ENDIF
 
 !## objective is to minimize
 C=-1
  
 !## get number of ">=" and "<="
 NUMLE= 0; NUMGE=0; DO I=1,NCON; IF(CONSTR_TYPE(I).EQ.1)NUMLE=NUMLE+1; IF(CONSTR_TYPE(I).EQ.2)NUMGE=NUMGE+1; ENDDO  

 WRITE(*,'(A,I10,A)') 'Solving ',NCON ,' Conditions'
 WRITE(*,'(A,I10,A)') '        ',NUMLE,' Constraints less/equal'
 WRITE(*,'(A,I10,A)') '        ',NUMGE,' Constraints greater/equal'
 
 !## solve system with simplex method
 CALL SMPLX_MAIN()
 
 !## check xvar() 
 DO I=1,NVAR; IF(SUM(ABS(A(:,I))).EQ.0.0)XVAR(I)=-1.0; ENDDO
 
 !## modellayers numbering 
 DO I=1,NVAR; WRITE(*,'(1X,I10,F10.2)') I,XVAR(I); ENDDO
 WRITE(*,'(/A,F10.2)') 'Objective Function Value:',ZOBJ
 WRITE(*,'(/A/)') TRIM(LPSTATUS)
 IF(ICNVG.NE.0)STOP
 
 DO I=1,NVAR; ILP(I,IPERC)=INT(XVAR(I),1); ENDDO
 NLAY=0; DO I=1,NVAR; IF(ILP(I,0).GE.0)NLAY=MAX(NLAY,ILP(I,0)+1); ENDDO; NLAY=NLAY+1   
 WRITE(*,'(A,I10)') 'NLAY=',NLAY

 IF(IPERC.EQ.0)THEN
  IF(.NOT.CUS_SMPLX_WRITE_POINTER())RETURN
 ENDIF

 IF(ALLOCATED(IN_CON))DEALLOCATE(IN_CON); IF(ALLOCATED(XVAR))DEALLOCATE(XVAR) 
 DEALLOCATE(A,B,C)
 
 CUS_SMPLX=.TRUE. 
 
 END FUNCTION CUS_SMPLX

 !###======================================================================
 LOGICAL FUNCTION CUS_SMPLX_WRITE_POINTER()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,IIDF,N,IZ,IROW,ICOL,ILAY
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IL
 CHARACTER(LEN=256) :: FNAME
 
 CUS_SMPLX_WRITE_POINTER=.FALSE.
 
 ILAY=0; DO IIDF=1,CUS_NLAY 

  !## fill in table
  N=ZINFO(IIDF)%NZ; ALLOCATE(IL(N))
  DO I=1,N; ILAY=ILAY+1; IL(I)=ILAY; ENDDO
  
  WRITE(*,'(2(I3.3,A))') IIDF,'-',CUS_NLAY,' Reading Pointer Files for '//TRIM(ZIDF(IIDF)%FNAME)//' ...'
  CALL IDFDEALLOCATEX(ZIDF(IIDF)); IF(.NOT.IDFREAD(ZIDF(IIDF),ZIDF(IIDF)%FNAME,1))RETURN

  DO IROW=1,ZIDF(IIDF)%NROW; DO ICOL=1,ZIDF(IIDF)%NCOL
   IF(ZIDF(IIDF)%X(ICOL,IROW).GT.0)THEN
    IZ=INT(ZIDF(IIDF)%X(ICOL,IROW))
    IF(XVAR(IL(IZ)).GE.0)THEN
     ZIDF(IIDF)%X(ICOL,IROW)=XVAR(IL(IZ))+1
    ELSE
     ZIDF(IIDF)%X(ICOL,IROW)=XVAR(IL(IZ))
    ENDIF
   ELSE
    !## remove the negative zone numbers that denote the buffers
    ZIDF(IIDF)%X(ICOL,IROW)=ZIDF(IIDF)%NODATA
   ENDIF
  ENDDO; ENDDO

  I=INDEX(ZIDF(IIDF)%FNAME,'\',.TRUE.); J=INDEX(ZIDF(IIDF)%FNAME,'_objects.idf',.TRUE.)
  FNAME=TRIM(OUTPUTFOLDER)//'\pointers\'//ZIDF(IIDF)%FNAME(I+1:J-1)//'_pointers.idf'
  IF(.NOT.IDFWRITE(ZIDF(IIDF),FNAME,1))STOP 'ERROR WRITING POINTER'

  CALL IDFDEALLOCATEX(ZIDF(IIDF)); DEALLOCATE(IL)
   
 ENDDO

 CUS_SMPLX_WRITE_POINTER=.TRUE.
 
 END FUNCTION CUS_SMPLX_WRITE_POINTER
 
 !###======================================================================
 INTEGER FUNCTION CUS_GETIVAR(IFILE,IZONE)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IFILE,IZONE
 INTEGER :: I
 
 CUS_GETIVAR=0; DO I=1,SIZE(TOPIDF)
  IF(I.EQ.IFILE)THEN; CUS_GETIVAR=CUS_GETIVAR+ABS(IZONE); EXIT
  ELSE; CUS_GETIVAR=CUS_GETIVAR+ZINFO(I)%NZ; ENDIF
 ENDDO

 END FUNCTION CUS_GETIVAR
 
 !###======================================================================
 LOGICAL FUNCTION CUS_READDISTANCES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER,PARAMETER :: DN=100
 INTEGER :: I,J,IOS,N
 
 CUS_READDISTANCES=.FALSE.

 IU=UTL_GETUNIT(); OPEN(IU,FILE=FDISTANCES,STATUS='OLD',IOSTAT=IOS)
 IF(IOS.NE.0)THEN; WRITE(*,'(A)') 'Can not open '//TRIM(FDISTANCES); RETURN; ENDIF
 READ(IU,*) CUS_NLAY; ALLOCATE(ZIDF(CUS_NLAY),TOPIDF(CUS_NLAY),BOTIDF(CUS_NLAY),ZINFO(CUS_NLAY))
 DO I=1,CUS_NLAY
  READ(IU,'(I10,1X,A)') J,TOPIDF(I)%FNAME; IF(.NOT.IDFREAD(TOPIDF(I),TOPIDF(I)%FNAME,0))RETURN
  READ(IU,'(I10,1X,A)') J,BOTIDF(I)%FNAME; IF(.NOT.IDFREAD(BOTIDF(I),BOTIDF(I)%FNAME,0))RETURN
  READ(IU,'(I10,1X,A)') J,ZIDF(I)%FNAME; IF(.NOT.IDFREAD(ZIDF(I),ZIDF(I)%FNAME,0))RETURN
  IF(ZIDF(I)%DMAX.LT.ZIDF(I)%DMIN)ZIDF(I)%DMAX=0.0
  ZINFO(I)%NZ=ZIDF(I)%DMAX; NULLIFY(ZINFO(I)%NP)
 ENDDO
 DO I=1,4; READ(IU,*); ENDDO
 
 N=CUS_NLAY; ALLOCATE(SMPLX(N))
 
 !## number of conditions (ncon)
 NCON=0; DO
  NCON=NCON+1
  IF(NCON.GT.N)THEN
   ALLOCATE(SMPLX_DUMMY(N+DN))
   SMPLX_DUMMY(1:N)=SMPLX(1:N)
   DEALLOCATE(SMPLX); SMPLX=>SMPLX_DUMMY; N=SIZE(SMPLX)
  ENDIF
  READ(IU,'(6I6,I10,99F10.2)',IOSTAT=IOS) SMPLX(NCON)%IF1,SMPLX(NCON)%IZ1,SMPLX(NCON)%IVAR1, &
                                          SMPLX(NCON)%IF2,SMPLX(NCON)%IZ2,SMPLX(NCON)%IVAR2, &
                                          SMPLX(NCON)%IAREA,SMPLX(NCON)%PERC
  IF(IOS.NE.0)EXIT
 ENDDO
 NCON=NCON-1
 
 !## check for conflicting conditions due to inaccuracies in input data
 DO I=1,NCON; DO J=1,NCON; IF(I.EQ.J)CYCLE
  IF(SMPLX(I)%IVAR1     .EQ.SMPLX(J)%IVAR1.AND. &
     ABS(SMPLX(I)%IVAR2).EQ.ABS(SMPLX(J)%IVAR2))THEN
   !## remove the one from the buffer - apparently zero-distances
!   IF(SMPLX(I)%PERC(1).EQ.0.0)SMPLX(I)%IAREA=0
!   IF(SMPLX(I)%IAREA.LE.SMPLX(J)%IAREA)SMPLX(I)%IAREA=0
!   IF(SMPLX(J)%IAREA.LE.SMPLX(I)%IAREA)SMPLX(J)%IAREA=0
   write(*,'(A,7I4,A3,7I4)') 'DDEF #',I,SMPLX(I)%IF1,SMPLX(I)%IZ1,SMPLX(I)%IVAR1,       &
                                        SMPLX(I)%IF2,SMPLX(I)%IZ2,SMPLX(I)%IVAR2,' - ', &
                                      J,SMPLX(J)%IF1,SMPLX(J)%IZ1,SMPLX(J)%IVAR1,       &
                                        SMPLX(J)%IF2,SMPLX(J)%IZ2,SMPLX(J)%IVAR2
  ENDIF 
 ENDDO; ENDDO
 
 !## number of variables to be estimated
 NVAR=0; DO I=1,CUS_NLAY; NVAR=NVAR+ZINFO(I)%NZ; ENDDO
 ALLOCATE(ILP(NVAR,0:NPERC)); ILP=0

 CUS_READDISTANCES=.TRUE.
 
 END FUNCTION CUS_READDISTANCES
      
 !###======================================================================
 LOGICAL FUNCTION CUS_DISTANCES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I,J,K,KK,IROW,ICOL,Z,ZZ,N,MX,IL1,IL2,IL
 REAL :: T,TT,B,BB,D
 INTEGER(KIND=1),ALLOCATABLE,DIMENSION(:) :: ILTB,IZTB

 CUS_DISTANCES=.FALSE.
 
 WRITE(IU,'(/A/)') 'Distances'
 WRITE(IU,'(6A6,A10,99F10.2)') '#file','izone','#','#file','izone','#','size',PERC 

 N=SUM(ZINFO%NZ); ALLOCATE(ILTB(SIZE(TOPIDF)),IZTB(N))

 DO I=1,SIZE(TOPIDF)

  WRITE(*,'(2(I3.3,A))') I,'-',SIZE(TOPIDF),' Computing distances for '//TRIM(TOPIDF(I)%FNAME)//' ...'
  CALL IDFDEALLOCATEX(TOPIDF(I)); IF(.NOT.IDFREAD(TOPIDF(I),TOPIDF(I)%FNAME,1))RETURN
  CALL IDFDEALLOCATEX(BOTIDF(I)); IF(.NOT.IDFREAD(BOTIDF(I),BOTIDF(I)%FNAME,1))RETURN
  CALL IDFDEALLOCATEX(ZIDF(I))  ; IF(.NOT.IDFREAD(ZIDF(I)  ,ZIDF(I)%FNAME  ,1))RETURN

  IF(IEXPZONE.GT.0)CALL CUS_EXTENT_TOPBOT(I)
  
  !## get array above/beneath
  ALLOCATE(DZTOP(TOPIDF(I)%NCOL,TOPIDF(I)%NROW),DZBOT(TOPIDF(I)%NCOL,TOPIDF(I)%NROW), &
           IZTOP(TOPIDF(I)%NCOL,TOPIDF(I)%NROW),IZBOT(TOPIDF(I)%NCOL,TOPIDF(I)%NROW), &
           ILTOP(TOPIDF(I)%NCOL,TOPIDF(I)%NROW),ILBOT(TOPIDF(I)%NCOL,TOPIDF(I)%NROW))  

  !## initialize arrays
  DZTOP=10.0E10; DZBOT=10.0E10; IZTOP=0; IZBOT=0; ILTOP=0; ILBOT=0; ILTB=INT(0,1); IZTB=INT(0,1)

  !## loop over all other files
  DO J=1,SIZE(TOPIDF)
   IF(I.EQ.J)CYCLE

   CALL IDFDEALLOCATEX(TOPIDF(J)); IF(.NOT.IDFREAD(TOPIDF(J),TOPIDF(J)%FNAME,1))RETURN
   CALL IDFDEALLOCATEX(BOTIDF(J)); IF(.NOT.IDFREAD(BOTIDF(J),BOTIDF(J)%FNAME,1))RETURN
   CALL IDFDEALLOCATEX(ZIDF(J))  ; IF(.NOT.IDFREAD(ZIDF(J)  ,ZIDF(J)%FNAME  ,1))RETURN

   !## construct buffer
   IF(IEXPZONE.GT.0)CALL CUS_EXTENT_TOPBOT(J)

   DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL

    !## skip nodata zone
    Z=INT(ZIDF(I)%X(ICOL,IROW)); IF(Z.LE.0)CYCLE !IF(Z.EQ.INT(ZIDF(I)%NODATA))CYCLE
    
    T=TOPIDF(I)%X(ICOL,IROW); B=BOTIDF(I)%X(ICOL,IROW)
    
    !## available thickness of i'th file
    IF(T-B.GT.0.0)THEN
     !## top, bot and zone number
     TT=TOPIDF(J)%X(ICOL,IROW); BB=BOTIDF(J)%X(ICOL,IROW); ZZ=INT(ZIDF(J)%X(ICOL,IROW))
     IF(ZZ.EQ.INT(ZIDF(I)%NODATA))CYCLE

     !## adjust tt/bb in case of buffer on top/bottom of it, ignore layer in between
     IF(ZZ.LT.0)THEN
      !## layer on top of it
      IF(TT.GE.T.AND.BB.LT.T)BB=MAX(BB,T)
      !## layer beneath it
      IF(BB.LE.B.AND.TT.GT.B)TT=MIN(TT,B)
     ENDIF
     
     !## available thickness of j'th file
     IF(TT-BB.GT.0.0)THEN
      !## available thickness of j'th file, compute distance
      IF(BB.GE.T)THEN     !## j'th file above (positive)
       D=BB-T
       !## do not include layers from buffer not connected to current layer
       IF(ZZ.LT.0.AND.D.NE.0.0)D=10.E10
       !## check whether this layer is closer than layer already processed
       IF(D.LT.DZTOP(ICOL,IROW))THEN
        ILTOP(ICOL,IROW)=J; IZTOP(ICOL,IROW)=ZZ; DZTOP(ICOL,IROW)=D
       ENDIF
      ELSEIF(TT.LE.B)THEN !## j'th file beneath (negative)
       D=B-TT
       !## do not include layers from buffer not connected to current layer
       IF(ZZ.LT.0.AND.D.NE.0.0)D=10.E10
       !## check whether this layer is closer than layer already processed
       IF(D.LT.DZBOT(ICOL,IROW))THEN
        ILBOT(ICOL,IROW)=J; IZBOT(ICOL,IROW)=ZZ; DZBOT(ICOL,IROW)=D
       ENDIF
      ENDIF
     ENDIF
    ENDIF
   ENDDO; ENDDO
      
   CALL IDFDEALLOCATEX(TOPIDF(J)); CALL IDFDEALLOCATEX(BOTIDF(J)); CALL IDFDEALLOCATEX(ZIDF(J))
    
  ENDDO
    
  !## clean buffer assignment for those allready assigned outside buffer
!if(.false.)then
  IF(IEXPZONE.GT.0)THEN

   !## fill in zones captured/found nearest of "real" connections of parts of layers
   DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL
    Z=INT(ZIDF(I)%X(ICOL,IROW)); IF(Z.LT.0)CYCLE
    J=ILTOP(ICOL,IROW); K=IZTOP(ICOL,IROW)
    IF(J.GT.0.AND.K.GT.0)THEN; ILTB(J)=INT(1,1); IZTB(K)=INT(1,1); ENDIF
    J=ILBOT(ICOL,IROW); K=IZBOT(ICOL,IROW)
    IF(J.GT.0.AND.K.GT.0)THEN; ILTB(J)=INT(1,1); IZTB(K)=INT(1,1); ENDIF
   ENDDO; ENDDO
   
!   !## set distance to zero for lateral connection
!   DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL
!    J=ILTOP(ICOL,IROW); K=IZTOP(ICOL,IROW)
!    IF(J.GT.0.AND.K.LT.0)THEN; DZTOP(ICOL,IROW)=0.0; ENDIF
!    J=ILBOT(ICOL,IROW); K=IZBOT(ICOL,IROW)
!    IF(J.GT.0.AND.K.LT.0)THEN; DZTOP(ICOL,IROW)=0.0; ENDIF
!   ENDDO; ENDDO

   DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL

    !## clean "buffer" whenever reference already made from "real" to "real" part of layer
    J=ILTOP(ICOL,IROW); K=IZTOP(ICOL,IROW)
    IF(J.GT.0.AND.K.LT.0)THEN
     K=ABS(K); IF(ILTB(J).EQ.INT(1,1).AND.IZTB(K).EQ.INT(1,1))THEN
      ILTOP(ICOL,IROW)=0; IZTOP(ICOL,IROW)=0 
     ENDIF
    ENDIF
    J=ILBOT(ICOL,IROW); K=IZBOT(ICOL,IROW)
    IF(J.GT.0.AND.K.LT.0)THEN
     K=ABS(K); IF(ILTB(J).EQ.INT(1,1).AND.IZTB(K).EQ.INT(1,1))THEN
      ILBOT(ICOL,IROW)=0; IZBOT(ICOL,IROW)=0
     ENDIF
    ENDIF

    IZTOP(ICOL,IROW)=ABS(IZTOP(ICOL,IROW))
    IZBOT(ICOL,IROW)=ABS(IZBOT(ICOL,IROW))
   
   ENDDO; ENDDO
  ENDIF
  
  TOPIDF(I)%X=DZTOP; TOPIDF(I)%NODATA=10.0E10
  IF(.NOT.IDFWRITE(  TOPIDF(I),'d:\iMOD-TEST\IMODBATCH_CUS\tmp\dztop_l'//TRIM(ITOS(i))//'.IDF',1))STOP
  TOPIDF(I)%X=DZBOT; TOPIDF(I)%NODATA=10.0E10
  IF(.NOT.IDFWRITE(  TOPIDF(I),'d:\iMOD-TEST\IMODBATCH_CUS\tmp\dzbot_l'//TRIM(ITOS(i))//'.IDF',1))STOP
  TOPIDF(I)%X=ILTOP; TOPIDF(I)%NODATA=0.0
  IF(.NOT.IDFWRITE(  TOPIDF(I),'d:\iMOD-TEST\IMODBATCH_CUS\tmp\iltop_l'//TRIM(ITOS(i))//'.IDF',1))STOP
  TOPIDF(I)%X=ILBOT; TOPIDF(I)%NODATA=0.0
  IF(.NOT.IDFWRITE(  TOPIDF(I),'d:\iMOD-TEST\IMODBATCH_CUS\tmp\ilbot_l'//TRIM(ITOS(i))//'.IDF',1))STOP
  TOPIDF(I)%X=IZTOP; TOPIDF(I)%NODATA=0.0
  IF(.NOT.IDFWRITE(  TOPIDF(I),'d:\iMOD-TEST\IMODBATCH_CUS\tmp\iztop_l'//TRIM(ITOS(i))//'.IDF',1))STOP
  TOPIDF(I)%X=IZBOT; TOPIDF(I)%NODATA=0.0
  IF(.NOT.IDFWRITE(  TOPIDF(I),'d:\iMOD-TEST\IMODBATCH_CUS\tmp\izbot_l'//TRIM(ITOS(i))//'.IDF',1))STOP

  !## number of zones --- allocate sort()
  ALLOCATE(DZ(ZINFO(I)%NZ))
  DO J=1,ZINFO(I)%NZ
   N=ZINFO(I)%NP(J)*2 !## above and beneath
   ALLOCATE(DZ(J)%D(N),DZ(J)%IZ(N))
  ENDDO

  !## loop over all other files to compute percentiles
  IL1=MAX(1,MIN(MINVAL(ILTOP),MINVAL(ILBOT))); IL2=MAX(MAXVAL(ILTOP),MAXVAL(ILBOT))
  DO IL=IL1,IL2
   IF(I.EQ.IL)CYCLE

   !## fill dz for ivar for each jvar
   DZ%NZ=0
   DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL
    Z=INT(ZIDF(I)%X(ICOL,IROW))
    Z=ABS(Z)
    IF(Z.GT.0)THEN
     !## try top current modellayer il
     IF(ILTOP(ICOL,IROW).EQ.IL)THEN
      ZZ                = IZTOP(ICOL,IROW)
      D                 = DZTOP(ICOL,IROW)
      DZ(Z)%NZ          = DZ(Z)%NZ+1
      DZ(Z)%D (DZ(Z)%NZ)= D
      DZ(Z)%IZ(DZ(Z)%NZ)= ZZ
     ENDIF
     !## try bottom current modellayer il
     IF(ILBOT(ICOL,IROW).EQ.IL)THEN
      ZZ                = IZBOT(ICOL,IROW)
      D                 = DZBOT(ICOL,IROW)
      DZ(Z)%NZ          = DZ(Z)%NZ+1
      DZ(Z)%D (DZ(Z)%NZ)= D
      DZ(Z)%IZ(DZ(Z)%NZ)=-ZZ
     ENDIF
    ENDIF
   ENDDO; ENDDO

   !## try next layer
   IF(SUM(DZ%NZ).EQ.0)CYCLE
   
   !## write results to logfile
   !## +1 for izone means relationship might be above
   !## -1 for izone means relationship might be beneath
   !## sort for zz values
   DO Z=1,ZINFO(I)%NZ
    IF(DZ(Z)%NZ.EQ.0.0)CYCLE
    CALL UTL_QKSORT_INT(DZ(Z)%IZ,DZ(Z)%D,DZ(Z)%NZ,DZ(Z)%NZ)
    !## get percentile for z according to zz
    KK=1; DO K=2,DZ(Z)%NZ
     IF(DZ(Z)%IZ(K).NE.DZ(Z)%IZ(K-1))THEN
      CALL UTL_GETMED(DZ(Z)%D(KK:),K-KK,-999.99,PERC,NPERC,MX,XMED)
      KK=K
      WRITE(IU,'(6I6,I10,99F10.2)') I,Z,CUS_GETIVAR(I,Z),IL,DZ(Z)%IZ(K-1),CUS_GETIVAR(IL,DZ(Z)%IZ(K-1)),MX,XMED 
     ENDIF
    ENDDO
    CALL UTL_GETMED(DZ(Z)%D(KK:),K-KK,-999.99,PERC,NPERC,MX,XMED)
    WRITE(IU,'(6I6,I10,99F10.2)') I,Z,CUS_GETIVAR(I,Z),IL,DZ(Z)%IZ(K-1),CUS_GETIVAR(IL,DZ(Z)%IZ(K-1)),MX,XMED 
   ENDDO

  ENDDO  

  DEALLOCATE(DZTOP,DZBOT,IZTOP,IZBOT,ILTOP,ILBOT)
  
  DO J=1,SIZE(DZ)
   IF(ASSOCIATED(DZ(J)%D)) DEALLOCATE(DZ(J)%D); IF(ASSOCIATED(DZ(J)%IZ))DEALLOCATE(DZ(J)%IZ)
  ENDDO
  DEALLOCATE(DZ)
  CALL IDFDEALLOCATEX(TOPIDF(I)); CALL IDFDEALLOCATEX(BOTIDF(I)); CALL IDFDEALLOCATEX(ZIDF(I))
 
 ENDDO
 CLOSE(IU)

 DEALLOCATE(ILTB,IZTB)

 CUS_DISTANCES=.TRUE.

 END FUNCTION CUS_DISTANCES
 
 !###======================================================================
 LOGICAL FUNCTION CUS_TRACE(IIDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIDF
 INTEGER :: IROW,ICOL,JROW,JCOL,IPZ,I,MAXTHREAD,NTHREAD,DTERM,IMENU,MAXN,TTHREAD
 REAL :: THICKNESS  
 INTEGER(KIND=1),POINTER,DIMENSION(:) :: ISPEC 
 INTEGER(KIND=2),POINTER,DIMENSION(:,:) :: THREAD,YSEL
 INTEGER,POINTER,DIMENSION(:) :: NT,NT_DUMMY
 
 CUS_TRACE=.FALSE.

 MAXTHREAD=1000; MAXN=MAXTHREAD; ALLOCATE(ISPEC(MAXTHREAD),THREAD(3,MAXTHREAD),YSEL(2,MAXTHREAD))
 DTERM= 1 !## 0=no diagonal (use 1 to apply diagonals,2=steepest)
 IMENU=-7 !4 !## 1=equal,2=less than,3=less or equal,4=greater than,5=greater than or equal,6=ne,7=never mind
          !## negative means according to last location during search
 THICKNESS=0.0 !## trace thicknesses greater than zero

 WRITE(*,'(A)') 'Writing thickness ...'
 IF(.NOT.IDFWRITE(MDLIDF(3),MDLIDF(3)%FNAME,1))STOP 'ERROR WRITING THICKNESSES'

 WRITE(*,'(A,F10.2)') 'Tracing thickness > 0.0, max. vertical stepsize =',CRIT_THICKNESS

 !## set thresshold
 MDLIDF(3)%X=CRIT_THICKNESS 
 
 !## search for clay extinctions
 IPZ=0; ALLOCATE(NT(10))
 DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL
    
  IF(MDLIDF(2)%X(ICOL,IROW).EQ.MDLIDF(2)%NODATA.AND. &  !## not yet visited
     MDLIDF(1)%X(ICOL,IROW).NE.MDLIDF(1)%NODATA)THEN    !## level.ne.nodata
    
   !## set begin values
   IPZ=IPZ+1; JROW=IROW; JCOL=ICOL; TTHREAD=0
   !## try to find another, close by start point associated with this zone
   DO

    NTHREAD=1; YSEL(1,NTHREAD)=JCOL; YSEL(2,NTHREAD)=JROW; MDLIDF(2)%X(JCOL,JROW)=REAL(IPZ)

    !## trace all ne equal nodata and step less than thickness (mdlidf(3))
    CALL IDFEDITTRACE(MDLIDF(1),MDLIDF(2),THREAD,YSEL,ISPEC,DTERM,IMENU,MAXTHREAD,MAXN, &
                      MDLIDF(1)%NODATA,NTHREAD,IPZ,THRESHOLD=MDLIDF(3))

    !## count total length of thread
    TTHREAD=TTHREAD+NTHREAD
    !## do not try to find another startlocation
    IF(IEXPZONE.EQ.0)EXIT
    !## try to find another start location for current zone
    IF(.NOT.CUS_EXTENTSEARCH(JROW,JCOL,IPZ))EXIT

   ENDDO
   
   !## extent zone by a buffer with -zone numbers filled in
   IF(IEXPZONE.GT.0)CALL CUS_EXTENT_ZONE(IPZ,TTHREAD)
      
   !## store number of poins for current zone
   IF(IPZ.GT.SIZE(NT))THEN
    I=SIZE(NT); ALLOCATE(NT_DUMMY(I*2)); NT_DUMMY(1:I)=NT(1:I); DEALLOCATE(NT); NT=>NT_DUMMY
   ENDIF
   NT(IPZ)=TTHREAD 
  ENDIF  
   
 ENDDO; ENDDO

 WRITE(*,'(A,I10,A/)') ' ... found ',IPZ,' individual zones'

 IF(.NOT.IDFWRITE(MDLIDF(2),MDLIDF(2)%FNAME,1))STOP 'ERROR WRITING ZONES'

 ZINFO(IIDF)%NZ=IPZ
 ALLOCATE(ZINFO(IIDF)%NP(ZINFO(IIDF)%NZ))
 DO I=1,IPZ; ZINFO(IIDF)%NP(I)=NT(I); ENDDO
   
 DEALLOCATE(THREAD,ISPEC,YSEL,NT)

 CUS_TRACE=.TRUE.
 
 END FUNCTION CUS_TRACE
 
 !###======================================================================
 LOGICAL FUNCTION CUS_EXTENTSEARCH(JROW,JCOL,IPZ)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPZ
 INTEGER,INTENT(OUT) :: JROW,JCOL
 INTEGER :: IROW,ICOL
 REAL :: DZ

 CUS_EXTENTSEARCH=.TRUE.
 
 !## find another start location close enough
 DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL
  IF(INT(MDLIDF(2)%X(ICOL,IROW)).EQ.IPZ)THEN
   !## search along square box
   DO JROW=MAX(1,IROW-IEXPZONE),MIN(IROW+IEXPZONE,MDLIDF(1)%NROW)
    DO JCOL=MAX(1,ICOL-IEXPZONE),MIN(ICOL+IEXPZONE,MDLIDF(1)%NCOL)
     !## found appropriate location, continue from there
     IF(MDLIDF(1)%X(JCOL,JROW).NE.MDLIDF(1)%NODATA.AND. &
        MDLIDF(2)%X(JCOL,JROW).EQ.MDLIDF(2)%NODATA)THEN
      DZ=MDLIDF(1)%X(ICOL,IROW)-MDLIDF(1)%X(JCOL,JROW)
      IF(ABS(DZ).LE.MDLIDF(3)%X(JCOL,JROW))RETURN
     ENDIF
    ENDDO
   ENDDO
  ENDIF
 ENDDO; ENDDO
 
 CUS_EXTENTSEARCH=.FALSE.
 
 END FUNCTION CUS_EXTENTSEARCH

 !###======================================================================
 SUBROUTINE CUS_EXTENT_ZONE(IPZ,TTHREAD)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IPZ
 INTEGER,INTENT(INOUT) :: TTHREAD
 INTEGER :: IROW,ICOL,JROW,JCOL

 !## find another start location close enough
 DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL
  IF(INT(MDLIDF(2)%X(ICOL,IROW)).EQ.IPZ)THEN
   !## search along square box
   DO JROW=MAX(1,IROW-IEXPZONE),MIN(IROW+IEXPZONE,MDLIDF(1)%NROW)
    DO JCOL=MAX(1,ICOL-IEXPZONE),MIN(ICOL+IEXPZONE,MDLIDF(1)%NCOL)
     !## found appropriate location, insert buffer
     IF(MDLIDF(2)%X(JCOL,JROW).EQ.MDLIDF(2)%NODATA)THEN
      MDLIDF(2)%X(JCOL,JROW)=-REAL(IPZ)
      TTHREAD=TTHREAD+1
     ENDIF
    ENDDO
   ENDDO
  ENDIF
 ENDDO; ENDDO
 
 END SUBROUTINE CUS_EXTENT_ZONE
 
 !###======================================================================
 SUBROUTINE CUS_EXTENT_TOPBOT(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 INTEGER :: IROW,ICOL,JROW,JCOL

 !## find another start location close enough
 DO IROW=1,TOPIDF(I)%NROW; DO ICOL=1,TOPIDF(I)%NCOL
  IF(INT(ZIDF(I)%X(ICOL,IROW)).GT.0)THEN
   !## search along square box
   DO JROW=MAX(1,IROW-IEXPZONE),MIN(IROW+IEXPZONE,TOPIDF(I)%NROW)
    DO JCOL=MAX(1,ICOL-IEXPZONE),MIN(ICOL+IEXPZONE,TOPIDF(I)%NCOL)
     !## found appropriate location, insert buffer
     IF(INT(ZIDF(I)%X(JCOL,JROW)).EQ.-INT(ZIDF(I)%X(ICOL,IROW)))THEN
      TOPIDF(I)%X(JCOL,JROW)=TOPIDF(I)%X(ICOL,IROW)
      BOTIDF(I)%X(JCOL,JROW)=BOTIDF(I)%X(ICOL,IROW)
     ENDIF
    ENDDO
   ENDDO
  ENDIF
 ENDDO; ENDDO
 
 END SUBROUTINE CUS_EXTENT_TOPBOT

 !###======================================================================
 LOGICAL FUNCTION CUS_CALCTHICKNESS(IIDF)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IIDF
 INTEGER :: IROW,ICOL,I,J,NCOR
 REAL :: T,B,D

 CUS_CALCTHICKNESS=.FALSE.
 
 WRITE(*,'(/A)') 'Reading data from:'
 WRITE(*,'(A)') '[TOP] - '//TRIM(TOPIDF(IIDF)%FNAME)
 !## no interpolation, take data as it is
 IF(.NOT.IDFREADSCALE(TOPIDF(IIDF)%FNAME,MDLIDF(1),10,0,0.0,0))RETURN
 WRITE(*,'(A)') '[BOT] - '//TRIM(BOTIDF(IIDF)%FNAME)
 !## no interpolation, take data as it is
 IF(.NOT.IDFREADSCALE(BOTIDF(IIDF)%FNAME,MDLIDF(2),10,0,0.0,0))RETURN
 
 !## check consistency (thickness<0.0)
 NCOR=0
 DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL  
  T=MDLIDF(1)%X(ICOL,IROW); B=MDLIDF(2)%X(ICOL,IROW) 
  IF(T.NE.MDLIDF(1)%NODATA.AND.B.NE.MDLIDF(2)%NODATA)THEN
   IF(T.LE.B)THEN
    MDLIDF(1)%X(ICOL,IROW)=MDLIDF(1)%NODATA; MDLIDF(2)%X(ICOL,IROW)=MDLIDF(2)%NODATA
   ENDIF
  ELSE
   MDLIDF(1)%X(ICOL,IROW)=MDLIDF(1)%NODATA; MDLIDF(2)%X(ICOL,IROW)=MDLIDF(2)%NODATA
  ENDIF
  IF(NCLIP.EQ.1)THEN
   !## check clip
   IF(CLIPIDF%X(ICOL,IROW).NE.ICLIP(IIDF,1).AND. &
      CLIPIDF%X(ICOL,IROW).NE.ICLIP(IIDF,2))THEN
    NCOR=NCOR+1; MDLIDF(1)%X(ICOL,IROW)=MDLIDF(1)%NODATA; MDLIDF(2)%X(ICOL,IROW)=MDLIDF(2)%NODATA
   ENDIF
  ENDIF
 ENDDO; ENDDO

 !## clip data for clip-idf
 IF(NCLIP.EQ.1)WRITE(*,'(A)') '[COR] - '//TRIM(ITOS(NCOR))

 I=INDEX(TOPIDF(IIDF)%FNAME,'\',.TRUE.); J=INDEX(TOPIDF(IIDF)%FNAME,'.',.TRUE.)
 TOPIDF(IIDF)%FNAME=TRIM(OUTPUTFOLDER)//'\topbottom\'//TOPIDF(IIDF)%FNAME(I+1:J-1)//'.idf'
 IF(.NOT.IDFWRITE(MDLIDF(1),TOPIDF(IIDF)%FNAME,1))STOP 'ERROR WRITING TOP'
 I=INDEX(BOTIDF(IIDF)%FNAME,'\',.TRUE.); J=INDEX(BOTIDF(IIDF)%FNAME,'.',.TRUE.)
 BOTIDF(IIDF)%FNAME=TRIM(OUTPUTFOLDER)//'\topbottom\'//BOTIDF(IIDF)%FNAME(I+1:J-1)//'.idf'
 IF(.NOT.IDFWRITE(MDLIDF(2),BOTIDF(IIDF)%FNAME,1))STOP 'ERROR WRITING BOT'
 IF(.NOT.IDFALLOCATEX(MDLIDF(3)))STOP 'Can not allocate mdlfidf(3)'
 
 !## compute thickness, reset mdlidf(2) and use mdlidf(1)=top
 DO IROW=1,MDLIDF(1)%NROW; DO ICOL=1,MDLIDF(1)%NCOL
  T=MDLIDF(1)%X(ICOL,IROW); B=MDLIDF(2)%X(ICOL,IROW); D=0.0
  IF(T.NE.MDLIDF(1)%NODATA.AND.B.NE.MDLIDF(2)%NODATA) D=MAX(0.0,T-B)
  MDLIDF(3)%X(ICOL,IROW)=D
  IF(D.EQ.0.0)MDLIDF(1)%X(ICOL,IROW)=MDLIDF(1)%NODATA
 ENDDO; ENDDO
 MDLIDF(3)%NODATA=0.0; MDLIDF(2)%X=MDLIDF(2)%NODATA
 
 I=INDEX(TOPIDF(IIDF)%FNAME,'\',.TRUE.); J=INDEX(TOPIDF(IIDF)%FNAME,'.',.TRUE.)
 MDLIDF(3)%FNAME =TRIM(OUTPUTFOLDER)//'\thickness\'//TOPIDF(IIDF)%FNAME(I+1:J-1)//'_thickness.idf'
 MDLIDF(2)%FNAME =TRIM(OUTPUTFOLDER)//'\objects\'  //TOPIDF(IIDF)%FNAME(I+1:J-1)//'_objects.idf'
 ZIDF(IIDF)%FNAME=MDLIDF(2)%FNAME

 WRITE(IU,'(I10,1X,A)') IIDF,TRIM(TOPIDF(IIDF)%FNAME)
 WRITE(IU,'(I10,1X,A)') IIDF,TRIM(BOTIDF(IIDF)%FNAME)
 WRITE(IU,'(I10,1X,A)') IIDF,TRIM(MDLIDF(2)%FNAME)

 CUS_CALCTHICKNESS=.TRUE.
 
 END FUNCTION CUS_CALCTHICKNESS
 
 !###======================================================================
 LOGICAL FUNCTION CUS_GETFILES()
 !###======================================================================
 IMPLICIT NONE
 CHARACTER(LEN=52) :: WC,CTYPE
 CHARACTER(LEN=256) :: ROOT
 INTEGER :: I,J,K
 
 CUS_GETFILES=.TRUE.; IF(ALLOCATED(TOPIDF))RETURN; CUS_GETFILES=.FALSE.
 
 WRITE(*,'(/A)') 'Searching for IDF files ...'

 !## get list of "regis"-files
 I=INDEX(REGISTOP,'\',.TRUE.); ROOT=REGISTOP(:I-1); WC=UTL_CAP(TRIM(REGISTOP(I+1:)),'U')
 IF(.NOT.UTL_DIRINFO_POINTER(ROOT,WC,REGISFILES,'F'))RETURN
 
 I=SIZE(REGISFILES); ALLOCATE(TOPIDF(I),BOTIDF(I),ZIDF(I),ZINFO(I))
 DO I=1,SIZE(REGISFILES)
  CALL IDFNULLIFY(TOPIDF(I)); CALL IDFNULLIFY(BOTIDF(I)); CALL IDFNULLIFY(ZIDF(I))
  NULLIFY(ZINFO(I)%NP); ZINFO(I)%NZ=0
 ENDDO
 DO I=1,SIZE(REGISFILES); REGISFILES(I)=UTL_CAP(TRIM(ROOT)//'\'//TRIM(REGISFILES(I)),'U'); ENDDO

 DO I=1,SIZE(REGISFILES)
  J=INDEX(REGISFILES(I),'\',.TRUE.)+1; K=INDEX(REGISFILES(I),TRIM(WC(2:)))-1
  CTYPE=REGISFILES(I)(J:K)
  TOPIDF(I)%FNAME=REGISFILES(I)
  BOTIDF(I)%FNAME=UTL_SUBST(REGISBOT,'*',TRIM(CTYPE))
 ENDDO
 
 IF(ASSOCIATED(REGISFILES))DEALLOCATE(REGISFILES)

 CUS_GETFILES=.TRUE.
 
 END FUNCTION CUS_GETFILES

 !###======================================================================
 LOGICAL FUNCTION CUS_READFILES()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 CUS_READFILES=.FALSE.
 
 WRITE(*,'(/A)') 'Reading IDF files ...'
 DO I=1,SIZE(TOPIDF)
  IF(.NOT.IDFREAD(TOPIDF(I),TOPIDF(I)%FNAME,0))RETURN
  IF(.NOT.IDFREAD(BOTIDF(I),BOTIDF(I)%FNAME,0))RETURN
 ENDDO
 IF(MDLIDF(1)%XMAX.EQ.MDLIDF(1)%XMIN)THEN
  IF(.NOT.IDF_EXTENT(SIZE(TOPIDF),TOPIDF,MDLIDF(1),2))RETURN
  IF(.NOT.IDF_EXTENT(SIZE(BOTIDF),BOTIDF,MDLIDF(2),2))RETURN
  IF(.NOT.IDF_EXTENT(2           ,MDLIDF,MDLIDF(1),2))RETURN
 ELSE
  CALL UTL_IDFSNAPTOGRID(MDLIDF(1)%XMIN,MDLIDF(1)%XMAX,MDLIDF(1)%YMIN,MDLIDF(1)%YMAX,MDLIDF(1)%DX,MDLIDF(1)%NCOL,MDLIDF(1)%NROW)
 ENDIF
 CALL IDFCOPY(MDLIDF(1),MDLIDF(2)); CALL IDFCOPY(MDLIDF(1),MDLIDF(3))
 
 CUS_READFILES=.TRUE.
 
 END FUNCTION CUS_READFILES

 !###====================================================================== 
 SUBROUTINE CUS_DEALLOCATE()
 !###======================================================================
 IMPLICIT NONE
 INTEGER :: I
 
 IF(ASSOCIATED(REGISFILES))DEALLOCATE(REGISFILES)
 
 IF(ALLOCATED(TOPIDF))THEN; CALL IDFDEALLOCATE(TOPIDF,SIZE(TOPIDF)); DEALLOCATE(TOPIDF); ENDIF
 IF(ALLOCATED(BOTIDF))THEN; CALL IDFDEALLOCATE(BOTIDF,SIZE(BOTIDF)); DEALLOCATE(BOTIDF); ENDIF
 IF(ALLOCATED(ZIDF))THEN;   CALL IDFDEALLOCATE(ZIDF,SIZE(ZIDF))    ; DEALLOCATE(ZIDF)  ; ENDIF
 IF(ALLOCATED(ZINFO))THEN
  DO I=1,SIZE(ZINFO); IF(ASSOCIATED(ZINFO(I)%NP))DEALLOCATE(ZINFO(I)%NP); ENDDO; DEALLOCATE(ZINFO)
 ENDIF
 IF(ALLOCATED(DZ))THEN
  DO I=1,SIZE(DZ)
   IF(ASSOCIATED(DZ(I)%D))DEALLOCATE(DZ(I)%D)
   IF(ASSOCIATED(DZ(I)%IZ))DEALLOCATE(DZ(I)%IZ)
  ENDDO   
  DEALLOCATE(DZ)
 ENDIF
 
 IF(ALLOCATED(DZTOP))DEALLOCATE(DZTOP); IF(ALLOCATED(DZBOT))DEALLOCATE(DZBOT) 
 IF(ALLOCATED(IZTOP))DEALLOCATE(IZTOP); IF(ALLOCATED(IZBOT))DEALLOCATE(IZBOT) 
 IF(ALLOCATED(ILTOP))DEALLOCATE(ILTOP); IF(ALLOCATED(ILBOT))DEALLOCATE(ILBOT) 
   
 END SUBROUTINE CUS_DEALLOCATE
  
END MODULE MOD_CUS