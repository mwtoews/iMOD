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
MODULE MOD_IPEST_GLM

CONTAINS

 !#####=================================================================
 SUBROUTINE IPEST_GLM_MAIN(RUNBAT)
 !#####=================================================================
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: RUNBAT
 
 END SUBROUTINE IPEST_GLM_MAIN
 
 !#####=================================================================
 SUBROUTINE IPEST_GLM_INIT()
 !#####=================================================================
 IMPLICIT NONE
 
!IF(ASSOCIATED(PEST%PARAM))THEN
!  DO I=1,SIZE(PEST%PARAM)
!   LINE=TRIM(ITOS(PEST%PARAM(I)%PACT))           //','// &
!        TRIM(PEST%PARAM(I)%PPARAM)               //','// &
!        TRIM(ITOS(PEST%PARAM(I)%PILS))           //','// &
!        TRIM(ITOS(PEST%PARAM(I)%PIZONE))         //','// &    
!        TRIM(RTOS(PEST%PARAM(I)%PINI,'G',7))     //','// &
!        TRIM(RTOS(PEST%PARAM(I)%PDELTA,'G',7))   //','// &
!        TRIM(RTOS(PEST%PARAM(I)%PMIN,'G',7))     //','// &
!        TRIM(RTOS(PEST%PARAM(I)%PMAX,'G',7))     //','// &
!        TRIM(RTOS(PEST%PARAM(I)%PINCREASE,'G',7))//','// &
!        TRIM(ITOS(PEST%PARAM(I)%PIGROUP))        //','// &
!        TRIM(ITOS(PEST%PARAM(I)%PLOG))
!   IF(TRIM(PEST%PARAM(I)%ACRONYM).NE.'')LINE=TRIM(LINE)//','//TRIM(PEST%PARAM(I)%ACRONYM)
!   WRITE(IU,'(A)') TRIM(LINE)
!  ENDDO
  
  END SUBROUTINE IPEST_GLM_INIT

  !#####=================================================================
  LOGICAL FUNCTION IPEST_GLM_NEXT()
  !#####=================================================================
  IMPLICIT NONE
!  REAL(KIND=8) :: IMPROVEMENT,F,GUPDATE
!  INTEGER :: I,J,ILOG,ISTOP

  IPEST_GLM_NEXT=.FALSE.

  !## compute objective function
  CALL IPEST_GLM_GETJ()
!
! IF(LSENS)THEN
!!  !## next parameter combination
!!  IF(.NOT.PESTNEXTSENS())STOP
!!  IF(.NOT.PESTNEXTGRAD())STOP
!  IF(.NOT.PESTNEXTGRAD())CALL PESTGRADIENT(root,idf)
! ELSEIF(LGRAD)THEN
!  !## what proces is going on?
!  IF(.NOT.PESTNEXTGRAD())THEN
!   !## get gradient
!   CALL PESTGRADIENT(ROOT,idf)
!   CALL PEST_ECHOPARAMETERS(GUPDATE)
!   LLNSRCH=.TRUE.; PEST_ILNSRCH=1; LGRAD=.FALSE.; PEST_IGRAD=0
!  ENDIF
! ELSEIF(LLNSRCH)THEN
!  !## no reduction of objection function, change (u(i))
!  IF(TJ.GT.TJOBJ)THEN
!!   DAMPINGFACTOR=DAMPINGFACTOR*NDAMPING
!!   NDAMPING=1.0 !## do it onces only
!   IF(.NOT.PESTUPGRADEVECTOR(0.5D0,.FALSE.))THEN
!    STOP 'ERROR PESTUPGRADEVECTOR IN LINESEARCH'
!   ENDIF !# half of current search-gradient
!!   CALL PEST_ECHOPARAMETERS(GUPDATE)
!   !## start next line-search
!   PEST_ILNSRCH=PEST_ILNSRCH+1
!  ELSE
!!   DAMPINGFACTOR=DAMPINGFACTOR/NDAMPING
!!   NDAMPING=ININDAMPING !## do it onces only
!
!   CALL PEST_ECHOPARAMETERS(GUPDATE)
!
!   !## update alpha for parameters in same group
!   DO I=1,SIZE(PARAM)
!    !## skip inactive parameters
!    IF(PARAM(I)%IACT.EQ.0)CYCLE
!    IF(PARAM(I)%IGROUP.GT.0)THEN
!     DO J=1,SIZE(PARAM)
!      IF(PARAM(I)%IGROUP.EQ.ABS(PARAM(J)%IGROUP))PARAM(J)%ALPHA(1)=PARAM(I)%ALPHA(1)
!     ENDDO
!    ENDIF
!   ENDDO
!   
!   IMPROVEMENT=0; DO I=1,SIZE(PARAM)
!    IF(PARAM(I)%IACT.EQ.0)CYCLE
!    IF(PARAM(I)%IGROUP.LT.0)CYCLE
!    IF(PARAM(I)%LOG)THEN
!     F=(EXP(PARAM(I)%ALPHA(1))/EXP(PARAM(I)%ALPHA(2)))*100.0D0
!     F=ABS(F-100.0D0)
!     IMPROVEMENT=IMPROVEMENT+F
!    ELSE
!     F=(PARAM(I)%ALPHA(1)/PARAM(I)%ALPHA(2))*100.0D0
!     F=ABS(F-100.0D0)
!     IMPROVEMENT=IMPROVEMENT+F
!    ENDIF 
!   ENDDO
!
!   WRITE(IUPESTEFFICIENCY,'(6E15.7)') TJ,SQRT(TJ),TJ/REAL(PEST_NOBS),REAL(SQRT(TJ))/REAL(PEST_NOBS),IMPROVEMENT,(TJOBJ/TJ) 
!
!   !WRITE(IUPESTRUNFILE,'(/A,I10/)') 'Copy in the runfile, iteration ',PEST_ITER
!   !DO I=1,SIZE(PARAM)
!   ! ILOG=0; IF(PARAM(I)%LOG)ILOG=1
!   ! IF(PARAM(I)%LOG)THEN
!   !  WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I4,1X),5(F10.3,1X),I4,1X,I2,1X,A)') ABS(PARAM(I)%IACT), &  !## iact
!   !      PARAM(I)%PTYPE, &         !## ptype
!   !      PARAM(I)%ILS, &           !## ilayer/system
!   !      PARAM(I)%IZONE, &         !## zone number
!   !      EXP(PARAM(I)%ALPHA(1)), & !## initial value
!   !      EXP(PARAM(I)%DELTA), &    !## finite difference step
!   !      EXP(PARAM(I)%MIN), &      !## minimal value
!   !      EXP(PARAM(I)%MAX),&       !## maximal value
!   !      PARAM(I)%FADJ,&           !## maximal adjust factor
!   !      ABS(PARAM(I)%IGROUP),&    !## group number
!   !      ILOG,&                    !## log transformed
!   !      TRIM(PARAM(I)%ACRONYM)
!   ! ELSE
!   !  WRITE(IUPESTRUNFILE,'(I2,1X,A,1X,2(I4,1X),5(F10.3,1X),I4,1X,I2,1X,A)') ABS(PARAM(I)%IACT), &  !## iact
!   !      PARAM(I)%PTYPE, & !## ptype
!   !      PARAM(I)%ILS, &   !## ilayer/system
!   !      PARAM(I)%IZONE, & !## zone number
!   !      PARAM(I)%ALPHA(1), &   !## initial value
!   !      PARAM(I)%DELTA, & !## finite difference step
!   !      PARAM(I)%MIN, &   !## minimal value
!   !      PARAM(I)%MAX,&    !## maximal value
!   !      PARAM(I)%FADJ,&   !## maximal adjust factor
!   !      ABS(PARAM(I)%IGROUP),& !## group number
!   !      ILOG, &            !## log transformed
!   !      TRIM(PARAM(I)%ACRONYM)
!   ! ENDIF 
!   !ENDDO
!
!   !## length of gradient update vector
!   IF(GUPDATE.LT.PEST_PADJ)THEN
!    CALL IMOD_UTL_PRINTTEXT('Process stopped, less than '//TRIM(IMOD_UTL_DTOS(PEST_PADJ,'F',3))//' of vector length',-1,IUPESTOUT); STOP
!   ENDIF
!
!   !## continue ?
!   IF(PEST_ITER+1.GT.PEST_NITER)THEN
!    PESTNEXT=.TRUE.  !## max. number of iterations reached
!    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Pest iteration terminated: PEST_ITER (='// &
!        TRIM(IMOD_UTL_ITOS(PEST_ITER))//') = PEST_NITER (='//TRIM(IMOD_UTL_ITOS(PEST_NITER))//')',-1,IUPESTOUT); STOP
!   ENDIF
!   IF(TJ.LE.0.0D0)THEN
!    PESTNEXT=.TRUE.
!    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Objective Function <= 0.0 ('// &
!       TRIM(IMOD_UTL_DTOS(TJ,'G',7))//')',-1,IUPESTOUT); STOP
!   ENDIF
!
!   IF(IMPROVEMENT.LE.PEST_JSTOP)THEN
!    PESTNEXT=.TRUE.  !## min. improvement reached
!    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('Pest iteration terminated decrease objective function ('// &
!        TRIM(IMOD_UTL_DTOS(100.0D0*IMPROVEMENT,'G',7))//'%) > PEST_JSTOP ('//TRIM(IMOD_UTL_DTOS(100.0D0*PEST_JSTOP,'G',7))//'%)',-1,IUPESTOUT); STOP
!   ENDIF
!
!   TJOBJ=TJ
!   !## replace old by new parameter values
!   PARAM%ALPHA(2)=PARAM%ALPHA(1)
!
!   !## next iteration
!   PEST_ITER=PEST_ITER+1
!   IF(.NOT.PESTNEXT)THEN
!    CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT); CALL IMOD_UTL_PRINTTEXT('',-1,IUPESTOUT)
!    CALL IMOD_UTL_PRINTTEXT(' *** Next Outer Iteration ***',-1,IUPESTOUT)
!   ENDIF
!   LLNSRCH=.FALSE.; LGRAD=.TRUE.; PEST_IGRAD=0; PEST_ILNSRCH=0
!   IF(.NOT.PESTNEXTGRAD())THEN
!   ENDIF
!  ENDIF
!
! ENDIF
 
 IPEST_GLM_NEXT=.TRUE.

 END FUNCTION IPEST_GLM_NEXT

 !###====================================================================
 SUBROUTINE IPEST_GLM_GETJ()
 !###====================================================================
 IMPLICIT NONE
 !INTEGER :: I,II,III,J,JJ,K,KK,ILAY,NROWIPFTXT,IUIPFTXT,NCOLIPFTXT, &
 !    IOS,NAJ,NP,N
 !REAL(KIND=8) :: X,Y,Z,H,WW,MC,MM,DHH,XCOR,YCOR,ZCOR,XCROSS,RFIT
 !CHARACTER(LEN=52) :: ID,TXT
 !DOUBLE PRECISION :: DHW
 !REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: TSNODATA,M,C,GF_H,GF_O,MCOPY,CCOPY
 !INTEGER(KIND=8),ALLOCATABLE,DIMENSION(:) :: IDATE
 !REAL(KIND=8),DIMENSION(2) :: PC,PM,DYN !## percentiles computed/measured
 !LOGICAL :: LEX
 !
 !CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Getting residuals from IPF files ...',0)
 !
 !DO I=1,ABS(IIPF)
 !
 ! !## read ipf steady-state
 ! IF(LSS)THEN
 !  J=INDEX(TS(I)%IPFNAME,CHAR(92),.TRUE.)+1; LINE=TRIM(ROOT)//CHAR(92)//TS(I)%IPFNAME(J:)
 ! ELSE
 !  J=INDEX(TS(I)%IPFNAME,CHAR(92),.TRUE.)+1
 !  LINE=TRIM(ROOT)//CHAR(92)//'timeseries'//CHAR(92)//TS(I)%IPFNAME(J:)
 ! ENDIF
 ! TS(I)%IUIPF=0; CALL IMOD_UTL_SWAPSLASH(LINE); CALL IMOD_UTL_OPENASC(TS(I)%IUIPF,LINE,'R')
 ! READ(TS(I)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(I)%NROWIPF
 ! READ(TS(I)%IUIPF,'(A256)') LINE; CALL IMOD_UTL_STRING(LINE); READ(LINE,*) TS(I)%NCOLIPF
 ! DO J=1,TS(I)%NCOLIPF; READ(TS(I)%IUIPF,*); ENDDO
 ! !## read iext,ext
 ! READ(TS(I)%IUIPF,'(A256)') LINE
 ! READ(LINE,*) TS(I)%IEXT,TS(I)%EXT
 !
 !ENDDO
 !
 !!## only one value per measurement
 !N=SUM(TS%NROWIPF)*NPER
 !IF(.NOT.ASSOCIATED(MSR%DH)) ALLOCATE(MSR%DH(0:SIZE(PARAM),N))
 !IF(.NOT.ASSOCIATED(MSR%W )) ALLOCATE(MSR%W (N)) 
 !IF(.NOT.ASSOCIATED(MSR%X )) ALLOCATE(MSR%X (N)) 
 !IF(.NOT.ASSOCIATED(MSR%Y )) ALLOCATE(MSR%Y (N)) 
 !IF(.NOT.ASSOCIATED(MSR%L )) ALLOCATE(MSR%L (N)) 
 !IF(.NOT.ASSOCIATED(MSR%CLABEL))ALLOCATE(MSR%CLABEL(N)) 
 !IF(.NOT.ALLOCATED(GF_H))ALLOCATE(GF_H(N))
 !IF(.NOT.ALLOCATED(GF_O))ALLOCATE(GF_O(N))
!
! !## initialise head-differences
! IF(PEST_IGRAD.EQ.0)THEN
!  DO I=1,SIZE(MSR%DH,2)
!   MSR%DH(PEST_IGRAD,I)=0.0
!  ENDDO
! ELSE
!  DO I=1,SIZE(MSR%DH,2)
!   MSR%DH(PEST_IGRAD,I)=MSR%DH(0,I) !## zero gradient in case parameter is fixed
!  ENDDO
! ENDIF
! 
! MSR%W=0.0
! TJ=0.0D0
!
! DO I=1,ABS(IIPF) 
!  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(I10,A)') I,','//TRIM(TS(I)%IPFNAME)
! ENDDO
!
! !## steady-state
! IF(LSS)THEN
!  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2A16,A11,6A16,A11,A32)') 'X,','Y,','ILAY,','MSR,','MDL,', &
!         'J,','WMDL,','WRESIDUAL,','WEIGH,','IPF,','LABEL'
! !## transient
! ELSE  
!  IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2A16,A11,8A16,A11,A32,1X,A15)') 'X,','Y,','ILAY,','WEIGH,','MSR,','MDL,','MDL-MSR,', &
!                                   'DYNMSR,','DYNMDL,','DYNMSR-DYNMDL,','CROSS-COR,','IPF,','LABEL','DATE'
! ENDIF
! 
! II=0
! 
! DO I=1,ABS(IIPF)
! 
!  IF(TS(I)%IEXT.EQ.0)THEN
!
!   DO J=1,TS(I)%NROWIPF
!   
!    II=II+1
!    READ(TS(I)%IUIPF,*) X,Y,ILAY,Z,MSR%W(II),H    !## w(i)=variance
!    !## weigh=1/sqrt(variance)
!    IF(TS(I)%IVCOL.GT.0)THEN
!     IF(MSR%W(II).LE.0.0D0)THEN
!      !## insert measurement only whenever h.gt.z
!      IF(H.GT.Z)THEN
!       MSR%W(II)=ABS(MSR%W(II))
!      ELSE
!       MSR%W(II)=0.0D0
!      ENDIF
!     ELSE
!      MSR%W(II)=1.0D0/SQRT(MSR%W(II))
!     ENDIF
!    ENDIF
!    
!    !## apply general multiplication for weight values
!    MSR%W(II)=FWIIPF*MSR%W(II)
!    
!    DHH=0.0D0
!    IF(ABS(H-Z).GT.PEST_DRES)THEN
!     DHH=H-Z
!    ENDIF
!    MSR%DH(PEST_IGRAD,II)=DHH  !## calculated - measured
!    DHW              =MSR%W(II)*(DHH**2.0D0)
!
!    MSR%X(II)=X
!    MSR%Y(II)=Y
!    MSR%L(II)=ILAY
!    MSR%CLABEL(II)='Measure'//TRIM(ITOS(J))//'_ipf'//TRIM(ITOS(I))
!
!    GF_H(II)         =MSR%W(II)*H
!    GF_O(II)         =MSR%W(II)*Z
!
!    TJ               = TJ+DHW
!    IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2(G15.7,1X),I10,1X,6(G15.7,1X),I10,1X,A32)') &
!        X,Y,ILAY,Z,H,DHW,MSR%W(II)*H,MSR%W(II)*(H-Z),MSR%W(II),I,MSR%CLABEL(II)
!
!   ENDDO
!  
!  !## transient
!  ELSE
!    
!   XCROSS=0.0D0
!   DO J=1,TS(I)%NROWIPF
!   
!    READ(TS(I)%IUIPF,*) X,Y,ILAY,ID,WW     !## w(i)=variance
!    !## weigh=1/stdev=1/sqrt(variance)
!    IF(TS(I)%IVCOL.GT.0)THEN
!     IF(WW.LE.0.0D0)THEN
!      WW=0.0D0
!     ELSE
!      WW=1.0/SQRT(WW)
!     ENDIF
!    ENDIF
!    
!    !## apply general multiplication for weight values
!    WW=FWIIPF*WW
!
!    LINE=TRIM(ROOT)//CHAR(92)//'timeseries'//CHAR(92)//TRIM(ID)//'.'//TRIM(TS(I)%EXT)
!    IUIPFTXT=GETUNIT(); OPEN(IUIPFTXT,FILE=LINE,STATUS='OLD',ACTION='READ')
!    
!    READ(IUIPFTXT,*) NROWIPFTXT
!    READ(IUIPFTXT,*) NCOLIPFTXT
!    ALLOCATE(TSNODATA(MAX(3,NCOLIPFTXT)))
!    DO K=1,NCOLIPFTXT; READ(IUIPFTXT,*) TXT,TSNODATA(K); ENDDO
!    ALLOCATE(M(NROWIPFTXT),C(NROWIPFTXT),IDATE(NROWIPFTXT),MCOPY(NROWIPFTXT),CCOPY(NROWIPFTXT))
!    IDATE=0; C=0.0; M=0.0; MCOPY=M; CCOPY=C
!    IF(NCOLIPFTXT.LT.3)TSNODATA(3)=TSNODATA(2)
!    
!    !## get mean measure
!    KK=0
!    DO K=1,NROWIPFTXT
!     KK=KK+1
!     READ(IUIPFTXT,*,IOSTAT=IOS) IDATE(KK),M(KK),C(KK) 
!
!     !## make double precision dates - if needed
!     IF(IDATE(KK).LT.100000000)IDATE(KK)=IDATE(KK)*1000000
!
!     !## error reading, skip it (can be caused by steady-state periods in between)
!     IF(IOS.NE.0)THEN; KK=KK-1; CYCLE; ENDIF
!
!     !## check period (if available)
!     IF(PEST_NPERIOD.GT.0)THEN
!      DO III=1,PEST_NPERIOD
!!       write(*,*) kk,iii,idate(kk),pest_iperiod(iii,1),pest_iperiod(iii,2)
!       IF(IDATE(KK).GE.PEST_IPERIOD(III,1).AND.IDATE(KK).LE.PEST_IPERIOD(III,2))EXIT
!      ENDDO
!      IF(III.GT.PEST_NPERIOD)C(KK)=TSNODATA(3)
!     ENDIF
!     IF(M(KK).EQ.TSNODATA(2).OR.C(KK).EQ.TSNODATA(3))KK=KK-1
!    ENDDO 
!
!    !## add this measurement
!    IF(KK.GT.0)THEN
!    
!     !## compute mean measurement in period
!     XCOR=-9999.99D0
!
!     !## mean values
!     MM=SUM(M(1:KK))/REAL(KK) !## measurements
!     MC=SUM(C(1:KK))/REAL(KK) !## computed
!     DO K=1,KK
!      MCOPY(K)=M(K)
!      CCOPY(K)=C(K)
!     ENDDO
!     !## percentiles
!     CALL IMOD_UTL_GETMED(MCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PM)
!     CALL IMOD_UTL_GETMED(CCOPY,KK,-999.99D0,(/10.0D0,90.0D0/),2,NAJ,PC)
!     DYN(1)=PM(2)-PM(1) !## measurements
!     DYN(2)=PC(2)-PC(1) !## computed
!     !## compute cross-correlation
!     IF(KK.GT.1)THEN
!      XCOR=0.0D0; YCOR=0.0D0; ZCOR=0.0D0
!      DO K=1,KK
!       XCOR=XCOR+(MM-M(K))*(MC-C(K))
!       YCOR=YCOR+(MM-M(K))**2.0D0
!       ZCOR=ZCOR+(MC-C(K))**2.0D0
!      ENDDO
!      IF(YCOR.NE.0.0.AND.ZCOR.NE.0.0)XCOR=XCOR/(SQRT(YCOR)*SQRT(ZCOR))
!      XCROSS=XCROSS+XCOR
!     ENDIF
!
!     !## add obseravtion
!     DO K=1,KK
!      II =II+1
!      DHH=0.0D0
!
!      !## accept residuals less than 0.1 
!      IF(ABS(C(K)-M(K)).GT.PEST_DRES)THEN
!!      IF(ABS(MC-MM).GT.PEST_DRES)THEN
!        !## target is residual (calculated minus measured)
!       DHH=DHH+PEST_ITARGET(1)*(C(K)-M(K)) !MC-MM)
!      ENDIF
!
!      IF(ABS(DYN(2)-DYN(1)).GT.PEST_DRES)THEN
!       !## target is dynamics (calculated minus measured)
!       DHH=DHH+PEST_ITARGET(2)*(DYN(2)-DYN(1))
!      ENDIF
!
!      MSR%DH(PEST_IGRAD,II)=DHH       !## - total sensitivity
!
!      MSR%X(II)=X
!      MSR%Y(II)=Y
!      MSR%L(II)=ILAY
!      MSR%CLABEL(II)=TRIM(ID)
!
!      !## weight, pest_itarget(.) should/will be summed to one
!      MSR%W(II)=WW
!
!      !## difference
!      DHW=MSR%W(II)*(DHH**2.0D0)
!      TJ=TJ+DHW
!
!      GF_H(II)=MSR%W(II)*C(K) 
!      GF_O(II)=MSR%W(II)*M(K)
!
!      IF(IUPESTRESIDUAL.GT.0)WRITE(IUPESTRESIDUAL,'(2(G15.7,1X),I10,1X,8(G15.7,1X),I10,1X,A32,1X,I15)') &
!         X,Y,ILAY,WW,M(K),C(K),C(K)-M(K),DYN(1),DYN(2),DYN(2)-DYN(1),XCOR,I,MSR%CLABEL(II),IDATE(K)
!
!      IF(PEST_ITARGET(1).EQ.0.0D0.AND.PEST_ITARGET(2).GT.0.0D0)EXIT
!     
!     ENDDO
!     
!    ENDIF
!    
!    DEALLOCATE(TSNODATA,C,M,MCOPY,CCOPY,IDATE)
!    CLOSE(IUIPFTXT)
!
!   ENDDO
!  ENDIF
!
!  CLOSE(TS(I)%IUIPF)
!
!  IF(TS(I)%NROWIPF.GT.0)THEN
!   IF(.NOT.LSS)CALL IMOD_UTL_PRINTTEXT('MEAN Cross-Correlation         : '// &
!          TRIM(IMOD_UTL_DTOS(REAL(XCROSS)/REAL(TS(I)%NROWIPF,8),'F',7))//' (n='//TRIM(ITOS(TS(I)%NROWIPF))//')',1)
!  ENDIF
!
! ENDDO
! PEST_NOBS=II
! 
! IF(PEST_NOBS.LE.0)THEN
!  CALL IMOD_UTL_PRINTTEXT('No measurements available within current spatial/temporal space.',2)
! ENDIF
! 
! !## run batch files
! CALL PEST_BATCHFILES()
!
! !## insert regularisation to objective function
! NP=0
! DO I=1,SIZE(PARAM)
!  IF(PARAM(I)%NODES.EQ.0.OR.PARAM(I)%IACT.EQ.0.OR.PARAM(I)%IGROUP.LE.0)CYCLE
!  NP=NP+1
! ENDDO
! 
! PJ=0.0D0
! IF(PEST_IREGULARISATION.EQ.1)CALL PEST_GETQPP(NP,.TRUE.,idf)
!  
! IF(LGRAD.AND.PEST_IGRAD.EQ.0)THEN
!  CALL IMOD_UTL_PRINTTEXT('Best Match Value   :             '//TRIM(IMOD_UTL_DTOS(TJ,'G',7)),-1,IUPESTOUT)
!  CALL IMOD_UTL_PRINTTEXT('Plausibility Value :             '//TRIM(IMOD_UTL_DTOS(PJ,'G',7)),-1,IUPESTOUT)
! ENDIF
! 
! TJ=TJ+PJ
!  
! IF(LGRAD.AND.PEST_IGRAD.EQ.0)THEN
!  CALL IMOD_UTL_PRINTTEXT('TOTAL Objective Function Value : '//TRIM(IMOD_UTL_DTOS(TJ,'G',7)),-1,IUPESTOUT)
!  CALL IMOD_UTL_PRINTTEXT('MEAN Objective Function Value  : '//TRIM(IMOD_UTL_DTOS(TJ/REAL(PEST_NOBS,8),'G',7))// &
!          ' (n='//TRIM(IMOD_UTL_ITOS(PEST_NOBS))//')',-1,IUPESTOUT)
!          
!  RFIT=PEST_GOODNESS_OF_FIT(GF_H,GF_O,PEST_NOBS)
!  CALL IMOD_UTL_PRINTTEXT('Goodness of Fit:                 '// &
!      TRIM(IMOD_UTL_DTOS(RFIT,'G',7))//' (n='//TRIM(IMOD_UTL_ITOS(PEST_NOBS))//')',-1,IUPESTOUT)
!  CALL IMOD_UTL_PRINTTEXT('>> Provides a measure of the extent to which variability of field measurements is explained',-1,IUPESTOUT)
!  CALL IMOD_UTL_PRINTTEXT('   by the calibrated model compared to that which can be constructed as purely random. <<',-1,IUPESTOUT)
! ENDIF
!
! IF(ALLOCATED(GF_H))DEALLOCATE(GF_H)
! IF(ALLOCATED(GF_O))DEALLOCATE(GF_O)
!  
! CALL PESTPROGRESS()
!
! IF(LGRAD)THEN
!  IF(PEST_IGRAD.EQ.0)THEN
!   IF(PEST_ITER.EQ.1)WRITE(IUPESTEFFICIENCY,'(3E15.7)') TJ,SQRT(TJ),TJ/REAL(PEST_NOBS)
!   TJOBJ=TJ
!  ELSE
!   PARAM(PEST_IGRAD)%TJOBJ=TJ
!  ENDIF
! ENDIF
! IF(LLNSRCH)THEN
!
! ENDIF
! 
! CALL IMOD_UTL_PRINTTEXT('',0); CALL IMOD_UTL_PRINTTEXT(' Finished Getting residuals from IPF files ...',0)

 END SUBROUTINE IPEST_GLM_GETJ

 !###====================================================================
 REAL FUNCTION PEST_GOODNESS_OF_FIT(X,Y,N)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: N
 REAL(KIND=8),INTENT(IN),DIMENSION(N) :: X,Y !## x=head; y=obs
 REAL(KIND=8) :: XN,YN,X1,X2,X3,YA
 INTEGER :: I
 
 !## compute nash-sutcliff
 PEST_GOODNESS_OF_FIT=0.0D0
 
 !## average observation
 YA=0.0D0; DO I=1,N; YA=YA+Y(I)          ; ENDDO; YA=YA/REAL(N)
 XN=0.0D0; DO I=1,N; XN=XN+ABS(Y(I)-X(I)); ENDDO; XN=XN**2.0D0
 YN=0.0D0; DO I=1,N; YN=YN+ABS(Y(I)-YA)  ; ENDDO; YN=YN**2.0D0
 
 PEST_GOODNESS_OF_FIT=1.0D0-XN/YN
 
! XN=SUM(X)/REAL(N)
! YN=SUM(Y)/REAL(N)
 
! X1=0.0; X2=0.0; X3=0.0
! DO I=1,N
!  X1=X1+(X(I)-XN)*(Y(I)-YN)
!  X2=X2+(X(I)-XN)**2.0
!  X3=X3+(Y(I)-YN)**2.0
! ENDDO

! IF(X2.NE.0.0.AND.X3.NE.0.0)PEST_GOODNESS_OF_FIT=X1/SQRT(X2*X3)
  
 END FUNCTION PEST_GOODNESS_OF_FIT 
 
END MODULE MOD_IPEST_GLM