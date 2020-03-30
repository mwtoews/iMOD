!!  Copyright (C) Stichting Deltares, 2005-2019.
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
MODULE MOD_RESIDUALPLOT

USE MOD_RESIDUALPLOT_PAR
USE WINTERACTER
USE MOD_DBL
USE IMODVAR, ONLY : DP_KIND,SP_KIND,RVERSION
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFNULLIFY,IDFDEALLOCATE
USE MOD_IPF_PAR, ONLY : IPF,NIPF,ASSF
USE MOD_IPF, ONLY : IPFREAD2,IPFALLOCATE
USE MOD_IPFASSFILE, ONLY : IPFOPENASSFILE,IPFREADASSFILELABEL,IPFREADASSFILE
USE MOD_IPFASSFILE_UTL
USE MOD_UTL
USE MOD_POLINT

CONTAINS

 !###===================================
 SUBROUTINE RESIDUAL_MAIN(ICOL)
 !###===================================
 IMPLICIT NONE
 INTEGER,DIMENSION(:),INTENT(IN) :: ICOL

 CALL WINDOWOPEN(FLAGS=HIDEWINDOW ) 
 CALL IGRCOLOURMODEL(24)
 
 !## read in complete ipf file
 IF(INDEX(UTL_CAP(INPUTFILE,'U'),'IPF').GT.0)THEN
  IF(.NOT.RESIDUAL_DATA_IPF(ICOL))STOP
 !## read in complete txtfile
 ELSE
  CALL RESIDUAL_DATA()
 ENDIF
 !## process txtfile into xyz variables
 CALL RESIDUAL_PROC()
 !## plot xyz variables
 IF(IPLOT.NE.3)CALL RESIDUAL_PLOT()
 !## clean
 CALL RESIDUAL_CLEAN()

 END SUBROUTINE RESIDUAL_MAIN

 !###===================================
 LOGICAL FUNCTION RESIDUAL_DATA_IPF(ICOL)
 !###===================================
 IMPLICIT NONE
 INTEGER,DIMENSION(:),INTENT(IN) :: ICOL
 INTEGER :: I,J,K,IU,N,NM
 REAL(KIND=DP_KIND) :: O,M
 CHARACTER(LEN=256) :: FNAME,DIR
 
 RESIDUAL_DATA_IPF=.FALSE.
 
 NIPF=1; CALL IPFALLOCATE(); IPF(1)%FNAME=INPUTFILE

 IPF(1)%XCOL=ICOL(1)
 IPF(1)%YCOL=ICOL(2)
 IF(ITRANSIENT.EQ.1)THEN
  !## not used reading the ipf-file, set them to the x/y coordinates
  IPF(1)%ZCOL =ICOL(1)
  IPF(1)%Z2COL=ICOL(2)
 ELSE
  IPF(1)%ZCOL =ICOL(3)
  IPF(1)%Z2COL=ICOL(4)
 ENDIF
 !## optional weight values
 IPF(1)%QCOL=ICOL(1); IF(ICOL(5).NE.0)IPF(1)%QCOL=ICOL(5)
 
 !## read entire ipf
 IF(.NOT.IPFREAD2(1,1,1))RETURN

 !## allocate memory
 NIPF=1; ALLOCATE(IPFR(NIPF))

 !## steady-state
 IF(ITRANSIENT.EQ.0)THEN 

  N=IPF(1)%NROW; IPFR(NIPF)%NPOINTS=N
  ALLOCATE(IPFR(1)%X(N),IPFR(1)%Y(N),IPFR(1)%L(N),IPFR(1)%O(N), &
           IPFR(1)%M(N),IPFR(1)%W(N))
  DO J=1,N
   READ(IPF(1)%INFO(ICOL(1),J),*) IPFR(1)%X(J)
   READ(IPF(1)%INFO(ICOL(2),J),*) IPFR(1)%Y(J)
   READ(IPF(1)%INFO(ICOL(3),J),*) IPFR(1)%O(J)
   READ(IPF(1)%INFO(ICOL(4),J),*) IPFR(1)%M(J)
   IF(ICOL(5).EQ.0)THEN; IPFR(1)%W(J)=1.0D0
   ELSE; READ(IPF(1)%INFO(ICOL(5),J),*) IPFR(1)%W(J); ENDIF
   IF(ICOL(6).EQ.0)THEN; IPFR(1)%L(J)=1
   ELSE; READ(IPF(1)%INFO(ICOL(6),J),*)  IPFR(1)%L(J); ENDIF
  ENDDO

 ELSE
 
  CALL IPFASSFILEALLOCATE(1)
  
  !## first to get number of points per ipf, second to read all in memory
  DIR=INPUTFILE(:INDEX(INPUTFILE,'\',.TRUE.)-1)
  DO I=1,2
   N=0
   DO J=1,IPF(1)%NROW

    FNAME=TRIM(DIR)//'\'//TRIM(IPF(1)%INFO(IPF(1)%ACOL,J))//'.'//TRIM(IPF(1)%FEXT)
    !## read dimensions of associated file
    IF(IPFOPENASSFILE(IU,1,FNAME))THEN
     !## time serie found
     IF(ASSF(1)%ITOPIC.EQ.1)THEN
      !## read associated file
      IF(IPFREADASSFILELABEL(IU,1,FNAME).AND.IPFREADASSFILE(IU,1,FNAME))THEN

       IF(IAVERAGE.EQ.1)THEN
        N=N+1
        IF(I.EQ.2)THEN
         IPFR(1)%O(N)=0.0D0
         IPFR(1)%M(N)=0.0D0
        ENDIF
       ENDIF
       
       NM=0
       DO K=1,ASSF(1)%NRASS

        !## store date - for first entered data line
        IF(I.EQ.2)THEN
         IF(K.EQ.1.AND.IAVERAGE.EQ.1)IPFR(1)%D(N)=ASSF(1)%IDATE(K)
        ENDIF

        O=ASSF(1)%MEASURE(ICOL(3)-1,K)
        M=ASSF(1)%MEASURE(ICOL(4)-1,K)
        !## both not equal to nodata
        IF(O.NE.ASSF(1)%NODATA(ICOL(3)).AND. &
           M.NE.ASSF(1)%NODATA(ICOL(4)))THEN
 
         !# no averaging, take all measurements
         IF(IAVERAGE.EQ.0)N=N+1; NM=NM+1

         IF(I.EQ.2)THEN
         
          READ(IPF(1)%INFO(ICOL(1),J),*)  IPFR(1)%X(N)
          READ(IPF(1)%INFO(ICOL(2),J),*)  IPFR(1)%Y(N)
       
          IF(ICOL(5).EQ.0)THEN; IPFR(1)%W(N)=1.0D0
          ELSE; READ(IPF(1)%INFO(ICOL(5),J),*) IPFR(1)%W(N); ENDIF
          IF(ICOL(6).EQ.0)THEN; IPFR(1)%L(N)=1
          ELSE; READ(IPF(1)%INFO(ICOL(6),J),*)  IPFR(1)%L(N); ENDIF

          IF(IAVERAGE.EQ.0)THEN
           IPFR(1)%D(N)=ASSF(1)%IDATE(K)
           IPFR(1)%O(N)=O 
           IPFR(1)%M(N)=M
          ELSE
           IPFR(1)%O(N)=IPFR(1)%O(N)+O 
           IPFR(1)%M(N)=IPFR(1)%M(N)+M
          ENDIF
          
         ENDIF
                 
        ENDIF  
       ENDDO

       IF(IAVERAGE.EQ.1.AND.I.EQ.2.AND.NM.GT.0)THEN
        IPFR(1)%O(N)=IPFR(1)%O(N)/REAL(NM)
        IPFR(1)%M(N)=IPFR(1)%M(N)/REAL(NM)
       ENDIF
       
       !## close associated text-file
       CLOSE(IU)
      ENDIF
     ENDIF
    ENDIF                                    
   ENDDO
   IF(I.EQ.1)THEN
    IPFR(1)%NPOINTS=N
    ALLOCATE(IPFR(1)%D(N),IPFR(1)%X(N),IPFR(1)%Y(N),IPFR(1)%L(N), &
             IPFR(1)%O(N),IPFR(1)%M(N),IPFR(1)%W(N))
   ENDIF
  ENDDO
 
 ENDIF

 RESIDUAL_DATA_IPF=.TRUE.
 
 END FUNCTION RESIDUAL_DATA_IPF
 
 !###===================================
 SUBROUTINE RESIDUAL_DATA()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,J,IU,NIPF,IOS,N,NN,MM,L
 INTEGER(KIND=DP_KIND) :: DD
 REAL(KIND=DP_KIND) :: WMDL,WRES,O,M,X,Y,W,JOBJ
 CHARACTER(LEN=256) :: LINE
 CHARACTER(LEN=32) :: C
 REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: GF_H,GF_O
 
 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(INPUTFILE),ACTION='READ',FORM='FORMATTED',STATUS='OLD')
 IF(IU.EQ.0)STOP
 
 DO I=1,2

  !## first to get number of ipf files and get number of points per ipf, third to read all in memory
  NIPF=0
  DO 
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   !## found ipf
   IF(INDEX(UTL_CAP(LINE,'U'),'.IPF').GT.0)THEN
    NIPF=NIPF+1
   ELSE
    EXIT
   ENDIF
  ENDDO
 
  IF(I.EQ.1)THEN
   ALLOCATE(IPFR(NIPF)); N=1
   DO J=1,NIPF
    ALLOCATE(IPFR(J)%X(N),IPFR(J)%Y(N),IPFR(J)%L(N),IPFR(J)%O(N),IPFR(J)%M(N),IPFR(J)%W(N),IPFR(J)%C(N))
    IF(ITRANSIENT.EQ.1)ALLOCATE(IPFR(J)%D(N))
    IF(IAVERAGE.EQ.1)  ALLOCATE(IPFR(J)%NS(N))
   ENDDO
  ENDIF
  
  DO J=1,NIPF
   IPFR(J)%NPOINTS=0; IPFR(J)%O=0.0D0; IPFR(J)%M=0.0D0
  ENDDO

  NN=0; MM=0
  DO

   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   !## steadystate
   IF(ITRANSIENT.EQ.0)READ(LINE,'(139X,I10)',IOSTAT=IOS) NIPF
   IF(ITRANSIENT.EQ.1)READ(LINE,'(171X,I10)',IOSTAT=IOS) NIPF
   IF(IOS.NE.0)THEN
    WRITE(*,'(A)') 'Error reading TXT file, are you sure to select the correct ITRANSIENT keyword ?'
    WRITE(*,'(A)') 'Skipping:'//TRIM(LINE)
    CYCLE
   ENDIF
   
   N=IPFR(NIPF)%NPOINTS; IF(I.EQ.1)N=MIN(N,1)

   IF(ITRANSIENT.EQ.1)THEN
    READ(LINE,'(2(F15.0,1X),I10,1X,3(F15.0,1X),91X,A32,1X,I15)') X,Y,L,W,O,M,C,DD
    C=UTL_CAP(C,'U')
    IF(C(1:3).EQ.'IPF')C=C(INDEX(C,'_')+1:)
    IF(N.GT.0.AND.IAVERAGE.EQ.1)THEN
     IF(X.NE.IPFR(NIPF)%X(N).OR.Y.NE.IPFR(NIPF)%Y(N).OR.IPFR(NIPF)%L(N).NE.L)THEN
      IPFR(NIPF)%O(N)=IPFR(NIPF)%O(N)/DBLE(NN)
      IPFR(NIPF)%M(N)=IPFR(NIPF)%M(N)/DBLE(NN)
      IF(I.EQ.2)THEN
       IPFR(NIPF)%NS(N)=UTL_NASH_SUTCLIFFE(GF_H,GF_O,NN)
      ENDIF
      !## start new observation
      MM=MAX(MM,NN); IPFR(NIPF)%NPOINTS=IPFR(NIPF)%NPOINTS+1; NN=0
      IF(I.EQ.2)N=IPFR(NIPF)%NPOINTS
     ENDIF
    ELSE
     IPFR(NIPF)%NPOINTS=IPFR(NIPF)%NPOINTS+1
     IF(I.EQ.2)THEN
      N=IPFR(NIPF)%NPOINTS; IPFR(NIPF)%O(N)=0.0D0; IPFR(NIPF)%M(N)=0.0D0
     ENDIF
    ENDIF
    IF(I.EQ.1)N=1    
    !## store data
    IPFR(NIPF)%X(N)=X
    IPFR(NIPF)%Y(N)=Y
    IPFR(NIPF)%L(N)=L
    IPFR(NIPF)%W(N)=W
    IPFR(NIPF)%C(N)=C
    IPFR(NIPF)%O(N)=IPFR(NIPF)%O(N)+O
    IPFR(NIPF)%M(N)=IPFR(NIPF)%M(N)+M
    IPFR(NIPF)%D(N)=DD
    NN=NN+1
    IF(I.EQ.2.AND.IAVERAGE.EQ.1)THEN
     GF_H(NN)=M; GF_O(NN)=O
    ENDIF
   ELSE
    READ(LINE,'(2(F15.0,1X),I10,1X,6(F15.0,1X))') IPFR(NIPF)%X(N),IPFR(NIPF)%Y(N), &
                                     IPFR(NIPF)%L(N),IPFR(NIPF)%O(N),IPFR(NIPF)%M(N), &
                                     JOBJ,WMDL,WRES,IPFR(NIPF)%W(N)
   ENDIF

  ENDDO
  
  IF(I.EQ.1)THEN
   DO J=1,SIZE(IPFR)
    N=IPFR(J)%NPOINTS
    DEALLOCATE(IPFR(J)%X,IPFR(J)%Y,IPFR(J)%L,IPFR(J)%O,IPFR(J)%M,IPFR(J)%W,IPFR(J)%C)
    IF(ITRANSIENT.EQ.1)DEALLOCATE(IPFR(J)%D)
    IF(IAVERAGE.EQ.1)DEALLOCATE(IPFR(J)%NS)
    ALLOCATE(IPFR(J)%X(N),IPFR(J)%Y(N),IPFR(J)%L(N),IPFR(J)%O(N), &
             IPFR(J)%M(N),IPFR(J)%W(N),IPFR(J)%C(N))
    IF(ITRANSIENT.EQ.1)ALLOCATE(IPFR(J)%D(N))
    IF(IAVERAGE.EQ.1)ALLOCATE(IPFR(J)%NS(N))
   ENDDO
   IF(IAVERAGE.EQ.1)THEN
    ALLOCATE(GF_H(MM),GF_O(MM))
   ENDIF
  ELSE
   IF(IAVERAGE.EQ.1)THEN
    IPFR(NIPF)%O(N)=IPFR(NIPF)%O(N)/DBLE(NN)
    IPFR(NIPF)%M(N)=IPFR(NIPF)%M(N)/DBLE(NN)
    IPFR(NIPF)%NS(N)=UTL_NASH_SUTCLIFFE(GF_H,GF_O,NN)
   ENDIF
  ENDIF

  REWIND(IU)
 
 ENDDO
 CLOSE(IU)

 END SUBROUTINE RESIDUAL_DATA

 !###===================================
 SUBROUTINE RESIDUAL_PROC()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,J,N,IU

 IF(IPLOT.EQ.3)THEN
  !## write down results
  IU=UTL_GETUNIT()
  OPEN(IU,FILE=TRIM(IPFNAME)//'_',STATUS='UNKNOWN',ACTION='WRITE')
  WRITE(IU,'(A)') 'NaN1#'
  WRITE(IU,'(A)') TRIM(ITOS(7+IAVERAGE))
  WRITE(IU,'(A)') 'XC'
  WRITE(IU,'(A)') 'YC'
  WRITE(IU,'(A)') 'ID'
  WRITE(IU,'(A)') 'OBS'
  WRITE(IU,'(A)') 'MDL'
  WRITE(IU,'(A)') 'WEIGHT'
  WRITE(IU,'(A)') 'MDL-OBS'
  IF(IAVERAGE.EQ.1)WRITE(IU,'(A)') 'NASH-SUTCLIFF'
  WRITE(IU,'(A)') '0,TXT'
 ENDIF

 !## okay we keep it simple - we plot everything - no selection yet
 N=SUM(IPFR%NPOINTS)

 !## allocate memory
 ALLOCATE(X(N),Y(N),Z(N))
 
 N=0
 DO I=1,SIZE(IPFR)
  IF(.NOT.(RESIDUAL_PROC_SELIPF(I)))CYCLE
  DO J=1,IPFR(I)%NPOINTS
   IF(.NOT.((RESIDUAL_PROC_SELLAY(I,J)).AND. &
            (RESIDUAL_PROC_SELDATE(I,J)).AND. &
            (RESIDUAL_PROC_SELWEIGHT(I,J))))CYCLE
   !## skip weigth is zero anyhow
   IF(IPFR(I)%W(J).LE.0.0D0)CYCLE
   
   N=N+1
   SELECT CASE (IPLOT)
    !## scatter measurement/observation
    CASE (1)
     IF(IWEIGHT.EQ.1)THEN
      X(N)=IPFR(I)%M(J)*IPFR(I)%W(J); Y(N)=IPFR(I)%O(J)*IPFR(I)%W(J); Z(N)=IPFR(I)%W(J)*(IPFR(I)%M(J)-IPFR(I)%O(J))**2.0D0
     ELSE
      X(N)=IPFR(I)%M(J); Y(N)=IPFR(I)%O(J); Z(N)=(IPFR(I)%M(J)-IPFR(I)%O(J))**2.0D0
     ENDIF
    !## histogram
    CASE (2) 
     IF(IWEIGHT.EQ.1)THEN
      X(N)=(IPFR(I)%M(J)*IPFR(I)%W(J))-(IPFR(I)%O(J)*IPFR(I)%W(J))
     ELSE
      X(N)=IPFR(I)%M(J)-IPFR(I)%O(J)
     ENDIF
    CASE(3)
     IF(IAVERAGE.EQ.0)THEN
      WRITE(IU,'(2(F15.7,A1),A,A1,4(F15.7,A1))') IPFR(I)%X(J),',',IPFR(I)%Y(J),',',TRIM(IPFR(I)%C(J)),',',IPFR(I)%O(J),',',IPFR(I)%M(J),',', &
                                IPFR(I)%W(J),',',IPFR(I)%M(J)-IPFR(I)%O(J)
     ELSE
      WRITE(IU,'(2(F15.7,A1),A,A1,5(F15.7,A1))') IPFR(I)%X(J),',',IPFR(I)%Y(J),',',TRIM(IPFR(I)%C(J)),',',IPFR(I)%O(J),',',IPFR(I)%M(J),',', &
                                IPFR(I)%W(J),',',IPFR(I)%M(J)-IPFR(I)%O(J),',',IPFR(I)%NS(J)
     ENDIF
   END SELECT
  ENDDO
 ENDDO
 
 IF(IPLOT.EQ.3)THEN
  CLOSE(IU)
  CALL UTL_MF2005_MAXNO(TRIM(IPFNAME)//'_',(/N/))
  RETURN
 ENDIF
 
 IF(N.EQ.0)THEN
  WRITE(*,*) 'No data points are found, check your initial settings if you selected the right layers, dates or ipfs'
  STOP
 ENDIF
 
 !## final number of points
 ALLOCATE(X_TMP(N),Y_TMP(N),Z_TMP(N))
 DO I=1,N; X_TMP(I)=X(I); Y_TMP(I)=Y(I); Z_TMP(I)=Z(I); ENDDO
 DEALLOCATE(X,Y,Z)
 X=>X_TMP; Y=>Y_TMP; Z=>Z_TMP
  
 !## subroutine to calculate histogram classes
 CALL RESIDUAL_PROC_HISTCLASS()
 
 !## regressions for goodness-of-fit
 GOF=UTL_GOODNESS_OF_FIT(X,Y,SIZE(X))
 NSC=UTL_NASH_SUTCLIFFE(X,Y,SIZE(X))
 
 END SUBROUTINE RESIDUAL_PROC

 !###=================================== 
 SUBROUTINE RESIDUAL_PROC_HISTCLASS()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,J
 
 !## define histogram classes (default whenever none given)
 IF(HCLASSES(1).EQ.HCLASSES(2))THEN
  HCLASSES(1)=-10.0D10
  HCLASSES(2)= -5.0D0
  DO I=3,SIZE(HCLASSES)-1
   HCLASSES(I)=HCLASSES(I-1)+0.5D0
  ENDDO
  HCLASSES(SIZE(HCLASSES))=10.0D10
 ENDIF
  
 !## count amount of points per class
 XCLASSES=0.0D0
 DO J=1,SIZE(X) 
  CALL POL1LOCATE(HCLASSES,SIZE(HCLASSES),X(J),I)
  !## add to the histogram class
  IF(I.GT.0.AND.I.LE.SIZE(XCLASSES))XCLASSES(I)=XCLASSES(I)+1
 ENDDO
 
 END SUBROUTINE RESIDUAL_PROC_HISTCLASS
 
 !###===================================
 LOGICAL FUNCTION RESIDUAL_PROC_SELLAY(I,J)
 !###===================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,J
 INTEGER :: K
 
 RESIDUAL_PROC_SELLAY=.TRUE.
 
 DO K=1,NLAYER
  RESIDUAL_PROC_SELLAY=IPFR(I)%L(J).EQ.ILAYER(K)
  IF(RESIDUAL_PROC_SELLAY)RETURN
 ENDDO 
 
 END FUNCTION RESIDUAL_PROC_SELLAY
 
 !###===================================
 LOGICAL FUNCTION RESIDUAL_PROC_SELDATE(I,J)
 !###===================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,J
 
 RESIDUAL_PROC_SELDATE=.TRUE.
 
 IF(ITRANSIENT.EQ.1)THEN
!  DO K=1,NRDATE
!WRITE(*,*) IPFR(I)%D(J)
!   RESIDUAL_PROC_SELDATE=IPFR(I)%D(J).LT.20140214000000 !EQ.IRDATE(K)
   IF(RESIDUAL_PROC_SELDATE)RETURN
!  ENDDO 
 ENDIF
 
 END FUNCTION RESIDUAL_PROC_SELDATE

 !###===================================
 LOGICAL FUNCTION RESIDUAL_PROC_SELWEIGHT(I,J)
 !###===================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,J 
 LOGICAL :: L1,L2
  
 !## always skip weight of zero
 RESIDUAL_PROC_SELWEIGHT=IPFR(I)%W(J).NE.0.0D0
 IF(RESIDUAL_PROC_SELWEIGHT)THEN
  L1=.TRUE.; IF(WC1.NE.0.0D0)L1=IPFR(I)%W(J).GE.WC1
  L2=.TRUE.; IF(WC2.NE.0.0D0)L2=IPFR(I)%W(J).LE.WC2
  RESIDUAL_PROC_SELWEIGHT=L1.AND.L2
  RETURN
 ENDIF
 
 END FUNCTION RESIDUAL_PROC_SELWEIGHT

 !###===================================
 LOGICAL FUNCTION RESIDUAL_PROC_SELIPF(I)
 !###===================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 INTEGER :: K
 
 RESIDUAL_PROC_SELIPF=.TRUE.
 
 !## function to count the amount of data points for the selected layers need to be plotted and which needs to be ommitted
 DO K=1,NIPFS
  RESIDUAL_PROC_SELIPF=I.EQ.IIPFS(K)
  IF(RESIDUAL_PROC_SELIPF)RETURN
 ENDDO 

 END FUNCTION RESIDUAL_PROC_SELIPF
  
 !###===================================
 SUBROUTINE RESIDUAL_PLOT()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,NSETS,IPLOTBMP,NCLR,MX,NBAR,NPOP
 REAL(KIND=DP_KIND) :: MINX,MINY,MAXX,MAXY,X1,X2,Y1,Y2,DY,AVG,VAR,XT
 CHARACTER(LEN=12),DIMENSION(:),ALLOCATABLE :: XLABELS
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IBARS
 CHARACTER(LEN=256) :: LINE
 REAL(KIND=DP_KIND),DIMENSION(3) :: XMED
 
 
 NBAR=SIZE(XCLASSES)
 ALLOCATE(XLABELS(NBAR))
 DO I=1,NBAR
  IF(I.EQ.1)THEN
   WRITE(XLABELS(I),UTL_GETFORMAT(HCLASSES(I+1))) HCLASSES(I+1)
   XLABELS(I)='<'//ADJUSTL(XLABELS(I))
  ELSEIF(I.EQ.NBAR)THEN
   WRITE(XLABELS(I),UTL_GETFORMAT(HCLASSES(I))) HCLASSES(I)
   XLABELS(I)='>'//ADJUSTL(XLABELS(I))
  ELSE
   WRITE(XLABELS(I),UTL_GETFORMAT(HCLASSES(I))) HCLASSES(I)
   XLABELS(I)=ADJUSTL(XLABELS(I))
  ENDIF
 ENDDO
   
 !## get minimum and maximum x and y values for plotting area scatter plot if not predefined
 IF(IXY.EQ.1)THEN
  MINX=XMIN; MAXX=XMAX; MINY=YMIN; MAXY=YMAX
 ELSE
  MINX=X(1); MAXX=X(1)
  DO I=2,SIZE(X); MINX=MIN(MINX,X(I)); MAXX=MAX(MAXX,X(I)); ENDDO
  MINY=Y(1); MAXY=Y(1)
  DO I=2,SIZE(Y); MINY=MIN(MINY,Y(I)); MAXY=MAX(MAXY,Y(I)); ENDDO
 ENDIF

 !## create bitmap - resolution
 CALL WBITMAPCREATE(IPLOTBMP,2000,2000)
 CALL IGRSELECT(DRAWBITMAP,IPLOTBMP)
 !## plot area is the entire bitmap
 CALL DBL_IGRAREA(0.0D0,0.0D0,1.0D0,1.0D0)
 CALL DBL_IGRUNITS(0.0D0,0.0D0,1.0D0,1.0D0)
  
 NSETS=1
 NCLR=WRGB(6,165,205) !150
 !## plot data in scatterplot
 IF(IPLOT.EQ.1)THEN
  !## settings: option 1=type of plot, 2=number of sets, 3=amount of points, 4=no extra plotting options, 5=reset styles (1=yes)
  CALL IPGNEWPLOT(PGSCATTERPLOT,NSETS,SIZE(X),0,1) 
  !## settings: option 1=set number, 2=marker number, 3=marker type (0-4), 4='not used', 5='not used', 6=marker colour (31=red), 7='not used'
  CALL IPGSTYLE(1,1,3,0,32,NCLR,1) 
  !## settings: option 1=set number, 2=marker type (14=dot)
  CALL IPGMARKER(1,14) 
  !## for scatter plot it is important to have a 1:1 ratio
  MINX=MIN(MINX,MINY); MAXX=MAX(MAXX,MAXY)
  CALL DBL_IPGUNITS(MINX,MINX,MAXX,MAXX)
 !## plot data in histogram
 ELSEIF(IPLOT.EQ.2)THEN
  CALL IPGNEWPLOT(PGHISTOGRAM,NSETS,SIZE(XCLASSES),PGLAYADJACENT,1)
  !## settings: option 1=fill style (0-5), 2=fill density (1-4), 3=fill angle (1-4), 4=primary colour (31=red), 5=secondary colour, 6='not used'
  CALL IPGSTYLE(1,4,2,0,NCLR,WRGB(6,165,205),1) 
  !## minimum x-value, minimum y-value, maximum x-value, maximum y-value
  !## to be computed ourselves beforehand in subroutine "RESIDUAL_PROC_HISTCLASS"
  X1=0.0D0; X2=REAL(NBAR); Y1=MINVAL(XCLASSES); Y2=MAXVAL(XCLASSES); DY=(Y2-Y1)/50.0D0; Y2=Y2+DY
  CALL DBL_IPGUNITS(X1,Y1,X2,Y2) 
 ENDIF

 !## clip graph on graphical area
 CALL IPGCLIPRECTANGLE('P')
 !## area of the graph
 CALL IPGAREA(0.125,0.1,0.95,0.9)
 CALL IPGBORDER()
  
 !## set all text black
 CALL IGRCOLOURN(WRGB(0,0,0))

 !## set graphic layout settings 
 CALL DBL_WGRTEXTFONT(IFAMILY=FFHELVETICA,ISTYLE=0,TWIDTH=0.01D0,THEIGHT=0.03D0)
 CALL IPGTITLEPOS(0.50)
 CALL IPGXLABELPOS(0.50)
 CALL IPGYLABELPOS(0.75)
 
 !## title, x-axis, y-axis
 IF(IPLOT.EQ.1)THEN
  IF(IWEIGHT.EQ.1)THEN
   CALL IPGTITLE('Calculated vs. Observed weighted heads','C')
  ELSE
   CALL IPGTITLE('Calculated vs. Observed heads','C')
  ENDIF
  CALL IPGXLABEL('Calculated heads (m)','C') 
  CALL IPGYLABELLEFT('Observed heads (m)','C9')
 ELSEIF(IPLOT.EQ.2)THEN
  IF(IWEIGHT.EQ.1)THEN
   CALL IPGTITLE('Histogram of weighted residuals','C')
  ELSE
   CALL IPGTITLE('Histogram of residuals','C')
  ENDIF
  CALL IPGXLABEL('Residual (model-measured) (m)','C') 
  CALL IPGYLABELLEFT('Count','C9') 
 ENDIF
 
 !## draw actual axis
 !## Draws a set of axes in the current Presentation Graphics area
 CALL IPGAXES() 
 !## set number of decimal places (automatic=-1, maximum=9)
 CALL IPGDECIMALPLACES(-1)

 CALL IPGXSCALEANGLE(0.0,0.0)
 !## describes the position of the X scale numbers or descriptions as a proportion of the distance from the edge 
 !## of the PG area to the edge of the main graphics area. 
 CALL IPGXSCALEPOS(0.2)
   
 IF(IPLOT.EQ.1)THEN
  !## adjust tick position for bottom X Axis
  CALL IPGXTICKPOS(2) 
  CALL IPGXTICKLENGTH(1.0)
  !## numbering and tick outside
  CALL IPGXSCALE('NT')
 ELSEIF(IPLOT.EQ.2)THEN
  ALLOCATE(IBARS(NBAR)); DO I=1,NBAR; IBARS(I)=I; ENDDO
  CALL IPGXUSERSCALEHIST(IBARS,NBAR)
  DEALLOCATE(IBARS)
!  CALL IPGXSCALE('T')
  CALL IPGXTEXT(XLABELS,NBAR)
 ENDIF

 !## adjust tick position for left Y Axis
 CALL IPGYTICKPOS(2) 
 CALL IPGYTICKLENGTH(1.0)
! CALL IPGYUSERSCALE(NPOINT=0)
 CALL IPGYSCALEANGLE(0.0,0.0)
 !## describes the position of the Y scale numbers or descriptions as a multiple of the length of a normal (i.e. non-log scale)
 !## Y-axis tick mark.  
 CALL IPGYSCALEPOS(1.5)
 !## numbering and tick outside
 CALL IPGYSCALELEFT('NT')
  
 CALL IPGXGRATICULES(DASHED) !LTYPE)
 CALL IPGYGRATICULES(DASHED) !LTYPE)

 !## draw graph
 IF(IPLOT.EQ.1)THEN
  CALL IPGSCATTERPLOT(REAL(X,4),REAL(Y,4),SIZE(X))
!  CALL IPGUNITSTOGRUNITS(MINX,MINX,X1,Y1)
!  CALL IPGUNITSTOGRUNITS(MAXX,MAXX,X2,Y2)
!  CALL DBL_IGRJOIN(X1,Y1,X2,Y2)
  CALL IPGPOLYLINE2((/REAL(MINX,4),REAL(MAXX,4)/),(/REAL(MINX,4),REAL(MAXX,4)/),2)
 ELSEIF(IPLOT.EQ.2)THEN
  CALL IPGHISTOGRAM(REAL(XCLASSES,4))
 ENDIF

 !## add legend to graph
 CALL IPGKEYAREA(0.20,0.80,0.40,0.90)
 !## clear legend area
 CALL IPGKEYALL((/'Dataset size: '//TRIM(ITOS(SIZE(X)))/),'C')
 
 !## put textblock on plotting area
 CALL DBL_WGRTEXTFONT(IFAMILY=FFHELVETICA,ISTYLE=0,TWIDTH=0.01D0,THEIGHT=0.03D0)
 CALL DBL_WGRTEXTBLOCK(0.75D0,0.0D0,1.0D0,0.05D0,'(c) Powered by iMOD '//TRIM(RVERSION_EXE)//', 2018')
  
 !## scatterplot
 IF(IPLOT.EQ.1)THEN
  LINE='Goodness of Fit: '//CHAR(32)//TRIM(RTOS(GOF,'F',3))//CHAR(13)//CHAR(10)// &
       'Nash Sutcliff:   '//CHAR(32)//TRIM(RTOS(NSC,'F',3))
  CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT,0.0D0,DIRHORIZ,ALIGNRIGHT)
  CALL DBL_WGRTEXTBLOCK(0.25D0,0.5D0,0.70D0,0.8D0,TRIM(LINE)) 
 ELSE
  !### write statistics
  AVG=SUM(X)/REAL(SIZE(X))
  CALL UTL_GETMED(X,SIZE(X),HUGE(1.0D0),(/10.0D0,50.0D0,90.0D0/),3,MX,XMED)

  CALL UTL_STDEF(X,SIZE(X),HUGE(1.0D0),VAR,XT,NPOP)

  LINE='Statistics'                             //CHAR(13)//CHAR(10)// &
       'Average:    '//CHAR(32)//TRIM(RTOS(AVG,'G',7))    //CHAR(13)//CHAR(10)// &
       'St.Dev:     '//CHAR(32)//TRIM(RTOS(VAR,'G',7))    //CHAR(13)//CHAR(10)// &
       'Minimal:    '//CHAR(32)//TRIM(RTOS(MINX,'G',7))   //CHAR(13)//CHAR(10)// &
       '10 Percent: '//CHAR(32)//TRIM(RTOS(XMED(1),'G',7))//CHAR(13)//CHAR(10)// &
       '50 Percent: '//CHAR(32)//TRIM(RTOS(XMED(2),'G',7))//CHAR(13)//CHAR(10)// &
       '90 Percent: '//CHAR(32)//TRIM(RTOS(XMED(3),'G',7))//CHAR(13)//CHAR(10)// &
       'Maximal:    '//CHAR(32)//TRIM(RTOS(MAXX,'G',7)) 
  CALL DBL_WGRTEXTORIENTATION(ALIGNLEFT,0.0D0,DIRHORIZ,ALIGNRIGHT)
  CALL DBL_WGRTEXTBLOCK(0.20D0,0.5D0,0.50D0,0.8D0,TRIM(LINE)) 
 ENDIF
 
 !## create outputfolder
 CALL UTL_CREATEDIR(BMPNAME(:INDEX(BMPNAME,'\',.TRUE.)-1))
 !## save graph
 CALL WBITMAPSAVE(IPLOTBMP,TRIM(BMPNAME))
 CALL WBITMAPDESTROY(IPLOTBMP)

 END SUBROUTINE RESIDUAL_PLOT
 
 !###===================================
 SUBROUTINE RESIDUAL_CLEAN()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I

 IF(ALLOCATED(IPFR))THEN
  DO I=1,SIZE(IPFR)
   IF(ASSOCIATED(IPFR(I)%D))DEALLOCATE(IPFR(I)%D)
   IF(ASSOCIATED(IPFR(I)%X))DEALLOCATE(IPFR(I)%X)
   IF(ASSOCIATED(IPFR(I)%Y))DEALLOCATE(IPFR(I)%Y)
   IF(ASSOCIATED(IPFR(I)%M))DEALLOCATE(IPFR(I)%M)
   IF(ASSOCIATED(IPFR(I)%O))DEALLOCATE(IPFR(I)%O)
   IF(ASSOCIATED(IPFR(I)%W))DEALLOCATE(IPFR(I)%W)
   IF(ASSOCIATED(IPFR(I)%L))DEALLOCATE(IPFR(I)%L)
   IF(ASSOCIATED(IPFR(I)%C))DEALLOCATE(IPFR(I)%C)
  ENDDO
  DEALLOCATE(IPFR)
 ENDIF
 IF(ASSOCIATED(X))DEALLOCATE(X)
 IF(ASSOCIATED(Y))DEALLOCATE(Y)
 IF(ASSOCIATED(Z))DEALLOCATE(Z)

 END SUBROUTINE RESIDUAL_CLEAN

END MODULE MOD_RESIDUALPLOT