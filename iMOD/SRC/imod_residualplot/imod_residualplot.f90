MODULE MOD_RESIDUALPLOT

USE MOD_RESIDUALPLOT_PAR
USE WINTERACTER
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFNULLIFY,IDFDEALLOCATE
USE MOD_UTL

CONTAINS

!###===================================
SUBROUTINE RESIDUAL_MAIN(INPUTFILE,BMPNAME,IPLOT)
!###===================================
IMPLICIT NONE
INTEGER,INTENT(IN) :: IPLOT
CHARACTER(LEN=*),INTENT(IN) :: INPUTFILE,BMPNAME

CALL WindowOpen(FLAGS=HideWindow ) !TITLE='3D surface from (x,y,z) triplets')

!## read in complete txtfile
CALL RESIDUAL_DATA()
!## process txtfile into xyz variables
CALL RESIDUAL_PROC()
!## plot xyz variables
CALL RESIDUAL_PLOT()
!## clean
CALL RESIDUAL_CLEAN()

END SUBROUTINE RESIDUAL_MAIN

!###===================================
SUBROUTINE RESIDUAL_DATA()
!###===================================
IMPLICIT NONE
INTEGER :: I,IU,NIPF,FIPF,IOS
CHARACTER(LEN=256) :: LINE

NIPF=0
FIPF=0
!## loop 1: to get read all data: amount of ipf-files, get size of ipf-file and allocate arrays
!## loop 2: to fill IDF-object arrays with retrieved data
DO I=1,2 
 IF(I.EQ.1)THEN
  IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(INPUTFILE),ACTION='READ',FORM='FORMATTED',STATUS='OLD')
  DO 
   READ(IU,*,IOSTAT=IOS) LINE
   IF(IOS.NE.0)EXIT
   
   FIPF=INDEX(LINE,'.IPF'); IF(FIPF.GT.0)NIPF=NIPF+1
  ENDDO
 ELSE
 
 ENDIF
ENDDO

END SUBROUTINE RESIDUAL_DATA

!###===================================
SUBROUTINE RESIDUAL_PROC()
!###===================================
IMPLICIT NONE
INTEGER :: I,J
CHARACTER(LEN=12),DIMENSION(6) :: COLNAMES

!## ff testset aanmaken
IF(ALLOCATED(X))THEN; DEALLOCATE(X); ENDIF
IF(ALLOCATED(Y))THEN; DEALLOCATE(Y); ENDIF
IF(ALLOCATED(Z))THEN; DEALLOCATE(Z); ENDIF
ALLOCATE(X(2),Y(2),Z(2))
X(1)=1.0; X(2)=2.0
Y(1)=4.0; Y(2)=1.0
Z(1)=3.0; Z(2)=4.0

COLNAMES(1)='X'; COLNAMES(2)='Y'; COLNAMES(3)='ILAY'
COLNAMES(4)='MSR'; COLNAMES(5)='MDL'; COLNAMES(6)='WEIGH'

!!## make selection from given variables and fill x,y,z arrays
!DO J=1,SIZE(IPF%X(:))
! IF(ICOL(1).EQ.1)THEN 
!  X(J)=IPF%X(J) !## 'X'
! ELSEIF(ICOL(1).EQ.2)THEN
!  X(J)=IPF%Y(J) !## 'Y'
! ELSEIF(ICOL(1).EQ.3)THEN
!  X(J)=IPF%Z(J) !## 'ILAY'
! ELSEIF(ICOL(1).EQ.4)THEN
!  X(J)=IPF%O(J) !## 'MSR'
! ELSEIF(ICOL(1).EQ.5)THEN
!  X(J)=IPF%L(J) !## 'MDL'
! ELSEIF(ICOL(1).EQ.9)THEN
!  X(J)=IPF%Q(J) !## 'WEIGH'
! ELSE
!  WRITE(*,*) "Error: the given column number cannot be processed. Choose column 1-5 or 9."
!  EXIT
! ENDIF
!ENDDO
!
!DO J=1,SIZE(IPF%X(:))
! IF(ICOL(2).EQ.1)THEN 
!  Y(J)=IPF%X(J) !## 'X'
! ELSEIF(ICOL(2).EQ.2)THEN
!  Y(J)=IPF%Y(J) !## 'Y'
! ELSEIF(ICOL(2).EQ.3)THEN
!  Y(J)=IPF%Z(J) !## 'ILAY'
! ELSEIF(ICOL(2).EQ.4)THEN
!  Y(J)=IPF%O(J) !## 'MSR'
! ELSEIF(ICOL(2).EQ.5)THEN
!  Y(J)=IPF%L(J) !## 'MDL'
! ELSEIF(ICOL(2).EQ.9)THEN
!  Y(J)=IPF%Q(J) !## 'WEIGH'
! ELSE
!  WRITE(*,*) "Error: the given column number cannot be processed. Choose column 1-5 or 9."
!  EXIT
! ENDIF
!ENDDO
!
!DO J=1,SIZE(IPF%X(:))
! IF(ICOL(1).EQ.1)THEN 
!  Z(J)=IPF%X(J) !## 'X'
! ELSEIF(ICOL(3).EQ.2)THEN
!  Z(J)=IPF%Y(J) !## 'Y'
! ELSEIF(ICOL(3).EQ.3)THEN
!  Z(J)=IPF%Z(J) !## 'ILAY'
! ELSEIF(ICOL(3).EQ.4)THEN
!  Z(J)=IPF%O(J) !## 'MSR'
! ELSEIF(ICOL(3).EQ.5)THEN
!  Z(J)=IPF%L(J) !## 'MDL'
! ELSEIF(ICOL(3).EQ.9)THEN
!  Z(J)=IPF%Q(J) !## 'WEIGH'
! ELSE
!  WRITE(*,*) "Error: the given column number cannot be processed. Choose column 1-5 or 9."
!  EXIT
! ENDIF
!ENDDO

END SUBROUTINE RESIDUAL_PROC

!###===================================
SUBROUTINE RESIDUAL_PLOT()
!###===================================
IMPLICIT NONE
INTEGER :: I,NSETS,IWIDTH,IHEIGHT,IPLOTBMP
INTEGER,DIMENSION(1) :: ICOLR
REAL :: MINX,MINY,MAXX,MAXY

!## get minimum and maximum x and y values for plotting area scatter plot)
MINX=0.0; MINY=0.0; MAXX=1.0; MAXY=1.0
DO I=1,SIZE(X)
 MINX=MIN(MINX,X(I)); MAXX=MAX(MAXX,X(I))
ENDDO
DO I=1,SIZE(Y)
 MINY=MIN(MINY,Y(I)); MAXY=MAX(MAXY,Y(I))
ENDDO

!## create bitmap
CALL WBITMAPCREATE(IPLOTBMP,500,500)
CALL IGRSELECT(DRAWBITMAP,IPLOTBMP)
CALL IGRAREA(0.100,0.100,0.900,0.900)

NSETS=1

!## plot data in scatterplot
IF(IPLOT.EQ.1)THEN
 CALL IPGNEWPLOT(PGSCATTERPLOT,NSETS,SIZE(X),0,1) !## settings: option 1=type of plot, 2=number of sets, 3=amount of points, 4=no extra plotting options, 5=reset styles (1=yes)
 CALL IPGSTYLE(1,1,3,0,32,31,1) !## settings: option 1=set number, 2=marker type (0-4), 3='not used', 4='not used', 5=marker colour (31=red), 6='not used'
 CALL IPGMARKER(1,14) !## settings: option 1=set number, 2=marker type (14=dot)
 CALL IPGUNITS(MINX,MINY,MAXX,MAXY)
!## plot data in histogram
ELSEIF(IPLOT.EQ.2)THEN
 CALL IPGNEWPLOT(PGHISTOGRAM,NSETS,SIZE(X),PGLAYOVERLAP,1)
 CALL IPGSTYLE(1,4,2,0,31,64,1) !## settings: option 1=fill style (0-5), 2=fill density (1-4), 3=fill angle (1-4), 4=primary colour (31=red), 5=secondary colour, 6='not used'
 CALL IPGUNITS(-5.0,0.0,5.0,1.0) !## minimum x-value, minimum y-value, maximum x-value, maximum y-value
ENDIF

CALL IPGCLIPRECTANGLE('G')
CALL IPGAREA(0.100,0.100,0.900,0.900)
 
!## set grahpic layout settings
!## title
CALL WGRTEXTFONT(IFAMILY=FFCOURIER,ISTYLE=0,WIDTH=0.0133,HEIGHT=0.0333)
CALL IGRCOLOURN(223)
CALL IPGTITLEPOS(0.50)
CALL IPGTITLE('TITLE','C')
 
!## x-axis
CALL IPGXLABELPOS(0.70)
CALL IPGXLABEL('X AXIS','C') 
 
!## y-axis
CALL IPGYLABELPOS(0.80)
CALL IPGYLABELLEFT('Y AXIS','CV') 
 
!## draw actual axis
CALL IGRCOLOURN(223)
CALL IPGAXES() 
 
!## Adjust tick position for bottom X Axis
CALL IPGXTICKPOS(1) 
CALL IPGXTICKLENGTH(1.00)
CALL IPGDECIMALPLACES(-1)
CALL IPGXUSERSCALE(NPOINT=0)
CALL IPGXSCALEANGLE(0.00,0.00)
CALL IPGXSCALEPOS(0.38)
CALL IPGXSCALE('NT')
 
!## Adjust tick position for bottom Y Axis
CALL IPGYTICKPOS(1) 
CALL IPGYTICKLENGTH(1.00)
CALL IPGDECIMALPLACES(-1)
CALL IPGYUSERSCALE(NPOINT=0)
CALL IPGYSCALEANGLE(0.00,0.00)
CALL IPGYSCALEPOS(1.50)
CALL IPGYSCALELEFT('NT')
  
!## draw graph
IF(IPLOT.EQ.1)THEN
 CALL IPGSCATTERPLOT(X,Y,SIZE(X))
ELSEIF(IPLOT.EQ.2)THEN
 CALL IPGHISTOGRAM(X)
ENDIF

!## add legend to graph
CALL IPGKEYAREA(0.8,0.8,0.95,0.95)

CALL IPGKEYALL((/'dataset1'/),'B')

!## save graph
CALL WBITMAPSAVE(IPLOTBMP,TRIM(BMPNAME))
CALL WBITMAPDESTROY(IPLOTBMP)

END SUBROUTINE RESIDUAL_PLOT

!###===================================
SUBROUTINE RESIDUAL_CLEAN()
!###===================================
IMPLICIT NONE
INTEGER :: I

IF(ALLOCATED(IPF))THEN
 DO I=1,SIZE(IPF)
  IF(ASSOCIATED(IPF(I)%D))DEALLOCATE(IPF(I)%D)
  IF(ASSOCIATED(IPF(I)%X))DEALLOCATE(IPF(I)%X)
  IF(ASSOCIATED(IPF(I)%Y))DEALLOCATE(IPF(I)%Y)
  IF(ASSOCIATED(IPF(I)%Z))DEALLOCATE(IPF(I)%Z)
  IF(ASSOCIATED(IPF(I)%O))DEALLOCATE(IPF(I)%O)
  IF(ASSOCIATED(IPF(I)%W))DEALLOCATE(IPF(I)%W)
  IF(ASSOCIATED(IPF(I)%L))DEALLOCATE(IPF(I)%L)
 ENDDO
 DEALLOCATE(IPF)
ENDIF
IF(ALLOCATED(X))DEALLOCATE(X)
IF(ALLOCATED(X))DEALLOCATE(Y)
IF(ALLOCATED(X))DEALLOCATE(Z)

END SUBROUTINE RESIDUAL_CLEAN

END MODULE MOD_RESIDUALPLOT