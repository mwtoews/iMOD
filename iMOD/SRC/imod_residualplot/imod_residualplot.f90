MODULE MOD_RESIDUALPLOT

USE MOD_RESIDUALPLOT_PAR
USE WINTERACTER
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFNULLIFY,IDFDEALLOCATE
USE MOD_UTL

CONTAINS

 !###===================================
 SUBROUTINE RESIDUAL_MAIN()
 !###===================================
 IMPLICIT NONE

 CALL WINDOWOPEN(FLAGS=HIDEWINDOW ) 

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
 INTEGER :: I,IU,NIPF,IOS
 CHARACTER(LEN=256) :: LINE

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(INPUTFILE),ACTION='READ',FORM='FORMATTED',STATUS='OLD')

 !## three loops, first to get number of ipf files, second to get number of points per ipf, third to read all in memory
 DO I=1,3
  NIPF=0
  !## skip first line
  READ(IU,*)
  DO 
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   !## found ipf
   IF(INDEX(UTL_CAP(LINE,'U'),'.IPF').GT.0)THEN
    NIPF=NIPF+1
    !## read empty line and header
    READ(IU,*); READ(IU,*)  
    !## read data block, count points
    IF(I.GE.2)IPFR(NIPF)%NPOINTS=0
    DO
     READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT; IF(TRIM(LINE).EQ.'')EXIT
     IF(I.GE.2)IPFR(NIPF)%NPOINTS=IPFR(NIPF)%NPOINTS+1
     IF(I.EQ.3)THEN
      IF(ITRANSIENT.EQ.1)THEN
       READ(LINE,*) IPFR(NIPF)%D(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%X(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%Y(IPFR(NIPF)%NPOINTS), &
                    IPFR(NIPF)%L(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%O(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%M(IPFR(NIPF)%NPOINTS)
      ELSE
       READ(LINE,*) IPFR(NIPF)%X(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%Y(IPFR(NIPF)%NPOINTS), &
                    IPFR(NIPF)%L(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%O(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%M(IPFR(NIPF)%NPOINTS)
      ENDIF
     ENDIF
    ENDDO
   ENDIF
   IF(I.EQ.2)THEN
    IF(ITRANSIENT.EQ.1)ALLOCATE(IPFR(NIPF)%D(IPFR(NIPF)%NPOINTS))
    ALLOCATE(IPFR(NIPF)%X(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%Y(IPFR(NIPF)%NPOINTS), &
             IPFR(NIPF)%L(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%O(IPFR(NIPF)%NPOINTS),IPFR(NIPF)%M(IPFR(NIPF)%NPOINTS))
   ENDIF
  ENDDO
  IF(I.EQ.1)ALLOCATE(IPFR(NIPF))
  !## position back to first position of txtfile
  REWIND(IU)
 ENDDO
 CLOSE(IU)

 END SUBROUTINE RESIDUAL_DATA

 !###===================================
 SUBROUTINE RESIDUAL_PROC()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,J,N

 !## okay we keep it simple - we plot everything - no selection yet
 N=SUM(IPFR%NPOINTS)

 !## allocate memory
 ALLOCATE(X(N),Y(N),Z(N))

 N=0
 DO I=1,SIZE(IPFR)
  DO J=1,IPFR(I)%NPOINTS
   N=N+1
   SELECT CASE (IPLOT)
    !## scatter measurement/observation
    CASE (1); X(N)=IPFR(I)%M(J); Y(N)=IPFR(I)%O(J)
    !## histogram
    CASE (2); X(N)=IPFR(I)%M(J)-IPFR(I)%O(J)
   END SELECT
  ENDDO
 ENDDO

 END SUBROUTINE RESIDUAL_PROC

 !###===================================
 SUBROUTINE RESIDUAL_PLOT()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,NSETS,IPLOTBMP
 REAL :: MINX,MINY,MAXX,MAXY

 !## get minimum and maximum x and y values for plotting area scatter plot)
 MINX=X(1); MAXX=X(1)
 DO I=2,SIZE(X); MINX=MIN(MINX,X(I)); MAXX=MAX(MAXX,X(I)); ENDDO
 MINY=Y(1); MAXY=Y(1)
 DO I=2,SIZE(Y); MINY=MIN(MINY,Y(I)); MAXY=MAX(MAXY,Y(I)); ENDDO

 !## create bitmap
 CALL WBITMAPCREATE(IPLOTBMP,500,500)
 CALL IGRSELECT(DRAWBITMAP,IPLOTBMP)
 CALL IGRAREA(0.0,0.0,1.0,1.0)

 NSETS=1

 !## plot data in scatterplot
 IF(IPLOT.EQ.1)THEN
  !## settings: option 1=type of plot, 2=number of sets, 3=amount of points, 4=no extra plotting options, 5=reset styles (1=yes)
  CALL IPGNEWPLOT(PGSCATTERPLOT,NSETS,SIZE(X),0,1) 
  !## settings: option 1=set number, 2=marker number, 3=marker type (0-4), 4='not used', 5='not used', 6=marker colour (31=red), 7='not used'
  CALL IPGSTYLE(1,1,3,0,32,31,1) 
  !## settings: option 1=set number, 2=marker type (14=dot)
  CALL IPGMARKER(1,14) 
  CALL IPGUNITS(MINX,MINY,MAXX,MAXY)
 !## plot data in histogram
 ELSEIF(IPLOT.EQ.2)THEN
  CALL IPGNEWPLOT(PGHISTOGRAM,NSETS,SIZE(X),PGLAYOVERLAP,1)
  !## settings: option 1=fill style (0-5), 2=fill density (1-4), 3=fill angle (1-4), 4=primary colour (31=red), 5=secondary colour, 6='not used'
  CALL IPGSTYLE(1,4,2,0,31,64,1) 
  !## minimum x-value, minimum y-value, maximum x-value, maximum y-value
  CALL IPGUNITS(-5.0,0.0,5.0,1.0) 
 ENDIF

 CALL IPGCLIPRECTANGLE('G')
! CALL IPGAREA(0.1,0.1,0.9,0.9)
 
 !## set grahpic layout settings
 CALL WGRTEXTFONT(IFAMILY=FFHELVETICA,ISTYLE=0,WIDTH=0.0133,HEIGHT=0.0333)
 CALL IGRCOLOURN(223)
 CALL IPGTITLEPOS(0.50)
 !## title
 CALL IPGTITLE('TITLE','C')
 
 !## x-axis
 CALL IPGXLABELPOS(0.50)
 CALL IPGXLABEL('X AXIS','C') 
 
 !## y-axis
 CALL IPGYLABELPOS(0.50)
 CALL IPGYLABELLEFT('Y AXIS','C9') 
 
 !## draw actual axis
 CALL IGRCOLOURN(223)
 !## Draws a set of axes in the current Presentation Graphics area
 CALL IPGAXES() 
 !## set number of decimal places (automatic, maximum=9)
 CALL IPGDECIMALPLACES(-1)
 
 !## adjust tick position for bottom X Axis
 CALL IPGXTICKPOS(1) 
 CALL IPGXTICKLENGTH(1.00)
! CALL IPGXUSERSCALE(NPOINT=0)
 CALL IPGXSCALEANGLE(0.00,0.00)
 CALL IPGXSCALEPOS(0.38)
 CALL IPGXSCALE('NT')
 
 !## adjust tick position for left Y Axis
 CALL IPGYTICKPOS(1) 
 CALL IPGYTICKLENGTH(1.00)
! CALL IPGYUSERSCALE(NPOINT=0)
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
 CALL IPGKEYAREA(0.05,0.80,0.15,0.95)

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

 IF(ALLOCATED(IPFR))THEN
  DO I=1,SIZE(IPFR)
   IF(ASSOCIATED(IPFR(I)%D))DEALLOCATE(IPFR(I)%D)
   IF(ASSOCIATED(IPFR(I)%X))DEALLOCATE(IPFR(I)%X)
   IF(ASSOCIATED(IPFR(I)%Y))DEALLOCATE(IPFR(I)%Y)
   IF(ASSOCIATED(IPFR(I)%M))DEALLOCATE(IPFR(I)%M)
   IF(ASSOCIATED(IPFR(I)%O))DEALLOCATE(IPFR(I)%O)
   IF(ASSOCIATED(IPFR(I)%W))DEALLOCATE(IPFR(I)%W)
   IF(ASSOCIATED(IPFR(I)%L))DEALLOCATE(IPFR(I)%L)
  ENDDO
  DEALLOCATE(IPFR)
 ENDIF
 IF(ALLOCATED(X))DEALLOCATE(X)
 IF(ALLOCATED(X))DEALLOCATE(Y)
 IF(ALLOCATED(X))DEALLOCATE(Z)

 END SUBROUTINE RESIDUAL_CLEAN

END MODULE MOD_RESIDUALPLOT