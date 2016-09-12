MODULE MOD_RESIDUALPLOT

USE MOD_RESIDUALPLOT_PAR
USE WINTERACTER
USE IMODVAR, ONLY : RVERSION
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFNULLIFY,IDFDEALLOCATE
USE MOD_UTL
USE MOD_POLINT

CONTAINS

 !###===================================
 SUBROUTINE RESIDUAL_MAIN()
 !###===================================
 IMPLICIT NONE

 CALL WINDOWOPEN(FLAGS=HIDEWINDOW ) 
 CALL IGRCOLOURMODEL(24)
 
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
   IF(.NOT.(RESIDUAL_PROC_SELLAY(I,J)))CYCLE
   N=N+1
   SELECT CASE (IPLOT)
    !## scatter measurement/observation
    CASE (1); X(N)=IPFR(I)%M(J); Y(N)=IPFR(I)%O(J)
    !## histogram
    CASE (2); X(N)=IPFR(I)%M(J)-IPFR(I)%O(J)
   END SELECT
  ENDDO
 ENDDO

 ALLOCATE(X_TMP(N),Y_TMP(N),Z_TMP(N))
 DO I=1,N; X_TMP(I)=X(I); Y_TMP(I)=Y(I); Z_TMP(I)=Z(I); ENDDO
 DEALLOCATE(X,Y,Z)
 X=>X_TMP; Y=>Y_TMP; Z=>Z_TMP
 
 !## subroutine to calculate histogram classes
 CALL RESIDUAL_PROC_HISTCLASS()
 
 END SUBROUTINE RESIDUAL_PROC

 !###=================================== 
 SUBROUTINE RESIDUAL_PROC_HISTCLASS()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,J
 
 !## define histogram classes
 HCLASSES(1)=-10.0E10
 HCLASSES(2)=-5.0
 DO I=3,SIZE(HCLASSES)-1
  HCLASSES(I)=HCLASSES(I-1)+0.5
 ENDDO
 HCLASSES(SIZE(HCLASSES))=10.0E10
 
 !## count amount of points per class
 XCLASSES=0.0
 DO J=1,SIZE(X) 
  CALL POL1LOCATE(HCLASSES,SIZE(HCLASSES),REAL(X(J),8),I)
  XCLASSES(I)=XCLASSES(I)+1
 ENDDO
 
 END SUBROUTINE RESIDUAL_PROC_HISTCLASS
 
 !###===================================
 LOGICAL FUNCTION RESIDUAL_PROC_SELLAY(I,J)
 !###===================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,J
 INTEGER :: K
 
 RESIDUAL_PROC_SELLAY=.TRUE.
 
 !## function to count the amount of data points for the selected layers need to be plotted and which needs to be ommitted
 DO K=1,NLAYER
  RESIDUAL_PROC_SELLAY=IPFR(I)%L(J).EQ.ILAYER(K)
  IF(RESIDUAL_PROC_SELLAY)RETURN
 ENDDO 

 END FUNCTION RESIDUAL_PROC_SELLAY
 
 !###===================================
 SUBROUTINE RESIDUAL_PLOT()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,NSETS,IPLOTBMP,IPLOTBMP2,NCLR
 REAL :: MINX,MINY,MAXX,MAXY,X1,X2,Y1,Y2,DY
 CHARACTER(LEN=4),DIMENSION(22) :: XLABELS=(/'<-5','-5','-4.5','-4','-3.5','-3','-2.5','-2','-1.5','-1','-0.5','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','>5'/)
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IBARS
 
 !## get minimum and maximum x and y values for plotting area scatter plot
 MINX=X(1); MAXX=X(1)
 DO I=2,SIZE(X); MINX=MIN(MINX,X(I)); MAXX=MAX(MAXX,X(I)); ENDDO
 MINY=Y(1); MAXY=Y(1)
 DO I=2,SIZE(Y); MINY=MIN(MINY,Y(I)); MAXY=MAX(MAXY,Y(I)); ENDDO

 !## create bitmap - resolution
! CALL IGRPALETTE(0,WRGB(6,165,205))
 CALL WBITMAPCREATE(IPLOTBMP,2000,2000)
 CALL IGRSELECT(DRAWBITMAP,IPLOTBMP)
 !## plot area is the entire bitmap
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(0.0,0.0,1.0,1.0)
  
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
  CALL IPGUNITS(MINX,MINX,MAXX,MAXX)
 !## plot data in histogram
 ELSEIF(IPLOT.EQ.2)THEN
  CALL IPGNEWPLOT(PGHISTOGRAM,NSETS,SIZE(XCLASSES),PGLAYADJACENT,1)
  !## settings: option 1=fill style (0-5), 2=fill density (1-4), 3=fill angle (1-4), 4=primary colour (31=red), 5=secondary colour, 6='not used'
  CALL IPGSTYLE(1,4,2,0,NCLR,WRGB(6,165,205),1) 
  !## minimum x-value, minimum y-value, maximum x-value, maximum y-value
  !## to be computed ourselves beforehand in subroutine "RESIDUAL_PROC_HISTCLASS"
!  CALL IPGUNITS(HCLASSES(2)-1,MINVAL(XCLASSES),HCLASSES(SIZE(HCLASSES)-1)+1,MAXVAL(XCLASSES)) !## in principe wil ik deze regel gebruiken, maar...
  X1=0.0; X2=22.0; Y1=MINVAL(XCLASSES); Y2=MAXVAL(XCLASSES); DY=(Y2-Y1)/50.0; Y2=Y2+DY
  CALL IPGUNITS(X1,Y1,X2,Y2)  !## ... deze regel is voor het testen en dat levert dezelfde resultaten op.
 ENDIF

 !## clip graph on graphical area
 CALL IPGCLIPRECTANGLE('P')
 !## area of the graph
 CALL IPGAREA(0.125,0.1,0.95,0.9)
 CALL IPGBORDER()
  
 !## set all text black
 CALL IGRCOLOURN(WRGB(0,0,0))

 !## set grahpic layout settings
 CALL WGRTEXTFONT(IFAMILY=FFHELVETICA,ISTYLE=0,WIDTH=0.0133,HEIGHT=0.0333)
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
! CALL IGRCOLOURN(223)
 !## Draws a set of axes in the current Presentation Graphics area
 CALL IPGAXES() 
 !## set number of decimal places (automatic=-1, maximum=9)
 CALL IPGDECIMALPLACES(-1)

 CALL IPGXSCALEANGLE(0.00,0.00)
 !## describes the position of the X scale numbers or descriptions as a proportion of the distance from the edge 
 !## of the PG area to the edge of the main graphics area. 
 CALL IPGXSCALEPOS(0.2)
   
 IF(IPLOT.EQ.1)THEN
  !## adjust tick position for bottom X Axis
  CALL IPGXTICKPOS(2) 
  CALL IPGXTICKLENGTH(1.00)
  !## numbering and tick outside
  CALL IPGXSCALE('NT')
 ELSEIF(IPLOT.EQ.2)THEN
  ALLOCATE(IBARS(22)); DO I=1,22; IBARS(I)=I; ENDDO
  CALL IPGXUSERSCALEHIST(IBARS,22)
  DEALLOCATE(IBARS)
!  CALL IPGXSCALE('T')
  CALL IPGXTEXT(XLABELS,22) 
 ENDIF

 !## adjust tick position for left Y Axis
 CALL IPGYTICKPOS(2) 
 CALL IPGYTICKLENGTH(1.00)
! CALL IPGYUSERSCALE(NPOINT=0)
 CALL IPGYSCALEANGLE(0.00,0.00)
 !## describes the position of the Y scale numbers or descriptions as a multiple of the length of a normal (i.e. non-log scale)
 !## Y-axis tick mark.  
 CALL IPGYSCALEPOS(1.5)
 !## numbering and tick outside
 CALL IPGYSCALELEFT('NT')
  
 CALL IPGXGRATICULES(DASHED) !LTYPE)
 CALL IPGYGRATICULES(DASHED) !LTYPE)

 !## draw graph
 IF(IPLOT.EQ.1)THEN
  CALL IPGSCATTERPLOT(X,Y,SIZE(X))
!  CALL IPGUNITSTOGRUNITS(MINX,MINX,X1,Y1)
!  CALL IPGUNITSTOGRUNITS(MAXX,MAXX,X2,Y2)
!  CALL IGRJOIN(X1,Y1,X2,Y2)
  CALL IPGPOLYLINE2((/MINX,MAXX/),(/MINX,MAXX/),2)
 ELSEIF(IPLOT.EQ.2)THEN
  CALL IPGHISTOGRAM(XCLASSES)
 ENDIF

 !## add legend to graph
 CALL IPGKEYAREA(0.20,0.80,0.40,0.90)
 !## clear legend area
 CALL IPGKEYALL((/'Dataset size: '//TRIM(ITOS(SIZE(X)))/),'C')
 
 !## put textblock on plotting area
 CALL WGRTEXTFONT(FFHELVETICA,ISTYLE=0,WIDTH=0.01,HEIGHT=0.03)
 CALL WGRTEXTBLOCK(0.75,0.0,1.0,0.05,'(c) Powered by iMOD '//TRIM(RVERSION)//', 2016')
  
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
 IF(ASSOCIATED(X))DEALLOCATE(X)
 IF(ASSOCIATED(Y))DEALLOCATE(Y)
 IF(ASSOCIATED(Z))DEALLOCATE(Z)

 END SUBROUTINE RESIDUAL_CLEAN

END MODULE MOD_RESIDUALPLOT