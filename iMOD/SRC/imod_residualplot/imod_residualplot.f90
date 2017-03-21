MODULE MOD_RESIDUALPLOT

USE MOD_RESIDUALPLOT_PAR
USE WINTERACTER
USE IMODVAR, ONLY : RVERSION
USE MOD_IDF_PAR, ONLY : IDFOBJ
USE MOD_IDF, ONLY : IDFREAD,IDFWRITE,IDFNULLIFY,IDFDEALLOCATE
USE MOD_IPF_PAR, ONLY : IPF,NIPF,ASSF
USE MOD_IPF, ONLY : IPFREAD2,IPFALLOCATE
USE MOD_IPFASSFILE, ONLY : IPFOPENASSFILE,IPFREADASSFILELABEL,IPFREADASSFILE,IPFASSFILEALLOCATE
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
 IF(INDEX(UTL_CAP(INPUTFILE,'U'),'IPF'))THEN
  CALL RESIDUAL_DATA_IPF(ICOL)
 !## read in complete txtfile
 ELSE
  CALL RESIDUAL_DATA()
 ENDIF
 !## process txtfile into xyz variables
 CALL RESIDUAL_PROC()
 !## plot xyz variables
 CALL RESIDUAL_PLOT()
 !## clean
 CALL RESIDUAL_CLEAN()

 END SUBROUTINE RESIDUAL_MAIN

 !###===================================
 SUBROUTINE RESIDUAL_DATA_IPF(ICOL)
 !###===================================
 IMPLICIT NONE
 INTEGER,DIMENSION(:),INTENT(IN) :: ICOL
 INTEGER :: I,J,K,IU,N
 REAL :: O,M
 CHARACTER(LEN=256) :: FNAME,DIR

 NIPF=1; CALL IPFALLOCATE(); IPF(1)%FNAME=INPUTFILE

 IPF(1)%XCOL=ICOL(1)
 IPF(1)%YCOL=ICOL(2)
 IF(ITRANSIENT.EQ.1)THEN
  !## not used reading the ipf-file, set them to the x/y coordinates
  IPF(1)%ZCOL=ICOL(1)
  IPF(1)%Z2COL=ICOL(2)
 ELSE
  IPF(1)%ZCOL=ICOL(3)
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
   IF(ICOL(5).EQ.0)THEN; IPFR(1)%W(J)=1.0
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

       DO K=1,ASSF(1)%NRASS

        O=ASSF(1)%MEASURE(ICOL(3)-1,K)
        M=ASSF(1)%MEASURE(ICOL(4)-1,K)
        !## both not equal to nodata
        IF(O.NE.ASSF(1)%NODATA(ICOL(3)).AND. &
           M.NE.ASSF(1)%NODATA(ICOL(4)))THEN
           
         N=N+1
 
         IF(I.EQ.2)THEN
          READ(IPF(1)%INFO(ICOL(1),J),*)  IPFR(1)%X(N)
          READ(IPF(1)%INFO(ICOL(2),J),*)  IPFR(1)%Y(N)
       
          IF(ICOL(5).EQ.0)THEN; IPFR(1)%W(N)=1.0
          ELSE; READ(IPF(1)%INFO(ICOL(5),J),*) IPFR(1)%W(N); ENDIF
          IF(ICOL(6).EQ.0)THEN; IPFR(1)%L(N)=1
          ELSE; READ(IPF(1)%INFO(ICOL(6),J),*)  IPFR(1)%L(N); ENDIF

          IPFR(1)%O(N)=O 
          IPFR(1)%M(N)=M

          !## store date
          IPFR(1)%D(N)=ASSF(1)%IDATE(I)
         ENDIF
                 
        ENDIF  
       ENDDO
       
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

 END SUBROUTINE RESIDUAL_DATA_IPF
 
 !###===================================
 SUBROUTINE RESIDUAL_DATA()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,IU,NIPF,IOS,N
 REAL :: J,WMDL,WRES,MMSR,MMDL,CC,DYNMSR,DYNMLD
 CHARACTER(LEN=256) :: LINE

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(INPUTFILE),ACTION='READ',FORM='FORMATTED',STATUS='OLD')
 IF(IU.EQ.0)STOP
 
 !## three loops, first to get number of ipf files, second to get number of points per ipf, third to read all in memory
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
 
 ALLOCATE(IPFR(NIPF)); REWIND(IU)
 
 DO I=1,2
  
  IPFR%NPOINTS=0
  DO J=1,SIZE(IPFR)+1; READ(IU,*); ENDDO
  
  DO 
   READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT
   IF(ITRANSIENT.EQ.1)THEN
    READ(LINE,'(160X,I10)') NIPF
   ELSE
    READ(LINE,'(160X,I10)') NIPF
   ENDIF
   IPFR(NIPF)%NPOINTS=IPFR(NIPF)%NPOINTS+1
   IF(I.EQ.2)THEN
    N=IPFR(NIPF)%NPOINTS
    IF(ITRANSIENT.EQ.1)THEN
     READ(LINE,'(2F15.7,I10,3F15.7)') IPFR(NIPF)%X(N),IPFR(NIPF)%Y(N), &
                                      IPFR(NIPF)%L(N),IPFR(NIPF)%W(N),IPFR(NIPF)%O(N),IPFR(NIPF)%M(N)
    ELSE
     READ(LINE,'(2F15.7,I10,6F15.7)') IPFR(NIPF)%X(N),IPFR(NIPF)%Y(N), &
                                      IPFR(NIPF)%L(N),IPFR(NIPF)%O(N),IPFR(NIPF)%M(N), &
                                      J,WMDL,WRES,IPFR(NIPF)%W(N)
    ENDIF
   ENDIF
  ENDDO
  
  IF(I.EQ.1)THEN
   DO J=1,SIZE(IPFR)
    N=IPFR(J)%NPOINTS
    ALLOCATE(IPFR(J)%X(N),IPFR(J)%Y(N),IPFR(J)%L(N),IPFR(J)%O(N), &
             IPFR(J)%M(N),IPFR(J)%W(N))
   ENDDO
  ENDIF

  REWIND(IU)
 ENDDO
 CLOSE(IU)

 END SUBROUTINE RESIDUAL_DATA

 !###===================================
 SUBROUTINE RESIDUAL_DATA_ORG()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,IU,NIPF,IOS,N
 REAL :: J,WMDL,WRES,MMSR,MMDL,CC,DYNMSR,DYNMLD
 CHARACTER(LEN=256) :: LINE

 IU=UTL_GETUNIT(); CALL OSD_OPEN(IU,FILE=TRIM(INPUTFILE),ACTION='READ',FORM='FORMATTED',STATUS='OLD')
 IF(IU.EQ.0)STOP
 
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
    IF(I.GE.2)N=0 
    DO
     READ(IU,'(A256)',IOSTAT=IOS) LINE; IF(IOS.NE.0)EXIT; IF(TRIM(LINE).EQ.'')EXIT
     IF(I.GE.2)N=N+1 
     IF(I.EQ.3)THEN
      IF(ITRANSIENT.EQ.1)THEN
       READ(LINE,'(I10,2F15.7,I10,3F15.7)') IPFR(NIPF)%D(N),IPFR(NIPF)%X(N),IPFR(NIPF)%Y(N), &
                                            IPFR(NIPF)%L(N),IPFR(NIPF)%W(N),IPFR(NIPF)%O(N),IPFR(NIPF)%M(N)
                                            
      ELSE
       READ(LINE,'(2F15.7,I10,6F15.7)') IPFR(NIPF)%X(N),IPFR(NIPF)%Y(N), &
                                        IPFR(NIPF)%L(N),IPFR(NIPF)%O(N),IPFR(NIPF)%M(N), &
                                        J,WMDL,WRES,IPFR(NIPF)%W(N)
      ENDIF
     ENDIF
    ENDDO
   ENDIF
   IF(I.EQ.2)THEN
    IPFR(NIPF)%NPOINTS=N
    IF(ITRANSIENT.EQ.1)ALLOCATE(IPFR(NIPF)%D(N))
    ALLOCATE(IPFR(NIPF)%X(N),IPFR(NIPF)%Y(N),IPFR(NIPF)%L(N),IPFR(NIPF)%O(N), &
             IPFR(NIPF)%M(N),IPFR(NIPF)%W(N))
   ENDIF
  ENDDO
  IF(I.EQ.1)ALLOCATE(IPFR(NIPF))
  !## position back to first position of txtfile
  REWIND(IU)
 ENDDO
 CLOSE(IU)

 END SUBROUTINE RESIDUAL_DATA_ORG

 !###===================================
 SUBROUTINE RESIDUAL_PROC()
 !###===================================
 IMPLICIT NONE
 INTEGER :: I,J,N !,IU

 !## okay we keep it simple - we plot everything - no selection yet
 N=SUM(IPFR%NPOINTS)

 !## allocate memory
 ALLOCATE(X(N),Y(N),Z(N))
 
 N=0
 DO I=1,SIZE(IPFR)
  IF(.NOT.(RESIDUAL_PROC_SELIPF(I)))CYCLE
  DO J=1,IPFR(I)%NPOINTS
   IF(.NOT.((RESIDUAL_PROC_SELLAY(I,J)).AND.(RESIDUAL_PROC_SELDATE(I,J)).AND.(RESIDUAL_PROC_SELWEIGHT(I,J))))CYCLE
   
   !## skip weigth is zero anyhow
   IF(IPFR(I)%W(J).LE.0)CYCLE
   
   N=N+1
   SELECT CASE (IPLOT)
    !## scatter measurement/observation
    CASE (1)
     IF(IWEIGHT.EQ.1)THEN
      X(N)=IPFR(I)%M(J)*IPFR(I)%W(J); Y(N)=IPFR(I)%O(J)*IPFR(I)%W(J); Z(N)=IPFR(I)%W(J)*(IPFR(I)%M(J)-IPFR(I)%O(J))**2.0
     ELSE
      X(N)=IPFR(I)%M(J); Y(N)=IPFR(I)%O(J); Z(N)=(IPFR(I)%M(J)-IPFR(I)%O(J))**2.0
     ENDIF
    !## histogram
    CASE (2) 
     IF(IWEIGHT.EQ.1)THEN
      X(N)=(IPFR(I)%M(J)*IPFR(I)%W(J))-(IPFR(I)%O(J)*IPFR(I)%W(J))
     ELSE
      X(N)=IPFR(I)%M(J)-IPFR(I)%O(J)
     ENDIF
   END SELECT
  ENDDO
 ENDDO
 
 IF(N.EQ.0)THEN
  WRITE(*,*) "No data points are found, check your initial settings if you selected the right layers, dates or ipf's"
  STOP
 ENDIF
 
 !## final number of points
 ALLOCATE(X_TMP(N),Y_TMP(N),Z_TMP(N))
 DO I=1,N; X_TMP(I)=X(I); Y_TMP(I)=Y(I); Z_TMP(I)=Z(I); ENDDO
 DEALLOCATE(X,Y,Z)
 X=>X_TMP; Y=>Y_TMP; Z=>Z_TMP
 
! !## write down results
! IU=UTL_GETUNIT()
! OPEN(IU,FILE='d:\IMOD-MODELS\IBRAHYM_V2.0\DBASE_DLD\IMOD_USER\MODELS\IB_V2.0.4_13\ENTIREMODEL_IPEST_V1\pest\bmp\DATA.TXT',STATUS='UNKNOWN',ACTION='WRITE')
! DO I=1,N
!  WRITE(IU,*) X(I),Y(I),Z(I)
! ENDDO
! CLOSE(IU)
  
 !## subroutine to calculate histogram classes
 CALL RESIDUAL_PROC_HISTCLASS()
 
 !## regressions for goodness-of-fit
 GOF=UTL_GOODNESS_OF_FIT(X,Y,SIZE(X))
 
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
 INTEGER :: K 
 
 RESIDUAL_PROC_SELDATE=.TRUE.
 
 IF(ITRANSIENT.EQ.1)THEN
  DO K=1,NRDATE
   RESIDUAL_PROC_SELDATE=IPFR(I)%D(J).EQ.IRDATE(K)
   IF(RESIDUAL_PROC_SELDATE)RETURN
  ENDDO 
 ENDIF
 
 END FUNCTION RESIDUAL_PROC_SELDATE

 !###===================================
 LOGICAL FUNCTION RESIDUAL_PROC_SELWEIGHT(I,J)
 !###===================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I,J 
 
! RESIDUAL_PROC_SELWEIGHT=.TRUE.
 
! IF(IWEIGHT.EQ.1)THEN
 !## always skip weight of zero
 RESIDUAL_PROC_SELWEIGHT=IPFR(I)%W(J).NE.0.0
 IF(RESIDUAL_PROC_SELWEIGHT)RETURN
! ENDIF
 
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
 INTEGER :: I,NSETS,IPLOTBMP,IPLOTBMP2,NCLR,MX
 REAL :: MINX,MINY,MAXX,MAXY,X1,X2,Y1,Y2,DY,AVG
 CHARACTER(LEN=4),DIMENSION(22) :: XLABELS=(/'<-5','-5','-4.5','-4','-3.5','-3','-2.5','-2','-1.5','-1','-0.5','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5','>5'/)
 INTEGER,ALLOCATABLE,DIMENSION(:) :: IBARS
 CHARACTER(LEN=256) :: LINE
 REAL,DIMENSION(3) :: XMED
 
 !## get minimum and maximum x and y values for plotting area scatter plot
 MINX=X(1); MAXX=X(1)
 DO I=2,SIZE(X); MINX=MIN(MINX,X(I)); MAXX=MAX(MAXX,X(I)); ENDDO
 MINY=Y(1); MAXY=Y(1)
 DO I=2,SIZE(Y); MINY=MIN(MINY,Y(I)); MAXY=MAX(MAXY,Y(I)); ENDDO

 !## create bitmap - resolution
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

 !## set graphic layout settings
 CALL WGRTEXTFONT(IFAMILY=FFHELVETICA,ISTYLE=0,WIDTH=0.0133,HEIGHT=0.0333)
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
  
 !## scatterplot
 IF(IPLOT.EQ.1)THEN
  CALL WGRTEXTORIENTATION(ALIGNRIGHT,0.0,DIRHORIZ,ALIGNRIGHT)
  LINE='Goodness of Fit: '//TRIM(RTOS(GOF,'F',3))
  CALL WGRTEXTBLOCK(0.20,0.5,0.50,0.8,TRIM(LINE)) 
 ELSE
  !### write statistics
  AVG=SUM(X)/REAL(SIZE(X))
  CALL UTL_GETMED(X,SIZE(X),HUGE(1.0),(/10.0,50.0,90.0/),3,MX,XMED)
  LINE='Statistics'                             //CHAR(13)//CHAR(10)// &
       'Average:    '//CHAR(32)//TRIM(RTOS(AVG,'G',7))    //CHAR(13)//CHAR(10)// &
       'Minimal:    '//CHAR(32)//TRIM(RTOS(MINX,'G',7))   //CHAR(13)//CHAR(10)// &
       '10 Percent: '//CHAR(32)//TRIM(RTOS(XMED(1),'G',7))//CHAR(13)//CHAR(10)// &
       '50 Percent: '//CHAR(32)//TRIM(RTOS(XMED(2),'G',7))//CHAR(13)//CHAR(10)// &
       '90 Percent: '//CHAR(32)//TRIM(RTOS(XMED(3),'G',7))//CHAR(13)//CHAR(10)// &
       'Maximal:    '//CHAR(32)//TRIM(RTOS(MAXX,'G',7)) 
  CALL WGRTEXTORIENTATION(ALIGNLEFT,0.0,DIRHORIZ,ALIGNRIGHT)
  CALL WGRTEXTBLOCK(0.20,0.5,0.50,0.8,TRIM(LINE)) 
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
  ENDDO
  DEALLOCATE(IPFR)
 ENDIF
 IF(ASSOCIATED(X))DEALLOCATE(X)
 IF(ASSOCIATED(Y))DEALLOCATE(Y)
 IF(ASSOCIATED(Z))DEALLOCATE(Z)

 END SUBROUTINE RESIDUAL_CLEAN

END MODULE MOD_RESIDUALPLOT