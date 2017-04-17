MODULE MOD_GRAPH

USE WINTERACTER
USE RESOURCE
USE MODPLOT, ONLY : MPW
USE MOD_GRAPH_PAR
USE MOD_UTL, ONLY : RTOS,ITOS,UTL_GDATE,UTL_SETTEXTSIZE,UTL_DRAWLEGENDBOX,JDATETOGDATE,JDATETOFDATE,UTL_GETFORMAT,UTL_GETAXESCALES, &
              SXVALUE,SYVALUE,NSX,NSY

CONTAINS

 !###======================================================================
 SUBROUTINE GRAPH_PLOT(XTXT,YTXT,LDATE)
 !###======================================================================
 IMPLICIT NONE
 LOGICAL ,INTENT(IN) :: LDATE
 CHARACTER(LEN=*),INTENT(IN) :: XTXT,YTXT
 INTEGER :: ITYPE,NG,IBITMAP
 TYPE(WIN_MESSAGE) :: MESSAGE
 
 NG=1
 IBITMAP=0
 
 CALL WDIALOGLOAD(ID_DSCENTOOL_FIGURE,ID_DSCENTOOL_FIGURE)
 CALL WDIALOGTITLE('Graph')
 CALL WDIALOGPUTIMAGE(ID_ZOOMIN,ID_ICONZOOMIN,1)
 CALL WDIALOGPUTIMAGE(ID_ZOOMOUT,ID_ICONZOOMOUT,1)
 CALL WDIALOGPUTIMAGE(ID_ZOOMFULL,ID_ICONZOOMFULL,1)
 CALL WDIALOGPUTIMAGE(ID_ZOOMBOX,ID_ICONZOOMBOX,1)
 CALL WDIALOGPUTIMAGE(ID_MOVE,ID_ICONMOVE,1)
 CALL WDIALOGPUTIMAGE(ID_COPY,ID_ICONCOPY,1)
 NG=1
 IF(SIZE(GRAPH,2).GT.1)THEN
  CALL WDIALOGPUTMENU(IDF_MENU1,GRAPHNAMES,SIZE(GRAPHNAMES),NG)
  CALL WDIALOGFIELDSTATE(IDF_LABEL2,3)
 ELSE
  CALL WDIALOGFIELDSTATE(IDF_MENU1,3)
  CALL WDIALOGFIELDSTATE(IDF_LABEL2,3)
 ENDIF
 CALL WDIALOGPUTSTRING(IDF_LABEL1,'Move your mouse in the graph!')
 !## position graph initially
 IF(ALLOCATED(GRAPHUNITS))DEALLOCATE(GRAPHUNITS)
 IF(ALLOCATED(GRAPHAREA))DEALLOCATE(GRAPHAREA)
 ALLOCATE(GRAPHUNITS(6,1),GRAPHAREA(4,1))
 CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.TRUE.)
 CALL WDIALOGSHOW(-1,-1,0,3)

 DO
  CALL WMESSAGE(ITYPE,MESSAGE)
  SELECT CASE (ITYPE)
   CASE (MOUSEMOVE)
    CALL GRAPH_MOUSE(MESSAGE,LDATE,0)
   CASE (PUSHBUTTON)
    SELECT CASE (MESSAGE%VALUE1)
     CASE (ID_ZOOMIN,ID_ZOOMOUT,ID_ZOOMFULL,ID_ZOOMBOX,ID_MOVE)
      CALL GRAPH_ZOOM(ID_DSCENTOOL_FIGURE,MESSAGE%VALUE1,IBITMAP,LDATE,NG,XTXT,YTXT)
     CASE (ID_COPY)
      CALL WCLIPBOARDPUTBITMAP(IBITMAP)
     CASE (IDHELP)
       CALL IMODGETHELP('5.9.2','TMO.PT.Start')              
     CASE (IDCANCEL)
      EXIT
    END SELECT
   CASE (FIELDCHANGED)
    SELECT CASE (MESSAGE%VALUE2)
     CASE (IDF_MENU1)
      CALL WDIALOGSELECT(ID_DSCENTOOL_FIGURE)
      CALL WDIALOGGETMENU(IDF_MENU1,NG)
      CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.TRUE.)
    END SELECT
   CASE (RESIZE,EXPOSE)
    !## refresh graph
    CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.FALSE.)
  END SELECT
 END DO

 CALL WBITMAPDESTROY(IBITMAP)

 IF(WINFOMOUSE(MOUSECURSOR).NE.CURARROW)CALL WCURSORSHAPE(CURARROW)
 CALL WDIALOGUNLOAD()
 DEALLOCATE(GRAPHUNITS,GRAPHAREA)
 CALL WINDOWSELECT(MPW%IWIN)
 CALL IGRSELECT(DRAWWIN,MPW%IWIN)
 CALL IGRAREA(0.0,0.0,1.0,1.0)
 CALL IGRUNITS(MPW%XMIN,MPW%YMIN,MPW%XMAX,MPW%YMAX)

 END SUBROUTINE GRAPH_PLOT

 !###======================================================================
 SUBROUTINE GRAPH_MOUSE(MESSAGE,LDATE,IDOWN)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: IDOWN
 LOGICAL,INTENT(IN) :: LDATE
 TYPE(WIN_MESSAGE),INTENT(IN) :: MESSAGE
 
 IF(MESSAGE%WIN.EQ.ID_DSCENTOOL_FIGURE)THEN
  IF(IDOWN.EQ.0.AND.WINFOMOUSE(MOUSECURSOR).NE.CURCROSSHAIR)CALL WCURSORSHAPE(CURCROSSHAIR)
  IF(LDATE)THEN
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'Date='//TRIM(JDATETOGDATE(INT(MESSAGE%GX)))//'; Ycrd='//TRIM(RTOS(MESSAGE%GY,'F',2)))
  ELSE
   CALL WDIALOGPUTSTRING(IDF_LABEL1,'Xcrd='//TRIM(RTOS(MESSAGE%GX,'F',2))//'; Ycrd='//TRIM(RTOS(MESSAGE%GY,'F',2)))
  ENDIF
 ELSE
  IF(IDOWN.EQ.0.AND.WINFOMOUSE(MOUSECURSOR).NE.CURARROW)CALL WCURSORSHAPE(CURARROW)
 ENDIF
 
 END SUBROUTINE GRAPH_MOUSE

 !###======================================================================
 SUBROUTINE GRAPH_DRAW(IBITMAP,NG,XTITLE,YTITLE,LDATE,LINI)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NG
 INTEGER,INTENT(INOUT) :: IBITMAP
 LOGICAL,INTENT(IN) :: LDATE,LINI
 CHARACTER(LEN=*),INTENT(IN) :: XTITLE,YTITLE
 INTEGER :: IW,IH,I,J,NP,IDD,IDF
 REAL :: DX,DY,XMIN,YMIN,XMAX,YMAX,X1,X2,XW1,XW2
 TYPE(AXESOBJ) :: AXES

 IDD=ID_DSCENTOOL_FIGURE
 IDF=IDF_PICTURE1
 
 !## childwindow - size for the bitmap
 CALL WDIALOGSELECT(IDD)
 CALL IGRSELECT(DRAWFIELD,IDF)
 IW=WINFODRAWABLE(DRAWABLEWIDTH)
 IH=WINFODRAWABLE(DRAWABLEHEIGHT)
 IF(IBITMAP.NE.0)CALL WBITMAPDESTROY(IBITMAP)
 CALL WBITMAPCREATE(IBITMAP,IW,IH)

 !## select proper bitmap
 CALL IGRSELECT(DRAWBITMAP,IBITMAP)
 !## change plotmode
 CALL IGRPLOTMODE(MODECOPY)

 !## plot axis and correct xmin,ymin,xmax,ymax for axes
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRAREA(0.0,0.0,1.0,1.0)

 IF(LINI)THEN
  IF(GRAPHDIM%IFIXX.EQ.0)THEN
   AXES%XMIN= HUGE(1.0) 
   AXES%XMAX=-HUGE(1.0) 
   DO I=1,SIZE(GRAPH,1)
    NP=GRAPH(I,NG)%NP
    AXES%XMIN=MIN(AXES%XMIN,MINVAL(GRAPH(I,NG)%RX(1:NP)))
    AXES%XMAX=MAX(AXES%XMAX,MAXVAL(GRAPH(I,NG)%RX(1:NP)))
   ENDDO
   IF(AXES%XMIN.EQ.AXES%XMAX)THEN
    AXES%XMIN=AXES%XMIN-1.0
    AXES%XMAX=AXES%XMAX+1.0
   ENDIF
  ELSE
   AXES%XMIN=GRAPHDIM%XMIN
   AXES%XMAX=GRAPHDIM%XMAX
  ENDIF
  IF(GRAPHDIM%IFIXY.EQ.0)THEN
   AXES%YMIN= HUGE(1.0) 
   AXES%YMAX=-HUGE(1.0) 
   DO I=1,SIZE(GRAPH,1)
    NP=GRAPH(I,NG)%NP
    AXES%YMIN=MIN(AXES%YMIN,MINVAL(GRAPH(I,NG)%RY(1:NP)))
    AXES%YMAX=MAX(AXES%YMAX,MAXVAL(GRAPH(I,NG)%RY(1:NP)))
   ENDDO
   IF(AXES%YMIN.EQ.AXES%YMAX)THEN
    AXES%YMIN=AXES%YMIN-1.0
    AXES%YMAX=AXES%YMAX+1.0
   ENDIF
  ELSE
   AXES%YMIN=GRAPHDIM%YMIN
   AXES%YMAX=GRAPHDIM%YMAX
  ENDIF
  PGXMIN=AXES%XMIN
  PGXMAX=AXES%XMAX
  PGYMIN=AXES%YMIN
  PGYMAX=AXES%YMAX
 ELSE
  AXES%XMIN=PGXMIN
  AXES%XMAX=PGXMAX
  AXES%YMIN=PGYMIN
  AXES%YMAX=PGYMAX
 ENDIF
 
 DX=(AXES%XMAX-AXES%XMIN)/10.0
 AXES%XMIN=AXES%XMIN-DX
 AXES%XMAX=AXES%XMAX+DX
 DY=(AXES%YMAX-AXES%YMIN)/10.0
 AXES%YMIN=AXES%YMIN-DY
 AXES%YMAX=AXES%YMAX+DY

 IF(GRAPHDIM%IFIXY.EQ.0)THEN
  AXES%IFIXX =0
  AXES%XINT  =10
 ELSE
  AXES%IFIXX =1
  AXES%XINT  =GRAPHDIM%XINT
  ALLOCATE(AXES%XTXT(INT(AXES%XINT)))
  AXES%XTXT=GRAPHDIM%XTXT
 ENDIF

 IF(GRAPHDIM%IFIXY.EQ.0)THEN
  AXES%IFIXY =0
  AXES%YINT  =10
 ELSE
  AXES%IFIXY =1
  AXES%YINT  =GRAPHDIM%YINT
  ALLOCATE(AXES%YTXT(INT(AXES%YINT)))
  AXES%YTXT=GRAPHDIM%YTXT
 ENDIF

 IF(GRAPHDIM%IFIXY2.EQ.0)THEN
  AXES%IFIXY2=0
  AXES%Y2INT=10
 ELSE
  AXES%IFIXY2=1
  AXES%Y2INT =GRAPHDIM%Y2INT
  ALLOCATE(AXES%Y2TXT(INT(AXES%Y2INT)))
  AXES%Y2TXT=GRAPHDIM%Y2TXT
 ENDIF

 AXES%IAXES=(/1,0/)       !## left/bottom axes only
 AXES%ICLRRASTER=WRGB(220,220,220)
 AXES%XFACTOR=1.0
 AXES%YFACTOR=1.0
 AXES%DXAXESL=40.0
 AXES%DYAXESB=20.0
 AXES%DYAXEST=75.0
 AXES%DXAXESR=150.0
 AXES%TFONT=FFHELVETICA   !## text-font
 AXES%YTITLE=YTITLE 
 AXES%XTITLE=XTITLE 
 AXES%LDATE=LDATE
 AXES%ICLRBACKGROUND=WRGB(123,152,168)

 !## plot axes and set units
 CALL GRAPH_PLOTAXES(AXES,1)

 DO I=SIZE(GRAPH,1),1,-1

  !## draw histogram
  IF(GRAPH(I,NG)%GTYPE.EQ.1)THEN
   DO J=1,GRAPH(I,NG)%NP

    XW1=0.0; XW2=0.0
    IF(J.LT.GRAPH(I,NG)%NP)THEN
     XW2=(GRAPH(I,NG)%RX(J+1)-GRAPH(I,NG)%RX(J))/2.0
    ELSEIF(J.GT.1)THEN
     XW1=(GRAPH(I,NG)%RX(J)-GRAPH(I,NG)%RX(J-1))/2.0
    ELSE
     DX=(AXES%XMAX-AXES%XMIN)/4.0; XW1=DX; XW2=DX
    ENDIF
     
    X1=GRAPH(I,NG)%RX(J)-XW1
    X2=GRAPH(I,NG)%RX(J)+XW2

    CALL IGRFILLPATTERN(SOLID)

    IF(MIN(0.0,GRAPH(I,NG)%RY(J)).NE.MAX(0.0,GRAPH(I,NG)%RY(J)))THEN    
     CALL IGRCOLOURN(GRAPH(I,NG)%ICLR)
     CALL IGRRECTANGLE(X1,MIN(0.0,GRAPH(I,NG)%RY(J)),X2,MAX(0.0,GRAPH(I,NG)%RY(J)))
     CALL IGRFILLPATTERN(OUTLINE)
     CALL IGRCOLOURN(WRGB(0,0,0))
     CALL IGRRECTANGLE(X1,MIN(0.0,GRAPH(I,NG)%RY(J)),X2,MAX(0.0,GRAPH(I,NG)%RY(J)))
    ELSE
     CALL IGRJOIN(X1,MIN(0.0,GRAPH(I,NG)%RY(J)),X2,MAX(0.0,GRAPH(I,NG)%RY(J)))
    ENDIF
   END DO

  !## draw lines
  ELSEIF(GRAPH(I,NG)%GTYPE.EQ.2)THEN

   CALL IGRLINETYPE(SOLIDLINE)
   CALL IGRCOLOURN(GRAPH(I,NG)%ICLR)
   DO J=1,GRAPH(I,NG)%NP-1
    CALL IGRJOIN(GRAPH(I,NG)%RX(J)  ,GRAPH(I,NG)%RY(J),&
                 GRAPH(I,NG)%RX(J+1),GRAPH(I,NG)%RY(J+1))
   END DO

  !## filled per pare (stackhistogram)
  ELSEIF(GRAPH(I,NG)%GTYPE.EQ.3)THEN

   CALL IGRFILLPATTERN(SOLID)
   CALL IGRCOLOURN(GRAPH(I,NG)%ICLR) 
   DO J=2,GRAPH(I,NG)%NP-1,2
    CALL IGRRECTANGLE(GRAPH(I,NG)%RX(J),GRAPH(I,NG)%RY(J),GRAPH(I,NG)%RX(J+1),GRAPH(I,NG)%RY(J+1))
   END DO
  ENDIF

 ENDDO

 CALL WGRTEXTORIENTATION(ALIGNLEFT,0.0,DIRHORIZ)
 DX=AXES%CHW*(AXES%XMAX-AXES%XMIN)
 DY=AXES%CHH*(AXES%YMAX-AXES%YMIN)
 
 XMIN=AXES%XMIN+DX
 XMAX=XMIN+DX*2.0
 YMAX=AXES%YMAX-DY
 !## ipattern:0=solid,1=line,2=points,3=legend
 DO I=1,SIZE(GRAPH,1)
  YMIN=YMAX-DY
  !## plot axes-text
  CALL IGRCOLOURN(WRGB(0,0,0))
  CALL WGRTEXTSTRING(XMAX+DX,(YMAX+YMIN)/2.0,' '//TRIM(GRAPH(I,NG)%LEGTXT))
  SELECT CASE (GRAPH(I,NG)%GTYPE)
   CASE (2)   !## lines
    CALL UTL_DRAWLEGENDBOX(XMIN,YMIN,XMAX,YMAX,GRAPH(I,NG)%ICLR,1,SOLIDLINE,1)
   CASE (1,3) !## filled                                                     
    CALL UTL_DRAWLEGENDBOX(XMIN,YMIN,XMAX,YMAX,GRAPH(I,NG)%ICLR,1,SOLIDLINE,0)
  END SELECT
  YMAX=YMIN
 ENDDO
 CALL IGRLINEWIDTH(2)
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRJOIN(AXES%XMIN,AXES%YMIN,AXES%XMAX,AXES%YMIN)
 CALL IGRLINEWIDTH(1)

 CALL WDIALOGSELECT(IDD)
 CALL IGRSELECT(DRAWFIELD,IDF)
 CALL WBITMAPPUT(IBITMAP,0,1)
  
 IF(ASSOCIATED(AXES%XTXT)) DEALLOCATE(AXES%XTXT)
 IF(ASSOCIATED(AXES%YTXT)) DEALLOCATE(AXES%YTXT)
 IF(ASSOCIATED(AXES%Y2TXT))DEALLOCATE(AXES%Y2TXT)

 END SUBROUTINE GRAPH_DRAW

 !###====================================================================
 SUBROUTINE GRAPH_ZOOM(IDD,IDZ,IBITMAP,LDATE,NG,XTXT,YTXT)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NG
 INTEGER,INTENT(OUT) :: IBITMAP
 CHARACTER(LEN=*),INTENT(IN) :: XTXT,YTXT
 INTEGER,INTENT(IN) :: IDZ,IDD
 LOGICAL,INTENT(IN) :: LDATE
 REAL,PARAMETER :: FZIN =0.75
 REAL,PARAMETER :: FZOUT=1.5
 TYPE(WIN_MESSAGE) :: MESSAGE
 INTEGER :: ITYPE,IDOWN,IDCURSOR,I
 REAL :: FZ,XC1,YC1,XC2,YC2,XC3,YC3,DX,DY
 LOGICAL :: LEX
 INTEGER,DIMENSION(6) :: ID
 DATA ID/ID_ZOOMIN,ID_ZOOMOUT,ID_ZOOMFULL,ID_ZOOMBOX,ID_MOVE,ID_COPY/

 IF(IDZ.EQ.ID_ZOOMIN)THEN
  FZ=FZIN
  IDCURSOR=ID_CURSORPOINTPLUS
 ELSEIF(IDZ.EQ.ID_ZOOMOUT)THEN
  FZ=FZOUT
  IDCURSOR=ID_CURSORPOINTMIN
 ELSEIF(IDZ.EQ.ID_ZOOMBOX)THEN
  IDCURSOR=ID_CURSORZOOMRECTANGLE
 ELSEIF(IDZ.EQ.ID_MOVE)THEN
  IDCURSOR=ID_CURSORHAND
 ELSE
  CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.TRUE.)
  RETURN
 ENDIF

 DO I=1,SIZE(ID); IF(ID(I).NE.IDZ)CALL WDIALOGFIELDSTATE(ID(I),0); END DO

 CALL WCURSORSHAPE(IDCURSOR)

 IDOWN=0
 LEX  =.FALSE.
 XC1  =0.0
 YC1  =0.0
 DO

  CALL WMESSAGE(ITYPE,MESSAGE)

  IF(MESSAGE%WIN.EQ.IDD)THEN

   SELECT CASE(ITYPE)

    CASE(MOUSEMOVE)
     CALL GRAPH_MOUSE(MESSAGE,LDATE,1)

     XC2=MESSAGE%GX
     YC2=MESSAGE%GY

     IF(IDZ.EQ.ID_MOVE)THEN
      IF(IDOWN.GT.0)THEN
       DX=XC1-XC2
       DY=YC1-YC2
       PGXMAX=PGXMAX+DX
       PGXMIN=PGXMIN+DX
       PGYMAX=PGYMAX+DY
       PGYMIN=PGYMIN+DY
       CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.FALSE.)
      ENDIF
     ELSEIF(IDZ.EQ.ID_ZOOMBOX)THEN

      IF(IDOWN.GT.0)THEN

       CALL IGRPLOTMODE(MODEXOR)
       CALL IGRCOLOURN(WRGB(255,255,255))
       CALL IGRFILLPATTERN(OUTLINE)
       CALL IGRLINETYPE(DASHED)

       IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC3,YC3)
       LEX=.FALSE.

       IF(IDOWN.EQ.1)THEN
        IF(XC1.NE.XC2.AND.YC1.NE.YC2)LEX=.TRUE.
        IF(LEX)CALL IGRRECTANGLE(XC1,YC1,XC2,YC2)
       ENDIF

      ENDIF
     ENDIF

     XC3=XC2
     YC3=YC2

    CASE (RESIZE,EXPOSE)
     !## refresh graph
     CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.FALSE.)

    CASE (MOUSEBUTUP)
     IF(IDZ.EQ.ID_MOVE)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        CALL WCURSORSHAPE(ID_CURSORHAND)
        IDOWN=0
      END SELECT
     ENDIF

    CASE (MOUSEBUTDOWN)

     IF(IDZ.EQ.ID_ZOOMIN.OR.IDZ.EQ.ID_ZOOMOUT)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        XC2 = XC3
        YC2 = YC3
        DX  = PGXMAX-PGXMIN
        DY  = PGYMAX-PGYMIN
        PGXMAX= XC2+0.5*DX*FZ
        PGXMIN= XC2-0.5*DX*FZ
        PGYMIN= YC2-0.5*DY*FZ
        PGYMAX= YC2+0.5*DY*FZ
        CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.FALSE.)
       CASE (3)
        EXIT
      END SELECT
     ELSEIF(IDZ.EQ.ID_MOVE)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        IF(IDOWN.EQ.0)THEN
         XC1  =XC2
         YC1  =YC2
         IDOWN=1
         CALL WCURSORSHAPE(ID_CURSORHANDGREP)
        ENDIF
       CASE (3)
        EXIT
      END SELECT
     ELSEIF(IDZ.EQ.ID_ZOOMBOX)THEN
      SELECT CASE (MESSAGE%VALUE1)
       CASE (1)
        IF(IDOWN.EQ.0)THEN
         XC1  =XC2
         YC1  =YC2
         IDOWN=1
        ELSE
         PGXMAX=MAX(XC1,XC3)
         PGXMIN=MIN(XC1,XC3)
         PGYMAX=MAX(YC1,YC3)
         PGYMIN=MIN(YC1,YC3)
         EXIT
        ENDIF
      END SELECT
     ENDIF

   END SELECT

  ENDIF
 ENDDO

 CALL WCURSORSHAPE(CURARROW)

 IF(IDZ.EQ.ID_ZOOMBOX)THEN
  CALL IGRPLOTMODE(MODECOPY)
  CALL IGRLINETYPE(SOLIDLINE)
 ENDIF

 CALL GRAPH_DRAW(IBITMAP,NG,XTXT,YTXT,LDATE,.FALSE.)

 DO I=1,SIZE(ID); IF(ID(I).NE.IDZ)CALL WDIALOGFIELDSTATE(ID(I),1); END DO

 END SUBROUTINE GRAPH_ZOOM

 !###====================================================================
 SUBROUTINE GRAPH_ALLOCATE(NI,NJ)
 !###====================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NI,NJ
 INTEGER :: I,J

 !## ni=number of lines 
 !## nj=number of seperated graphs
 ALLOCATE(GRAPH(NI,NJ))
 DO I=1,NI
  DO J=1,NJ
   NULLIFY(GRAPH(I,J)%RX,GRAPH(I,J)%RY)
  ENDDO
 ENDDO
 ALLOCATE(GRAPHNAMES(NJ))
 
 END SUBROUTINE GRAPH_ALLOCATE

 !###====================================================================
 SUBROUTINE GRAPH_DEALLOCATE()
 !###====================================================================
 IMPLICIT NONE
 INTEGER :: I,J

 IF(ALLOCATED(GRAPH))THEN
  DO I=1,SIZE(GRAPH,1)
   DO J=1,SIZE(GRAPH,2)
    IF(ASSOCIATED(GRAPH(I,J)%RX))   DEALLOCATE(GRAPH(I,J)%RX)
    IF(ASSOCIATED(GRAPH(I,J)%RY))   DEALLOCATE(GRAPH(I,J)%RY)
   END DO
  END DO
  DEALLOCATE(GRAPH)
 ENDIF
 IF(ALLOCATED(GRAPHNAMES))DEALLOCATE(GRAPHNAMES)
 IF(ASSOCIATED(GRAPHDIM%XTXT)) DEALLOCATE(GRAPHDIM%XTXT)
 IF(ASSOCIATED(GRAPHDIM%YTXT)) DEALLOCATE(GRAPHDIM%YTXT)
 IF(ASSOCIATED(GRAPHDIM%Y2TXT))DEALLOCATE(GRAPHDIM%Y2TXT)

 END SUBROUTINE GRAPH_DEALLOCATE

 !###======================================================================
 SUBROUTINE GRAPH_PLOTAXES(AXES,IWINID)
 !###======================================================================
 IMPLICIT NONE
 TYPE(AXESOBJ),INTENT(INOUT) :: AXES
 INTEGER,INTENT(IN) :: IWINID
 CHARACTER(LEN=20) :: CDATE
 INTEGER :: I,J,IWD,IHD,IWS,IHS,NL
 REAL :: DX,DY,DXTIC,DYTIC,X,Y,DMX1,RATIO,DMX2,DMY1,DMY2,TWIDTH,THEIGHT,&
         XASMIN,XASMAX,YASMIN,YASMAX,XASINT,YASINT,SX_RATIO,SY_RATIO,   &
         Y2ASMIN,Y2ASMAX,Y2ASINT,D2Y,INT_BU
 REAL :: XJDCOR !## correction for x in case of large numbers caused by julian-date

 !## drawable settings
 IWD=WINFODRAWABLE(DRAWABLEWIDTH)
 IHD=WINFODRAWABLE(DRAWABLEHEIGHT)

 !## screen setting
 IWS=WINFOSCREEN(SCREENWIDTH)
 IHS=WINFOSCREEN(SCREENHEIGHT)

 SX_RATIO=REAL(IWS)/REAL(IWD)
 SY_RATIO=REAL(IHS)/REAL(IHD)

 !## determine axes min/max values
 XASMIN=AXES%XMIN
 XASMAX=AXES%XMAX

 XJDCOR=0.0
 !## correct for large julian-date numbers
 IF(AXES%LDATE)THEN
  XJDCOR=-XASMIN
  XASMIN= XASMIN+XJDCOR
  XASMAX= XASMAX+XJDCOR
 ENDIF
 
 YASMIN=AXES%YMIN
 YASMAX=AXES%YMAX

 !## second y-axes
 IF(AXES%IAXES(2).EQ.1)THEN
  Y2ASMIN=AXES%Y2MIN
  Y2ASMAX=AXES%Y2MAX
  Y2ASINT=AXES%Y2INT
 ENDIF
 
 CALL UTL_GETAXESCALES(XASMIN,YASMIN,XASMAX,YASMAX)

 XASINT=AXES%XINT
 YASINT=AXES%YINT

 IF(AXES%IFIXX.EQ.0)THEN

  XASINT=SXVALUE(2)-SXVALUE(1)
  XASMIN=SXVALUE(1)

 ENDIF

 IF(AXES%IFIXY.EQ.0)THEN

  YASINT=SYVALUE(2)-SYVALUE(1) 
  YASMIN=SYVALUE(1)

 ENDIF

 !## second y-axes
 IF(AXES%IAXES(2).EQ.1)THEN
  IF(AXES%IFIXY2.EQ.0)THEN

   CALL UTL_GETAXESCALES(XASMIN,Y2ASMIN,XASMAX,Y2ASMAX)

   Y2ASINT=SYVALUE(2)-SYVALUE(1)
   Y2ASMIN=SYVALUE(1)

  ENDIF
 ENDIF

 DX=AXES%XMAX-AXES%XMIN
 DY=AXES%YMAX-AXES%YMIN

 IF(AXES%IAXES(2).EQ.1)THEN
  D2Y =AXES%Y2MAX-AXES%Y2MIN
  DMY1=D2Y*(SY_RATIO*(1.0/AXES%DYAXESB))
  DMY2=D2Y*(SY_RATIO*(1.0/AXES%DYAXEST))
  GRAPHUNITS(5,IWINID)=AXES%Y2MIN-DMY1
  GRAPHUNITS(6,IWINID)=AXES%Y2MAX+DMY2
 ENDIF

 !## compute marge - depends on size - textsize depend on these distances!!!
 DMX1=DX*(SX_RATIO*(1.0/AXES%DXAXESL))
 DMX2=DX*(SX_RATIO*(1.0/AXES%DXAXESR))
 DMY1=DY*(SY_RATIO*(1.0/AXES%DYAXESB))
 DMY2=DY*(SY_RATIO*(1.0/AXES%DYAXEST))

 GRAPHUNITS(1,IWINID)=AXES%XMIN-DMX1+XJDCOR
 GRAPHUNITS(3,IWINID)=AXES%XMAX+DMX2+XJDCOR
 GRAPHUNITS(2,IWINID)=AXES%YMIN-DMY1
 GRAPHUNITS(4,IWINID)=AXES%YMAX+DMY2

 !## tic length
 DXTIC=DMX1/8.0
 DYTIC=DMY1/8.0

 NL=1; IF(AXES%XTITLE.NE.''.OR.AXES%YTITLE.NE.'')NL=NL+1

 !## entire bitmap
 CALL IGRAREACLEAR()
 CALL IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))
 RATIO=(GRAPHUNITS(3,IWINID)-GRAPHUNITS(1,IWINID))/(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID))
 
 !## compute proper textsize as ratio of dy
 CALL UTL_SETTEXTSIZE(TWIDTH,THEIGHT,((AXES%YMIN-GRAPHUNITS(2,IWINID))/(NL+0.5))/(GRAPHUNITS(4,IWINID)-GRAPHUNITS(2,IWINID)))
 CALL WGRTEXTFONT(AXES%TFONT,WIDTH=TWIDTH,HEIGHT=THEIGHT,ISTYLE=0)
 AXES%CHH=THEIGHT
 AXES%CHW=TWIDTH

 !## fill deltares blue colour
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRCOLOURN(AXES%ICLRBACKGROUND)
 CALL IGRRECTANGLE(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID))

 !## white drawing area
 CALL IGRFILLPATTERN(SOLID)
 CALL IGRCOLOURN(WRGB(255,255,255))
 CALL IGRRECTANGLE(AXES%XMIN+XJDCOR,AXES%YMIN,AXES%XMAX+XJDCOR,AXES%YMAX)

 CALL IGRLINEWIDTH(1)
 CALL IGRLINETYPE(DASHED)

 !##=== vertical axes ===

 CALL WGRTEXTORIENTATION(ALIGNCENTRE,90.0,DIRHORIZ,ALIGNCENTRE)

 I=0; INT_BU=YASINT
 DO
  !## get length of vertical axes
  I=I+1; Y=YASMIN-YASINT; DY=0.0
  J=0; DO
   Y=Y+YASINT
   IF(Y.GE.AXES%YMAX)EXIT
   IF(Y.GT.AXES%YMIN)THEN
    J=J+1
    IF(ASSOCIATED(AXES%YTXT))THEN
     DY=DY+1.0*WGRTEXTLENGTH(TRIM(AXES%YTXT(J)))*WINFOGRREAL(GRAPHICSCHHEIGHT)
    ELSE
     WRITE(CDATE,UTL_GETFORMAT(Y*AXES%YFACTOR)) Y*AXES%YFACTOR; CDATE=ADJUSTL(CDATE)
     DY=DY+1.0*WGRTEXTLENGTH(TRIM(CDATE))*WINFOGRREAL(GRAPHICSCHHEIGHT)
    ENDIF
   ENDIF
  END DO

  IF(.NOT.ASSOCIATED(AXES%YTXT))THEN
   !## increase interval in case axes is too big
   IF(DY.LE.(AXES%YMAX-AXES%YMIN))EXIT
   YASINT=INT_BU*REAL(I)*0.5
  ELSE
   EXIT
  ENDIF
  
 ENDDO
 
 !## vertical axes - vertical plotting
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRCOLOURN(WRGB(256,256,256))
 Y=YASMIN-YASINT
 DO
  Y=Y+YASINT/4.0
  IF(Y.GT.AXES%YMAX)EXIT
  IF(Y.GT.AXES%YMIN)THEN
   CALL IGRJOIN((AXES%XMIN+XJDCOR)-DXTIC/2.0,Y,(AXES%XMIN+XJDCOR)+DXTIC/2.0,Y)
   CALL IGRJOIN((AXES%XMAX+XJDCOR)-DXTIC/2.0,Y,(AXES%XMAX+XJDCOR)+DXTIC/2.0,Y)
  ENDIF
 ENDDO
 
 !## vertical axes - vertical plotting
 Y =YASMIN-YASINT
 DX=DMX1/REAL(NL+1)
 J=0; DO
  Y=Y+YASINT
  IF(Y.GT.AXES%YMAX)EXIT
  IF(Y.GT.AXES%YMIN)THEN
   J=J+1
   CALL IGRLINETYPE(DASHED)
   CALL IGRCOLOURN(AXES%ICLRRASTER)
   CALL IGRJOIN(AXES%XMIN+XJDCOR,Y,AXES%XMAX+XJDCOR,Y) !## raster
   CALL IGRLINETYPE(SOLIDLINE)
   CALL IGRCOLOURN(WRGB(256,256,256))
   CALL IGRJOIN((AXES%XMIN+XJDCOR)-DXTIC,Y,(AXES%XMIN+XJDCOR)+DXTIC,Y)
   !## plot right axes in case no secondary y-axes is plotted
   IF(AXES%IAXES(2).EQ.0)CALL IGRJOIN((AXES%XMAX+XJDCOR)-DXTIC,Y,(AXES%XMAX+XJDCOR)+DXTIC,Y)
   IF(ASSOCIATED(AXES%YTXT))THEN
    CALL WGRTEXTSTRING((AXES%XMIN+XJDCOR)-DX,Y,TRIM(AXES%YTXT(J)))
   ELSE
    !## plot axes-text
    CALL WGRTEXTREAL((AXES%XMIN+XJDCOR)-DX,Y,Y*AXES%YFACTOR,TRIM(UTL_GETFORMAT(Y*AXES%YFACTOR)))
   ENDIF
  ENDIF
 END DO

 !## vertical axes
 IF(LEN_TRIM(AXES%YTITLE).NE.0)THEN
  X=(AXES%XMIN+XJDCOR)-(2.0*DX)
  Y=(AXES%YMAX+AXES%YMIN)/2.0
  IF(AXES%YFACTOR.NE.1.0)THEN
   CALL WGRTEXTSTRING(X,Y,TRIM(AXES%YTITLE)//' (x '//TRIM(RTOS(1.0/AXES%YFACTOR,'F',2))//')')
  ELSE
   CALL WGRTEXTSTRING(X,Y,TRIM(AXES%YTITLE))
  ENDIF
 ENDIF

 !## second vertical axes - vertical plotting
 IF(AXES%IAXES(2).EQ.1)THEN

  CALL IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(5,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(6,IWINID))

  I=0; INT_BU=Y2ASINT
  DO
   !## get length of vertical axes
   I=I+1; Y=Y2ASMIN-Y2ASINT; DY=0.0
   J=0; DO
    Y=Y+Y2ASINT
    IF(Y.GE.AXES%Y2MAX)EXIT
    IF(Y.GT.AXES%Y2MIN)THEN
     J=J+1
     IF(ASSOCIATED(AXES%Y2TXT))THEN
      DY=DY+1.0*WGRTEXTLENGTH(TRIM(AXES%Y2TXT(J)))*WINFOGRREAL(GRAPHICSCHHEIGHT)
     ELSE
      WRITE(CDATE,UTL_GETFORMAT(Y)) Y; CDATE=ADJUSTL(CDATE)
      DY=DY+1.0*WGRTEXTLENGTH(TRIM(CDATE))*WINFOGRREAL(GRAPHICSCHHEIGHT)
     ENDIF
    ENDIF
   END DO

   IF(.NOT.ASSOCIATED(AXES%Y2TXT))THEN
    !## increase interval in case axes is too big
    IF(DY.LT.(AXES%Y2MAX-AXES%Y2MIN))EXIT
    Y2ASINT=INT_BU*REAL(I)*0.5
   ELSE
    EXIT
   ENDIF
   
  ENDDO

  CALL IGRLINETYPE(SOLIDLINE)
  CALL IGRCOLOURN(WRGB(256,0,0))
  Y=Y2ASMIN-Y2ASINT
  DO
   Y=Y+Y2ASINT
   IF(Y.GT.AXES%Y2MAX)EXIT
   IF(Y.GT.AXES%Y2MIN)THEN
    CALL IGRJOIN(AXES%XMAX+DXTIC/2.0+XJDCOR,Y,AXES%XMAX-DXTIC/2.0+XJDCOR,Y)
    CALL IGRJOIN(AXES%XMAX+DXTIC/2.0+XJDCOR,Y,AXES%XMAX-DXTIC/2.0+XJDCOR,Y)
   ENDIF
  END DO

  Y =Y2ASMIN-Y2ASINT
  DX=DMX1/REAL(NL+1)
  J=0; DO
   Y=Y+Y2ASINT
   IF(Y.GT.AXES%Y2MAX)EXIT
   IF(Y.GT.AXES%Y2MIN)THEN
    J=J+1
    CALL IGRLINETYPE(DOTTED)
    CALL IGRCOLOURN(AXES%ICLRRASTER)
    CALL IGRJOIN(AXES%XMIN+XJDCOR,Y,AXES%XMAX+XJDCOR,Y)
    CALL IGRLINETYPE(SOLIDLINE)
    CALL IGRCOLOURN(WRGB(256,0,0))
    CALL IGRJOIN(AXES%XMAX+DXTIC+XJDCOR,Y,AXES%XMAX-DXTIC+XJDCOR,Y)
    !## plot axes-text
    IF(ASSOCIATED(AXES%Y2TXT))THEN
     CALL WGRTEXTSTRING(AXES%XMAX+DX+XJDCOR,Y,TRIM(AXES%Y2TXT(J)))
    ELSE
     CALL WGRTEXTREAL(AXES%XMAX+DX+XJDCOR,Y,Y,TRIM(UTL_GETFORMAT(Y)))
    ENDIF
   ENDIF
  END DO

  !## vertical axes
  IF(LEN_TRIM(AXES%YTITLE).NE.0)THEN
   X= AXES%XMAX+XJDCOR+(2.0*DX)
   Y=(AXES%Y2MAX+AXES%Y2MIN)/2.0
   CALL WGRTEXTSTRING(X,Y,TRIM(AXES%Y2TITLE))
  ENDIF

  CALL IGRUNITS(GRAPHUNITS(1,1),GRAPHUNITS(2,1),GRAPHUNITS(3,1),GRAPHUNITS(4,1))

 ENDIF

 !##=== horizontal axes ===

 CALL WGRTEXTORIENTATION(ALIGNCENTRE,0.0,DIRHORIZ,ALIGNCENTRE)

 !## now I know the textsize, determine number of classes
 I=0; INT_BU=XASINT
 DO
  !## get length of vertical axes
  I=I+1; X=XASMIN-XASINT; DX=0.0

  J=0; DO
   X=X+XASINT
   IF(X.GT.AXES%XMAX+XJDCOR)EXIT
   IF(X.GT.AXES%XMIN+XJDCOR)THEN
    J=J+1
    IF(ASSOCIATED(AXES%XTXT))THEN
     DX=DX+1.2*WGRTEXTLENGTH(TRIM(AXES%XTXT(J)))*WINFOGRREAL(GRAPHICSCHWIDTH)
    ELSE
     IF(AXES%LDATE)THEN
      CDATE=JDATETOFDATE(X-XJDCOR,AXES%XOFFSET)
      DX=DX+1.2*WGRTEXTLENGTH(TRIM(CDATE))*WINFOGRREAL(GRAPHICSCHWIDTH)
     ELSE
      WRITE(CDATE,UTL_GETFORMAT(X)) X; CDATE=ADJUSTL(CDATE)
      DX=DX+1.2*WGRTEXTLENGTH(TRIM(CDATE))*WINFOGRREAL(GRAPHICSCHWIDTH)
     ENDIF
    ENDIF
   ENDIF
  END DO

  IF(.NOT.ASSOCIATED(AXES%XTXT))THEN
   !## increase interval in case axes is too big
   IF(DX.LE.(AXES%XMAX-AXES%XMIN))EXIT
   XASINT=INT_BU*REAL(I)*0.5
  ELSE
   EXIT
  ENDIF

 ENDDO

 !## horizontal axes - horizontal plotting

 !## minor axes
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRCOLOURN(WRGB(256,256,256))
 X=XASMIN-XASINT
 DO
  X=X+XASINT/4.0
  IF(X.GT.AXES%XMAX+XJDCOR)EXIT
  IF(X.GT.AXES%XMIN+XJDCOR)THEN
   CALL IGRJOIN(X,AXES%YMIN-DYTIC/2.0,X,AXES%YMIN+DYTIC/2.0)
   CALL IGRJOIN(X,AXES%YMAX-DYTIC/2.0,X,AXES%YMAX+DYTIC/2.0)
  ENDIF
 ENDDO
 
 X = XASMIN-XASINT
 DY= DMY1/REAL(NL+1)
 J=0; DO 
  X=X+XASINT
  IF(X.GT.AXES%XMAX+XJDCOR)EXIT
  IF(X.GT.AXES%XMIN+XJDCOR)THEN
   J=J+1
   CALL IGRLINETYPE(DASHED)
   CALL IGRCOLOURN(AXES%ICLRRASTER)
   CALL IGRJOIN(X,AXES%YMIN,X,AXES%YMAX)
   CALL IGRLINETYPE(SOLIDLINE)
   CALL IGRCOLOURN(WRGB(256,256,256))
   CALL IGRJOIN(X,AXES%YMIN-DYTIC,X,AXES%YMIN+DYTIC)
   CALL IGRJOIN(X,AXES%YMAX-DYTIC,X,AXES%YMAX+DYTIC)
   IF(ASSOCIATED(AXES%XTXT))THEN
    CALL WGRTEXTSTRING(X,AXES%YMIN-DY,TRIM(AXES%XTXT(J)))
   ELSE
    IF(AXES%LDATE)THEN
     CDATE=JDATETOFDATE(X-XJDCOR,AXES%XOFFSET)
     CALL WGRTEXTSTRING(X,AXES%YMIN-DY,TRIM(CDATE))
    ELSE
     CALL WGRTEXTREAL(X,AXES%YMIN-DY,X*AXES%XFACTOR,TRIM(UTL_GETFORMAT((X+XJDCOR)*AXES%XFACTOR)))
    ENDIF
   ENDIF
  ENDIF
 END DO

 !## horizontal axes
 IF(LEN_TRIM(AXES%XTITLE).NE.0)THEN
  X=(AXES%XMAX+AXES%XMIN)/2.0
  Y= AXES%YMIN-2.0*DY
  IF(AXES%XFACTOR.NE.1.0)THEN
   CALL WGRTEXTSTRING(X+XJDCOR,Y,TRIM(AXES%XTITLE)//' (x '//TRIM(RTOS(1.0/AXES%XFACTOR,'F',2))//')')
  ELSE
   CALL WGRTEXTSTRING(X+XJDCOR,Y,TRIM(AXES%XTITLE))
  ENDIF
 ENDIF

 CALL IGRCOLOURN(WRGB(256,256,256))
 CALL IGRLINETYPE(SOLIDLINE)

 !## back-translation
 GRAPHUNITS(1,IWINID)=GRAPHUNITS(1,IWINID)-XJDCOR
 GRAPHUNITS(3,IWINID)=GRAPHUNITS(3,IWINID)-XJDCOR
 CALL IGRUNITS(GRAPHUNITS(1,IWINID),GRAPHUNITS(2,IWINID),GRAPHUNITS(3,IWINID),GRAPHUNITS(4,IWINID)) 
 
 !## black axes
 CALL IGRLINETYPE(SOLIDLINE)
 CALL IGRFILLPATTERN(OUTLINE)
 CALL IGRLINEWIDTH(2)
 CALL IGRCOLOURN(WRGB(0,0,0))
 CALL IGRRECTANGLE(AXES%XMIN,AXES%YMIN,AXES%XMAX,AXES%YMAX)
 CALL IGRLINEWIDTH(1)

 CALL GRAPH_PLOTAXES_VIEW(AXES,IWINID)

 END SUBROUTINE GRAPH_PLOTAXES
  
 !###======================================================================
 SUBROUTINE GRAPH_PLOTAXES_VIEW(AXES,IWINID)
 !###======================================================================
 IMPLICIT NONE
 TYPE(AXESOBJ),INTENT(INOUT) :: AXES
 INTEGER,INTENT(IN) :: IWINID
 REAL :: DX,DY,DXA,DYA
 REAL :: X1V,Y1V,X2V,Y2V

 !## for now - only map active drawing area
 DX = GRAPHUNITS(3,1)-GRAPHUNITS(1,IWINID)
 DY = GRAPHUNITS(4,1)-GRAPHUNITS(2,IWINID)
 !## correct units for marge
 X1V=(AXES%XMIN-GRAPHUNITS(1,IWINID))/DX
 X2V=(AXES%XMAX-GRAPHUNITS(1,IWINID))/DX
 Y1V=(AXES%YMIN-GRAPHUNITS(2,IWINID))/DY
 Y2V=(AXES%YMAX-GRAPHUNITS(2,IWINID))/DY

 DXA=WINFOGRREAL(GRAPHICSAREAMAXX)-WINFOGRREAL(GRAPHICSAREAMINX)
 DYA=WINFOGRREAL(GRAPHICSAREAMAXY)-WINFOGRREAL(GRAPHICSAREAMINY)

 X1V=X1V*DXA
 X2V=X2V*DXA
 Y1V=Y1V*DYA
 Y2V=Y2V*DYA

 X1V=X1V+WINFOGRREAL(GRAPHICSAREAMINX)
 X2V=X2V+WINFOGRREAL(GRAPHICSAREAMINX)
 Y1V=Y1V+WINFOGRREAL(GRAPHICSAREAMINY)
 Y2V=Y2V+WINFOGRREAL(GRAPHICSAREAMINY)

 CALL IGRVIEWPORT(X1V,Y1V,X2V,Y2V)

 END SUBROUTINE GRAPH_PLOTAXES_VIEW
 
END MODULE MOD_GRAPH