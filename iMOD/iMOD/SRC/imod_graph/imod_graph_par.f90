MODULE MOD_GRAPH_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: GRAPHUNITS 
REAL(KIND=DP_KIND),DIMENSION(:,:),ALLOCATABLE :: GRAPHAREA

TYPE AXESOBJ
 CHARACTER(LEN=50) :: XTITLE,YTITLE,Y2TITLE   !xtitle,ytitle = AXES
 REAL(KIND=DP_KIND) :: XFACTOR,YFACTOR              !XFACTOR,YFACTOR = mult. factors
 LOGICAL :: LDATE                     !ldate - plot date
 INTEGER :: IFIXX                     !ifix - fixed x-axes
 INTEGER :: IFIXY                     !ifix - fixed y-axes
 INTEGER :: IFIXY2                    !ifix - fixed y2-axes
 REAL(KIND=DP_KIND) :: XINT,YINT,Y2INT              !xint,yint - interval
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: XTXT=>NULL()  !## label xaxes
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: YTXT=>NULL()  !## label yaxes
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: Y2TXT=>NULL() !## label y2axes
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XPOS=>NULL()  !## position for labeling
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: YPOS=>NULL()  !## position for labeling
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: Y2POS=>NULL() !## position for labeling
 INTEGER :: ICLRRASTER                !iclrraster - colour
 INTEGER :: XOFFSET                   !offset for julian dates
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX,Y2MIN,Y2MAX     !XMIN,YMIN,XMAX,YMAX = dimensions of current graph
 REAL(KIND=DP_KIND) :: DXAXESL,DXAXESR,DYAXESB,DYAXEST     !1.0D0/fraction of space occupied by left,right,bottom and top axes
 INTEGER :: TFONT
 INTEGER,DIMENSION(2) :: IAXES  !## left/bottom, and top/right
 REAL(KIND=DP_KIND) :: CHH,CHW                !## characterheight,characterwidth
 INTEGER :: ICLRBACKGROUND !## background color
END TYPE AXESOBJ

TYPE GRAPHOBJ
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: RX,RY !## x and y values
 INTEGER :: GTYPE !## graph type 1=solid 2=lines 3=histogram
 INTEGER :: NP  !## no. points
 CHARACTER(LEN=50) :: LEGTXT !## legend text
 CHARACTER(LEN=50) :: CTYPE  !## attribute type
 INTEGER :: ICLR
END TYPE GRAPHOBJ
TYPE GRAPHDIMOBJ
 CHARACTER(LEN=16),POINTER,DIMENSION(:) :: XTXT=>NULL()  !## label xaxes
 CHARACTER(LEN=16),POINTER,DIMENSION(:) :: YTXT=>NULL()  !## label yaxes
 CHARACTER(LEN=16),POINTER,DIMENSION(:) :: Y2TXT=>NULL() !## label y2axes
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XPOS=>NULL()  !## position for labeling
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: YPOS=>NULL()  !## position for labeling
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: Y2POS=>NULL() !## position for labeling
 INTEGER :: IFIXX=0                     !ifix - fixed x-axes
 INTEGER :: IFIXY=0                     !ifix - fixed y-axes
 INTEGER :: IFIXY2=0                    !ifix - fixed y2-axes
 REAL(KIND=DP_KIND) :: XINT,YINT,Y2INT              !xint,yint - interval
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX,Y2MIN,Y2MAX     !XMIN,YMIN,XMAX,YMAX = dimensions of current graph
END TYPE GRAPHDIMOBJ
TYPE(GRAPHOBJ),DIMENSION(:,:),ALLOCATABLE :: GRAPH
TYPE(GRAPHDIMOBJ) :: GRAPHDIM
CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: GRAPHNAMES
REAL(KIND=DP_KIND) :: PGXMIN,PGXMAX,PGYMIN,PGYMAX

END MODULE MOD_GRAPH_PAR