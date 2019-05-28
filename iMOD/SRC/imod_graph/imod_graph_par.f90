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
 REAL(KIND=DP_KIND) :: CHH,CHW,CHS                !## characterheight,characterwidth,charactersize
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
 CHARACTER(LEN=50) :: GRAPHNAMES
 CHARACTER(LEN=16) :: XTITLE  !## x title
 CHARACTER(LEN=16) :: YTITLE  !## y title
 LOGICAL :: LDATE
 CHARACTER(LEN=16),POINTER,DIMENSION(:) :: XTXT=>NULL()  !## label xaxes
 CHARACTER(LEN=16),POINTER,DIMENSION(:) :: YTXT=>NULL()  !## label yaxes
 CHARACTER(LEN=16),POINTER,DIMENSION(:) :: Y2TXT=>NULL() !## label y2axes
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: XPOS=>NULL()  !## position for labeling
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: YPOS=>NULL()  !## position for labeling
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: Y2POS=>NULL() !## position for labeling
 INTEGER :: IFIXX=0                     !ifix - fixed x-axes
 INTEGER :: IFIXY=0                     !ifix - fixed y-axes
 INTEGER :: IFIXY2=0                    !ifix - fixed y2-axes
 INTEGER :: IGROUP                      !connected to which graph group
 REAL(KIND=DP_KIND) :: XINT,YINT,Y2INT              !xint,yint - interval
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX,Y2MIN,Y2MAX     !XMIN,YMIN,XMAX,YMAX = dimensions of current graph
 REAL(KIND=DP_KIND) :: TEXTSIZE                            !xint,yint - interval
END TYPE GRAPHDIMOBJ
TYPE(GRAPHOBJ),DIMENSION(:,:),ALLOCATABLE :: GRAPH
TYPE(GRAPHDIMOBJ),DIMENSION(:),ALLOCATABLE :: GRAPHDIM  !## number of graphs
REAL(KIND=DP_KIND),DIMENSION(:),ALLOCATABLE :: PGXMIN,PGXMAX,PGYMIN,PGYMAX

LOGICAL :: LEXPORTIT
INTEGER :: IGBITMAP

END MODULE MOD_GRAPH_PAR