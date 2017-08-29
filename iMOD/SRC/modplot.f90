!!  Copyright (C) Stichting Deltares, 2005-2017.
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
MODULE MODPLOT

USE MOD_IDF_PAR, ONLY : IDFOBJ

 INTEGER,PARAMETER :: MXCLR    =255  !## stretched
 INTEGER,PARAMETER :: MXCGRAD  =7    !## max. color for gradient
 INTEGER,PARAMETER :: MXCLASS  =50   !## classed
 INTEGER,PARAMETER :: MXMPLOT  =1000 !## maximum plots in mp and mdf objects
 INTEGER,PARAMETER :: MAXIAXES =50   !## maximum number of column in associates txt-files
 INTEGER,DIMENSION(MXCGRAD) :: CLRGIVEN
 DATA CLRGIVEN/1,43,85,128,170,212,255/ !## number of the 255 stretched legend for the 7 buttons

 TYPE BITMAPOBJ
  INTEGER :: IACT,ITYPE,NCOL,NROW,NCLR,COMPR,CDEPT,IBITMAP,IW,IH
  INTEGER :: IX1,IX2,IY1,IY2
  REAL :: GX1,GX2,GY1,GY2
  CHARACTER(LEN=256) :: FNAME
 END TYPE BITMAPOBJ

 TYPE LEGENDOBJ
  INTEGER,DIMENSION(MXCLR) :: RGB
  REAL,DIMENSION(0:MXCLR)  :: CLASS
  INTEGER,DIMENSION(MXCGRAD) :: CGRAD           !## selected color for gradient
  INTEGER,DIMENSION(MXCGRAD) :: ICLRGRAD        !## selected color for gradient
  CHARACTER(LEN=50),DIMENSION(MXCLR) :: LEGTXT
  CHARACTER(LEN=150) :: HEDTXT
  INTEGER :: NCLR
 END TYPE LEGENDOBJ

 TYPE MPLOTOBJ
  LOGICAL                  :: IACT                      !## plot active
  LOGICAL                  :: ISEL                      !## plot selected
  INTEGER                  :: IPLOT                     !## plot type,1=idf,2=ipf,3=iff,4=isg,5=mdf,6=gen

  TYPE(IDFOBJ)             :: IDF                       !## idf-structure

  INTEGER                  :: NLIDF                     !## number of listed idf to be plotted

  INTEGER,DIMENSION(MAXIAXES) :: IAXES  !## which axes to be used for each column in the associated files

  INTEGER                  :: XCOL
  INTEGER                  :: YCOL
  INTEGER                  :: ZCOL
  INTEGER                  :: Z2COL
  INTEGER                  :: HCOL                      !## high-light
  INTEGER                  :: PCOL                      !## plot-column for ipf labeling

  INTEGER                  :: SYMBOL                    !## line/point symbol
  INTEGER                  :: FADEOUT                   !## fade out depth
  INTEGER                  :: THICKNESS                 !## line/point symbol

  INTEGER                  :: IDFI                      !## plot idfi
  INTEGER                  :: IEQ                       !## grid type - misbruikt voor ipf's (label plotting)
  INTEGER                  :: IDFKIND                   !## plot idfkind (0=GRID,1=CONTOUR,2=VECTOR)
  REAL                     :: UMIN,UMAX                 !## user given statistics - voor iff's/ipf's/gen's
  REAL                     :: DMIN,DMAX                 !## date given statistics - voor ipf's/ipf's/gen's
  INTEGER                  :: UNITS                     !## 1=default,2=mm/day,3=m/day

  REAL                     :: XMIN,YMIN,XMAX,YMAX       !## coordinates within mplot-box (m) - for ipf's
  INTEGER                  :: ILEG                      !## use legend, for ipf/iff/gen
  INTEGER                  :: IATTRIB                   !## use legend, for ipf/iff/gen selected attribute
  INTEGER                  :: SCOLOR                    !## color number for serie-plotting
  INTEGER                  :: PRFTYPE                   !## prof.type line(0)/filled(1)
  INTEGER                  :: ISCREEN                   !## usage of screen 
  INTEGER                  :: ASSCOL1                   !## columns to express those to be used for plotting the associated files.
  INTEGER                  :: ASSCOL2                   !## columns to express those to be used for plotting the associated files.
  INTEGER,DIMENSION(2)     :: ASSFILES                  !## plot associated files for selected row in IPF (assfiles>0, or all assfiles<0)
  INTEGER                  :: ILEGDLF                   !## number of the legend from the dlf
  TYPE(LEGENDOBJ) :: LEG
  CHARACTER(LEN=256)       :: IDFNAME                   !## name of idf/ipf-file
  CHARACTER(LEN=50)        :: ALIAS
  INTEGER                  :: TSIZE                     !## text size (1 t/m 10 * 0.02)
  INTEGER,DIMENSION(2)     :: ICPERC                    !## Associated file attributes: column numbers
  INTEGER                  :: GPERC1,GPERC2             !## checkbox value for plotting values (e.g. percentages) 
 END TYPE MPLOTOBJ

 TYPE MPWINOBJ
  INTEGER              :: IBITMAP                   !## number of bitmap stores entire window
  INTEGER              :: NACT                      !## number of active idf's
  INTEGER              :: DIX,DIY                   !## size of bitmap (pixels)
  INTEGER              :: IWIN                      !## child window handle
  INTEGER              :: IX,IY                     !## current view-point (pixels)
  INTEGER              :: DXP,DYP                   !## size of paper (cm)
  REAL                 :: XMIN,XMAX,YMIN,YMAX       !## coordinates current bitmap (m)
 END TYPE MPWINOBJ
 TYPE(MPLOTOBJ),DIMENSION(MXMPLOT)      :: MP
 !TYPE(MPLOTOBJ),DIMENSION(:),ALLOCATABLE :: MP_BU
 TYPE(MPWINOBJ)                         :: MPW
 INTEGER,DIMENSION(MXMPLOT)             :: ACTLIST  !##which idf is selected
 INTEGER,DIMENSION(MXMPLOT)             :: DRWLIST  !##which idf is drawn truly
 INTEGER                                :: CNCLR    !## COPY purposes
 INTEGER,DIMENSION(MXCLR)               :: CRGB     !## COPY purposes
 REAL,DIMENSION(0:MXCLR)                :: CCLASS   !## COPY purposes
 CHARACTER(LEN=50),DIMENSION(MXCLR)     :: CLEGTXT  !## COPY purposes
 CHARACTER(LEN=150)                     :: CHEDTXT  !## COPY purposes
 INTEGER,DIMENSION(MXCGRAD)             :: CCGRAD   !## selected color for gradient
 CHARACTER(LEN=256)                     :: PLOTNAME !##configuration name
 REAL,DIMENSION(MXMPLOT,4)              :: POLAREAXY !## saves extent of each idf (xmin,ymin,xmax,ymax)
 INTEGER,DIMENSION(MXMPLOT)             :: IPOLACT=0 !## saves whether an idf raster is drawn or not (iact=1 --> idf is drawn)

 TYPE CONTOBJ
  REAL :: ALAB,VLAB,XLAB,YLAB 
 END TYPE CONTOBJ
 TYPE(CONTOBJ),POINTER,DIMENSION(:) :: CONT,CONT_BU
 INTEGER :: NLAB
 REAL :: LABDIST

 TYPE ZOOMOBJ
  REAL,POINTER,DIMENSION(:,:) :: ZOOMXY,ZOOMXY_BU
  INTEGER :: NZOOM !## number of zoom coordinates
  INTEGER :: IZOOM !## selected zoom coordinate
 END TYPE ZOOMOBJ
 TYPE(ZOOMOBJ) :: ZM
 
END MODULE
