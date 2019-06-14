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
MODULE MOD_RESIDUALPLOT_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
TYPE IPFRESOBJ
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: X => NULL()              !## x-coordinate
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: Y => NULL()              !## y-coordinate
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: M => NULL()              !## computed values
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: O => NULL()              !## observation
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:) :: W => NULL()              !## weight value
 INTEGER,POINTER,DIMENSION(:) :: L => NULL()           !## layer
 INTEGER(KIND=DP_KIND),POINTER,DIMENSION(:) :: D => NULL()           !## date
 INTEGER :: NPOINTS                                    !## number of points in ipf
END TYPE IPFRESOBJ
TYPE(IPFRESOBJ),DIMENSION(:),ALLOCATABLE :: IPFR       !## stores original values from txtfile per ipf
REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: X,Y,Z                     !## values to be plotted
REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: X_TMP,Y_TMP,Z_TMP         !## values to be plotted
REAL(KIND=DP_KIND) :: XMAX,XMIN,YMAX,YMIN               !## scatterplot range x and y axis
CHARACTER(LEN=256) :: INPUTFILE,BMPNAME,IPFNAME        !## name of txtfile
INTEGER :: IPLOT,IWEIGHT                               !## plot type
INTEGER :: ITRANSIENT,IAVERAGE,IXY                     !## transient/steadystate (lacking date)
INTEGER,POINTER,DIMENSION(:) :: ILAYER,IIPFS,IRDATE    !## all layer numbers/IPF-files to be read
INTEGER :: NLAYER,NIPFS,NRDATE                         !## amount of layers/IPF-files to be read
REAL(KIND=DP_KIND) :: GOF                                            !## goodness-of-fit with linear regression
REAL(KIND=DP_KIND) :: WC1,WC2                                        !## weight lower and upper class
REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: HCLASSES                  !## (user) defined histogram classes
REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: XCLASSES                  !## amount of points per defined histogram class for x-array

END MODULE MOD_RESIDUALPLOT_PAR