!!  Copyright (C) Stichting Deltares, 2005-2016.
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
MODULE MOD_TSTAT_PAR

USE WINTERACTER
USE RESOURCE

CHARACTER(LEN=256) :: GENNAME,IPFNAME1,IPFNAME2,OUTNAME,SURFACELEVEL,POINTERIDF
CHARACTER(LEN=50) :: CXPERC
INTEGER :: IGEN,RELATECOLIPF1,RELATECOLIPF2,DMY1,DMY2,IINVERSE,NZONE,ICOLLECT
INTEGER,DIMENSION(:),ALLOCATABLE :: IZONE
REAL :: XLAG,DLAG,HNODATA

INTEGER,DIMENSION(12) :: ID
CHARACTER(LEN=256) :: FNAME1,FNAME2,DIR1,DIR2
CHARACTER(LEN=1024) :: LINE
CHARACTER(LEN=50),DIMENSION(:),ALLOCATABLE :: CVARS
INTEGER,DIMENSION(:),ALLOCATABLE :: IVARS
CHARACTER(LEN=2),DIMENSION(2) :: ICOLDATE,ICOLVARS
INTEGER :: NVARS,SOBSDATE,EOBSDATE

CHARACTER(LEN=256),DIMENSION(:),ALLOCATABLE :: IPFFILE
INTEGER,DIMENSION(:),ALLOCATABLE :: IHCOL,IMCOL,IWCOL,ILCOL,W_TYPE
TYPE OBSOBJ
 REAL :: DH,DHW,COR
 INTEGER :: ILAY
 REAL :: X,Y,M,H,W,MH,MM
 CHARACTER(LEN=52) :: CID
END TYPE OBSOBJ
TYPE(OBSOBJ),POINTER,DIMENSION(:) :: RES,RES_DUMMY

DATA ID/IDF_INTEGER1,IDF_INTEGER2,IDF_INTEGER3,IDF_INTEGER4, &
        IDF_MENU2,   IDF_MENU3,   IDF_LABEL1,  IDF_LABEL2,   &
        IDF_CHECK3,  IDF_OPEN3,   IDF_STRING3, ID_AUTO/

END MODULE MOD_TSTAT_PAR
