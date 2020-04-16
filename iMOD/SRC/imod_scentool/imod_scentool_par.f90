!!  Copyright (C) Stichting Deltares, 2005-2020.
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
MODULE MOD_SCENTOOL_PAR

USE MOD_IDF_PAR, ONLY : IDFOBJ
USE IMODVAR, ONLY : DP_KIND,SP_KIND

INTEGER,PARAMETER :: MXRES=2
CHARACTER(LEN=50),DIMENSION(MXRES) :: RESLIST
DATA RESLIST/'Phreatic Heads','DrawDown'/ !,'Flowlines Well System','Flowlines Other Wells'/

REAL(KIND=DP_KIND) :: NODATAGRID=-99999.0

!!#obj. voor packages to be manipulated

TYPE ST1WEL_SUB1
 CHARACTER(LEN=10) :: ID
 REAL(KIND=DP_KIND) :: X,Y,Z1,Z2
END TYPE ST1WEL_SUB1
TYPE ST1WEL_SUB2
 INTEGER :: IDATE
 REAL(KIND=DP_KIND) :: QRATE
END TYPE ST1WEL_SUB2
TYPE ST1OBS_SUB1
 INTEGER :: IDATE
 REAL(KIND=DP_KIND) :: MEASURE
END TYPE ST1OBS_SUB1

!## wells
TYPE ST1WEL
 CHARACTER(LEN=24) :: CNAME
 INTEGER :: ISYMBOL !## symbol number
 INTEGER :: ICLR    !## colouring of system
 INTEGER :: ITYPE   !## type to distribute extraction rate (1=mean,2=kd-weighed)
 INTEGER :: ILOCT   !## location transformation 1=msl 2=surfacelevel
 INTEGER :: NLOC    !## number of individual extraction well rates
 TYPE(ST1WEL_SUB1),POINTER,DIMENSION(:) :: LOC
 INTEGER :: NQ      !## number of rate-dates
 TYPE(ST1WEL_SUB2),POINTER,DIMENSION(:) :: Q
END TYPE ST1WEL

INTEGER :: NROWQ    !## number of row in grid q
INTEGER :: NROWL    !## number of row in grid loc

!## cutouts
TYPE ST1CUT
 CHARACTER(LEN=24) :: CNAME
 REAL(KIND=DP_KIND) :: Z          !## depth of cutout
 INTEGER :: NXY     !## number of points
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: XY !## locations
END TYPE ST1CUT

!## observations
TYPE ST1OBS
 CHARACTER(LEN=24) :: CNAME
 INTEGER :: ISYMBOL !## symbol number
 INTEGER :: ICLR    !## colour
 INTEGER :: ILOCT   !## location transformation 1=msl 2=surfacelevel
 INTEGER :: ITYPE   !## (OBSOLUTE) type to distribute extraction rate (1=mean,2=kd-weighed)
 INTEGER :: NLOC    !## number of individual extraction well rates
 TYPE(ST1WEL_SUB1),POINTER,DIMENSION(:) :: LOC
 INTEGER :: NZ      !## number of rate-dates
 TYPE(ST1OBS_SUB1),POINTER,DIMENSION(:) :: Z
! REAL(KIND=DP_KIND) :: X,Y       !## locations
! INTEGER :: NZ     !## number of observations
! REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: TZ  !## rates (jdate,z)
END TYPE ST1OBS

!## monitoring
TYPE ST1MON
 CHARACTER(LEN=24) :: CNAME
 INTEGER :: ITYPE  !## itype of z coordinate (0=tov maaiveld, 1=tov nap)
 INTEGER :: NXY    !## number of points on segment (always two!)
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: XY  !## location of segment
 INTEGER :: NXZ    !## number of intervals on segment
 REAL(KIND=DP_KIND),POINTER,DIMENSION(:,:) :: XZ  !## location of segment
END TYPE ST1MON

!## results
TYPE ST1RES
 CHARACTER(LEN=256) :: CNAME
 INTEGER :: IRES   !## selected
 !INTEGER :: NFILES !## number of files
 !CHARACTER(LEN=256),POINTER,DIMENSION(:) :: IDFNAME
END TYPE ST1RES

REAL(KIND=DP_KIND),ALLOCATABLE,DIMENSION(:) :: SIMDELT  !## simulation delt
INTEGER,ALLOCATABLE,DIMENSION(:) :: SIMJDATE !## julian date of simulation
INTEGER :: SIMNPER !## simulation nper

TYPE(ST1WEL),DIMENSION(:),ALLOCATABLE :: WEL
TYPE(ST1CUT),DIMENSION(:),ALLOCATABLE :: CUT
TYPE(ST1OBS),DIMENSION(:),ALLOCATABLE :: OBS
TYPE(ST1MON),DIMENSION(:),ALLOCATABLE :: MON
TYPE(ST1RES),DIMENSION(:),ALLOCATABLE :: RES

INTEGER :: NWEL   ,NCUT   ,NOBS   ,NMON   ,NRES   !## number of systems available
INTEGER :: IWEL   ,ICUT   ,IOBS   ,IMON           !## number of systems selected
INTEGER :: MAXNWEL,MAXNCUT,MAXNOBS,MAXNMON,MAXNRES
INTEGER :: NSCNCONF

TYPE ST1CONF
 CHARACTER(LEN=256) :: PRJF    !## prjfile
 CHARACTER(LEN=100) :: PRJNAME !## prjname to be displayed
END TYPE ST1CONF
TYPE(ST1CONF),DIMENSION(:),ALLOCATABLE :: CONF

CHARACTER(LEN=256) :: RESDIR,SCFFNAME

TYPE(IDFOBJ),ALLOCATABLE,DIMENSION(:) :: DDNIDF,CIDF,TOPIDF,BOTIDF,KHVIDF
INTEGER,ALLOCATABLE,DIMENSION(:) :: IKD

!INTEGER :: STNLAY !## number of top/bottom layers to be used assigning wells

CONTAINS

 !###======================================================================
 SUBROUTINE ST1INITVAR()
 !###======================================================================

 END SUBROUTINE ST1INITVAR

END MODULE MOD_SCENTOOL_PAR

