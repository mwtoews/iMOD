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
MODULE ROVAR

USE MOD_IDF_PAR, ONLY : IDFOBJ

INTEGER, PARAMETER		  :: MAXINDT = 1000  !max number of natuurdoeltypen natuurdoeltypen
INTEGER, PARAMETER		  :: MXC = 4	  !max number of colums for natuurdoeltypen
INTEGER, PARAMETER       	  :: MXRFC = 100 	  !max number of repro functions
INTEGER,PARAMETER			  :: MAXOPT = 6
REAL,ALLOCATABLE,DIMENSION(:)   :: IGLD,IKWL,INDR,CELRVW !arrays 
REAL,ALLOCATABLE,DIMENSION(:,:) :: GVGRVW,GLGRVW,DSTRVW !boundary conditions for gvg, glg and stress to drought (A1,B1,B2,A2)
REAL,ALLOCATABLE,DIMENSION(:)   :: B,M,C   	  !arrays for reprofunctions, indices B,M,C
CHARACTER(LEN=52) :: DRTYPE		   ! effect keuze
REAL,PARAMETER         		  :: FLXWNS = 0.5    ! gewenste minimale kwelflux voor kwelafhankelijke natuur (m/d)
INTEGER    			        :: RDOELMIN        ! minimale doelrealisatie die 'goed' wordt gevonden (0-100%)

INTEGER,PARAMETER		        :: MAXIR = 25      !Maximaal aantal IRmaatregelen
INTEGER, PARAMETER	        :: MAXHLPC = 14    !aantal help gewassen (zijn er 14)
INTEGER				  :: NRECS,IRCREC,NCOLS	   !resp. NO RECORDS IN DIALOG GRID, no ircosts
INTEGER				  :: ROSIZE,MAXIMP         !SIZE OF ROOBJ, MAX Number of Impulses
INTEGER,ALLOCATABLE	:: LGNLUT(:,:),NDTLUT(:)   !arrays LGN and nature doeltype
REAL,DIMENSION(:,:,:),ALLOCATABLE	:: EXPORTTAB  !Per tabblad, per rij, per kolom
REAL,ALLOCATABLE		:: ROSCENCOSTS(:,:,:),CCOSTS(:,:)

CHARACTER(LEN=100),ALLOCATABLE,DIMENSION(:,:)	:: RWSD,CLSD  !BESCHRIJVING RIJEN,KOLOMMEN
CHARACTER(LEN=256)			:: LEGEND(2)
CHARACTER(LEN=256),ALLOCATABLE	:: IDFS(:,:)
CHARACTER(LEN=50),ALLOCATABLE	:: DAGR(:), DNDT(:)  !descriptions of croptypes and natuurdoeltype
CHARACTER(LEN=256),ALLOCATABLE	:: RESULTIDFS(:,:) !SCENARIO RESP. REFERENCE RESULT
CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: ROVISIDF	  !array met daarin idf's die kunnen worden gevisualiseerd (hangen de tabs)
CHARACTER(LEN=256)		  :: STDLEG

TYPE IRTC							!TOTAL COSTS
 CHARACTER(LEN=50)	:: IRNM
 REAL :: IRTC
END TYPE IRTC
TYPE(IRTC),DIMENSION(:),ALLOCATABLE :: IRTCST

!TYPE ROOBJ
! INTEGER :: NROW,NCOL,IU
! REAL :: XMIN,YMIN,XMAX,YMAX,DX,DY,NODATA
!END TYPE ROOBJ
TYPE(IDFOBJ),DIMENSION(:),ALLOCATABLE :: RO

TYPE VISVAR						!VISUALISATION VARIABLES
 INTEGER			:: IU, IDF			!IU = unit number, idf = dialog object
 CHARACTER(LEN=256)	:: FNLEG
END TYPE VISVAR
TYPE(VISVAR),DIMENSION(:),ALLOCATABLE :: VARVIS

!ROSCEN VARIABLES 
!TYPE ROSOBJ
! INTEGER :: NROW,NCOL,IU
! REAL :: XMIN,YMIN,XMAX,YMAX,DX,DY,NODATA
! CHARACTER(LEN=256) :: IDFNM
!END TYPE ROSOBJ
TYPE(IDFOBJ),DIMENSION(:,:),ALLOCATABLE :: ROSCEN

CHARACTER(LEN=25),DIMENSION(MAXOPT)	:: RONAME
DATA RONAME /'Wet_Damage_Agriculture','Drought_Damage_Agriculture','Objective_Wet_Agriculture',&
		 'Objective_Drought_Agriculture','Objective_Nature','Objective_Urban_Area'/
!CHARACTER(LEN=25),DIMENSION(MAXOPT)	:: ROSUBS
!DATA ROSUBS /'natschade_lanbouw','droogteschade_landbouw','doelrealisatie_lb_nat',&
!		 'doelrealisatie_lb_droog','doelrealisatie_natuur','doelrealisatie_stedelijk'/

!twee maal een array voor bijhouden van kaartmateriaal
!SUBTYPE			  IACT 	GHG	GLG		LUSE	SOIL	  LUT1      LUT2	    LUT3
!1 natschade_lanbouw         0  ghgmap	glgkaart	lgn5	soil	  hlp_wet	costtable -   
!2 droogteschade_landbouw    0  ghgmap	glgkaart	lgn5	soil	  hlp_dry	costtable -   
!3 doelrealisatie_lb_naT     0  ghgmap	glgkaart	lgn5	soil	  hlp_wet	costtable -   
!4 doelrealisatie_lb_droog   0  ghgmap	glgkaart	lgn5	soil	  hlp_dry	costtable -   
!5 doelrealisatie_natuur     0  ghgmap	glgkaart	ndt	rfcsoil abiot.txt rfc.lut   ndt.lut
!6 doelrealisatie_stedelijk  0  ghgmap	-            lgn5  -      URBANRANGE.LUT  -   -

TYPE FILEPREFS
 INTEGER :: IACT
 CHARACTER(LEN=256)	:: GHG,GLG,LUSE,SOIL,LUT1,LUT2,LUT3
END TYPE FILEPREFS
TYPE(FILEPREFS),DIMENSION(:),ALLOCATABLE :: SCENPREF,REFPREF
END MODULE


