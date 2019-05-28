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
MODULE MOD_MSPINSPECTOR_PAR

USE IMODVAR, ONLY : DP_KIND,SP_KIND
USE MOD_IDF_PAR
USE RESOURCE

TYPE DATADXCTYPE
 INTEGER :: ILAY,IROW,ICOL,MFID
END TYPE DATADXCTYPE
TYPE DXCTYPE
 TYPE(DATADXCTYPE),POINTER,DIMENSION(:) :: INFO      ! file content 
 CHARACTER(LEN=35),POINTER,DIMENSION(:) :: LABEL     ! name label for each variable
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT      ! unit of variable 
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL   ! Value for each parameter at mouse location
 INTEGER,POINTER,DIMENSION(:) :: IACT                ! pointer: user settings for (not) displaying  a variable 
 INTEGER,POINTER,DIMENSION(:) :: DXCIREC             ! records of Modflow ID's under mouse (1: first svat 2: optional irrigation svat)
 INTEGER :: MXID                                     ! maximum number of ID's   
END TYPE DXCTYPE
TYPE(DXCTYPE) :: DXC

TYPE DATAMODSVATTYPE
 INTEGER :: MFID,SVATID,LY
END TYPE DATAMODSVATTYPE
TYPE MODSVATTYPE
 TYPE(DATAMODSVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE MODSVATTYPE
TYPE(MODSVATTYPE) :: MODSVAT

TYPE DATAIDFSVATTYPE
 INTEGER :: SVAT,ROW,COL
 REAL(KIND=DP_KIND) :: X_CORD,Y_CORD
 INTEGER :: MFID_RURBAN,MFID_IRR    !  corresponding Modflow ID, found in Mod2Svat.inp, needed to get location data from <name>.dxc file
END TYPE DATAIDFSVATTYPE
TYPE IDFSVATTYPE
 TYPE(DATAIDFSVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=35),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE IDFSVATTYPE
TYPE(IDFSVATTYPE) :: IDFSVAT

TYPE DATAAREASVATTYPE
 INTEGER :: LUSE,SOIL,METE,NUND !,SVAT,SLK,LUK,NM
 REAL(KIND=DP_KIND) :: ARK,RZ,SURF,TEMP,LCFPREP,LCFPOT
 INTEGER :: REC_IDFSVAT,REC_SCAPSVAT    !  Additional parameters : irec of SVAT in IDFSVAT and SCAPSVAT
END TYPE DATAAREASVATTYPE
TYPE AREASVATTYPE
 TYPE(DATAAREASVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=50),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE AREASVATTYPE
TYPE(AREASVATTYPE) :: AREASVAT

TYPE DATAINFISVATTYPE
 INTEGER :: NUND
 REAL(KIND=DP_KIND) :: QINBASIC,CTOP_DOWN,CTOP_UP,CBOT,SC2
END TYPE DATAINFISVATTYPE
TYPE INFISVATTYPE
 TYPE(DATAINFISVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=50),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE INFISVATTYPE
TYPE(INFISVATTYPE) :: INFISVAT

TYPE DATASCAPSVATTYPE
 INTEGER :: SVAT,SVATAB,LYAB,SWNRAB
 REAL(KIND=DP_KIND) :: FMMXABGW,FMMXABSW,FXABGW,FXABSW
END TYPE DATASCAPSVATTYPE
TYPE SCAPSVATTYPE
 TYPE(DATASCAPSVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=50),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE SCAPSVATTYPE
TYPE(SCAPSVATTYPE) :: SCAPSVAT

TYPE DATALUSESVATTYPE
 INTEGER :: LU,VGLU
 REAL(KIND=DP_KIND) :: ALPHACRIT,P1FD,P2FD,P3HFD,P3LFD,P4FD,T3HFD,T3LFD,PBGSPLU,FREVSPLU,& 
                       GISPLU,TIGISPLU,RPSPLU,TDBGSPLU,TDEDSPLU,FECMNLU,ALBEDOLU,RSCDRYLU,&
                       RSCWETLU,KDIF,KDIR,ECMAXLU,ECSLOPLU
 CHARACTER(LEN=19) :: LUNA
END TYPE DATALUSESVATTYPE
TYPE LUSESVATTYPE
 TYPE(DATALUSESVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=50),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE LUSESVATTYPE
TYPE(LUSESVATTYPE) :: LUSESVAT

TYPE DATAMETEGRIDTYPE
 INTEGER :: IY
 INTEGER :: JD !## julian data of precipitation/evaporation
 REAL(KIND=DP_KIND) :: TD
 CHARACTER(LEN=256) :: PRECGRID,ETREFGRID
END TYPE DATAMETEGRIDTYPE
TYPE METEGRIDTYPE
 TYPE(DATAMETEGRIDTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=30),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER,POINTER,DIMENSION(:) :: GRAPH               
 INTEGER :: MXID
END TYPE METEGRIDTYPE
TYPE(METEGRIDTYPE) :: METEGRID

TYPE DATASVATPRECTYPE
 INTEGER :: SVAT, ROW, COLUMN
END TYPE DATASVATPRECTYPE
TYPE SVATPRECTYPE
 TYPE(DATASVATPRECTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=35),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE SVATPRECTYPE
TYPE(SVATPRECTYPE) :: SVATPREC

TYPE DATASVATETREFTYPE
 INTEGER :: SVAT, ROW, COLUMN
END TYPE DATASVATETREFTYPE
TYPE SVATETREFTYPE
 TYPE(DATASVATETREFTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=35),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE SVATETREFTYPE
TYPE(SVATETREFTYPE) :: SVATETREF

TYPE DATAFACTSVATTYPE
 INTEGER :: VG,DY
 REAL(KIND=DP_KIND) :: CSVG,LAIVG,VXICVG,FAEVVG,FAEIVG,FAEBSVG,FAEPDVG,CHVG,DRPZVG
END TYPE DATAFACTSVATTYPE
TYPE FACTSVATTYPE
 TYPE(DATAFACTSVATTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=50),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER,POINTER,DIMENSION(:) :: GRAPH
 INTEGER :: MXID
END TYPE FACTSVATTYPE
TYPE(FACTSVATTYPE) :: FACTSVAT

TYPE DATATIOPSIMTYPE
 INTEGER :: IY,IO,IP
 REAL(KIND=DP_KIND) :: TD
END TYPE DATATIOPSIMTYPE
TYPE TIOPSIMTYPE
 TYPE(DATATIOPSIMTYPE),POINTER,DIMENSION(:) :: INFO
 CHARACTER(LEN=24),POINTER,DIMENSION(:) :: LABEL
 CHARACTER(LEN=10),POINTER,DIMENSION(:) :: UNIT     
 CHARACTER(LEN=15),POINTER,DIMENSION(:) :: INSPVAL 
 INTEGER,POINTER,DIMENSION(:) :: IACT
 INTEGER :: MXID
END TYPE TIOPSIMTYPE
TYPE(TIOPSIMTYPE) :: TIOPSIM

INTEGER,PARAMETER :: MXMSFILES=11
CHARACTER(LEN=9),DIMENSION(MXMSFILES) :: MSFILES = &
    (/'DXC', 'MOD2SVAT','IDF_SVAT','AREA_SVAT','LUSE_SVAT','FACT_SVAT','INFI_SVAT','SCAP_SVAT','METE_GRID','SVATPREC','SVATETREF'/)   !## INP files not needed: PARASIM, TIOPSIM

INTEGER,DIMENSION(MXMSFILES) :: MSFILES_IACT

CHARACTER(LEN=512) :: ROOT,LINE,FNAME
CHARACTER(LEN=128) :: MNAME,MFNAME
TYPE(IDFOBJ) :: MSPIDF               ! variable use e.g. pointer of selected irrigatied cells
TYPE(IDFOBJ) :: MSPSCAPSVAT  ! layer of irrigation source  
TYPE(IDFOBJ) :: SVATRU,SVATUR,SVATIR,SVATIRS ! GRID position of SVAT for rural, urban, irrigation and irrigationsource
TYPE(IDFOBJ) :: METEO
INTEGER :: IYBG,TDBG     !PARA_SIM: starting time of calculations [d] / year number of starting time
INTEGER :: MSPSDATE,MSPEDATE
INTEGER :: TAB3GRIDSIZE

!## iMODBATCH
INTEGER(KIND=DP_KIND) :: MSPRCH_FYR,MSPRCH_TYR
INTEGER :: MSPRCH_NYEAR,SCOPT
INTEGER,DIMENSION(:),POINTER :: MSPRCH_IYEAR
CHARACTER(LEN=256) :: SOURCEDIR,RESDIR,STOAVG
TYPE(IDFOBJ) :: HEAD1,HEAD2,SC1,QMODF,NETRCH


END MODULE MOD_MSPINSPECTOR_PAR