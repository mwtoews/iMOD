!!  Copyright (C) Stichting Deltares, 2005-2014.
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
MODULE MOD_BATCH_PAR

USE WINTERACTER
USE RESOURCE

INTEGER,PARAMETER :: NBAT=37

TYPE MODBAT
 CHARACTER(LEN=52) :: CFUNC
 INTEGER :: N
 CHARACTER(LEN=8),POINTER,DIMENSION(:) :: LVL
 CHARACTER(LEN=52),POINTER,DIMENSION(:) :: KEY
END TYPE MODBAT

TYPE(MODBAT),DIMENSION(NBAT) :: BAT

CONTAINS

 !###======================================================================
 SUBROUTINE BATCHINIT()
 !###======================================================================
 IMPLICIT NONE
 
 CALL BATCHINIT_PLOT(1)
 CALL BATCHINIT_IDFCALC(2)
 CALL BATCHINIT_IDFSCALE(3)
 CALL BATCHINIT_IDFMEAN(4)
 CALL BATCHINIT_IDFCONSISTENCY(5)
 CALL BATCHINIT_IDFMERGE(6)
 CALL BATCHINIT_GXG(7)
 CALL BATCHINIT_WBALANCE(8)
 CALL BATCHINIT_IMPORTSOBEK(9)
 CALL BATCHINIT_AHNFILTER(10)
 CALL BATCHINIT_CREATEIDF(11)
 CALL BATCHINIT_CREATEASC(12)
 CALL BATCHINIT_IMPORTMODFLOW(13)
 CALL BATCHINIT_IDFSTAT(14)
 CALL BATCHINIT_IPFSTAT(15)
 CALL BATCHINIT_MODELCOPY(16)
 CALL BATCHINIT_IMODPATH(17)
 CALL BATCHINIT_IPFSAMPLE(18)
 CALL BATCHINIT_MKWELLIPF(19)
 CALL BATCHINIT_XYZTOIDF(20)
 CALL BATCHINIT_ISGGRID(21)
 CALL BATCHINIT_ISGADDCROSSSECTION(22)
 CALL BATCHINIT_ISGSIMPLIFY(23)
 CALL BATCHINIT_DINO2IPF(24)
 CALL BATCHINIT_IDFTIMESERIE(25)
 CALL BATCHINIT_BMPTILING(26)
 CALL BATCHINIT_CREATESUBMODEL(27)
 CALL BATCHINIT_CREATESOF(28)
 CALL BATCHINIT_DRNSURF(29)
 CALL BATCHINIT_SOLID(30)
 CALL BATCHINIT_IPFSPOTIFY(31)
 CALL BATCHINIT_ASSIGNWELL(32)
 CALL BATCHINIT_GEN2ISG(33)
 CALL BATCHINIT_GEOTOP(34)
 CALL BATCHINIT_GEF2IPF(35)
 CALL BATCHINIT_CREATEIZONE(36)
 CALL BATCHINIT_IPFRESIDUAL(37)

 CALL WDIALOGPUTMENU(IDF_MENU1,BAT%CFUNC,SIZE(BAT),1)
 
 END SUBROUTINE BATCHINIT


 !###======================================================================
 SUBROUTINE BATCHINIT_PLOT(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="PLOT"; BAT(I)%N= 29
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="OUTFILE"
 BAT(I)%LVL(2)="(OPT)"; BAT(I)%KEY(2)="IDFILE"
 BAT(I)%LVL(3)="(DEP)"; BAT(I)%KEY(3)="IDFLEGFILE"
 BAT(I)%LVL(4)="(DEP)"; BAT(I)%KEY(4)="IDFLEGTXT"
 BAT(I)%LVL(5)="(DEP)"; BAT(I)%KEY(5)="IDFSTYLE"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="IPFFILE"
 BAT(I)%LVL(7)="(DEP)"; BAT(I)%KEY(7)="IPFXCOL"
 BAT(I)%LVL(8)="(DEP)"; BAT(I)%KEY(8)="IPFYCOL"
 BAT(I)%LVL(9)="(DEP)"; BAT(I)%KEY(9)="IPFHCOL"
 BAT(I)%LVL(10)="(DEP)"; BAT(I)%KEY(10)="NLABELS"
 BAT(I)%LVL(11)="(DEP2)"; BAT(I)%KEY(11)="ILABELS"
 BAT(I)%LVL(12)="(DEP)"; BAT(I)%KEY(12)="IPFSTYLE"
 BAT(I)%LVL(13)="(DEP2)"; BAT(I)%KEY(13)="IPFILCOL"
 BAT(I)%LVL(14)="(DEP2)"; BAT(I)%KEY(14)="IPFLEGFILE"
 BAT(I)%LVL(15)="(DEP2)"; BAT(I)%KEY(15)="IPFLEGTEXT"
 BAT(I)%LVL(16)="(OPT)"; BAT(I)%KEY(16)="IFFFILE"
 BAT(I)%LVL(17)="(DEP)"; BAT(I)%KEY(17)="IFFLEGFILE"
 BAT(I)%LVL(18)="(DEP)"; BAT(I)%KEY(18)="IFFLEGTEXT"
 BAT(I)%LVL(19)="(OPT)"; BAT(I)%KEY(19)="NGEN"
 BAT(I)%LVL(20)="(DEP)"; BAT(I)%KEY(20)="GENFILE{i=1,NGEN}"
 BAT(I)%LVL(21)="(DEP)"; BAT(I)%KEY(21)="GENCOLOUR{i=1,NGEN}"
 BAT(I)%LVL(22)="(OPT)"; BAT(I)%KEY(22)="LEGSIZE"
 BAT(I)%LVL(23)="(OPT)"; BAT(I)%KEY(23)="WINDOW"
 BAT(I)%LVL(24)="(OPT)"; BAT(I)%KEY(24)="TITLE"
 BAT(I)%LVL(25)="(OPT)"; BAT(I)%KEY(25)="SUBTITLE"
 BAT(I)%LVL(26)="(OPT)"; BAT(I)%KEY(26)="FIGTXT"
 BAT(I)%LVL(27)="(OPT)"; BAT(I)%KEY(27)="PRJTXT"
 BAT(I)%LVL(28)="(OPT)"; BAT(I)%KEY(28)="YFRACLEGEND"
 BAT(I)%LVL(29)="(OPT)"; BAT(I)%KEY(29)="RESOLUTION"

 END SUBROUTINE BATCHINIT_PLOT

 !###======================================================================
 SUBROUTINE BATCHINIT_IDFCALC(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IDFCALC"; BAT(I)%N= 13
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="FUNC"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="ABC{i=1,NREPEAT}"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="AC{i=1,NREPEAT}"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="BC{i=1,NREPEAT}"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="NREPEAT"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="SOURCEDIRA"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="SOURCEDIRB"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="SOURCEDIRC"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="USENODATA"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="NODATAVALUE"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="GENFILE"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="IEQUI"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="WINDOW"

 END SUBROUTINE BATCHINIT_IDFCALC

 !###======================================================================
 SUBROUTINE BATCHINIT_IDFSCALE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IDFSCALE"; BAT(I)%N= 18
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="SCALESIZE"
 BAT(I)%LVL(2)="(OPT)"; BAT(I)%KEY(2)="SCLTYPE_UP"
 BAT(I)%LVL(3)="(DEP)"; BAT(I)%KEY(3)="ANI_X"
 BAT(I)%LVL(4)="(DEP)"; BAT(I)%KEY(4)="ANI_Z"
 BAT(I)%LVL(5)="(DEP)"; BAT(I)%KEY(5)="DH_X"
 BAT(I)%LVL(6)="(DEP)"; BAT(I)%KEY(6)="DH_Y"
 BAT(I)%LVL(7)="(DEP)"; BAT(I)%KEY(7)="DH_Z"
 BAT(I)%LVL(8)="(DEP)"; BAT(I)%KEY(8)="QRATE"
 BAT(I)%LVL(9)="(DEP)"; BAT(I)%KEY(9)="AQFR_KD"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="SCLTYPE_DOWN"
 BAT(I)%LVL(11)="(COMP)"; BAT(I)%KEY(11)="SOURCEIDF"
 BAT(I)%LVL(12)="(COMP)"; BAT(I)%KEY(12)="SOURCEDIR"
 BAT(I)%LVL(13)="(COMP)"; BAT(I)%KEY(13)="OUTFILE"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="PERCENTILE"
 BAT(I)%LVL(15)="(OPT)"; BAT(I)%KEY(15)="WEIGHFACTOR"
 BAT(I)%LVL(16)="(OPT)"; BAT(I)%KEY(16)="BLOCK"
 BAT(I)%LVL(17)="(OPT)"; BAT(I)%KEY(17)="WINDOW"
 BAT(I)%LVL(18)="(OPT)"; BAT(I)%KEY(18)="BUFFER"

 END SUBROUTINE BATCHINIT_IDFSCALE

 !###======================================================================
 SUBROUTINE BATCHINIT_IDFMEAN(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IDFMEAN"; BAT(I)%N= 12
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="ILAYER"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="SDATE"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="EDATE"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="NDIR"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="SOURCEDIR{i=1,NDIR}"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="CFUNC"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="IYEAR"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="NPERIOD"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="PERIOD{i=1,NPERIOD}"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="ISEL"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="GENFNAME"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="IDFNAME"

 END SUBROUTINE BATCHINIT_IDFMEAN

 !###======================================================================
 SUBROUTINE BATCHINIT_IDFCONSISTENCY(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IDFCONSISTENCY"; BAT(I)%N=  6
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NLAY"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="OUTPUTFOLDER"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="TOP_L{i=1,NLAY}"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="BOT_L{i=1,NLAY}"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="WINDOW"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="CELL_SIZE"

 END SUBROUTINE BATCHINIT_IDFCONSISTENCY

  !###======================================================================
 SUBROUTINE BATCHINIT_(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 END SUBROUTINE BATCHINIT_
 
 !###======================================================================
 SUBROUTINE BATCHINIT_IDFMERGE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IDFMERGE"; BAT(I)%N=  6
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NMERGE"
 BAT(I)%LVL(2)="(OPT)"; BAT(I)%KEY(2)="SOURCEIDF{i=1,NMERGE}"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="SOURCEDIR"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="TARGETIDF"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="WINDOW"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="MSKNAME"

 END SUBROUTINE BATCHINIT_IDFMERGE

 !###======================================================================
 SUBROUTINE BATCHINIT_GXG(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="GXG"; BAT(I)%N= 12
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NDIR"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="SOURCEDIR{i=1,NBAL}"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="ILAYER"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="SURFACEIDF"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="SYEAR"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="EYEAR"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="IYEAR"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="STARTMONTH"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="IPERIOD"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="ISEL"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="GENFNAME"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="IDFNAME"

 END SUBROUTINE BATCHINIT_GXG

 !###======================================================================
 SUBROUTINE BATCHINIT_WBALANCE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="WBALANCE"; BAT(I)%N= 15
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NBAL"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="BAL{i=1,NBAL}"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="ILAYER"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="NDIR"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="SOURCEDIR{i=1,NDIR}"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="OUTPUTNAME{i=1,NDIR}"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="BAL{i}_ISYS(i=1,NDIR}"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="SDATE"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="EDATE"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="IYEAR"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="NPERIOD"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="PERIOD{i=1,NPERIOD}"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="ISEL"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="GENFNAME"
 BAT(I)%LVL(15)="(OPT)"; BAT(I)%KEY(15)="IDFNAME"

 END SUBROUTINE BATCHINIT_WBALANCE

 !###======================================================================
 SUBROUTINE BATCHINIT_IMPORTSOBEK(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IMPORTSOBEK"; BAT(I)%N=  4
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="ISGNAME"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="NETWORKTP"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="CALCHIS"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="STRUCHIS"

 END SUBROUTINE BATCHINIT_IMPORTSOBEK

 !###======================================================================
 SUBROUTINE BATCHINIT_AHNFILTER(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="AHNFILTER"; BAT(I)%N= 18
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NAHN"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="IDFFILE{i=1,NAHN}"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="XCRIT"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="NSCRIT"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="DPW"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="LOCCRIT"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="DP1"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="DP2"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="INTXCRIT"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="CORXCRIT"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="NCORXCRIT"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="IGNORENODATA"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="NWINDOW"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="WINDOW{i=1,NWINDOW}"
 BAT(I)%LVL(15)="(COMP)"; BAT(I)%KEY(15)="OUTFILE"
 BAT(I)%LVL(16)="(COMP)"; BAT(I)%KEY(16)="OUTFILE{i=1,NWINDOW}"
 BAT(I)%LVL(17)="(OPT)"; BAT(I)%KEY(17)="BUFFER"
 BAT(I)%LVL(18)="(OPT)"; BAT(I)%KEY(18)="IAGGREGATEY"

 END SUBROUTINE BATCHINIT_AHNFILTER

 !###======================================================================
 SUBROUTINE BATCHINIT_CREATEIDF(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="CREATEIDF"; BAT(I)%N=  3
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="SOURCEDIR"
 BAT(I)%LVL(2)="(OPT)"; BAT(I)%KEY(2)="TOPWC"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="BOTEL"

 END SUBROUTINE BATCHINIT_CREATEIDF

 !###======================================================================
 SUBROUTINE BATCHINIT_CREATEASC(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="CREATEASC"; BAT(I)%N=  1
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="SOURCEDIR"

 END SUBROUTINE BATCHINIT_CREATEASC

 !###======================================================================
 SUBROUTINE BATCHINIT_IMPORTMODFLOW(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IMPORTMODFLOW"; BAT(I)%N=  8
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="MVERSION"
 BAT(I)%LVL(2)="(OPT)"; BAT(I)%KEY(2)="BASFILE"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="NAMFILE"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="OUTDIR"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="LLCORNER"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="SDATE"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="PACKAGESUM"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="RIV5TH"

 END SUBROUTINE BATCHINIT_IMPORTMODFLOW

 !###======================================================================
 SUBROUTINE BATCHINIT_IDFSTAT(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IDFSTAT"; BAT(I)%N=  2
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="SOURCEDIR"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="OUTFILE"

 END SUBROUTINE BATCHINIT_IDFSTAT

 !###======================================================================
 SUBROUTINE BATCHINIT_IPFSTAT(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IPFSTAT"; BAT(I)%N= 18
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="IPF1"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="OUTFILE"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="VARIABLES"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="ICOLDATE1"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="ICOLVARS1"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="IPF2"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="ICOLDATE2"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="ICOLVARS2"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="GENFILE"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="PERCENTILES"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="IINVERSE"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="RELATECOLIPF1"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="RELATECOLIPF2"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="XLAG"
 BAT(I)%LVL(15)="(OPT)"; BAT(I)%KEY(15)="DLAG"
 BAT(I)%LVL(16)="(OPT)"; BAT(I)%KEY(16)="DMY1"
 BAT(I)%LVL(17)="(OPT)"; BAT(I)%KEY(17)="DMY2"
 BAT(I)%LVL(18)="(OPT)"; BAT(I)%KEY(18)="SURFACELEVEL"

 END SUBROUTINE BATCHINIT_IPFSTAT

 !###======================================================================
 SUBROUTINE BATCHINIT_MODELCOPY(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="MODELCOPY"; BAT(I)%N=  4
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="RUNFILE"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="RESDIR"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="WINDOW"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="CLIPDIR"

 END SUBROUTINE BATCHINIT_MODELCOPY

 !###======================================================================
 SUBROUTINE BATCHINIT_IMODPATH(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IMODPATH"; BAT(I)%N= 10
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="RUNFILE"
 BAT(I)%LVL(2)="(OPT)"; BAT(I)%KEY(2)="IPOSTP"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="IFFFLOW"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="IPFFLOW"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="IDFFLOW"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="IPFFNAME"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="IXCOL"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="IYCOL"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="ILABELCOL"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="ILAYCOL"

 END SUBROUTINE BATCHINIT_IMODPATH

 !###======================================================================
 SUBROUTINE BATCHINIT_IPFSAMPLE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IPFSAMPLE"; BAT(I)%N=  5
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="IPFFILE_IN"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="IPFFILE_OUT"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="SOURCEDIR"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="IXCOL"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="IYCOL"

 END SUBROUTINE BATCHINIT_IPFSAMPLE

 !###======================================================================
 SUBROUTINE BATCHINIT_MKWELLIPF(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="MKWELLIPF"; BAT(I)%N= 21
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NLAY"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="TOPIDF{i=1,NLAY}"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="BOTIDF{i=1,NLAY}"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="KDIDF{i=1,NLAY}"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="CIDF{i=1,NLAY-1}"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="NIPF"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="IPF{i=1,NIPF}"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="IXCOL"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="IYCOL"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="IQCOL"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="ITCOL"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="IBCOL"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="ISS"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="SDATE"
 BAT(I)%LVL(15)="(OPT)"; BAT(I)%KEY(15)="EDATE"
 BAT(I)%LVL(16)="(OPT)"; BAT(I)%KEY(16)="MAXC"
 BAT(I)%LVL(17)="(OPT)"; BAT(I)%KEY(17)="MINKH"
 BAT(I)%LVL(18)="(OPT)"; BAT(I)%KEY(18)="ICLAY"
 BAT(I)%LVL(19)="(OPT)"; BAT(I)%KEY(19)="HNODATA"
 BAT(I)%LVL(20)="(OPT)"; BAT(I)%KEY(20)="FNODATA"
 BAT(I)%LVL(21)="(OPT)"; BAT(I)%KEY(21)="IFRAC"

 END SUBROUTINE BATCHINIT_MKWELLIPF

 !###======================================================================
 SUBROUTINE BATCHINIT_XYZTOIDF(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="XYZTOIDF"; BAT(I)%N= 27
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="XYZFILE"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="IPFFILE"
 BAT(I)%LVL(3)="(DEP)"; BAT(I)%KEY(3)="IXCOL"
 BAT(I)%LVL(4)="(DEP)"; BAT(I)%KEY(4)="IYCOL"
 BAT(I)%LVL(5)="(DEP)"; BAT(I)%KEY(5)="IZCOL"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="SOURCEDIR"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="TARGETDIR"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="NODATA"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="CS"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="WINDOW"
 BAT(I)%LVL(11)="(COMP)"; BAT(I)%KEY(11)="IDFFILE_IN"
 BAT(I)%LVL(12)="(DEP)"; BAT(I)%KEY(12)="IDFFILE_POINTER"
 BAT(I)%LVL(13)="(COMP)"; BAT(I)%KEY(13)="IDFFILE"
 BAT(I)%LVL(14)="(COMP)"; BAT(I)%KEY(14)="GRIDFUNC"
 BAT(I)%LVL(15)="(DEP)"; BAT(I)%KEY(15)="PERCENTILE"
 BAT(I)%LVL(16)="(DEP)"; BAT(I)%KEY(16)="MINP"
 BAT(I)%LVL(17)="(DEP)"; BAT(I)%KEY(17)="MAXP"
 BAT(I)%LVL(18)="(DEP)"; BAT(I)%KEY(18)="RANGE"
 BAT(I)%LVL(19)="(DEP)"; BAT(I)%KEY(19)="SILL"
 BAT(I)%LVL(20)="(DEP)"; BAT(I)%KEY(20)="NUGGET"
 BAT(I)%LVL(21)="(DEP)"; BAT(I)%KEY(21)="KTYPE"
 BAT(I)%LVL(22)="(DEP)"; BAT(I)%KEY(22)="STDEVIDF"
 BAT(I)%LVL(23)="(DEP)"; BAT(I)%KEY(23)="LAGINTERVAL"
 BAT(I)%LVL(24)="(DEP)"; BAT(I)%KEY(24)="LAGDISTANCE"
 BAT(I)%LVL(25)="(DEP)"; BAT(I)%KEY(25)="HCLOSE"
 BAT(I)%LVL(26)="(DEP)"; BAT(I)%KEY(26)="RCLOSE"
 BAT(I)%LVL(27)="(DEP)"; BAT(I)%KEY(27)="NINNER"

 END SUBROUTINE BATCHINIT_XYZTOIDF

 !###======================================================================
 SUBROUTINE BATCHINIT_ISGGRID(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="ISGGRID"; BAT(I)%N= 16
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="ISGFILE_IN"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="CELL_SIZE"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="MINDEPTH"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="OUTPUTFOLDER"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="POSTFIX"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="NODATA"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="WINDOW"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="ISAVE"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="ICDIST"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="ISIMGRO"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="MINDEPTH"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="IEXPORT"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="IPERIOD"
 BAT(I)%LVL(14)="(DEP)"; BAT(I)%KEY(14)="SDATE"
 BAT(I)%LVL(15)="(DEP)"; BAT(I)%KEY(15)="EDATE"
 BAT(I)%LVL(16)="(DEP)"; BAT(I)%KEY(16)="DDATE"

 END SUBROUTINE BATCHINIT_ISGGRID

 !###======================================================================
 SUBROUTINE BATCHINIT_ISGADDCROSSSECTION(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="ISGADDCROSSSECTION"; BAT(I)%N=  8
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="ISGFILE_IN"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="CROSS_PNTR"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="CROSS_BATH"
 BAT(I)%LVL(4)="(DEP)"; BAT(I)%KEY(4)="CELL_SIZE"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="CROSSSECTION_IN"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="WIDTH_IDF"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="MAXDIST"
 BAT(I)%LVL(8)="(COMP)"; BAT(I)%KEY(8)="ISGFILE_OUT"

 END SUBROUTINE BATCHINIT_ISGADDCROSSSECTION

 !###======================================================================
 SUBROUTINE BATCHINIT_ISGSIMPLIFY(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="ISGSIMPLIFY"; BAT(I)%N=  4
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="ISGFILE_IN"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="ZTOLERANCE"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="NODATA"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="ISGFILE_OUT"

 END SUBROUTINE BATCHINIT_ISGSIMPLIFY

 !###======================================================================
 SUBROUTINE BATCHINIT_DINO2IPF(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="DINO2IPF"; BAT(I)%N=  4
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="CSVFILE"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="WINDOW"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="GENFILE"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="IPFFILE"

 END SUBROUTINE BATCHINIT_DINO2IPF

 !###======================================================================
 SUBROUTINE BATCHINIT_IDFTIMESERIE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IDFTIMESERIE"; BAT(I)%N=  7
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="IPF1"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="IPF2"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="SDATE"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="EDATE"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="ILAY"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="SOURCEDIR"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="LABELCOL"

 END SUBROUTINE BATCHINIT_IDFTIMESERIE

 !###======================================================================
 SUBROUTINE BATCHINIT_BMPTILING(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="BMPTILING"; BAT(I)%N=  2
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="BMPFILE"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="OUTPUTFOLDER"

 END SUBROUTINE BATCHINIT_BMPTILING

 !###======================================================================
 SUBROUTINE BATCHINIT_CREATESUBMODEL(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="CREATESUBMODEL"; BAT(I)%N=  4
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="DSIZE"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="CSIZE"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="IBOUND"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="SUBMODELFILE"

 END SUBROUTINE BATCHINIT_CREATESUBMODEL

 !###======================================================================
 SUBROUTINE BATCHINIT_CREATESOF(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="CREATESOF"; BAT(I)%N=  6
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="LEVELIDF"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="SOFIDF"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="IFLOW"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="DTERM"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="POINTERIDF"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="WINDOW"

 END SUBROUTINE BATCHINIT_CREATESOF

 !###======================================================================
 SUBROUTINE BATCHINIT_DRNSURF(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="DRNSURF"; BAT(I)%N= 11
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="SURFIDF"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="PNTRIDF"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="LUSEIDF"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="NLUSE"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="ILUSE"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="TDRAINAGE"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="TSURFLEVEL"
 BAT(I)%LVL(8)="(COMP)"; BAT(I)%KEY(8)="PERCENTILE"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="WINDOW"
 BAT(I)%LVL(10)="(COMP)"; BAT(I)%KEY(10)="CELL_SIZE"
 BAT(I)%LVL(11)="(COMP)"; BAT(I)%KEY(11)="OUTIDF"

 END SUBROUTINE BATCHINIT_DRNSURF

 !###======================================================================
 SUBROUTINE BATCHINIT_SOLID(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I
 
 BAT(I)%CFUNC="SOLID"; BAT(I)%N=  33
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NLAY"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="OUTPUTFOLDER"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="\MASK"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="\FFRAC"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="\CFRAC"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="MDL_TOP_{i=1,NLAY}"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="MDL_BOT_{i=1,NLAY}"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="MDL_KD_{i=1,NLAY}"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="MDL_VC_{i=1,NLAY}"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="MDL_KHV_{i=1,NLAY}"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="MDL_KVA_{i=1,NLAY}"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="MDL_KVV_{i=1,NLAY}"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="MDL_KDFRAC_{i=1,NLAY}"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="MDL_CFRAC_{i=1,NLAY}" 
 BAT(I)%LVL(15)="(COMP)"; BAT(I)%KEY(15)="TOP_L{i=1,NLAY}"
 BAT(I)%LVL(16)="(OPT)"; BAT(I)%KEY(16)="ICLC_TL{i=1,NLAY}"
 BAT(I)%LVL(17)="(COMP)"; BAT(I)%KEY(17)="BOT_L{i=1,NLAY}"
 BAT(I)%LVL(18)="(OPT)"; BAT(I)%KEY(18)="ICLC_BL{i=1,NLAY}"
 BAT(I)%LVL(19)="(COMP)"; BAT(I)%KEY(19)="IMASK"
 BAT(I)%LVL(20)="(OPT)"; BAT(I)%KEY(20)="ZOFFSET"
 BAT(I)%LVL(21)="(COMP)"; BAT(I)%KEY(21)="IHYPO"
 BAT(I)%LVL(22)="(OPT)"; BAT(I)%KEY(22)="DZ(.)"
 BAT(I)%LVL(23)="(OPT)"; BAT(I)%KEY(23)="IMIDELEV"
 BAT(I)%LVL(24)="(OPT)"; BAT(I)%KEY(24)="IBNDCHK"
 BAT(I)%LVL(25)="(OPT)"; BAT(I)%KEY(25)="HCLOSE"
 BAT(I)%LVL(26)="(OPT)"; BAT(I)%KEY(26)="MICNVG"
 BAT(I)%LVL(27)="(COMP)"; BAT(I)%KEY(27)="ICKDC"
 BAT(I)%LVL(28)="(COMP)"; BAT(I)%KEY(28)="REGISTOP"
 BAT(I)%LVL(29)="(COMP)"; BAT(I)%KEY(29)="REGISBOT"
 BAT(I)%LVL(30)="(COMP)"; BAT(I)%KEY(30)="REGISKHV"
 BAT(I)%LVL(31)="(COMP)"; BAT(I)%KEY(31)="REGISKVV" 
 BAT(I)%LVL(32)="(OPT)"; BAT(I)%KEY(32)="WINDOW"
 BAT(I)%LVL(33)="(OPT)"; BAT(I)%KEY(33)="CELL_SIZE"

 END SUBROUTINE BATCHINIT_SOLID
 
 !###======================================================================
 SUBROUTINE BATCHINIT_IPFSPOTIFY(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IPFSPOTIFY"; BAT(I)%N=  14
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))
 
 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="IPFFILE_IN"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="IPFFILE_OUT"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="IXCOL"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="IYCOL"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="IFCOL"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="IZ1COL"
 BAT(I)%LVL(7)="(OPT)"; BAT(I)%KEY(7)="IZ2COL"
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="ILCOL" 
 BAT(I)%LVL(9)="(COMP)"; BAT(I)%KEY(9)="OUTPUTFOLDER"
 BAT(I)%LVL(10)="(COMP)"; BAT(I)%KEY(10)="NLAY"
 BAT(I)%LVL(11)="(COMP)"; BAT(I)%KEY(11)="TOP_L{i=1,NLAY}"
 BAT(I)%LVL(12)="(COMP)"; BAT(I)%KEY(12)="BOT_L{i=1,NLAY}"
 BAT(I)%LVL(13)="(COMP)"; BAT(I)%KEY(13)="REGISTOP"
 BAT(I)%LVL(14)="(COMP)"; BAT(I)%KEY(14)="REGISBOT"
  
 END SUBROUTINE BATCHINIT_IPFSPOTIFY
 
 !###======================================================================
 SUBROUTINE BATCHINIT_ASSIGNWELL(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="ASSIGNWELL"; BAT(I)%N=  10
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="IPFFILE_IN"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="IXCOL"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="IYCOL"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="IDCOL"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="IZ1COL"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="IZ2COL"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="NFORMATIONS"
 BAT(I)%LVL(8)="(COMP)"; BAT(I)%KEY(8)="FORMATIONS{i=1,NFORMATIONS}"
 BAT(I)%LVL(9)="(COMP)"; BAT(I)%KEY(9)="TOP_L{i=1,NLAY}"
 BAT(I)%LVL(10)="(COMP)"; BAT(I)%KEY(10)="BOT_L{i=1,NLAY}"
 
 END SUBROUTINE BATCHINIT_ASSIGNWELL
 
 !###======================================================================
 SUBROUTINE BATCHINIT_GEN2ISG(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="GEN2ISG"; BAT(I)%N=  21
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NLAY"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="TOPIDF{i=1,NLAY}"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="BOTIDF{i=1,NLAY}"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="KDIDF{i=1,NLAY}"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="CIDF{i=1,NLAY}"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="NIPF"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="IPF{i=1,NIPF}" 
 BAT(I)%LVL(8)="(OPT)"; BAT(I)%KEY(8)="IXCOL"
 BAT(I)%LVL(9)="(OPT)"; BAT(I)%KEY(9)="IYCOL"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="IQCOL"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="ITCOL"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="IBCOL"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="ISS"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="SDATE"
 BAT(I)%LVL(15)="(OPT)"; BAT(I)%KEY(15)="EDATE"
 BAT(I)%LVL(16)="(OPT)"; BAT(I)%KEY(16)="MAXC"
 BAT(I)%LVL(17)="(OPT)"; BAT(I)%KEY(17)="MINKH"
 BAT(I)%LVL(18)="(OPT)"; BAT(I)%KEY(18)="ICLAY"
 BAT(I)%LVL(19)="(OPT)"; BAT(I)%KEY(19)="HNODATA"
 BAT(I)%LVL(20)="(OPT)"; BAT(I)%KEY(20)="FNODATA"
 BAT(I)%LVL(21)="(OPT)"; BAT(I)%KEY(21)="IFRAC"
  
 END SUBROUTINE BATCHINIT_GEN2ISG
 
 !###======================================================================
 SUBROUTINE BATCHINIT_GEOTOP(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="GEOTOP"; BAT(I)%N=  14
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="RESULTFOLDER"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="NLAYG"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="KVG_L{i=1,NLAYG}"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="KHG_L{i=1,NLAYG}"
 BAT(I)%LVL(5)="(COMP)"; BAT(I)%KEY(5)="NLAYM"
 BAT(I)%LVL(6)="(COMP)"; BAT(I)%KEY(6)="IBM_L{i=1,NLAYM}"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="SHM_L{i=1,NLAYM}"
 BAT(I)%LVL(8)="(COMP)"; BAT(I)%KEY(8)="TPM_L{i=1,NLAYM}"
 BAT(I)%LVL(9)="(COMP)"; BAT(I)%KEY(9)="BTM_L{i=1,NLAYM}"
 BAT(I)%LVL(10)="(COMP)"; BAT(I)%KEY(10)="KHM_L{i=1,NLAYM}"
 BAT(I)%LVL(11)="(COMP)"; BAT(I)%KEY(11)="KAM_L{i=1,NLAYM}"
 BAT(I)%LVL(12)="(COMP)"; BAT(I)%KEY(12)="KVM_L{i=1,NLAYM}"
 BAT(I)%LVL(13)="(OPT)"; BAT(I)%KEY(13)="WINDOW"
 BAT(I)%LVL(14)="(OPT)"; BAT(I)%KEY(14)="CELLSIZE"
  
 END SUBROUTINE BATCHINIT_GEOTOP
 
 !###======================================================================
 SUBROUTINE BATCHINIT_GEF2IPF(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="GEF2IPF"; BAT(I)%N=  4
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="GEFDIR"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="IPFFILE"
 BAT(I)%LVL(3)="(OPT)"; BAT(I)%KEY(3)="WINDOW"
 BAT(I)%LVL(4)="(OPT)"; BAT(I)%KEY(4)="GENFILE"
  
 END SUBROUTINE BATCHINIT_GEF2IPF

 !###======================================================================
 SUBROUTINE BATCHINIT_CREATEIZONE(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="CREATEIZONE"; BAT(I)%N=  9
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="OFOLDER"
 BAT(I)%LVL(2)="(COMP)"; BAT(I)%KEY(2)="PFOLDER"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="NLAY"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="MINF"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="IZONEOFFSET"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="IGROUPOFFSET"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="NFORMATIONS"
 BAT(I)%LVL(8)="(COMP)"; BAT(I)%KEY(8)="FORMATIONS{i=1,NFORMATIONS}"  
 BAT(I)%LVL(9)="(COMP)"; BAT(I)%KEY(9)="TPARAMETER"
 
 END SUBROUTINE BATCHINIT_CREATEIZONE

 !###======================================================================
 SUBROUTINE BATCHINIT_IPFRESIDUAL(I)
 !###======================================================================
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: I

 BAT(I)%CFUNC="IPFRESIDUAL"; BAT(I)%N=  13
 ALLOCATE(BAT(I)%LVL(BAT(I)%N),BAT(I)%KEY(BAT(I)%N))

 BAT(I)%LVL(1)="(COMP)"; BAT(I)%KEY(1)="NIPF"
 BAT(I)%LVL(2)="(OPT)"; BAT(I)%KEY(2)="POINTERIDF"
 BAT(I)%LVL(3)="(COMP)"; BAT(I)%KEY(3)="NZONE"
 BAT(I)%LVL(4)="(COMP)"; BAT(I)%KEY(4)="IZONE{i=1,NZONE}"
 BAT(I)%LVL(5)="(OPT)"; BAT(I)%KEY(5)="ICOLLECT"
 BAT(I)%LVL(6)="(OPT)"; BAT(I)%KEY(6)="HNODATA"
 BAT(I)%LVL(7)="(COMP)"; BAT(I)%KEY(7)="IPFFILE{i=1,NIPF}"
 BAT(I)%LVL(8)="(COMP)"; BAT(I)%KEY(8)="WTYPE{i=1,NIPF}"  
 BAT(I)%LVL(9)="(COMP)"; BAT(I)%KEY(9)="IWCOL{i=1,NIPF}"
 BAT(I)%LVL(10)="(OPT)"; BAT(I)%KEY(10)="IHCOL{i=1,NIPF}"
 BAT(I)%LVL(11)="(OPT)"; BAT(I)%KEY(11)="IMCOL{i=1,NIPF}"
 BAT(I)%LVL(12)="(OPT)"; BAT(I)%KEY(12)="ILCOL{i=1,NIPF}"
 BAT(I)%LVL(13)="(COMP)"; BAT(I)%KEY(13)="OUTNAME"
 
 END SUBROUTINE BATCHINIT_IPFRESIDUAL

END MODULE MOD_BATCH_PAR