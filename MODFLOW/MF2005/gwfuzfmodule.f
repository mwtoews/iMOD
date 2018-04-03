c   Copyright (C) Stichting Deltares, 2005-2017.
c
c   This file is part of iMOD.
c
c   This program is free software: you can redistribute it and/or modify
c   it under the terms of the GNU General Public License as published by
c   the Free Software Foundation, either version 3 of the License, or
c   (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   You should have received a copy of the GNU General Public License
c   along with this program.  If not, see <http://www.gnu.org/licenses/>.
c
c   Contact: imod.support@deltares.nl
c   Stichting Deltares
c   P.O. Box 177
c   2600 MH Delft, The Netherlands.
c
c   iMod is partly based on the USGS MODFLOW2005 source code;
c   for iMOD the USGS MODFLOW2005 source code has been expanded
c   and extensively modified by Stichting Deltares.
c   The original USGS MODFLOW2005 source code can be downloaded from the USGS
c   website http://www.usgs.gov/. The original MODFLOW2005 code incorporated
c   in this file is covered by the USGS Software User Rights Notice;
c   you should have received a copy of this notice along with this program.
c   If not, see <http://water.usgs.gov/software/help/notice/>.
      MODULE GWFUZFMODULE
        CHARACTER(LEN=64) :: Version_uzf
        REAL,PARAMETER :: CLOSEZERO=1.0E-15
        DOUBLE PRECISION,PARAMETER :: NEARZERO=1.0D-30
        DOUBLE PRECISION,PARAMETER :: ZEROD15=1.0D-15, ZEROD9=1.0D-09
        DOUBLE PRECISION,PARAMETER :: ZEROD6=1.0D-06
        DOUBLE PRECISION,PARAMETER :: ZEROD7=1.0D-07
        INTEGER         ,PARAMETER :: IRUNBIG = 10000
        INTEGER,SAVE,POINTER :: NUMCELLS, TOTCELLS, Iseepsupress, IPRCNT
        INTEGER,SAVE,POINTER :: ITHTIFLG, ITHTRFLG
        DOUBLE PRECISION,SAVE :: THETAB, FLUXB, FLUXHLD2
        DOUBLE PRECISION,SAVE,DIMENSION(:),POINTER :: CHECKTIME
        INTEGER,SAVE,DIMENSION(:),POINTER :: MORE
        INTEGER,SAVE,DIMENSION(:,:),POINTER :: LAYNUM
        INTEGER,SAVE,POINTER   ::NUZTOP, IUZFOPT, IRUNFLG, IETFLG, IUZM
        INTEGER,SAVE,POINTER   ::IUZFCB1, IUZFCB2, NTRAIL, NWAV, NSETS
        INTEGER,SAVE,POINTER   ::IUZFB22, IUZFB11
        INTEGER,SAVE,POINTER   ::NUZGAG, NUZGAGAR, NUZCL, NUZRW, IGSFLOW
        INTEGER,SAVE,POINTER   ::RTSOLUTE, IETBUD
        INTEGER,SAVE,  DIMENSION(:),    POINTER :: ITRLSTH
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: IRUNBND, IUZFBND
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: IUZLIST, NWAVST
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: IUZHOLD
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: LTRLST, ITRLST
        INTEGER,SAVE,  DIMENSION(:),  POINTER :: LTRLIT, ITRLIT
        REAL,   SAVE,POINTER   ::TOTRUNOFF, SURFDEP
        REAL,   SAVE,  DIMENSION(:),    POINTER :: FBINS
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: SEEPOUT, EXCESPP, VKS
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: AIR_ENTRY, H_ROOT
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: REJ_INF
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: TO_CFP
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: EPS, THTS, THTI, THTR
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: PETRATE, ROOTDPTH
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: WCWILT, FINF
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: UZFETOUT, GWET
        REAL,   SAVE,  DIMENSION(:,:),  POINTER :: FNETEXFIL, CUMGWET
        DOUBLE PRECISION, SAVE, DIMENSION(:),  POINTER :: CUMUZVOL 
        DOUBLE PRECISION, SAVE, DIMENSION(:),  POINTER :: UZTSRAT
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZFLWT, UZSTOR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZDPST, UZTHST
        DOUBLE PRECISION, SAVE, DIMENSION(:),POINTER :: UZDPIT, UZTHIT
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZSPST, UZFLST
        DOUBLE PRECISION, SAVE, DIMENSION(:),POINTER :: UZSPIT, UZFLIT
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: DELSTOR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: UZOLSFLX
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: HLDUZF
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: RTSOLWC
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: RTSOLFL
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),POINTER :: RTSOLDS
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:),POINTER :: UZTOTBAL
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:),POINTER :: GRIDSTOR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:),POINTER :: GRIDET
      TYPE GWFUZFTYPE
        INTEGER,     POINTER   ::NUZTOP, IUZFOPT, IRUNFLG, IETFLG, IUZM
        INTEGER,     POINTER   ::IUZFCB1, IUZFCB2, NTRAIL, NWAV, NSETS
        INTEGER,     POINTER   ::IUZFB22, IUZFB11
        INTEGER,     POINTER   ::NUZGAG, NUZGAGAR, NUZCL, NUZRW, IGSFLOW
        INTEGER,     POINTER   ::RTSOLUTE, IETBUD
        INTEGER,     POINTER   ::NUMCELLS, TOTCELLS, Iseepsupress,IPRCNT
        INTEGER,     POINTER   ::ITHTIFLG, ITHTRFLG
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: CHECKTIME
        INTEGER,DIMENSION(:),POINTER :: MORE
        INTEGER,DIMENSION(:,:),POINTER :: LAYNUM
        INTEGER,       DIMENSION(:),    POINTER :: ITRLSTH
        INTEGER,       DIMENSION(:,:),  POINTER :: IRUNBND, IUZFBND
        INTEGER,       DIMENSION(:,:),  POINTER :: IUZLIST, NWAVST
        INTEGER,       DIMENSION(:,:),  POINTER :: IUZHOLD
        INTEGER,       DIMENSION(:,:),  POINTER :: LTRLST, ITRLST
        INTEGER,       DIMENSION(:),  POINTER :: LTRLIT, ITRLIT
        REAL,          POINTER            ::TOTRUNOFF, SURFDEP
        REAL,          DIMENSION(:),    POINTER :: FBINS
        REAL,          DIMENSION(:,:),  POINTER :: SEEPOUT, EXCESPP, VKS
        REAL,          DIMENSION(:,:),  POINTER :: AIR_ENTRY, H_ROOT
        REAL,          DIMENSION(:,:),  POINTER :: REJ_INF
        REAL,          DIMENSION(:,:),  POINTER :: TO_CFP
        REAL,          DIMENSION(:,:),  POINTER :: EPS, THTS, THTI, THTR
        REAL,          DIMENSION(:,:),  POINTER :: PETRATE, ROOTDPTH
        REAL,          DIMENSION(:,:),  POINTER :: WCWILT, FINF
        REAL,          DIMENSION(:,:),  POINTER :: UZFETOUT, GWET
        REAL,          DIMENSION(:,:),  POINTER :: FNETEXFIL, CUMGWET
        DOUBLE PRECISION,       DIMENSION(:),  POINTER :: CUMUZVOL
        DOUBLE PRECISION,       DIMENSION(:),  POINTER :: UZTSRAT
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZFLWT, UZSTOR
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZDPST, UZTHST
        DOUBLE PRECISION,       DIMENSION(:),POINTER :: UZDPIT, UZTHIT
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZSPST, UZFLST
        DOUBLE PRECISION,       DIMENSION(:),POINTER :: UZSPIT, UZFLIT
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: DELSTOR
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: UZOLSFLX
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: HLDUZF
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: RTSOLWC
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: RTSOLFL
        DOUBLE PRECISION,       DIMENSION(:,:),POINTER :: RTSOLDS
        DOUBLE PRECISION,       DIMENSION(:,:,:),POINTER :: UZTOTBAL
        DOUBLE PRECISION,       DIMENSION(:,:,:),POINTER :: GRIDSTOR
        DOUBLE PRECISION,       DIMENSION(:,:,:),POINTER :: GRIDET
      END TYPE
      TYPE(GWFUZFTYPE), SAVE:: GWFUZFDAT(10)
      END MODULE GWFUZFMODULE
