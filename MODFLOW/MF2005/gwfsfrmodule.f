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
      MODULE GWFSFRMODULE
        CHARACTER(LEN=64), SAVE :: Version_sfr
        DOUBLE PRECISION,PARAMETER :: NEARZERO=1.0D-30
        DOUBLE PRECISION,SAVE :: THETAB, FLUXB, FLUXHLD2
        REAL,PARAMETER :: CLOSEZERO=1.0E-15
        INTEGER, SAVE :: Nfoldflbt, NUMTAB, MAXVAL
!        INTEGER,SAVE,                 POINTER:: IDVFLG   !diverison recharge is active flag
!        INTEGER,SAVE,  DIMENSION(:),  POINTER:: DVRCH   !(diverted recharge flag; then reharge cell count)
!        INTEGER,SAVE,  DIMENSION(:,:,:),POINTER:: DVRCELL !(store cells to apply diverted recharge)
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: RECHSAVE  !(store original recharge values)
!        REAL,   SAVE,  DIMENSION(:,:),POINTER:: DVRPERC  !(Percentage of diversion applied to each cell)
!        REAL,   SAVE,  DIMENSION(:),POINTER:: DVEFF  !(store efficiency factor)
        INTEGER,SAVE,POINTER:: NSS, NSTRM, NSFRPAR, ISTCB1, ISTCB2
        INTEGER,SAVE,POINTER:: IUZT, MAXPTS, IRTFLG, NUMTIM, NSEGDIM
        INTEGER,SAVE,POINTER:: ISFROPT, NSTRAIL, ISUZN, NSFRSETS
        INTEGER,SAVE,POINTER:: NUZST, NSTOTRL, NUMAVE
        INTEGER,SAVE,POINTER:: ITMP, IRDFLG, IPTFLG, NP
        REAL,   SAVE,POINTER:: CONST, DLEAK, WEIGHT, SFRRATIN, SFRRATOUT
        REAL   ,SAVE,POINTER:: FLWTOL, STRMDELSTOR_CUM, STRMDELSTOR_RATE
        DOUBLE PRECISION,SAVE,POINTER:: TOTSPFLOW
        INTEGER,SAVE,  DIMENSION(:),  POINTER:: IOTSG, NSEGCK
        INTEGER,SAVE,  DIMENSION(:),  POINTER:: ITRLSTH
        INTEGER,SAVE,  DIMENSION(:,:),POINTER:: ISEG, IDIVAR, ISTRM
        INTEGER,SAVE,  DIMENSION(:,:),POINTER:: LTRLIT, LTRLST
        INTEGER,SAVE,  DIMENSION(:,:),POINTER:: ITRLIT, ITRLST, NWAVST
        REAL,   SAVE,  DIMENSION(:),  POINTER:: STRIN, STROUT, FXLKOT
        REAL,   SAVE,  DIMENSION(:),  POINTER:: UHC, SGOTFLW, DVRSFLW
        REAL,   SAVE,  DIMENSION(:),  POINTER:: SFRUZBD
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: SEG, STRM, SFRQ
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: HWDTH, HWTPRM
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: QSTAGE, XSEC
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: AVDPT, AVWAT, WAT1
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: CONCQ, CONCRUN, CONCPPT
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: TABFLOW, TABTIME   !Reading Spedified inflow
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: FNETSEEP           !writing net seepage in UZF
        INTEGER,SAVE,  DIMENSION(:,:),POINTER:: ISFRLIST           !Reading Spedified inflow
        DOUBLE PRECISION,SAVE,DIMENSION(:),  POINTER:: THTS,THTR,EPS
        DOUBLE PRECISION,SAVE,DIMENSION(:),  POINTER:: FOLDFLBT, THTI
        DOUBLE PRECISION,SAVE,DIMENSION(:),  POINTER:: SUMLEAK,SUMRCH
        DOUBLE PRECISION,SAVE,DIMENSION(:),  POINTER:: HLDSFR
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: UZFLWT, UZSTOR
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: UZWDTH, UZSEEP
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: DELSTOR, WETPER
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: UZDPIT, UZDPST
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: UZTHIT, UZTHST
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: UZSPIT, UZSPST
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: UZFLIT, UZFLST
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: UZOLSFLX, HSTRM
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: QSTRM, SLKOTFLW
        DOUBLE PRECISION,SAVE,DIMENSION(:,:),POINTER:: DLKOTFLW,DLKSTAGE
      TYPE GWFSFRTYPE
!        INTEGER,                      POINTER:: IDVFLG   !diverison recharge is active flag
!        INTEGER,       DIMENSION(:),  POINTER:: DVRCH      !Diversions to recharge
!        INTEGER,       DIMENSION(:,:,:),  POINTER:: DVRCELL  !Diversions to recharge
        REAL,          DIMENSION(:,:),POINTER:: RECHSAVE  !Diversions to recharge
!        REAL,          DIMENSION(:,:),POINTER:: DVRPERC  !Diversions to recharge
!        REAL,          DIMENSION(:),POINTER:: DVEFF  !Diversions to recharge
        INTEGER,     POINTER:: NSS, NSTRM, NSFRPAR, ISTCB1, ISTCB2
        INTEGER,     POINTER:: IUZT, MAXPTS, IRTFLG, NUMTIM, NSEGDIM
        INTEGER,     POINTER:: ISFROPT, NSTRAIL, ISUZN, NSFRSETS
        INTEGER,     POINTER:: NUZST, NSTOTRL, NUMAVE
        INTEGER,     POINTER:: ITMP, IRDFLG, IPTFLG, NP
        REAL,        POINTER:: CONST, DLEAK, WEIGHT, SFRRATIN, SFRRATOUT
        REAL,        POINTER:: FLWTOL, STRMDELSTOR_CUM, STRMDELSTOR_RATE
        DOUBLE PRECISION, POINTER:: TOTSPFLOW
        INTEGER,       DIMENSION(:),  POINTER:: IOTSG, NSEGCK
        INTEGER,       DIMENSION(:),  POINTER:: ITRLSTH
        INTEGER,       DIMENSION(:,:),POINTER:: ISEG, IDIVAR, ISTRM
        INTEGER,       DIMENSION(:,:),POINTER:: LTRLIT, LTRLST
        INTEGER,       DIMENSION(:,:),POINTER:: ITRLIT, ITRLST, NWAVST
        REAL,          DIMENSION(:),  POINTER:: STRIN, STROUT, FXLKOT
        REAL,          DIMENSION(:),  POINTER:: UHC, SGOTFLW, DVRSFLW
        REAL,          DIMENSION(:),  POINTER:: SFRUZBD
        REAL,          DIMENSION(:,:),POINTER:: SEG, STRM, SFRQ
        REAL,          DIMENSION(:,:),POINTER:: HWDTH, HWTPRM
        REAL,          DIMENSION(:,:),POINTER:: QSTAGE, XSEC
        REAL,          DIMENSION(:,:),POINTER:: AVDPT, AVWAT, WAT1
        REAL,          DIMENSION(:,:),POINTER:: CONCQ, CONCRUN, CONCPPT
        REAL,          DIMENSION(:,:),POINTER:: TABFLOW, TABTIME  ! Reading SPecified inflow
        REAL,          DIMENSION(:,:),POINTER:: FNETSEEP          !writing net seepage in UZF
        INTEGER,       DIMENSION(:,:),POINTER:: ISFRLIST
        DOUBLE PRECISION,     DIMENSION(:),  POINTER:: THTS,THTR,EPS
        DOUBLE PRECISION,     DIMENSION(:),  POINTER:: FOLDFLBT, THTI
        DOUBLE PRECISION,     DIMENSION(:),  POINTER:: SUMLEAK, SUMRCH
        DOUBLE PRECISION,     DIMENSION(:),  POINTER:: HLDSFR
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: UZFLWT, UZSTOR
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: UZWDTH, UZSEEP
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: DELSTOR, WETPER
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: UZDPIT, UZDPST
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: UZTHIT, UZTHST
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: UZSPIT, UZSPST
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: UZFLIT, UZFLST
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: UZOLSFLX, HSTRM
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: QSTRM, SLKOTFLW
        DOUBLE PRECISION,     DIMENSION(:,:),POINTER:: DLKOTFLW,DLKSTAGE
      END TYPE
      TYPE(GWFSFRTYPE), SAVE:: GWFSFRDAT(10)
      END MODULE GWFSFRMODULE
