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

      MODULE M_MF2005_IU

      IMPLICIT NONE

      INTEGER, PARAMETER :: IUBCF6 =  1, IUWEL  =  2, IUDRN  =  3
      INTEGER, PARAMETER :: IURIV  =  4, IUEVT  =  5
      INTEGER, PARAMETER :: IUGHB  =  7, IURCH  =  8, IUSIP  =  9
      INTEGER, PARAMETER :: IUDE4  = 10,              IUOC   = 12
      INTEGER, PARAMETER :: IUPCG  = 13, IULMG  = 14, IUGWT  = 15
      INTEGER, PARAMETER :: IUFHB  = 16, IURES  = 17, IUSTR  = 18
      INTEGER, PARAMETER :: IUIBS  = 19, IUCHD  = 20, IUHFB6 = 21
      INTEGER, PARAMETER :: IULAK  = 22, IULPF  = 23, IUDIS  = 24
      INTEGER, PARAMETER ::              IUPVAL = 26
      INTEGER, PARAMETER :: IUHOB  = 28
      INTEGER, PARAMETER :: IUZONE = 31, IUMULT = 32, IUDROB = 33
      INTEGER, PARAMETER :: IURVOB = 34, IUGBOB = 35, IUSTOB = 36
      INTEGER, PARAMETER :: IUHUF2 = 37, IUCHOB = 38, IUETS  = 39
      INTEGER, PARAMETER :: IUDRT  = 40,              IUGMG  = 42
      INTEGER, PARAMETER :: IUHYD  = 43, IUSFR  = 44
      INTEGER, PARAMETER :: IUGAGE = 46, IULVDA = 47
      INTEGER, PARAMETER :: IULMT6 = 49, IUMNW2 = 50, IUMNWI = 51
      INTEGER, PARAMETER :: IUMNW1 = 52, IUKDEP = 53, IUSUB  = 54
      INTEGER, PARAMETER :: IUUZF  = 55, IUGWM  = 56, IUSWT  = 57
      INTEGER, PARAMETER :: IUCFP  = 58, IUPWT  = 59, IUVDF  = 60
      INTEGER, PARAMETER ::              IUSCR  = 62, IUNRS  = 63
      INTEGER, PARAMETER :: IUDXC  = 64, IUANI  = 65, IUPKS  = 66
      INTEGER, PARAMETER ::              IUMET  = 68

      END MODULE M_MF2005_IU

      MODULE GLOBAL
        logical, save :: lipest = .false.                               ! IPEST
        PARAMETER(NIUNIT=100)
        INTEGER, SAVE, POINTER    ::NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
        INTEGER, SAVE, POINTER    ::ITMUNI,LENUNI,IXSEC,ITRSS,INBAS
        INTEGER, SAVE, POINTER    ::IFREFM,NODES,IOUT,MXITER
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::IUNIT(:)
        DOUBLE PRECISION, SAVE, DIMENSION(:,:,:), POINTER ::HNEW
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LBOTM
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYCBD
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYHDT
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LAYHDS
        REAL,    SAVE,    DIMENSION(:),     POINTER ::PERLEN
        character(len=14),save,dimension(:),pointer ::npertxt
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::NSTP
        REAL,    SAVE,    DIMENSION(:),     POINTER ::TSMULT
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::ISSFLG
        REAL(KIND=8),SAVE,    DIMENSION(:), POINTER ::DELR
        REAL(KIND=8),SAVE,    DIMENSION(:), POINTER ::DELC
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::BOTM
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::HOLD
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IBOUND
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CR
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CC
        real,    save,    dimension(:,:,:), pointer ::kdsv              ! ILAY_ZERO
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CV
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::HCOF
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::RHS
        REAL(kind=8),SAVE,    DIMENSION(:,:,:), POINTER ::BUFF
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::STRT
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::DDREF
        INTEGER, SAVE,    DIMENSION(:,:,:), POINTER ::IACTCELL          ! PKS
      TYPE GLOBALTYPE
        INTEGER,POINTER    :: NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD
        INTEGER,POINTER    :: ITMUNI,LENUNI,IXSEC,ITRSS,INBAS
        INTEGER,POINTER    :: IFREFM,NODES,IOUT,MXITER
        INTEGER,    DIMENSION(:),     POINTER ::IUNIT
        REAL(KIND=8),DIMENSION(:,:,:), POINTER ::HNEW
        INTEGER,    DIMENSION(:),     POINTER ::LBOTM
        INTEGER,    DIMENSION(:),     POINTER ::LAYCBD
        INTEGER,    DIMENSION(:),     POINTER ::LAYHDT
        INTEGER,    DIMENSION(:),     POINTER ::LAYHDS
        REAL,       DIMENSION(:),     POINTER ::PERLEN
        CHARACTER(LEN=14),DIMENSION(:),POINTER :: NPERTXT
        INTEGER,    DIMENSION(:),     POINTER ::NSTP
        REAL,       DIMENSION(:),     POINTER ::TSMULT
        INTEGER,    DIMENSION(:),     POINTER ::ISSFLG
        REAL(KIND=8),DIMENSION(:),    POINTER ::DELR
        REAL(KIND=8),DIMENSION(:),    POINTER ::DELC
        REAL,       DIMENSION(:,:,:), POINTER ::BOTM
        REAL,       DIMENSION(:,:,:), POINTER ::HOLD
        INTEGER,    DIMENSION(:,:,:), POINTER ::IBOUND
        REAL,       DIMENSION(:,:,:), POINTER ::CR
        REAL,       DIMENSION(:,:,:), POINTER ::CC
        REAL,       DIMENSION(:,:,:), POINTER ::KDSV                    ! PWT
        REAL,       DIMENSION(:,:,:), POINTER ::CV
        REAL,       DIMENSION(:,:,:), POINTER ::HCOF
        REAL,       DIMENSION(:,:,:), POINTER ::RHS
        REAL(kind=8),DIMENSION(:,:,:), POINTER ::BUFF
        REAL,       DIMENSION(:,:,:), POINTER ::STRT
        REAL,       DIMENSION(:,:,:), POINTER ::DDREF
        INTEGER,    DIMENSION(:,:,:), POINTER ::IACTCELL                ! PKS
      END TYPE GLOBALTYPE
      TYPE(GLOBALTYPE),SAVE ::GLOBALDAT(10)
      END MODULE GLOBAL
