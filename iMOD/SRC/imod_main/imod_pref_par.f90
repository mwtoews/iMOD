!!  Copyright (C) Stichting Deltares, 2005-2018.
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
MODULE MOD_PREF_PAR

INTEGER,PARAMETER :: MAXPREF =31     !## max. preferences
CHARACTER(LEN=256) :: PREFDIR
CHARACTER(LEN=256),DIMENSION(MAXPREF) :: PREFVAL
CHARACTER(LEN=25),DIMENSION(MAXPREF) :: PREF
DATA PREF/'USER', &          ! 1
          'TOP25',&          ! 2
          'VECTOR',&         ! 3
          'HELPFILE',&       ! 4
          'DBASE',&          ! 5
          'IRDBASE',&        ! 6
          'TAGS',&           ! 7
          'MODFLOW',&        ! 8
          '---empty---', &   ! 9
          'PUMPINGTOOL',&    !10
          'NORTHARROW',&     !11
          '---empty---', &   !12
          'ACROBATREADER', & !13
          'LANDUSE', &       !14
          'HLPSOIL', &       !15
          'NDT', &           !16
          'NDT_LUT', &       !17
          'ABIOT_LUT', &     !18
          'RFCSOIL', &       !19
          'RFC_LUT', &       !20
          'HLP_DRY', &       !21
          'HLP_WET', &       !22
          'URBAN_RANGE', &   !23
          'CROP_COSTS', &    !24
          '7ZIP', &          !25
          'SUBSURFEXDBASE', &!26
          'PLUGIN1', &       !27
          'PLUGIN2', &       !28
          'FFMPEG', &        !29 used to create the movie (and to play if not other available)
          'FFPLAY', &        !30 used to play the movie
          'VLCPLAYER'/       !31

END MODULE MOD_PREF_PAR

