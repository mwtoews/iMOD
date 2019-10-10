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

      module gwfmetmodule

         implicit none

         character(len=300), save, pointer :: runcomment
         character(len=300), save, pointer :: coord_descr

!         integer, save, pointer :: idate_save !## 0=begindate 1=enddate

         integer, save, pointer :: time_syear
         integer, save, pointer :: time_smonth
         integer, save, pointer :: time_sday
         integer, save, pointer :: time_shour
         integer, save, pointer :: time_sminute
         integer, save, pointer :: time_ssecond
         real(kind=8), save, pointer :: time_sjd ! start
         real(kind=8), save, pointer :: time_cjd ! current
         character(len=300), save, pointer :: time_cstring
         character(len=300), save, pointer :: time_ostring
         real(kind=8), save, pointer :: coord_xll
         real(kind=8), save, pointer :: coord_yll
         real(kind=8), save, pointer :: gcoord_xll
         real(kind=8), save, pointer :: gcoord_yll
         real(kind=8), save, pointer :: coord_xur
         real(kind=8), save, pointer :: coord_yur
         real(kind=8), save, pointer :: gcoord_xur
         real(kind=8), save, pointer :: gcoord_yur
         real(kind=8), save, pointer :: coord_xll_nb
         real(kind=8), save, pointer :: coord_yll_nb
         real(kind=8), save, pointer :: coord_xur_nb
         real(kind=8), save, pointer :: coord_yur_nb
         integer, save, pointer :: iss
         integer, save, pointer :: ieq
         character(len=300), save, pointer :: resultdir
         character(len=300), save, pointer :: debugdir
         logical, save, pointer :: ibound_fixed_west
         logical, save, pointer :: ibound_fixed_east
         logical, save, pointer :: ibound_fixed_north
         logical, save, pointer :: ibound_fixed_south
         real(kind=8), dimension(:), save, pointer :: cdelr
         real(kind=8), dimension(:), save, pointer :: cdelc
         logical, save, pointer :: save_no_buf
         integer, save, pointer :: savedouble
         logical, save, pointer :: write_debug_idf
         type gwfmettype
            character(len=300), pointer :: runcomment
            character(len=300), pointer :: coord_descr

!            integer, pointer :: idate_save

            integer, pointer :: time_syear
            integer, pointer :: time_smonth
            integer, pointer :: time_sday
            integer, pointer :: time_shour
            integer, pointer :: time_sminute
            integer, pointer :: time_ssecond
            double precision, pointer :: time_sjd
            double precision, pointer :: time_cjd
            character(len=300), pointer :: time_cstring
            character(len=300), pointer :: time_ostring
            real(kind=8), pointer :: coord_xll
            real(kind=8), pointer :: coord_yll
            real(kind=8), pointer :: coord_xur
            real(kind=8), pointer :: coord_yur
            real(kind=8), pointer :: gcoord_xll
            real(kind=8), pointer :: gcoord_yll
            real(kind=8), pointer :: gcoord_xur
            real(kind=8), pointer :: gcoord_yur
            real(kind=8), pointer :: coord_xll_nb
            real(kind=8), pointer :: coord_yll_nb
            real(kind=8), pointer :: coord_xur_nb
            real(kind=8), pointer :: coord_yur_nb
            integer, pointer :: iss
            integer, pointer :: ieq
            character(len=300), pointer :: resultdir
            character(len=300), pointer :: debugdir
            logical, pointer :: ibound_fixed_west
            logical, pointer :: ibound_fixed_east
            logical, pointer :: ibound_fixed_north
            logical, pointer :: ibound_fixed_south
            real(kind=8), dimension(:), pointer :: cdelr
            real(kind=8), dimension(:), pointer :: cdelc
            logical, pointer :: save_no_buf
            integer, pointer :: savedouble
            logical, pointer :: write_debug_idf
         end type

         type(gwfmettype), save :: gwfmetdat(10)

      end module gwfmetmodule
