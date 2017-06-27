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

      module pksmpims_mod
      
      implicit none
      
      integer, parameter :: mvid = -12345
      
      logical, save :: pksactive = .false.

      ! local IDs
      integer, save :: nid = 0
      integer, dimension(:), allocatable, save :: ids

      ! global svat --> IDs
      integer, save :: mxgsvatid = 0
      integer, dimension(:,:), allocatable, save :: gsvat2id
      
      end module pksmpims_mod
