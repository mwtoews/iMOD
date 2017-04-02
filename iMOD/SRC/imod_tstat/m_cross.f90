!!  Copyright (C) Stichting Deltares, 2005-2017.
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

 !> description
 !! module for data storage for cross-calculations
 module m_cross

    ! declaration section
    ! ------------------------------------------------------------------------------


    type series
       integer, pointer :: n       ! nullify done by subroutine m_cross_init
       real, pointer    :: x(:)    ! nullify done by subroutine m_cross_init
       real, pointer    :: y(:)    ! nullify done by subroutine m_cross_init
    end type


    type(series), pointer      :: psr
    type(series), target, save :: ser(2)

    ! some usefull help pointers
    integer, pointer           :: n,n1,n2
    real, pointer,dimension(:) :: x,y,x1,y1,x2,y2

    ! temporary arrays
    real, allocatable, dimension(:) :: tsw1,tsw2,tsw3
    double precision, allocatable, dimension(:) :: tsw4

    ! logical to check or module has to be inited (compatibility with Fortran90)
    logical, save :: crossInit=.true.

 end module m_cross

!> routine to init structure of m_cross module
!! necessary for compatibility with Fortran 90
 subroutine m_cross_init()

 use m_cross
 implicit none
 integer i

 if (crossInit) then
    ! initialise ser structure
    do i=1,size(ser)
       nullify(ser(i)%n)
       nullify(ser(i)%x)
       nullify(ser(i)%y)
    enddo
 endif
 crossInit=.false.

 return
 end
