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
subroutine tsclean()

! declaration section
! ------------------------------------------------------------------------------
 use m_cross

 implicit none


! arguments


! local variables
 integer   i


! functions

! program section
! ------------------------------------------------------------------------------


! initialise m_cross module
 call m_cross_init()


! nullify ser(2) when pointing to same memory as ser(1)
 if (associated(ser(1)%x,ser(2)%x)) nullify(ser(2)%x)
 if (associated(ser(1)%y,ser(2)%y)) nullify(ser(2)%y)
 if (associated(ser(1)%n,ser(2)%n)) nullify(ser(2)%n)


! deallocate ser-structure
 do i=1,2
    if (associated(ser(i)%x)) then
       deallocate(ser(i)%x,ser(i)%y,ser(i)%n)
       nullify(ser(i)%x)
       nullify(ser(i)%y)
       nullify(ser(i)%n)
    endif
 enddo


! deallocate temporary arrays
 if (allocated(tsw1)) deallocate(tsw1)
 if (allocated(tsw2)) deallocate(tsw2)
 if (allocated(tsw3)) deallocate(tsw3)
 if (allocated(tsw4)) deallocate(tsw4)


! end of program
 return
end
