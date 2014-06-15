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
!> description
!! create temporary arrays
subroutine createtmp(nlag,ncol,noff)

! declaration section
! ------------------------------------------------------------------------------
 use m_cross

 implicit none


! arguments
 integer, intent(in)  :: nlag,ncol,noff


! local variables
 integer   nmx,ntsw1,ntsw2,ntsw3,ntsw4


! functions

! program section
! ------------------------------------------------------------------------------

! pointers
 n1=>ser(1)%n
 n2=>ser(2)%n
 nmx=max(n1,n2)


! allocate temporary arrys
 ntsw1=max(1,max(noff,nmx*nlag))
 ntsw2=max(1,max(noff,nmx*nlag))
 ntsw3=max(1,noff)
 ntsw4=max(1,nlag*ncol)
 if (allocated(tsw1)) then
    if (size(tsw1).lt.ntsw1) deallocate(tsw1)
    if (size(tsw2).lt.ntsw2) deallocate(tsw2)
    if (size(tsw3).lt.ntsw3) deallocate(tsw3)
    if (size(tsw4).lt.ntsw4) deallocate(tsw4)
 endif
 if (.not.allocated(tsw1)) allocate(tsw1(ntsw1))
 if (.not.allocated(tsw2)) allocate(tsw2(ntsw2))
 if (.not.allocated(tsw3)) allocate(tsw3(ntsw3))
 if (.not.allocated(tsw4)) allocate(tsw4(ntsw4))


! end of program
 return
end
