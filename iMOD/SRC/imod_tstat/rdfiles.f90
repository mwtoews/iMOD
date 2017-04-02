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
!! read 2 data files and store the data in the module
subroutine rdfiles(file1,file2,coldate,colval,mv,string1,string2,exitcode)


! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 character (len=*), intent(in)  :: file1      !> input file name 1
 character (len=*), intent(in)  :: file2      !> input file name 2
                                              !! when this filename is equal to file1
                                              !! or when the filename is missing this
                                              !! data will not be read but the pointers
                                              !! of series 2 will poijt to ther arrays 
                                              !! of series 1
 character (len=*), DIMENSION(2), intent(in)  :: coldate    !> column name for date values
 character (len=*), DIMENSION(2), intent(in)  :: colval     !> column name for values
                                              !! when colnames are given the input files
                                              !! are interpeted as iModflow text files
 character (len=*), intent(out) :: string1    !> identification of data of file1
 character (len=*), intent(out) :: string2    !> identification of data of file2
 real             , intent(in)  :: mv         !> missing value code

 integer          , intent(out) :: exitcode   !> exit status, 0=OK


! local variables
 integer   l1,l2 !,ftype


! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

! init
 exitcode = 0


! read file 1
 call rdfile4(file1,string1,mv,1,coldate(1),colval(1),exitcode)
 if(exitcode.ne.0)write(*,'(A)') 'file= '//TRIM(FILE1)

! read file 2
 if (exitcode.eq.0) then
    l1=cfn_length(file1)
    l2=cfn_length(file2)
    if (l2.le.0) then
       ! series 2 equal to series 1
       call rdequal()
    else
       if (file1(1:l1).ne.file2(1:l2)) then
          ! read file2
          call rdfile4(file2,string2,mv,2,coldate(2),colval(2),exitcode)
          if(exitcode.ne.0)write(*,'(A)') 'file= '//TRIM(FILE2)
       else
          ! series 2 equal to series 1
          call rdequal()
       endif
    endif
 endif


! end of program
 return
end
