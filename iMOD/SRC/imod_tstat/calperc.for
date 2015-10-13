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

      subroutine calperc(array,narr,mv,perc,p)                !*URT*

c description:
c ------------------------------------------------------------------------------
c calculate percentile of data arrray        !*UDS*
c array may contain missing values
c array will be sorted

c declaration section
c ------------------------------------------------------------------------------

      USE MOD_QKSORT

      implicit none


c arguments
      integer   narr          ! (I) number of elements                   !*UAR*

      real      array(narr),  ! (I/O) data                   !*UAR*
     1          mv,           ! (I) missing value
     1          perc,         ! (I)   to be calculated percentile
     1          p             ! (O)   calculated value


c local variables
      integer   i,n


c functions
      real      cfn_perc_r


c include files


c program section
c ------------------------------------------------------------------------------


c remove missing values
      n=0
      do i=1,narr
         if (array(i).ne.mv) then
            n=n+1
            array(n)=array(i)
         endif
      enddo


c sort
      if (n.gt.1) then
         CALL UTL_QKSORT(N,N,ARRAY)
         p=cfn_perc_r(array,n,perc)
      else
         p=mv
      endif


c end of program
      return
      end
