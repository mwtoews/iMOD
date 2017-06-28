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

      subroutine sgwf2ins1psv(igrid)
c save mf2005 instance pointers
      use m_mf2005_main

      implicit none

c arguments
      integer, intent(in)  :: igrid

c program section
c ------------------------------------------------------------------------------

      mi(igrid)%nsol    => nsol
      mi(igrid)%iouts   => iouts
      mi(igrid)%kper    => kper
      mi(igrid)%kkper   => kkper
      mi(igrid)%kstp    => kstp
      mi(igrid)%kkstp   => kkstp
      mi(igrid)%kiter   => kiter
      mi(igrid)%kkiter  => kkiter
      mi(igrid)%icnvg   => icnvg
      mi(igrid)%ilmtfmt => ilmtfmt
      mi(igrid)%issmt3d => issmt3d
      mi(igrid)%iumt3d  => iumt3d
      mi(igrid)%timesteptime       => timesteptime
      mi(igrid)%initTimeStep       => initTimeStep
      mi(igrid)%timeStepCalculated => timeStepCalculated
      mi(igrid)%solverConverged    => solverConverged

c end of program
      return
      end
c ******************************************************************************
      subroutine sgwf2ins1pnt(igrid)
c restore mf2005 instance pointers
      use m_mf2005_main

      implicit none

c arguments
      integer, intent(in)  :: igrid

c program section
c ------------------------------------------------------------------------------

      nsol    => mi(igrid)%nsol
      iouts   => mi(igrid)%iouts
      kper    => mi(igrid)%kper
      kkper   => mi(igrid)%kkper
      kstp    => mi(igrid)%kstp
      kkstp   => mi(igrid)%kkstp
      kiter   => mi(igrid)%kiter
      kkiter  => mi(igrid)%kkiter
      icnvg   => mi(igrid)%icnvg
      ilmtfmt => mi(igrid)%ilmtfmt
      issmt3d => mi(igrid)%issmt3d
      iumt3d  => mi(igrid)%iumt3d
      timesteptime       => mi(igrid)%timesteptime
      initTimeStep       => mi(igrid)%initTimeStep
      timeStepCalculated => mi(igrid)%timeStepCalculated
      solverConverged    => mi(igrid)%solverConverged

c end of program
      return
      end
c ******************************************************************************
      subroutine sgwf2ins1da(igrid)
c deallocate mf2005 instance pointers
      use m_mf2005_main

      implicit none

c arguments
      integer, intent(in)  :: igrid

c program section
c ------------------------------------------------------------------------------

      deallocate(mi(igrid)%nsol)
      deallocate(mi(igrid)%iouts)
      deallocate(mi(igrid)%kper)
      deallocate(mi(igrid)%kkper)
      deallocate(mi(igrid)%kstp)
      deallocate(mi(igrid)%kkstp)
      deallocate(mi(igrid)%kiter)
      deallocate(mi(igrid)%kkiter)
      deallocate(mi(igrid)%icnvg)
      deallocate(mi(igrid)%ilmtfmt)
      deallocate(mi(igrid)%issmt3d)
      deallocate(mi(igrid)%iumt3d)
      deallocate(mi(igrid)%timesteptime)
      deallocate(mi(igrid)%initTimeStep)
      deallocate(mi(igrid)%timeStepCalculated)
      deallocate(mi(igrid)%solverConverged)

c end of program
      return
      end
c ******************************************************************************


