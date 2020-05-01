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
!! allocate arrays and read data
!> description
!! perform formulate for DXC Flux
!! Seawat: Modified for variable density flow    
subroutine vdf2dxc1fm(igrid)

! declaration section
! ------------------------------------------------------------------------------
 use global,       only:ibound,rhs
 use gwfdxcmodule
 use VDFMODULE,    only:DENSEREF,PS

 implicit none


! arguments
 integer  , intent(in)     :: igrid       !> grid number
 REAL DENSE


! local variables
 integer   i,icol,irow,ilay

 real      q


! program section
! ------------------------------------------------------------------------------

! set pointers
 call sgwf2bas7pnt(igrid)
 call sgwf2dxc1pnt(igrid)

! perform formulate
 do i=1,ndxc
    icol=dxcic(i)
    irow=dxcir(i)
    ilay=dxcil(i)
    if(ilay.eq.0) cycle ! PKS
    q   =dxcuzflux(i)
    if (q.ne.dxcmv) then
       ! not a missing value
       if (ibound(icol,irow,ilay).gt.0) then
       ! Active Cell
       ! add flux to the RHS       
!SEAWAT: conserve mass
         DENSE=DENSEREF                                        !VDF
         IF(q.LT.0) DENSE=PS(icol,irow,ilay)                   !VDF
         q = q*DENSE                                           !VDF       
!          rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-q           !VDF
           rhs(icol,irow,ilay)=rhs(icol,irow,ilay)-q*denseref  !VDF 
       endif
    endif
 enddo

! save pointers
 call sgwf2bas7psv(igrid)

! end of program
 return
end

! ******************************************************************************

!> description
!! calculate budget terms for DXC fluxes
subroutine vdf2dxc1bd(KSTP,KPER,igrid)

! declaration section
! ------------------------------------------------------------------------------
 USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF
 USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,VBVL,VBNM
 use gwfdxcmodule
 use VDFMODULE,    only:DENSEREF,PS

 implicit none


! arguments
 integer  , intent(in)     :: kstp        !> time step number
 integer  , intent(in)     :: kper        !> stress period number
 integer  , intent(in)     :: igrid       !> grid number


! local variables
 integer   i,icol,irow,ilay
 real      rin,rout,q,zero

 integer   ibd,ibdlbl

 CHARACTER (len=16)  :: TEXT
 DOUBLE PRECISION    ::  RATIN,RATOUT
 REAL                :: DENSE

 DATA TEXT /'      DXC FLUXES'/


! program section
! ------------------------------------------------------------------------------


! set pointers
 call sgwf2dxc1pnt(igrid)


!C1------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
!C1------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IdxcCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IdxcCB.GT.0) IBD=ICBCFL
      IBDLBL=0

!C3------CLEAR THE BUFFER.
 DO ilay=1,NLAY
    DO irow=1,NROW
       DO icol=1,NCOL
          BUFF(ICol,IRow,ILay)=ZERO
       enddo
    enddo
 enddo

 do i=1,ndxc
    icol=dxcic(i)
    irow=dxcir(i)
    ilay=dxcil(i)
    if (ilay.eq.0) cycle ! PKS
    q   =dxcuzflux(i)
    if (q.ne.dxcmv) then
       ! not a missing value
       if (ibound(icol,irow,ilay).gt.0) then
          ! Active Cell
          ! add flux to the RHS
!SEAWAT: conserve mass
         DENSE=DENSEREF
         IF(q.LT.0) DENSE=PS(icol,irow,ilay)                   !VDF
         q = q*DENSE                                           !VDF
          buff(icol,irow,ilay)=buff(icol,irow,ilay)+q/DENSE    !VDF (CONVERT TO VOLUMETRIC FLUX)
          if (q.ge.zero) then
             ratin =ratin  + q
          else
             ratout=ratout - q
          endif
       endif
    endif
 enddo


!C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
!C6------CALL UBUDSV TO SAVE THEM.
 IF (IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IdxcCB,BUFF,NCOL,NROW,NLAY,IOUT)

!C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
 RIN=RATIN
 ROUT=RATOUT
 VBVL(3,MSUM)=RIN
 VBVL(4,MSUM)=ROUT
 VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
 VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
 VBNM(MSUM)=TEXT

!C8------INCREMENT BUDGET TERM COUNTER(MSUM).
 MSUM=MSUM+1


! end of program
 return
end

! ******************************************************************************