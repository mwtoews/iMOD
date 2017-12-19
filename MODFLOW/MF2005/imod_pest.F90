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

subroutine pest1log()

! modules
use global, only: lipest

#ifdef IPEST
use imod_utl, only: imod_utl_printtext,imod_utl_itos
use pestvar, only: pest_iter,lgrad,llnsrch,pest_igrad,iupestout
#endif

implicit none

! locals
character(len=256) :: line

!###======================================================================

#ifndef IPEST
lipest = .false.; return
#endif

if (.not.lipest) return

#ifdef IPEST

call imod_utl_printtext('',-1,iupestout)
if(lgrad)then
   if(pest_iter.eq.0)then
      call imod_utl_printtext('Initial simulation',-1,iupestout)
   else
      line='Cycle '//TRIM(imod_utl_itos(pest_igrad))//': Derivatives by Forward Finite-Difference'
      call imod_utl_printtext(trim(line),-1,iupestout)
   ENDIF
ENDIF
if(llnsrch)then
   call imod_utl_printtext('Line Search',-1,iupestout)
endif

#endif

end subroutine pest1log

!###====================================================================
subroutine pest1alpha_grid(ptype,a,nrow,ncol,nlay,iout,a2)
!###====================================================================

use imod_utl, only: imod_utl_printtext,imod_utl_itos,imod_utl_rtos,imod_utl_createdir
use global, only: lipest, ibound

#ifdef IPEST
use pestvar, only: param, pest_iter,lgrad,llnsrch,pest_igrad,iupestout
#endif

implicit none

! arguments
integer, intent(in) :: nrow, ncol, nlay, iout
real, dimension(ncol,nrow,nlay), intent(inout) :: a
real, dimension(ncol,nrow,nlay), intent(in), optional :: a2
character(len=2), intent(in) :: ptype
CHARACTER(LEN=1024) :: FNAME,DIR

! parameters
real, parameter :: tiny=1.0e-20

! locals
character(len=256) :: line
integer :: i, j, k, ils, irow, icol
real :: c, fct, ppart

CHARACTER(LEN=2),DIMENSION(6) :: PPPARAM
DATA PPPARAM/'KD','KH','KV','VC','SC','VA'/ !## variable for pilotpoints

!###======================================================================

#ifndef IPEST
lipest = .false.; return
#endif

if (.not.lipest) return

#ifdef IPEST

!## initialize parameters
if(pest_iter.eq.0)then

   do i=1,size(param)

      if (trim(param(i)%ptype).ne.trim(ptype)) cycle

      ils=param(i)%ils !## layer

      IF(PARAM(I)%ZTYPE.EQ.0)THEN

       if(.not.associated(param(i)%x))allocate(param(i)%x(param(i)%nodes))
       select case (trim(ptype))
       case('KD','KH','KV','VA','SC','AF','EP','RE')
          do j=1,param(i)%nodes
             irow=param(i)%irow(j); icol=param(i)%icol(j)
             !## only modify active/constant head nodes
             if(ibound(icol,irow,ils).ne.0)then
              param(i)%x(j)=a(icol,irow,ils)*param(i)%f(j)
             endif
          enddo
       case('VC')   !## vertical c values
          do j=1,param(i)%nodes
             irow=param(i)%irow(j); icol=param(i)%icol(j)
             if(ibound(icol,irow,ils).ne.0)then
              c = 1/(a(icol,irow,ils)+tiny)
              param(i)%x(j)=c*param(i)%f(j)
             endif
          enddo
       case('AA')   !## anisotropy angle
          if (.not.present(a2)) then
             call imod_utl_printtext('Cannot apply PEST scaling factor for anisotrophy angle',2)
          end if
          do j=1,param(i)%nodes
             irow=param(i)%irow(j); icol=param(i)%icol(j)
             if(ibound(icol,irow,ils).ne.0)then
              if (a2(icol,irow,ils).lt.1.0) then
                param(i)%x(j)=a(icol,irow,ils)*param(i)%f(j)
              endif
             endif
          enddo
       case default
          call imod_utl_printtext('Something went wrong for PEST, missing parameter '//trim(ptype),2)
       end select
      !## pilot points, save initial values for f -- hoeft toch niet???
      ELSEIF(PARAM(I)%ZTYPE.EQ.1)THEN

      end if
    end do
end if

do i=1,size(param)

   if (trim(param(i)%ptype).ne.trim(ptype)) cycle

   !## skip pilot-points
   IF(PARAM(I)%ZTYPE.EQ.1)CYCLE

   fct=param(i)%alpha(1); ils=param(i)%ils !## layer
   IF(PARAM(I)%LOG)FCT=EXP(FCT)

   select case (trim(ptype))
   case('KD','KH','KV','VA','SC','AF','EP','RE')
      do j=1,param(i)%nodes
         irow=param(i)%irow(j); icol=param(i)%icol(j)
         if(ibound(icol,irow,ils).ne.0)then
           ppart           =a(icol,irow,ils)-param(i)%x(j)
           a(icol,irow,ils)=ppart+param(i)%x(j)*fct
         endif
      enddo
   case('VC')   !## vertical c values
      do j=1,param(i)%nodes
         irow=param(i)%irow(j); icol=param(i)%icol(j)
         if(ibound(icol,irow,ils).ne.0)then
          c = 1/(a(icol,irow,ils)+tiny)
          ppart = c-param(i)%x(j)
          c = ppart + param(i)%x(j)*fct
          a(icol,irow,ils)=1/(c+tiny)
         endif
      enddo
   case('AA')   !## anisotropy angle
      if (.not.present(a2)) then
         call imod_utl_printtext('Cannot apply PEST scaling factor for anisotrophy angle',2)
      end if
      do j=1,param(i)%nodes
         irow=param(i)%irow(j); icol=param(i)%icol(j)
         if(ibound(icol,irow,ils).ne.0)then
          if (a2(icol,irow,ils).lt.1.0) then
            ppart           =a(icol,irow,ils)-param(i)%x(j)
            a(icol,irow,ils)=ppart+param(i)%x(j)*fct
          endif
         endif
      enddo

   case default
      call imod_utl_printtext('Something went wrong for PEST, missing parameter '//trim(ptype),2)

   end select
   if(param(i)%igroup.gt.0)then
    line=' * '//param(i)%ptype//' adjusted ('//trim(imod_utl_itos(param(i)%nodes))// &
      ')with alpha='//trim(imod_utl_rtos(fct,'f',7))
    call imod_utl_printtext(trim(line),-1,iupestout)
   endif
end do
#endif

#ifdef IPEST_PILOTPOINTS
 !## process any pilotpoints per modellayer/ adjustable parameter
 DO IPP=1,SIZE(PPPARAM); DO ILS=1,NLAY
  DO K=1,2
   NXYZ=0
   DO I=1,SIZE(PARAM)
   
    if (trim(param(i)%ptype).ne.trim(ptype)) cycle

    !## skip NONE pilot-points
    IF(PARAM(I)%ZTYPE.NE.1)CYCLE
    !## not similar parameter
    IF(PARAM(I)%PTYPE.NE.PPPARAM(IPP))CYCLE
    !## not correct modellayer
    IF(PARAM(I)%ILS.NE.ILS)CYCLE

    FCT=PARAM(I)%ALPHA(1)
    IF(K.EQ.2)THEN
     LINE=' * Module '//PARAM(I)%PTYPE//' adjusted ('//TRIM(ITOS(SIZE(PARAM(I)%XY,1)))// &
         ') location(s) as PILOTPOINT with alpha='//TRIM(RTOS(EXP(FCT),'F',7))
     CALL imod_utl_printtext(TRIM(LINE),1) 
    ENDIF
    
    DO J=1,SIZE(PARAM(I)%XY,1)
     NXYZ=NXYZ+1
     IF(K.EQ.2)THEN
      XYZ(NXYZ,1)=PARAM(I)%XY(J,1); XYZ(NXYZ,2)=PARAM(I)%XY(J,2); XYZ(NXYZ,3)=FCT 
     ENDIF
    ENDDO
   ENDDO
   IF(NXYZ.EQ.0)EXIT
   IF(K.EQ.1)ALLOCATE(XYZ(NXYZ,3))
  ENDDO
  !# next parameter, this one is not used
  IF(NXYZ.EQ.0)CYCLE

  NODATA=-999.99; ALLOCATE(XPP(NCOL,NROW)); XPP=NODATA
  RANGE=PEST_GETRANGE()
    
  CALL imod_utl_printtext('Kriging applied Range:'//TRIM(RTOS(RANGE,'F',2))//' meter',1)

  !## apply kriging interpolation
  CALL KRIGING_MAIN(NXYZ,XYZ(:,1),XYZ(:,2),XYZ(:,3),DELR,DELC,NROW,NCOL,XPP,NODATA,RANGE,PEST_KTYPE)
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IBOUND(ICOL,IROW,ILAY).EQ.0)THEN
    XPP(ICOL,IROW)=NODATA
   ELSE
    XPP(ICOL,IROW)=EXP(XPP(ICOL,IROW))
    !## areas outside range do get a value of 0.0, convert to 1.0
    IF(XPP(ICOL,IROW).EQ.0.0)XPP(ICOL,IROW)=1.0
   ENDIF
  ENDDO; ENDDO
  
  WRITE(FNAME,'(A,I5.5,A)') TRIM(DIR)//CHAR(92)//PPPARAM(IPP),ILAY,'.IDF'
  CALL WRITEIDF(XPP,1,NCOL,1,NROW,1,NROW,NCOL,1,DELR(0),DELC(NROW),SIMCSIZE,NODATA,FNAME)

  SELECT CASE (PPPARAM(IPP))
   CASE ('KD','KH','VC','KV','SC','VA')  !## transmissivities
    A(:,:,ILS)=A(:,:,ILS)*XPP
  END SELECT

  DEALLOCATE(XYZ,XPP)
 
 ENDDO; ENDDO
#endif 

#ifdef IPEST
 !## export only initially
 if(PEST_IGRAD.eq.0)then
  do i=1,size(param)
   if (trim(param(i)%ptype).ne.trim(ptype)) cycle

   DIR='.\pest\pest_parameters_c'//trim(imod_utl_itos(pest_iter))
   CALL IMOD_UTL_CREATEDIR(DIR)
   do ils=1,nlay
    fname=trim(dir)//'\'//trim(ptype)//'_l'//trim(imod_utl_itos(ils))//'.idf'
    CALL met1wrtidf(fname,a(:,:,ils),ncol,nrow,-999.0,iout)
   enddo
   exit
  enddo
 endif
#endif
 
end subroutine

#ifdef IPEST_PILOTPOINTS
!###====================================================================
REAL FUNCTION PEST_GETRANGE()
!###====================================================================
IMPLICIT NONE
    
PEST_GETRANGE=0.9*SQRT((DELR(NCOL)-DELR(0))**2.0+(DELC(0)-DELC(NROW))**2.0)
  
END FUNCTION PEST_GETRANGE
#endif
 
!###====================================================================
subroutine pest1alpha_list(ptype,nlist,rlist,ldim,mxlist,iopt1,iopt2)
!###====================================================================

! modules
use global, only: lipest

#ifdef IPEST
use global, only: buff
use imod_utl, only: imod_utl_printtext, imod_utl_itos, imod_utl_rtos
use m_mf2005_main, only: kper
use pestvar, only: param, pest_iter, iupestout
#endif

implicit none

! arguments
character(len=2), intent(in) :: ptype
integer, intent(in) :: nlist, ldim, mxlist
real, dimension(ldim,mxlist), intent(inout) :: rlist
integer, intent(in), optional :: iopt1, iopt2

! locals
character(len=256) :: line
character(len=1024) :: errmsg
integer :: i, j, k, ils, irow, icol, idat, irivsubsys, irivrfact,&
           idrnsubsys, ihfbfact, nadj
real :: ppart, fct

!###======================================================================

#ifndef IPEST
lipest = .false.; return
#endif

if (.not.lipest) return

#ifdef IPEST

! first, mark the cells
buff = 0.
do i=1,size(param)

   if (trim(param(i)%ptype).ne.trim(ptype)) cycle
   ils=param(i)%ils ! system/layer

   select case (trim(ptype))
   case('RC','IC') ! river/isg conductances
      errmsg = 'Cannot apply PEST scaling factor for river/isg conductance'
      if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
      irivsubsys = iopt1
      if (irivsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)

      if (trim(ptype).eq.'IC') ils=-ils
      do j = 1, nlist ! match sybsystem number
         irow=rlist(2,j); icol=rlist(3,j)
         if (int(ils).eq.int(rlist(irivsubsys,j))) buff(icol,irow,1) = real(j)
      end do
      idat = 5 !4
   case('RI','II') ! river/isg infiltration factors
      errmsg = 'Cannot apply PEST scaling factor for river/isg infiltration factor'
      if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
      if (.not.present(iopt2)) call imod_utl_printtext(trim(errmsg),2)
      irivsubsys = iopt1; irivrfact = iopt2
      if (irivrfact.eq.0.or.irivsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)

      if (trim(ptype).eq.'II') ils=-ils
      do j = 1, nlist ! match sybsystem number
         irow=rlist(2,j); icol=rlist(3,j)
         if (int(ils).eq.int(rlist(irivsubsys,j))) buff(icol,irow,1) = real(j)
      end do
      idat = irivrfact
   case('DC') ! drain conductances
      errmsg = 'Cannot apply PEST scaling factor for drain conductance'
      if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
      idrnsubsys = iopt1
      if (idrnsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)
      do j = 1, nlist ! match sybsystem number
         irow=rlist(2,j); icol=rlist(3,j)
         if (int(ils).eq.int(rlist(idrnsubsys,j))) buff(icol,irow,1) = real(j)
      end do
      idat = 5 !2 !4
   end select
end do

! allocate
if (pest_iter.eq.0) then
   do i=1,size(param)
      if (trim(param(i)%ptype).ne.trim(ptype)) cycle
      ils=param(i)%ils ! system

      if(.not.associated(param(i)%x))allocate(param(i)%x(param(i)%nodes))
      select case (trim(ptype))
      case('RC','RI','IC','II','DC')
         do j=1,param(i)%nodes
            irow=param(i)%irow(j); icol=param(i)%icol(j)
            k = int(buff(icol,irow,1))
            if (k.gt.0) then
               param(i)%x(j)=rlist(idat,k)*param(i)%f(j)
            end if
         end do
      end select
   end do
end if

! adjust
do i=1,size(param)
   nadj = 0
   if (trim(param(i)%ptype).ne.trim(ptype)) cycle
   fct=param(i)%alpha(1); ils=param(i)%ils ! system

   IF(PARAM(I)%LOG)FCT=EXP(FCT); 

   select case (trim(ptype))
   case('RC','RI','IC','II','DC')
      do j=1,param(i)%nodes
         irow=param(i)%irow(j); icol=param(i)%icol(j)
         k = int(buff(icol,irow,1))
         if (k.gt.0) then
            nadj = nadj + 1
            ppart        =rlist(idat,k)-param(i)%x(j)
            rlist(idat,k)=ppart+param(i)%x(j)*fct
         end if
      end do
   case('HF')
      errmsg = 'Cannot apply PEST scaling factor for horizontal flow barrier'
      if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
      ihfbfact = iopt1
      if (ihfbfact.ne.1) call imod_utl_printtext(trim(errmsg),2)
      do j = 1, nlist
         if (int(rlist(7,j)).eq.ils) then
            nadj = nadj + 1
            rlist(6,j) = rlist(6,j)*fct
         end if
      end do
   end select

   line=' * '//param(i)%ptype//' adjusted ('//trim(imod_utl_itos(nadj))//') with alpha='//trim(imod_utl_rtos(fct,'f',7))
   call imod_utl_printtext(trim(line),-1,iupestout)

end do

#endif

end subroutine




