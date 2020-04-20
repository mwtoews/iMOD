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
use imod_utl, only: imod_utl_printtext,imod_utl_itos,imod_utl_dtos
use pestvar, only: pest_iter,lgrad,llnsrch,pest_igrad,iupestout,pest_ktype

implicit none

! locals
character(len=256) :: line

!###======================================================================

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

end subroutine pest1log

!###====================================================================
subroutine pest1alpha_grid(ptype,a,nrow,ncol,nlay,iout,a2)
!###====================================================================
use imod_utl, only: imod_utl_printtext,imod_utl_itos,imod_utl_dtos,imod_utl_createdir, &
   utl_kriging_range,utl_kriging_main
use gwfmetmodule, only: cdelr, cdelc, resultdir
use global, only: lipest, ibound 
use pestvar, only: param, pest_iter,lgrad,llnsrch,pest_igrad,iupestout,pest_ktype,pest_krange,blnkout,pest_maxpnt

implicit none

! arguments
integer, intent(in) :: nrow, ncol, nlay, iout
real, dimension(ncol,nrow,nlay), intent(inout) :: a
!real(kind=8),dimension(:,:,:),allocatable :: a_dbl
real, dimension(ncol,nrow,nlay), intent(in), optional :: a2
character(len=2), intent(in) :: ptype
real(kind=8),dimension(:,:),allocatable :: xyz,xpp
integer :: nxyz,ipp,ilay
character(len=1024) :: fname,dir

! parameters
real(kind=8), parameter :: tiny=1.0d-20

! locals
character(len=256) :: line
integer :: i, j, k, ils, irow, icol
real(kind=8) :: c, fct, ppart, range, nodata

CHARACTER(LEN=2),DIMENSION(7) :: PPPARAM
DATA PPPARAM/'KD','KH','KV','VC','SC','VA','SY'/ !## variable for pilotpoints

!###======================================================================

if(.not.lipest)return

call sgwf2met1pnt(1) !igrid)

!## initialize parameters
if(pest_iter.eq.0)then

   do i=1,size(param)

      if (trim(param(i)%ptype).ne.trim(ptype)) cycle

      ils=param(i)%ils !## layer

      IF(PARAM(I)%ZTYPE.EQ.0)THEN

       if(.not.associated(param(i)%x))allocate(param(i)%x(param(i)%nodes))
       select case (trim(ptype))
       case('KD','KH','KV','VA','SC','AF','EP','SY')
          do j=1,param(i)%nodes
             irow=param(i)%irow(j); icol=param(i)%icol(j)
             !## only modify active/constant head nodes
             if(ibound(icol,irow,ils).ne.0)then
              param(i)%x(j)=a(icol,irow,ils)*param(i)%f(j)
             endif
          enddo
       !## can become unconfined and then it is not layer 1
       case('RE')
          do j=1,param(i)%nodes
             irow=param(i)%irow(j); icol=param(i)%icol(j)
             param(i)%x(j)=a(icol,irow,ils)*param(i)%f(j)
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

DIR=trim(resultdir)//'\pest\parameters_cycle'//trim(imod_utl_itos(pest_iter))
!dir='.\pest\pest_parameters_c'//trim(imod_utl_itos(pest_iter))
CALL IMOD_UTL_CREATEDIR(DIR)

do i=1,size(param)

   if (trim(param(i)%ptype).ne.trim(ptype)) cycle

   !## skip pilot-points
   IF(PARAM(I)%ZTYPE.EQ.1)CYCLE

   fct=param(i)%alpha(1); ils=param(i)%ils !## layer
   IF(PARAM(I)%LOG)FCT=EXP(FCT)

   select case (trim(ptype))
   case('KD','KH','KV','VA','SC','AF','EP','SY')
      do j=1,param(i)%nodes
         irow=param(i)%irow(j); icol=param(i)%icol(j)
         if(ibound(icol,irow,ils).ne.0)then
           ppart           =a(icol,irow,ils)-param(i)%x(j)
           a(icol,irow,ils)=ppart+param(i)%x(j)*fct
         endif
      enddo
   !## can become unconfined and then it is not layer 1
   case('RE')
      do j=1,param(i)%nodes
         irow=param(i)%irow(j); icol=param(i)%icol(j)
         ppart           =a(icol,irow,ils)-param(i)%x(j)
         a(icol,irow,ils)=ppart+param(i)%x(j)*fct
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
   
   if(param(i)%acronym.eq.'')then
    line='Param. '//trim(imod_utl_itos(i))//'['//param(i)%ptype//',n='//trim(imod_utl_itos(param(i)%nodes))// &
       ',f='//trim(imod_utl_dtos(fct,'f',7))//',ils='//trim(imod_utl_itos(ils))//']'
   else
    line='Param. '//trim(imod_utl_itos(i))//'['//trim(param(i)%acronym)//','//param(i)%ptype//',n='//trim(imod_utl_itos(param(i)%nodes))// &
       ',f='//trim(imod_utl_dtos(fct,'f',7))//',ils='//trim(imod_utl_itos(ils))//']'
   endif
   write(*,'(a)') trim(line)
!      WRITE(*,*) TRIM(LINE)
 end do

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
     LINE=' * Module '//PARAM(I)%PTYPE//' adjusted ('//TRIM(IMOD_UTL_ITOS(SIZE(PARAM(I)%XY,1)))// &
         ') location(s) as PILOTPOINT with alpha='//TRIM(IMOD_UTL_dTOS(EXP(FCT),'F',7))
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

  NODATA=-999.99D0; ALLOCATE(XPP(NCOL,NROW)); XPP=NODATA
!  IF(PEST_KRANGE.GT.0.0)THEN
  RANGE=PEST_KRANGE
!  ELSE
!   RANGE=UTL_KRIGING_RANGE(CDELR(0),CDELR(NCOL),CDELC(NROW),CDELC(0))
!  ENDIF

  CALL imod_utl_printtext('Kriging applied Range:'//TRIM(IMOD_UTL_dTOS(RANGE,'F',2))//' meter',1)
  
  !## apply kriging interpolation
  CALL UTL_KRIGING_MAIN(NXYZ,XYZ(:,1),XYZ(:,2),XYZ(:,3),CDELR,CDELC,NROW,NCOL,XPP,NODATA,RANGE,PEST_KTYPE, &
     BLNKOUT,PEST_MAXPNT)
  DO IROW=1,NROW; DO ICOL=1,NCOL
   IF(IBOUND(ICOL,IROW,ILS).EQ.0)THEN
    XPP(ICOL,IROW)=NODATA
   ELSE
!    IF(ASSOCIATED(BLNKOUT%X))THEN
    IF(BLNKOUT%X(ICOL,IROW).LE.0.0)XPP(ICOL,IROW)=0.0
!    ENDIF
    XPP(ICOL,IROW)=EXP(XPP(ICOL,IROW))
    !## areas outside range do get a value of 0.0, convert to 1.0
    IF(XPP(ICOL,IROW).EQ.0.0)XPP(ICOL,IROW)=1.0
   ENDIF
  ENDDO; ENDDO
  
  fname=trim(dir)//char(92)//'kriging_'//trim(ppparam(ipp))//'_l'//trim(imod_utl_itos(ils))//'.idf'
  CALL met1wrtidf(fname,xpp,ncol,nrow,nodata,iout)

  SELECT CASE (PPPARAM(IPP))
   CASE ('KD','KH','VC','KV','SC','VA','SY','EP')  !## transmissivities
    DO IROW=1,NROW; DO ICOL=1,NCOL
     IF(IBOUND(ICOL,IROW,ILS).EQ.0)CYCLE
     A(ICOL,IROW,ILS)=A(ICOL,IROW,ILS)*XPP(ICOL,IROW)
    ENDDO; ENDDO
   CASE DEFAULT
    WRITE(*,*) 'CANNOT COME HERE'; PAUSE
  END SELECT

  DEALLOCATE(XYZ,XPP)
 
 ENDDO; ENDDO

 !## export only initially
 if(PEST_IGRAD.eq.0)then
  do i=1,size(param)
   if (trim(param(i)%ptype).ne.trim(ptype)) cycle

   do ils=1,nlay
    fname=trim(dir)//'\'//trim(ptype)//'_l'//trim(imod_utl_itos(ils))//'.idf'
    CALL met1wrtidf(fname,real(a(:,:,ils),8),ncol,nrow,-999.0D0,iout)
   enddo
   exit
  enddo
 endif

 end subroutine

!###====================================================================
subroutine pest1alpha_list(ptype,nlist,rlist,ldim,mxlist,iopt1,iopt2)
!###====================================================================

! modules
use global, only: lipest,buff,ncol,nrow
use imod_utl, only: imod_utl_printtext, imod_utl_itos, imod_utl_dtos
use m_mf2005_main, only: kper
use pestvar, only: param, pest_iter, iupestout

implicit none

! arguments
character(len=2), intent(in) :: ptype
integer, intent(in) :: nlist, ldim, mxlist
real, dimension(ldim,mxlist), intent(inout) :: rlist
integer, intent(in) :: iopt1, iopt2

! locals
character(len=256) :: line
character(len=1024) :: errmsg
integer :: i, j, k, ils, irow, icol, idat, irivsubsys, irivrfact,ilay, &
           idrnsubsys, iwelsubsys, ihfbfact, ighbsubsys, nadj,inode
real(kind=8) :: ppart, fct

!###======================================================================

if(.not.lipest)return

!## first, mark the cells
do i=1,size(param)

  !## not this package - skip it
  if (trim(param(i)%ptype).ne.trim(ptype)) cycle

  !## fill buff with location of zone
  buff=0.0d0
  do j=1,param(i)%nodes
   irow=param(i)%irow(j); icol=param(i)%icol(j)
   !## save zone location number
   buff(icol,irow,1)=real(j,8)
  enddo

  !## system/layer
  ils=param(i)%ils 
  !## factor
  fct=param(i)%alpha(1); if(param(i)%log)fct=exp(fct)
  
  !## adjust
  nadj = 0

  !## modify package with location in zone
  select case (trim(ptype))
   case('RC','IC') ! river/isg conductances
     errmsg = 'Cannot apply PEST scaling factor for river/isg conductance'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
     irivsubsys = iopt1
     if (irivsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)

     idat = 5
     if (trim(ptype).eq.'IC') ils=-ils
     do j = 1, nlist
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(irivsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('RL','IL') ! river/isg river levels
     errmsg = 'Cannot apply PEST scaling factor for river/isg levels'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
     irivsubsys = iopt1
     if (irivsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)

     idat = 4
     if (trim(ptype).eq.'IL') ils=-ils
     do j = 1, nlist
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(irivsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('RB','IB') ! river/isg bottom levels
     errmsg = 'Cannot apply PEST scaling factor for river/isg bottom levels'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
     irivsubsys = iopt1
     if (irivsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)

     idat = 6
     if (trim(ptype).eq.'IB') ils=-ils
     do j = 1, nlist
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(irivsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('RI','II') ! river/isg infiltration factors
     errmsg = 'Cannot apply PEST scaling factor for river/isg infiltration factor'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
!     if (.not.present(iopt2)) call imod_utl_printtext(trim(errmsg),2)
     irivsubsys = iopt1; irivrfact = iopt2
     if (irivrfact.eq.0.or.irivsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)

     idat = irivrfact
     if (trim(ptype).eq.'II') ils=-ils
     do j = 1, nlist
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(irivsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('DC') ! drain conductances
     errmsg = 'Cannot apply PEST scaling factor for drain conductance'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
     idrnsubsys = iopt1
     if (idrnsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)
     idat = 5
     do j = 1, nlist ! match sybsystem number
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(idrnsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('DL') ! drain level
     errmsg = 'Cannot apply PEST scaling factor for drain level'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
     idrnsubsys = iopt1
     if (idrnsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)
     idat = 4
     do j = 1, nlist ! match sybsystem number
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(idrnsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('GC') ! general head conductances
     errmsg = 'Cannot apply PEST scaling factor for general conductance'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
     ighbsubsys = iopt1
     if (ighbsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)
     idat = 5
     do j = 1, nlist ! match sybsystem number
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(ighbsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('QR') ! well rates
     errmsg = 'Cannot apply PEST scaling factor for well rates'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
     iwelsubsys = iopt1
     if (iwelsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)
     idat = 4
     do j = 1, nlist ! match sybsystem number
        irow=rlist(2,j); icol=rlist(3,j)
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
        !## adjust for selected system
        if (ils.eq.int(rlist(iwelsubsys,j)))then
         nadj=nadj+1
         rlist(idat,j)=rlist(idat,j)*fct
        endif
     end do

   case('MQ') ! multinode well rates
     errmsg = 'Cannot apply PEST scaling factor for multinode-well rates'
!     if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
!     iwelsubsys = iopt1
!     if (iwelsubsys.eq.0) call imod_utl_printtext(trim(errmsg),2)
     idat = 3
     do j = 1, nlist ! match sybsystem number
        irow=rlist(1,j); icol=rlist(2,j)
        !## skip as list can be longer than neccessary
        if(irow.le.0.or.icol.le.0)cycle
        !## not in current zone
        if(buff(icol,irow,1).eq.0.0d0)cycle
!        !## adjust for selected system
!        if (ils.eq.int(rlist(iwelsubsys,j)))then
        nadj=nadj+1
        rlist(idat,j)=rlist(idat,j)*fct
!        endif
     end do

    case('HF')
      errmsg = 'Cannot apply PEST scaling factor for horizontal flow barrier'
!      if (.not.present(iopt1)) call imod_utl_printtext(trim(errmsg),2)
      ihfbfact = iopt1
      if (ihfbfact.ne.1) call imod_utl_printtext(trim(errmsg),2)
      do j = 1, nlist
         !## adjust for selected system
         if (int(rlist(7,j)).eq.ils) then
            nadj = nadj + 1
            rlist(6,j) = rlist(6,j)*fct
         end if
      end do
   end select

    if(trim(param(i)%acronym).eq.'')then
     line='Param. '//trim(imod_utl_itos(i))//'['//param(i)%ptype//',n='//trim(imod_utl_itos(param(i)%nodes))// &
       ',f='//trim(imod_utl_dtos(fct,'f',7))//',ils='//trim(imod_utl_itos(ils))//']'
     else
      line='Param. '//trim(imod_utl_itos(i))//'['//trim(param(i)%acronym)//','//param(i)%ptype//',n='//trim(imod_utl_itos(param(i)%nodes))// &
       ',f='//trim(imod_utl_dtos(fct,'f',7))//',ils='//trim(imod_utl_itos(ils))//']'
    endif
    write(*,'(a)') trim(line)

end do

end subroutine
