!!  Copyright (C) Stichting Deltares, 2005-2019.
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
!!
MODULE MOD_UDF_BENCH

USE MOD_IDF_PAR
USE MOD_IDF, ONLY : IDFREAD,IDFGETVAL,IDFIROWICOL
USE MOD_UTL,ONLY : UTL_GETUNIT,UTL_TIMING

contains

subroutine udf_bench()

! program idf2udf
  ! modules
  use ifport
!  USE MODPLOT_idf
  use MOD_UDF_UTL
  implicit none
  ! locals  
  type(idfobj) :: idf
  character(len=mxlen) :: idffile, udffile, csvfile
  integer :: ncol, nrow
  REAL(KIND=DP_KIND) :: xmin, ymin, xmax, ymax, dxy, nodata, x, y
  integer :: nbench
  integer :: nrxy
  REAL(KIND=DP_KIND), dimension(:,:), allocatable :: rxy
  integer :: i, icol, irow, n, ibench
  integer, dimension(8) :: t1, t2  
  integer, dimension(:), allocatable :: ndiff 
  REAL(KIND=DP_KIND), dimension(:,:), allocatable :: td
  REAL(KIND=DP_KIND), dimension(:), allocatable :: idfval, udfval
  character(len=mxlen) :: s
  character(len=mxlen), dimension(4) :: sa
  integer :: iu
!------------------------------------------------------------------
  call getarg(1,idffile) 
  call getarg(2,udffile) 
  call getarg(3,s); read(s,*) nbench 
  call getarg(4,csvfile) 
  allocate(td(2,nbench),ndiff(nbench))
  
  ! open the IDF file
  if (.not.idfread(idf,idffile,0)) then
    write(*,*) 'Could not read '//trim(idffile)
!     call imod_utl_printtext('Could not read '//trim(idffile),2)
  end if
  ncol = idf%ncol; nrow = idf%nrow; xmin = idf%xmin; ymin =  idf%ymin; dxy = idf%dx; nodata = idf%nodata
  xmax = xmin + ncol*dxy; ymax = ymin + nrow*dxy

  ! read the UDF file
  if(UDF_OPEN(idf,udffile,1,iu))then;
   call UDF_deallocatemesh()
  endif
  
  do ibench = 1, nbench
      
    nrxy = 10**(ibench-1)    
    write(*,*) 'Running benchmark for # random xy: ',nrxy
    
    ! allocate variables
    allocate(rxy(2,nrxy),idfval(nrxy),udfval(nrxy))    
    
    ! generate random coordinates
    do i = 1, nrxy
      x = (rand(0)*(xmax-xmin))+xmin    
      y = (rand(0)*(ymax-ymin))+ymin
      x = max(xmin,x); x = min(x,xmax)
      y = max(ymin,y); y = min(y,ymax)
      rxy(1,i) = x; rxy(2,i) = y
    end do
    
    ! test calling IDF
    call date_and_time(values=t1)
    do i = 1, nrxy  
      call idfirowicol(idf,irow,icol,rxy(1,i),rxy(2,i))
      idfval(i)=idfgetval(idf,irow,icol)
    end do
    call date_and_time(values=t2)
    call UTL_TIMING(t1,t2,td(1,ibench))
  
    ! test calling UDF
    call date_and_time(values=t1)
    do i = 1, nrxy
      call readudfxy(rxy(1,i),rxy(2,i),nodata,udfval(i),iu)  
    end do    
    call date_and_time(values=t2)
    call UTL_TIMING(t1,t2,td(2,ibench))
    
    ! check values
    n = 0
    do i = 1, nrxy
      if (idfval(i) /= udfval(i)) then
        n = n + 1
      end if
    end do
    ndiff(ibench) = n
    
    ! deallocate
    deallocate(rxy,idfval,udfval)
    
  end do
  close(iu)
    
  ! write the csv file
  write(*,*) 'Writing ',trim(csvfile),'...'
  iu = utl_getunit()
  open(unit=iu,file=csvfile,status='replace')
  write(iu,'(a)') 'ncall,tidf,tudf,ndiff'
  do ibench = 1, nbench
    nrxy = 10**(ibench-1)   
    write(sa(1),*) nrxy
    write(sa(2),*) td(1,ibench)
    write(sa(3),*) td(2,ibench)
    write(sa(4),*) ndiff(ibench)
    do i = 1, 3
       write(sa(i),'(2a)') trim(adjustl(sa(i))),','    
    end do   
    write(s,'(4a)')(trim(adjustl(sa(i))),i=1,4) 
    write(iu,'(a)') trim(s)
  end do  
  close(iu) 
  
  end subroutine udf_bench
  
end module mod_udf_bench