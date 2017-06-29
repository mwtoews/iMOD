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

      !> MPP initialization phase 1.
      !! Set mpptyp flag and initialize array to store unit numbers.
      subroutine pks7mpiini1( writesto )

      use pksmpi_mod, only: mpiactive, nrproc, myrank, mpptyp, mppser,
     &                    mppini2, data_nfile, myproc                   
      
      use pksmpiwrp_mod, only: pks7mpiwrpnrproc, pks7mpiwrpmyrank

      implicit none

c...     arguments
      logical, intent(out) :: writesto !< (O) logical indicating that this process should write to std output
c.......................................................................
c
c...     initalize MPI
      mpiactive = .true.
      call pks7mpiwrpinit()

      nrproc = pks7mpiwrpnrproc
      myrank = pks7mpiwrpmyrank      
      myproc = myrank + 1
      
C################ TEMP
c      nrproc = 2

c...     set mpptyp flag and specify write to standard output
      if (nrproc.le.1) then ! serial case, no MPP
         mpptyp = mppser
         writesto = .true.
      else ! parallel case, MPP
         !mpptyp = mppini1
         mpptyp = mppini2
         data_nfile = 0
         writesto = .false.
      end if
      
      return
      end subroutine pks7mpiini1

      !> MPP initialization phase 2.
      !! This subroutine reads data from files needed for MPP.
      subroutine pks7mpiini2( in, iout )

c      use xymod
      use pksmpi_mod

      implicit none

c...     arguments
      integer, intent(in) :: in !< (I) lun MPP package file
      integer, intent(in) :: iout
      
c...     local variables
      integer :: iproc, i, idum, n    
      character(len=200) :: line, key, keyval
      integer :: lloc, istart, istop, ios
      real :: r
      logical :: lok
c.......................................................................
      ! defaults        
      partopt       = 0 
      novlapimpsol  = 1
      stenimpsol    = 2
      verbose       = 0
      skipmetis     = 0
      readpartmetis = 0
      nrprocmetis   = 0
      writegnodes   = 0
      mpiloosely    = 0
      metismethod   = 0
      gncol         = 0
      gnrow         = 0
      
#ifdef PKSUNS
      call pks7mpitmetisoptinit()                                       ! JV
#endif      
      
      lok = .true.
      pksopt1: do
         lloc=1
         call sswr_rd_comm(in)
         call urdcom(in,iout,line)
         call urword(line,lloc,istart,istop,1,i,r,iout,in)
         
         select case (line(istart:istop))
            case ('PARTOPT')   
               call urword(line,lloc,istart,istop,2,partopt,r,iout,in)
            case ('NOVLAPIMPSOL')   
               call urword(line,lloc,istart,istop,2,novlapimpsol,r,
     &            iout,in)
            case ('STENIMPSOL')   
               call urword(line,lloc,istart,istop,2,stenimpsol,r,
     &            iout,in)
            case ('VERBOSE')   
               call urword(line,lloc,istart,istop,2,verbose,r,
     &            iout,in)
            case ('GNCOL')
               call urword(line,lloc,istart,istop,2,gncol,r,
     &            iout,in)
            case ('GNROW')
               call urword(line,lloc,istart,istop,2,gnrow,r,
     &            iout,in)      
            case ('PRESSAKEY')  
               if (myrank.eq.0) then
                  write(*,*) 'Press a key...' 
                  pause
               end if
               call pks7mpibarrier()
#ifdef PKSUNS                
            case ('NRPROCMETIS')   
               call urword(line,lloc,istart,istop,2,nrprocmetis,r,
     &            iout,in)
            case ('SKIPMETIS')   
               call urword(line,lloc,istart,istop,2,skipmetis,r,
     &            iout,in)
            case ('READPARTMETIS')   
               call urword(line,lloc,istart,istop,0,i,r,iout,in)
               partmetisfile = line(istart:istop)
               readpartmetis = 1
            case ('WRITEGNODES')   
               call urword(line,lloc,istart,istop,2,writegnodes,r,
     &            iout,in)
            case ('MPILOOSELY')   
               call urword(line,lloc,istart,istop,2,mpiloosely,r,
     &            iout,in)
            case ('METISMETHOD')   
               call urword(line,lloc,istart,istop,2,metismethod,r,
     &            iout,in)
#endif               
            case('END')
               rewind(in) 
               exit
            case default
#ifdef PKSUNS                
            ! check for metis option for unstructured
               key = line(istart:istop) 
               call urword(line,lloc,istart,istop,1,partopt,r,iout,in)
               call pks7mpitmetisoptadd(trim(key),line(istart:istop),
     &            lok) 
#endif
         end select    
      end do pksopt1
C
      if (partopt.ne.0 .and.
     &    abs(partopt).ne.1 .and. abs(partopt).ne.2
     &    .and. abs(partopt).ne.3 .and. abs(partopt).ne.4 .and.
     &    abs(partopt).ne.5)  then
         if (myrank.eq.0) write(*,*) 'STOP. wrong partopt.'
         lok = .false.
      end if
      if(.not.lok) then
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if   
         
      novlapmax = novlapimpsol

c         checks      
      if (verbose.ne.0 .and. verbose.ne.1) then
         if (myrank.eq.0) 
     &      write(*,*) 'STOP. wrong verbose option (0/1).'
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if
      if (nrprocmetis.lt.0) then
         n = nargs()
         if (n.eq.3) then
            call getarg(2,line)
            read(line,*) nrprocmetis
         else
            nrprocmetis = 0
         end if   
      end if      
      if (nrprocmetis.gt.0) then
         if (verbose.ne.1) then
            nrprocmetis = 0  
         end if   
      end if       
  
      pksopt2: do
         lloc=1
         call sswr_rd_comm(in)
         call urdcom(in,iout,line)
         call urword(line,lloc,istart,istop,1,i,r,iout,in)
         select case (line(istart:istop))
         case ('PARTDATA')   
             lloc=1    
             call sswr_rd_comm(in)
             call urdcom(in,iout,line)
             
             if (abs(partopt).eq.1 .or. abs(partopt).eq.2 ) then
                call urword(line,lloc,istart,istop,2,nrproc_ncol,r,
     &             iout,in)
                call urword(line,lloc,istart,istop,2,nrproc_nrow,r,
     &             iout,in)
                allocate( nrproc_col(nrproc_ncol+1),
     &             nrproc_row(nrproc_nrow+1) )
                nrproc_col = 0
                nrproc_row = 0 
                if (nrproc_ncol*nrproc_nrow.ne.nrproc) then
                write(*,*) 'STOP. wrong nrproc_ncol and/or nrproc_nrow.'
                call pks7mpiwrpfinalize()
                call pksstop(' ')
               end if
            end if             
             
            if (abs(partopt).eq.1) then
               lloc=1    
               call sswr_rd_comm(in)
               call urdcom(in,iout,line)
                
               do i = 1, nrproc_ncol
                  call urword(line,lloc,istart,istop,2,nrproc_col(i),r,
     &               iout,in)
               end do
               lloc = 1
               call sswr_rd_comm(in)
               call urdcom(in,iout,line)
               do i = 1, nrproc_nrow
                  call urword(line,lloc,istart,istop,2,nrproc_row(i),r,
     &               iout,in)
               end do
               if (myrank.eq.0) then
                  write(*,*) 'partop = 1:'
                  write(*,*) 'nrproc_col:',(nrproc_col(i), i = 1, 
     &               nrproc_ncol)
                  write(*,*) 'nrproc_row:',(nrproc_row(i), i = 1, 
     &               nrproc_nrow)
               end if
            end if
     
            if (abs(partopt).eq.3) then
               allocate(partitionsminmax(nrproc,4))
               call urword(line,lloc,istart,istop,2,idum,r,
     &            iout,in)
               if (idum.ne.nrproc) then
                  write(*,*) 'STOP. nrproc does not match for paropt 3.'
                 call pks7mpiwrpfinalize()
                 call pksstop(' ')
               end if
               do iproc = 1, nrproc
                  lloc=1                  
                  call sswr_rd_comm(in)
                  call urdcom(in,iout,line)
                  do i = 1, 4
                     call urword(line,lloc,istart,istop,2,
     &                  partitionsminmax(iproc,i),r,iout,in)
                  end do           
               end do
            end if             
             
            case('END')
               rewind(in) 
               exit
         end select    
      end do pksopt2
     
#ifndef PKSUNS
      if (gnrow.gt.0 .and. gncol.gt.0) then
         pksopt3: do
            lloc=1
            call sswr_rd_comm(in)
            call urdcom(in,iout,line)
            call urword(line,lloc,istart,istop,1,i,r,iout,in)
            select case (line(istart:istop))
               case ('GDELR')   
                  allocate(gdelr(gncol)) 
                  call sswr_rd_comm(in)
                  call urdcom(in,iout,line)
                  read(line,*,iostat=ios)(gdelr(i),i=1,gncol)
                  if (ios.ne.0) then
                     deallocate(gdelr) 
                     allocate(gdelr(1)) 
                     lloc=1 
                     call urword(line,lloc,istart,istop,3,i,r,iout,in)
                     gdelr(1) = r
                  end if    
               case ('GDELC')   
                  allocate(gdelc(gnrow)) 
                  call sswr_rd_comm(in)
                  call urdcom(in,iout,line)
                  read(line,*,iostat=ios)(gdelc(i),i=1,gnrow)
                  if (ios.ne.0) then
                     deallocate(gdelc) 
                     allocate(gdelc(1)) 
                     lloc=1 
                     call urword(line,lloc,istart,istop,3,i,r,iout,in)
                     gdelc(1) = r
                  end if    
               case('END')
                  rewind(in) 
                  exit
            end select    
         end do pksopt3
      end if
#endif
      return
      end subroutine pks7mpiini2

      !> Set delr/delc in combination with the MET-package
      subroutine pks7mpisetdelrdelc(delr,delc,ncol,nrow,in)

      use pksmpi_mod, only: myproc, mpptyp, mppser, mppini1,
     1                      iovl, inovl, gdelr, gdelc, gncol, gnrow,
     3                      proc_icolmin, proc_icolmax,
     4                      proc_irowmin, proc_irowmax  
      
      implicit none      

c...     arguments
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      real, dimension(ncol), intent(out) :: delr
      real, dimension(nrow), intent(out) :: delc
      integer, intent(in) :: in
      
c...     locals
      integer :: j, i, i1, i2
c.......................................................................      
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return
      
      ! delr
      if (minval(gdelr).eq.maxval(gdelr))then
         delr = gdelr(1)    
      else
         i1 = proc_icolmin(myproc,iovl)
         i2 = proc_icolmax(myproc,iovl)
         j = 0
         do i = i1, i2
            j = j + 1
            delr(j) = gdelr(i)
         end do    
      end if
      
      ! delc
      if (minval(gdelc).eq.maxval(gdelc))then
         delc = gdelc(1)    
      else
         i1 = proc_irowmin(myproc,iovl)
         i2 = proc_irowmax(myproc,iovl)
         j = 0
         do i = i1, i2
            j = j + 1
            delc(j) = gdelc(i)
         end do    
      end if
      
      ! read dummy values
      call u1dreldum(in,gncol) ! delr
      call u1dreldum(in,gnrow) ! delc
           
      end subroutine pks7mpisetdelrdelc
      
      !> Store unit numbers that are to be closed or rewinded later
      !! in case of MPP.
      subroutine pks7mpistorelun( iu,iflen,fname,fmtarg,accarg,
     &                         filstat,filact,filtyp)

      use pksmpi_mod

      implicit none
c
c...     arguments
      integer           , intent(in) :: iu 
      integer           , intent(inout) :: iflen
      character(len=300), intent(inout) :: fname
      character(len=20) , intent(in) :: fmtarg
      character(len=20) , intent(in) :: accarg
      character(len=7)  , intent(in) :: filstat
      character(len=20) , intent(in) :: filact
      character(len=20) , intent(inout) :: filtyp
      
c...     locals
      character(len=300) :: fnamep
      logical :: lex
      integer :: i
c.......................................................................

      if (mpptyp.ne.mppini2) return
      if (index(filtyp,'MPPLOC').gt.0) then
         fnamep=fname(1:iflen) 
         call pks7mpifname(fnamep,iflen)
         inquire(file=fnamep,exist=lex)
         if (.not.lex) then
            write(*,'(a,1x,a)') 'Warning, ignoring MPPLOC for ',
     1         trim(fname) 
            i = index(filtyp,'(')
            if (i.gt.0) filtyp(i:) = ''
         else
            fname = fnamep    
         end if   
      end if    

      data_nfile = data_nfile + 1

      data_iu(data_nfile)      = iu
      data_iflen(data_nfile)   = iflen
      data_fname(data_nfile)   = fname
      data_fmtarg(data_nfile)  = fmtarg
      data_accarg(data_nfile)  = accarg
      data_filstat(data_nfile) = filstat
      data_filact(data_nfile)  = filact
      data_filtyp(data_nfile)  = filtyp
      
      call upcase(data_accarg(data_nfile))
      
      return
      end subroutine pks7mpistorelun
     
      subroutine pks7mpichkread( iu, ft, flg )
      use pksmpi_mod

      implicit none
c
c...     arguments
      integer, intent(in)  :: iu
      character(len=*), intent(in) :: ft
      logical, intent(out) :: flg

c...     locals
      integer :: i
c.......................................................................

      flg = .false.
      if (mpptyp.eq.mppser) then
         return
      end if   

      do i = 1, data_nfile
         if ((data_iu(i).eq.iu).and.
     1       (index(data_filtyp(i),trim(ft)).gt.0)) then
            flg = .true.
            exit
         end if
      end do 
      
      end subroutine pks7mpichkread      
            
      subroutine pks7mpiappenddata(iuin,iout)
      
!      use gwfbasmodule, only: ihedfm,iddnfm,ihedun,iddnun
      use pksmpi_mod
      
      implicit none
      
      ! arguments
      integer, intent(in) :: iuin
      integer, intent(in) :: iout

      ! locals
      character(len=300):: fname
      character(len=20) :: fmtarg
      character(len=20) :: accarg
      character(len=7)  :: filstat
      character(len=20) :: filact
      integer   :: i, j, iu, iflen      
c.......................................................................

      if (mpptyp.ne.mppini2) return

      do i = 1, data_nfile
         fname   = data_fname(i)
         iflen   = data_iflen(i)
         iu      = data_iu(i)
         fmtarg  = data_fmtarg(i)
         accarg  = data_accarg(i)
         filstat = data_filstat(i) 
         filact  = data_filact(i)
         
         ! append file name
         if (iu.eq.iuin) then
            if (myrank.gt.0) close(unit=iu)
            call pks7mpibarrier()
            if (myrank.eq.0) close(unit=iu,status='delete')
            call pks7mpifname(fname,iflen) 
         ! open file
            open(unit=iu,
     &         file=fname(1:iflen),
     &         form=fmtarg,
     &         access=accarg,
     &         status=filstat,
     &         action=filact,err=2000)
         end if
      end do
    
      return
 2000 CONTINUE
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (pks7mpiappenddata)')
      CALL pksstop(' ')
      
      return
      end subroutine pks7mpiappenddata    
      
      !> subroutine to write debug d.p. array
      subroutine pks7mpiwritedbg(x,n)
      use pksmpi_mod, only: myrank, l2gnod
      
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x

      character(len=100) :: fname 
      integer, parameter :: lun = 99
      integer :: iflen, i          
      
      fname = 'dval'; iflen = 4
      call pks7mpifname(fname,iflen)
      write(*,'(a,i3.3,1x,a,1x,a)') 'p',myrank,': writing',
     1   trim(fname),'...'
      
      open(unit=lun,file=fname,form='formatted',
     1     access='sequential',status='unknown',action='write')
     
      write(lun,*) n 
      do i = 1, n
         write(lun,*) l2gnod(i), x(i)
      end do   
      close(lun)
      
      end subroutine pks7mpiwritedbg
         
      !> set geometry for the structured case
      subroutine pks7mpisetgnodes( ncol, nrow, nlay)
      
c...     modules      
      use pksmpi_mod, only: mpptyp, mppser, mppini1, 
     1                      gncol, gnrow, gnlay, gnodes 

      implicit none
      
c...     arguments
      integer, intent(in) :: ncol  !< (I) number of global columns
      integer, intent(in) :: nrow  !< (I) number of global rows
      integer, intent(in) :: nlay  !< (I) number of global lays
      
c.......................................................................

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return
 
c...     store global ncol, nrow, nlay and nodes
      gncol  = ncol
      gnrow  = nrow
      gnlay  = nlay
      gnodes = ncol*nrow*nlay
      
      end subroutine pks7mpisetgnodes
      
      !> MPP partitioning in row and column direction.
      subroutine pks7mpipart( ncol, nrow, nlay, insms, iout )

      use pksmpi_mod

      implicit none
c
c...     arguments
      integer, intent(in) :: ncol  !< (I) number of global columns
      integer, intent(in) :: nrow  !< (I) number of global rows
      integer, intent(in) :: nlay  !< (I) number of (global) lays
      integer, intent(in) :: insms
      integer, intent(in) :: iout

c...     local variables
      logical :: lin
      integer :: i, j, k, nodes, nrc, nr, nc, iproc, jproc, nmax, n, m,
     1           girow, gicol, ilay     
      integer, dimension(:,:), allocatable :: wrk
      integer, dimension(8) :: comm
      double precision :: mask

c...     RCB      
      real :: nodata
      real, dimension(:,:), allocatable :: loadptr
      character(len=24) :: aname
      logical :: lfound
      character(len=200) :: line
      integer :: lloc, istart, istop
      real    :: r      
      
c...     functions
      integer :: pks7mpig2lnode, pks7mpil2gnode
      logical :: pks7mpignodeinpart
c.......................................................................
 
c...     set global geometry
      call pks7mpisetgnodes( ncol, nrow, nlay)

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

c...     allocate for all processes.
c        Dim 1 = overlap, dim 2 = no overlap
      allocate( proc_ncol(nrproc,2) )    ! number of columns
      allocate( proc_nrow(nrproc,2) )    ! number of rows
      allocate( proc_nodes(nrproc,2) )   ! number of nodes
      allocate( proc_icolmin(nrproc,2) ) ! column start index
      allocate( proc_icolmax(nrproc,2) ) ! column end index
      allocate( proc_irowmin(nrproc,2) ) ! row start index
      allocate( proc_irowmax(nrproc,2) )  ! row end index
 
c...     determine number of processes in column/row direction
c        (nrproc_ncol, nrproc_nrow) and number of column/rows per process
c        (proc_ncol, proc_nrow)
      if (partopt.eq.0) ! default partitioning
     &   call pks7mpipartdef( proc_icolmin, proc_icolmax, 
     &                     proc_irowmin, proc_irowmax )
      if (abs(partopt).eq.1)  ! user specifies partition
     &   call pks7mpipartuser( proc_icolmin, proc_icolmax, 
     &                      proc_irowmin, proc_irowmax )
      if (abs(partopt).eq.2) ! load balancing
     &   call pks7mpipartbal( proc_icolmin, proc_icolmax, 
     &                     proc_irowmin, proc_irowmax, insms, iout )
      if (abs(partopt).eq.3) ! irregular
     &   call pks7mpipartuser2( proc_icolmin, proc_icolmax, 
     &                       proc_irowmin, proc_irowmax)
      if (abs(partopt).eq.4) ! load balancing
     &   call pks7mpipartuser3( proc_icolmin, proc_icolmax, 
     &                       proc_irowmin, proc_irowmax, insms, iout )
#ifndef PKSMPINORCB      
      if (abs(partopt).eq.5) then ! recursive coordinate bisection
c...         read the load pointer 
         allocate( loadptr(gncol,gnrow) )
         lfound = .false.
         pksopt: do
            lloc=1
            call sswr_rd_comm(insms)
            call urdcom(insms,iout,line)
            call urword(line,lloc,istart,istop,1,i,r,iout,insms)
            select case (line(istart:istop))
               case ('PARTDATA')   
                  lfound = .true.
                  lloc=1
                  call sswr_rd_comm(insms)
                  call urdcom(insms,iout,line)
                  call urword(line,lloc,istart,istop,3,i,nodata,iout,
     &               insms)
                  aname = 'mpp load pointer        '
                  call u2drel(loadptr(1,1),aname,gnrow,gncol,0,insms,
     &               iout)
               case('END')
                  rewind(insms) 
                  exit
               case default
                  write (iout,*) 'unrecognized PKS option: ', 
     &                            line(istart:istop)             
            end select    
         end do pksopt
     
         if (.not.lfound) then
            write(*,*) 'Could not find PARTLOAD'
            call pks7mpiwrpfinalize()
            call pksstop(' ')
         end if  
         
         call pks7mpipartrcb( proc_icolmin, proc_icolmax, 
     &                        proc_irowmin, proc_irowmax,
     &                        ncol, nrow, loadptr, 0.)
         deallocate(loadptr)
      end if   
#endif
      
c...     determine number of nodes per process
      proc_nodes = 0
      do iproc = 1, nrproc
         proc_nodes(iproc,inovl) = gnlay*
     1      (proc_irowmax(iproc,inovl)-proc_irowmin(iproc,inovl)+1)*
     1      (proc_icolmax(iproc,inovl)-proc_icolmin(iproc,inovl)+1)
         proc_ncol(iproc,inovl) = proc_icolmax(iproc,inovl)-
     &      proc_icolmin(iproc,inovl)+1
         proc_nrow(iproc,inovl) = proc_irowmax(iproc,inovl)-
     &      proc_irowmin(iproc,inovl)+1
      end do

c...     correct for partition overlap
      do iproc = 1, nrproc
         proc_icolmin(iproc,iovl) =
     &      max(proc_icolmin(iproc,inovl)-novlapmax, 1)
         proc_icolmax(iproc,iovl) =
     &      min(proc_icolmax(iproc,inovl)+novlapmax, gncol)

         proc_irowmin(iproc,iovl) =
     &      max(proc_irowmin(iproc,inovl)-novlapmax, 1)
         proc_irowmax(iproc,iovl) =
     &      min(proc_irowmax(iproc,inovl)+novlapmax, gnrow)

         proc_ncol(iproc,iovl) = proc_icolmax(iproc,iovl)-
     &      proc_icolmin(iproc,iovl)+1
         proc_nrow(iproc,iovl) = proc_irowmax(iproc,iovl)-
     &      proc_irowmin(iproc,iovl)+1

         proc_nodes(iproc,iovl) =
     &     proc_ncol(iproc,iovl)*proc_nrow(iproc,iovl)*gnlay
      end do

c         write partition data
c...     master process write partition information to out file
      if (myrank.eq.0) then
         write(iout,10) nrproc
   10    format(1x,'Number of MPI processes:',1x,i3)
         write(iout,15) 'implicit solver',novlapimpsol

   15    format(1x,a,':',1x,'overlap of',1x,i2,1x,'cells')

         do iproc = 1, nrproc
            write(iout,30) iproc-1,
     &               proc_ncol(iproc,inovl),
     &               proc_nrow(iproc,inovl),
     &               proc_nodes(iproc,inovl),
     &               proc_icolmin(iproc,inovl),
     &               proc_icolmax(iproc,inovl),
     &               proc_irowmin(iproc,inovl),
     &               proc_irowmax(iproc,inovl),
     &               proc_ncol(iproc,iovl),
     &               proc_nrow(iproc,iovl),
     &               proc_nodes(iproc,iovl),
     &               proc_icolmin(iproc,iovl),
     &               proc_icolmax(iproc,iovl),
     &               proc_irowmin(iproc,iovl),
     &               proc_irowmax(iproc,iovl)            
            end do
   30       format(1x,'p',i3.3,1x,':',
     &             1x,2(i4,1x),i7,4(1x,i4),1x,'|',
     &             1x,2(i4,1x),i7,4(1x,i4),1x)      
      end if 
      
      if (verbose.eq.1) then
         !call timing_write() 
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if      
      
c allocate wrk
      nmax = 2*gncol + 2*gnrow
      allocate(wrk(nmax,2), topol(nrproc,nrproc))
      topol = 0
  
c determine the topology
      do iproc = 1, nrproc
         do jproc = 1, nrproc 
c           get cells in band
            call pks7mpigetoverlap(proc_icolmin(iproc,inovl), 
     &                          proc_icolmax(iproc,inovl),
     &                          proc_irowmin(iproc,inovl),
     &                          proc_irowmax(iproc,inovl),
     &                          stenimpsol, 1, wrk, nmax, n,
     &                          comm)
c           loop over cells
            do i = 1, n
c               if (myrank.eq.0) 
c     &            write(*,*) 'iproc, wrk:',iproc,wrk(i,1), wrk(i,2)
               lin =  pks7mpignodeinpart(wrk(i,1), wrk(i,2), 
     &                          proc_icolmin(jproc,inovl),
     &                          proc_icolmax(jproc,inovl),
     &                          proc_irowmin(jproc,inovl),
     &                          proc_irowmax(jproc,inovl))        
c              label        
               if (lin) topol(iproc,jproc) = 1
            end do 
         end do
      end do

c band nodes   
      call pks7mpigetoverlap(proc_icolmin(myproc,inovl), 
     &                    proc_icolmax(myproc,inovl),
     &                    proc_irowmin(myproc,inovl),
     &                    proc_irowmax(myproc,inovl),
     &                    -stenimpsol, novlapmax, wrk, nmax, n,
     &                    comm)
      nbandnodes = gnlay*n 
      allocate(bandnodes(nbandnodes))
      m = 0
      do ilay = 1, gnlay
         do i = 1, n
            m = m + 1
            if (wrk(i,1).lt.0 .and. wrk(i,2).lt.0) then
               bandnodes(m) = -pks7mpig2lnode(abs(wrk(i,1)),
     &                                     abs(wrk(i,2)),ilay)
            else
               bandnodes(m) = pks7mpig2lnode(wrk(i,1),wrk(i,2),ilay)
            end if  
         end do
      end do
 
c...     dealocate
      deallocate(wrk)
     
c...    create local-to-global mask
      allocate(l2gnod(proc_nodes(myproc,iovl)))
      do n = 1, proc_nodes(myproc,iovl)
         m = pks7mpil2gnode( n, myrank )
         call pks7mpimaskn( mask, n,
     1      proc_ncol(myproc,iovl), proc_nrow(myproc,iovl), nlay )
         if(mask.lt.1.d0) m = -m
         l2gnod(n) = m
      end do   
      
c...     close the MPP package input file
c     close(in)

      return
      end subroutine pks7mpipart
     
      subroutine pks7mpisetmask(mask,nodes)
      use pksmpi_mod

      implicit none
c
c...     arguments
      integer, intent(in)           :: nodes        !< (I) number of node
      double precision, intent(out) :: mask(nodes)  !< (I) mask array

c...     local variables
      integer :: i, j, n, nrc, girow, gicol     
c.......................................................................

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) then
         mask = 1d0
         return       
      end if

c...     mask
      mask = 1.0d0
      do i = 1, proc_nrow(myproc,iovl)
         do j = 1, proc_ncol(myproc,iovl)
            n = j + (i-1)*proc_ncol(myproc,iovl)
            gicol = proc_icolmin(myproc,iovl)+j-1
            girow = proc_irowmin(myproc,iovl)+i-1
            if (gicol.lt.proc_icolmin(myproc,inovl) .or.
     &          gicol.gt.proc_icolmax(myproc,inovl) .or.
     &          girow.lt.proc_irowmin(myproc,inovl) .or.
     &          girow.gt.proc_irowmax(myproc,inovl) ) then
               mask(n) = 0.0d0
            end if
         end do 
      end do 

      return
      end subroutine pks7mpisetmask

      subroutine pks7mpigetoverlap(ic1, ic2, ir1, ir2, stencil, ovl,
     1                          colrow, nmax, n, comm)

      use pksmpi_mod, only: gncol, gnrow, myrank 

      implicit none

      integer, intent(in) :: ic1
      integer, intent(in) :: ic2
      integer, intent(in) :: ir1
      integer, intent(in) :: ir2
      integer, intent(in) :: stencil
      integer, intent(in) :: ovl
      integer, intent(in) :: nmax
      integer, dimension(nmax,2), intent(out) :: colrow
      integer, intent(out) :: n
      integer, dimension(8), intent(out) :: comm
      
c locals
      logical :: ln, ls, le, lw
      integer :: icol, irow

      n = 0
      ln = .false.
      ls = .false.
      le = .false.
      lw = .false.
      comm = 0

c north
      irow = ir1-ovl 
      if (irow.ge.1) then
         ln = .true.      
         comm(1) = 1  
         do icol = ic1, ic2
            n = n + 1  
            if (n.gt.nmax) then
               if (myrank.eq.0) write(*,*) 'STOP. n > nmax, n.'
               call pks7mpiwrpfinalize()
               call pksstop(' ')
            end if
            colrow(n,1) = icol
            colrow(n,2) = irow
         end do
      end if
c south
      irow = ir2+ovl 
      if (irow.le.gnrow) then
         ls = .true.
         comm(2) = 1  
         do icol = ic1, ic2
            n = n + 1       
            if (n.gt.nmax) then
               if (myrank.eq.0) write(*,*) 'STOP. n > nmax, s.'
               call pks7mpiwrpfinalize()
               call pksstop(' ')
            end if
            colrow(n,1) = icol
            colrow(n,2) = irow
         end do
      end if
c west
      icol = ic1-ovl 
      if (icol.ge.1) then
         lw = .true.
         comm(3) = 1  
         do irow = ir1, ir2
            n = n + 1       
            if (n.gt.nmax) then
               if (myrank.eq.0) write(*,*) 'STOP. n > nmax, w.'
               call pks7mpiwrpfinalize()
               call pksstop(' ')
            end if
            colrow(n,1) = icol
            colrow(n,2) = irow
         end do
      end if
c east
      icol = ic2+ovl 
      if (icol.le.gncol) then
         le = .true.
         comm(4) = 1  
         do irow = ir1, ir2
            n = n + 1                
            if (n.gt.nmax) then
               if (myrank.eq.0) write(*,*) 'STOP. n > nmax, e.'
               call pks7mpiwrpfinalize()
               call pksstop(' ')
            end if
            colrow(n,1) = icol
            colrow(n,2) = irow
         end do
      end if
c 
      if (abs(stencil).eq.2 .or. stencil.lt.0) then
c north-west       
         if (ln.and.lw) then
            comm(5) = 1  
            irow = ir1-ovl 
            do icol = ic1-ovl, ic1-1
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, nw 1.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
            icol = ic1-ovl
            do irow = ir1-ovl+1, ir1-1
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, nw 2.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
         end if 
c north-east       
         if (ln.and.le) then
            comm(6) = 1  
            irow = ir1-ovl 
            do icol = ic2+1, ic2+ovl
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, ne 1.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
            icol = ic2+ovl
            do irow = ir1-ovl+1, ir1-1
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, ne 2.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
         end if 
c south-west       
         if (ls.and.lw) then
            comm(7) = 1  
            irow = ir2+ovl 
            do icol = ic1-ovl, ic1-1
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, sw 1.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
            icol = ic1-ovl
            do irow = ir2+1, ir2+ovl-1
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, sw 2.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
         end if 
c south-east       
         if (ls.and.le) then
            comm(8) = 1  
            irow = ir2+ovl 
            do icol = ic2+1, ic2+ovl
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, se 1.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
            icol = ic2+ovl
            do irow = ir2+1, ir2+ovl-1
               n = n + 1       
               if (n.gt.nmax) then
                  if (myrank.eq.0) write(*,*) 'STOP. n > nmax, se 2.'
                  call pks7mpiwrpfinalize()
                  call pksstop(' ')
               end if
               colrow(n,1) = icol
               colrow(n,2) = irow
               if (abs(stencil).eq.1) then
                  colrow(n,1) = -colrow(n,1)
                  colrow(n,2) = -colrow(n,2)
               end if 
            end do
         end if 
      elseif (stencil.eq.1) then ! do nothing
      else
         if (myrank.eq.0) write(*,*) 'STOP. invalid stencil:',stencil
         call pks7mpiwrpfinalize()
         call pksstop(' ')      
      end if

      end subroutine

      logical function pks7mpignodeinpart(ic, ir, ic1, ic2, ir1, ir2)
     
      use pksmpi_mod, only: myrank

      implicit none

      integer, intent(in) :: ic
      integer, intent(in) :: ir
      integer, intent(in) :: ic1
      integer, intent(in) :: ic2
      integer, intent(in) :: ir1
      integer, intent(in) :: ir2

      pks7mpignodeinpart = .false.
      if (ic1.le.ic .and. ic.le.ic2 .and. ir.le.ir2 .and. ir.ge.ir1) 
     1   pks7mpignodeinpart = .true.

      end function

      subroutine pks7mpiminmax(nrproc_nrow, nrproc_ncol, 
     1   proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax,
     1   proc_ncol, proc_nrow ) 
      use pksmpi_mod, only: nrproc, inovl

      implicit none

      integer, intent(in) :: nrproc_nrow
      integer, intent(in) :: nrproc_ncol
      integer, dimension(nrproc,2), intent(out) :: proc_icolmin
      integer, dimension(nrproc,2), intent(out) :: proc_icolmax
      integer, dimension(nrproc,2), intent(out) :: proc_irowmin
      integer, dimension(nrproc,2), intent(out) :: proc_irowmax
      integer, dimension(nrproc,2), intent(in) :: proc_ncol
      integer, dimension(nrproc,2), intent(in) :: proc_nrow

      integer :: i, j, nc, nr, iproc

c...     determine start column/row indices
      do i = 1, nrproc_nrow
         nc = 0
         do j = 1, nrproc_ncol
            iproc = j + (i-1)*nrproc_ncol
            proc_icolmin(iproc,inovl) = nc + 1
            nc = nc + proc_ncol(iproc,inovl)
            proc_icolmax(iproc,inovl) = nc
         end do
      end do
      do j = 1, nrproc_ncol
         nr = 0
         do i = 1, nrproc_nrow
            iproc = j + (i-1)*nrproc_ncol
            proc_irowmin(iproc,inovl) = nr + 1
            nr = nr + proc_nrow(iproc,inovl)
            proc_irowmax(iproc,inovl) = nr
         end do
      end do
        
      end subroutine

      !> MPP partitioning: default
      subroutine pks7mpipartdef( proc_icolmin, proc_icolmax, 
     &                        proc_irowmin, proc_irowmax )

      use pksmpi_mod, only: nrproc, gncol, gnrow, inovl, novlapmax
      use pksmpi_mod, only: myrank

      implicit none

c...     arguments
      integer, dimension(nrproc,2), intent(out) :: proc_icolmin
      integer, dimension(nrproc,2), intent(out) :: proc_icolmax
      integer, dimension(nrproc,2), intent(out) :: proc_irowmin
      integer, dimension(nrproc,2), intent(out) :: proc_irowmax     

c...     local variables
      logical, parameter :: debug = .false.
      integer :: iproc, i, j, n, nc, nrc, nrr, blksize, nproc
      integer :: wncol, wnrow
      integer, dimension(:), allocatable :: divide
      integer                    :: nrproc_ncol !< number of column processes
      integer                    :: nrproc_nrow !< number of row processes
      integer, dimension(:,:), allocatable :: proc_ncol   !< number of columns for each process
      integer, dimension(:,:), allocatable :: proc_nrow   !< number of rows for each process
     
      character(len=100) :: fname
      integer, dimension(:,:), allocatable :: proc_jcolmin
      integer, dimension(:,:), allocatable :: proc_jcolmax
      integer, dimension(:,:), allocatable :: proc_jrowmin
      integer, dimension(:,:), allocatable :: proc_jrowmax    

c.......................................................................
   
      if (debug) then
         nproc = 128
         allocate(proc_jcolmin(nproc,2),
     &            proc_jcolmax(nproc,2),
     &            proc_jrowmin(nproc,2),
     &            proc_jrowmax(nproc,2))
      else
         nproc = nrproc 
      end if
      allocate(proc_ncol(nproc,2),proc_nrow(nproc,2))

c...     allocate work array
      allocate( divide(nproc) )

      do i = 1, nproc
         j = nproc/i
         if (i*j.eq.nproc) then
            divide(i) = 1
         else
            divide(i) = 0
         end if
      end do
      
      wncol = max(gncol-2*novlapmax,1)
      wnrow = max(gnrow-2*novlapmax,1)
      
      blksize = sqrt(real(wncol*wnrow/nproc))
      n  = min( wncol, wnrow )
      nc = max( 1, int(n/blksize) )
      j  = 1

      do while (divide(nc).eq.0)
         if(divide(nc+j).eq.0) then
            if (divide(nc-j).eq.0) then
               j=j-1
            else
               nc=nc+j
            endif
         else
            nc=nc+j
         endif
      enddo

      if (wncol.lt.wnrow) then
         nrproc_ncol = nc
         nrproc_nrow = nproc/nc
      else
         nrproc_ncol = nproc/nc
         nrproc_nrow = nc
      end if

      iproc = 0
      nrr = wnrow
      do i = nrproc_nrow, 1, -1
         nrc = wncol
         do j = nrproc_ncol, 1, -1
            iproc = iproc + 1
            proc_ncol(iproc,inovl) = int(nrc/j)
            proc_nrow(iproc,inovl) = int(nrr/i)
            nrc = nrc - proc_ncol(iproc,inovl)
         end do
         nrr = nrr - proc_nrow(iproc,inovl)
      end do 
      
      deallocate( divide )

c fill partitions
      if (debug) then
         call pks7mpiminmax(nrproc_nrow, nrproc_ncol, 
     1      proc_jcolmin, proc_jcolmax, proc_jrowmin, proc_jrowmax,
     1      proc_ncol, proc_nrow )
 
c         write(fname,'(a,i3.3,a)') 'partitions_np',nproc,'.gen'
c         call writepartgen(fname, nproc, gnrow,
c     &       proc_jcolmin, proc_jcolmax, proc_jrowmin, proc_jrowmax)

         call pks7mpiwrpfinalize()
         call pksstop(' ')
      else
         call pks7mpiminmax(nrproc_nrow, nrproc_ncol, 
     1      proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax,
     1      proc_ncol, proc_nrow )
      end if

c...     correct for partition overlap
      do iproc = 1, nrproc
         proc_icolmin(iproc,inovl) = proc_icolmin(iproc,inovl)+novlapmax
         proc_icolmax(iproc,inovl) = proc_icolmax(iproc,inovl)+novlapmax
         proc_irowmin(iproc,inovl) = proc_irowmin(iproc,inovl)+novlapmax
         proc_irowmax(iproc,inovl) = proc_irowmax(iproc,inovl)+novlapmax
         if (proc_icolmin(iproc,inovl).eq.novlapmax+1)
     1      proc_icolmin(iproc,inovl) = 1   
         if (proc_icolmax(iproc,inovl).eq.novlapmax+wncol)
     1      proc_icolmax(iproc,inovl) = gncol
         if (proc_irowmin(iproc,inovl).eq.novlapmax+1)
     1      proc_irowmin(iproc,inovl) = 1
         if (proc_irowmax(iproc,inovl).eq.novlapmax+wnrow)
     1      proc_irowmax(iproc,inovl) = gnrow
      end do      
      
      deallocate(proc_ncol,proc_nrow)

      return
      end subroutine pks7mpipartdef

      !> MPP partitioning: load balance
      subroutine pks7mpipartbal( proc_icolmin, proc_icolmax, 
     &                        proc_irowmin, proc_irowmax, in, iout )

      use pksmpi_mod, only: nrproc, gncol, gnrow, nrproc_ncol,
     &                   nrproc_nrow, inovl

      implicit none

c...     arguments
      integer, dimension(nrproc,2), intent(out) :: proc_icolmin
      integer, dimension(nrproc,2), intent(out) :: proc_icolmax
      integer, dimension(nrproc,2), intent(out) :: proc_irowmin
      integer, dimension(nrproc,2), intent(out) :: proc_irowmax     
      integer                   , intent(in)    :: in      !< unit number MPP input file
      integer                   , intent(in)    :: iout

c...     local variables
      integer, dimension(nrproc,2)      :: proc_ncol  !< number of column processes
      integer, dimension(nrproc,2)      :: proc_nrow  !< number of row processes
      integer, dimension(nrproc_ncol+1) :: nrproc_col !< number of columns for each process
      integer, dimension(nrproc_nrow+1) :: nrproc_row !< number of rows for each process
       character(len=24) :: aname
      integer, dimension(:), allocatable :: colsum, rowsum
      real, dimension(:,:), allocatable :: loadptr
      integer :: i, j, l, ltot, irow, icol, n, iproc
      real    :: dl, nodata
      
      logical :: lfound
      character(len=200) :: line
      integer :: lloc, istart, istop
      real    :: r
c.......................................................................

      allocate( loadptr(gncol,gnrow) )
     
      allocate( rowsum(gncol), colsum(gnrow) )
      do irow = 1, gnrow
         n = 0
         do icol = 1, gncol
            if (loadptr(icol,irow).gt.0.) n = n + 1
         end do
         if (irow.eq.1) then
            colsum(irow) = n
         else
            colsum(irow) = colsum(irow-1) + n
         end if
      end do
      do icol = 1, gncol
         n = 0
         do irow = 1, gnrow
            if (loadptr(icol,irow).gt.0.) n = n + 1
         end do
         if (icol.eq.1) then
            rowsum(icol) = n
         else
            rowsum(icol) = rowsum(icol-1) + n
         end if
      end do

      if (colsum(gnrow).ne.rowsum(gncol)) then
         write(*,*) 'Program error partopt 2'
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if
      ltot = colsum(gnrow)

      nrproc_col(1) = 1
      do i = 1, nrproc_ncol-1
         l = int(i*ltot/nrproc_ncol)
         icol = 0
         dl = 1e10
         do j = 1, gncol
            if (abs(l-rowsum(j)).lt.dl) then
              icol = j
              dl = abs(l-rowsum(j))
            end if
         end do
         if (icol.eq.0) then
            write(*,*) 'Program error partopt 2'
            call pks7mpiwrpfinalize()
            call pksstop(' ')
         else
            nrproc_col(i+1) =  min(icol+1,gncol)
         end if
      end do

      nrproc_row(1) = 1
      do i = 1, nrproc_nrow-1
         l = int(i*ltot/nrproc_nrow)
         irow = 0
         dl = 1e10
         do j = 1, gnrow
            if (abs(l-colsum(j)).lt.dl) then
              irow = j
              dl = abs(l-colsum(j))
            end if
         end do
         if (irow.eq.0) then
            write(*,*) 'Program error partopt 2'
            call pks7mpiwrpfinalize()
            call pksstop(' ')
         else
            nrproc_row(i+1) = min(irow+1,gnrow)
         end if
      end do

      deallocate( loadptr, colsum, rowsum )

      nrproc_col(nrproc_ncol+1) = gncol + 1
      nrproc_row(nrproc_nrow+1) = gnrow + 1

      do i = 1, nrproc_nrow
         do j = 1, nrproc_ncol
            iproc = j + (i-1)*nrproc_ncol
            proc_ncol(iproc,inovl) = nrproc_col(j+1)-
     &                           nrproc_col(j)
            proc_nrow(iproc,inovl) = nrproc_row(i+1)-
     &                           nrproc_row(i)
         end do
      end do

c fill partitions
      call pks7mpiminmax(nrproc_nrow, nrproc_ncol, 
     1   proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax,
     1   proc_ncol, proc_nrow )
 
      return
      end subroutine pks7mpipartbal

      !> MPP partitioning: load balance
      subroutine pks7mpipartuser3( proc_icolmin, proc_icolmax, 
     &                          proc_irowmin, proc_irowmax, in, iout )

      use pksmpi_mod, only: nrproc, inovl, myrank, gncol, gnrow

      implicit none

c...     arguments
      integer, dimension(nrproc,2), intent(out) :: proc_icolmin
      integer, dimension(nrproc,2), intent(out) :: proc_icolmax
      integer, dimension(nrproc,2), intent(out) :: proc_irowmin
      integer, dimension(nrproc,2), intent(out) :: proc_irowmax     
      integer                   , intent(in)    :: in      !< unit number MPP input file
      integer                   , intent(in)    :: iout

c...     local variables
      character(len=24) :: aname
      real, dimension(:,:), allocatable :: partitions
      integer :: icol, irow, iproc
      real :: nodata
      
      logical :: lfound
      character(len=200) :: line
      integer :: i, lloc, istart, istop
      real    :: r
c.......................................................................
#ifndef PKSMPINORCB
      allocate( partitions(gncol,gnrow) )

      lfound = .false.
      pksopt: do
         lloc=1
         call sswr_rd_comm(in)
         call urdcom(in,iout,line)
         call urword(line,lloc,istart,istop,1,i,r,iout,in)
         select case (line(istart:istop))
            case ('PARTDATA')   
               lfound = .true.
               lloc=1
               call sswr_rd_comm(in)
               call urdcom(in,iout,line)
               call urword(line,lloc,istart,istop,3,i,nodata,iout,in)
               aname = 'mpp load pointer        '
               call u2drel(partitions,aname,gnrow,gncol,0,in,iout) 
            case('END')
               rewind(in) 
               exit
            case default
               write (iout,*) 'unrecognized PKS option: ', 
     &                         line(istart:istop)             
         end select    
      end do pksopt     
 
      proc_icolmin = gncol + 1
      proc_icolmax = 0
      proc_irowmin = gnrow + 1
      proc_irowmax = 0

      do irow = 1, gnrow
         do icol = 1, gncol
            iproc = int(partitions(icol,irow))
            if (iproc.gt.nrproc) then
               if (myrank.eq.0) write(*,*) 'STOP. iproc > nrproc.',iproc
               call pks7mpiwrpfinalize()
               call pksstop(' ')
            end if
            if (iproc.gt.0) then
               proc_icolmin(iproc,inovl) = 
     &            min(icol,proc_icolmin(iproc,inovl)) 
               proc_icolmax(iproc,inovl) =
     &            max(icol,proc_icolmax(iproc,inovl)) 
               proc_irowmin(iproc,inovl) =
     &            min(irow,proc_irowmin(iproc,inovl)) 
               proc_irowmax(iproc,inovl) =
     &            max(irow,proc_irowmax(iproc,inovl)) 
            end if            
         end do
      end do   

      deallocate( partitions )
#else      
      proc_icolmin = 0
      proc_icolmax = 0
      proc_irowmin = 0
      proc_irowmax = 0
#endif
      return
      end subroutine pks7mpipartuser3

      !> MPP partitioning: load balance
      subroutine pks7mpipartrcb( proc_icolmin, proc_icolmax, 
     &                        proc_irowmin, proc_irowmax,
     &                        ncol, nrow, loadptr, nodata )

      use pksmpi_mod, only: nrproc, inovl, myrank

      implicit none

c...     arguments
      integer, dimension(nrproc,2), intent(out) :: proc_icolmin
      integer, dimension(nrproc,2), intent(out) :: proc_icolmax
      integer, dimension(nrproc,2), intent(out) :: proc_irowmin
      integer, dimension(nrproc,2), intent(out) :: proc_irowmax
      integer, intent(in)                       :: ncol
      integer, intent(in)                       :: nrow
      real, dimension(ncol,nrow), intent(in)    :: loadptr
      real, intent(in)                          :: nodata

c...     local variables
       integer :: icol, irow, iproc, maxlev, ilev

      character(len=100) :: fname
      integer, parameter :: maxproc = 256
      integer, dimension(maxproc,2) :: proc_jcolmin
      integer, dimension(maxproc,2) :: proc_jcolmax
      integer, dimension(maxproc,2) :: proc_jrowmin
      integer, dimension(maxproc,2) :: proc_jrowmax    
      integer :: nproc
      logical :: ok
      real, dimension(:,:), allocatable :: partitions 
      integer :: ic1, ic2, ir1, ir2, load
      integer :: i, f
      integer, dimension(:), allocatable :: ncut
      
c.......................................................................

c      do nproc = 1, 128 ! debug

      nproc = nrproc
c      nproc = 144 ! debug    

      allocate(ncut(nproc/2))
      call pks7mpiprimefactors(nproc,ncut,maxlev)
      
      if (.true.) then
      ilev   = 0
      iproc  = 0
      call pks7mpircb(loadptr,nodata,ncol,nrow,1,ncol,1,nrow,
     &   ilev,maxlev,iproc,nproc, 
     &   proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax,
     &   ncut)
      end if

      if (.false. .and. myrank.eq.0) then ! debug

      proc_jcolmin = 0
      proc_jcolmax = 0
      proc_jrowmin = 0
      proc_jrowmax = 0
      
      ilev   = 0
      iproc  = 0
      call pks7mpircb(loadptr,nodata,ncol,nrow,1,ncol,1,nrow,
     &   ilev,maxlev,iproc, nproc, 
     &   proc_jcolmin, proc_jcolmax, proc_jrowmin, proc_jrowmax,
     &   ncut)

      allocate(partitions(ncol,nrow))
      partitions = -9999.
      ok = .true.
      do iproc = 1, nproc
         ic1 = proc_jcolmin(iproc,inovl)
         ic2 = proc_jcolmax(iproc,inovl)
         ir1 = proc_jrowmin(iproc,inovl)
         ir2 = proc_jrowmax(iproc,inovl)
         load = 0
         do irow = ir1, ir2
            do icol = ic1, ic2
               if (partitions(icol,irow).gt.-9999.) then
                   write(*,*) 'ERROR, non overlap detected.' 
                   write(*,*) 'partition overwritten', 
     &                partitions(icol,irow) 
                   ok = .false.
                else
                   partitions(icol,irow) = iproc
                   if (loadptr(icol,irow).gt.0.) load = load + 1
               end if 
            end do
         end do
         write(*,*) 'load iproc:',iproc,load
      end do
      write(fname,'(a,i3.3,a)') 'partitions_np',nproc,'.asc'
      !call rwriteasc2d(partitions,gncol,gnrow,
    ! 1              fname,-9999.)
c      write(fname,'(a,i3.3,a)') 'partitions_np',nproc,'.gen'
c      call writepartgen(fname, nproc, gnrow,
c     &    proc_jcolmin, proc_jcolmax, proc_jrowmin, proc_jrowmax)

      if (.not.ok) then 
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if

      deallocate(partitions) 
      end if ! debug
      
      deallocate(ncut)
c      end do ! debug

c      if (myrank.eq.0 .and. .false.) then
c         write(fname,'(a,i3.3,a)') 'partitions_np',nproc,'.gen'
c         call writepartgen(fname, nproc, gnrow,
c     &       proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax)
c      end if

c      call pks7mpiwrpfinalize()
c      call pksstop(' ')

      return
      end subroutine pks7mpipartrcb
 
      recursive subroutine pks7mpircb(a,nodata,ncol,nrow,
     &   ic1,ic2,ir1,ir2,ilev,maxlev,iproc,
     &   nrproc,proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax,
     &   ncut)

      use pksmpi_mod, only: inovl
      implicit none

c...     arguments
      integer, intent(inout) :: ilev
      integer, intent(in)    :: maxlev
      integer, intent(inout) :: iproc
      integer, intent(in)    :: nrproc
      integer, intent(in)    :: ncol, nrow
      real, intent(in) :: nodata
      integer, intent(in)    :: ic1,ic2,ir1,ir2
      real, dimension(ncol,nrow), intent(in) :: a 
      integer, dimension(nrproc,2), intent(inout) :: proc_icolmin
      integer, dimension(nrproc,2), intent(inout) :: proc_icolmax
      integer, dimension(nrproc,2), intent(inout) :: proc_irowmin
      integer, dimension(nrproc,2), intent(inout) :: proc_irowmax  
      integer, dimension(maxlev), intent(in) :: ncut   

c...     parameters
      integer, dimension(nrproc) :: cutidx

c...     local variables
      integer :: ic1b, ic2b, ir1b, ir2b, ncolb, nrowb, icut, 
     &           ic1c, ic2c, ir1c, ir2c, ncolc, nrowc, jlev     
      real, dimension(ncol) :: rowsum
      real, dimension(nrow) :: colsum 
c.......................................................................

      ilev = ilev + 1 
      if (ilev <= maxlev) then
         ! determine bounding box
         call pks7mpibbox(a,ncol,nrow,
     &                 ic1,ic2,ir1,ir2, 
     &                 ic1b,ic2b,ir1b,ir2b,ncolb,nrowb,nodata)
         if (nrowb.gt.ncolb) then ! horizontal cut(s)
            call pks7mpicolsum(a,ncol,nrow,
     &                      ic1b,ic2b,ir1b,ir2b,nrowb,colsum,nodata)
            call pks7mpisplit(colsum,nrowb,cutidx,ncut(ilev))
            ir1c = ir1b
            do icut = 1, ncut(ilev)
               ir2c = ir1b + cutidx(icut) - 1
               nrowc = ir2c - ir1c + 1
               jlev = ilev
               call pks7mpircb(a,nodata,ncol,nrow,
     &                      ic1b,ic2b,ir1c,ir2c,
     &                      jlev,maxlev,iproc,nrproc,
     &                      proc_icolmin,proc_icolmax, 
     &                      proc_irowmin,proc_irowmax,ncut)
               ir1c = ir2c + 1
            end do
         else ! vertical cut(s)
            call pks7mpirowsum(a,ncol,nrow,
     &                      ic1b,ic2b,ir1b,ir2b,ncolb,rowsum,nodata)
            call pks7mpisplit(rowsum, ncolb, cutidx, ncut(ilev))
            ic1c = ic1b
            do icut = 1, ncut(ilev)
               ic2c = ic1b + cutidx(icut) - 1
               ncolc = ic2c - ic1c + 1
               jlev = ilev
               call pks7mpircb(a,nodata,ncol,nrow,
     &                      ic1c,ic2c,ir1b,ir2b, 
     &                      jlev,maxlev,iproc,nrproc,
     &                      proc_icolmin,proc_icolmax, 
     &                      proc_irowmin,proc_irowmax,ncut)
               ic1c = ic2c + 1
            end do    
          end if
      else
         iproc = iproc + 1
         proc_icolmin(iproc,inovl) = ic1
         proc_icolmax(iproc,inovl) = ic2
         proc_irowmin(iproc,inovl) = ir1
         proc_irowmax(iproc,inovl) = ir2
      end if

      end subroutine

      subroutine pks7mpiprimefactors(num, factors, f)
      implicit none
      integer, intent(in) :: num
      integer,intent(out), dimension((num/2))::factors 
      integer, intent(inout) :: f
      integer :: i, n
      i = 2  
      f = 1
      n = num 
      do
        if (mod(n,i) == 0) then
            factors(f) = i
            f = f+1
            n = n/i
        else
            i = i+1
        end if
        if (n == 1) then       
            f = f-1
            exit
        end if
      end do
      end subroutine

      subroutine pks7mpibbox(a,ncol,nrow,ic1i,ic2i,ir1i,ir2i,
     &                    ic1b,ic2b,ir1b,ir2b,ncolo,nrowo,nodata)

      implicit none
     
      integer, intent(in)  :: ncol, nrow
      real, dimension(ncol,nrow), intent(in) :: a
      real, intent(in) :: nodata
      integer, intent(in)  :: ic1i, ic2i, ir1i, ir2i
      integer, intent(out) :: ic1b, ic2b, ir1b, ir2b
      integer, intent(out) :: ncolo, nrowo
       
      real, parameter :: tiny = 1.0e-20      
      integer :: irow, icol

      ic1b = ncol + 1
      ic2b = 0
      ir1b = nrow + 1
      ir2b = 0

      do irow = ir1i, ir2i
         do icol = ic1i, ic2i
            if (abs(a(icol,irow)-nodata).gt.tiny) then
               ic1b = min(ic1b,icol)  
               ic2b = max(ic2b,icol)  
               ir1b = min(ir1b,irow)  
               ir2b = max(ir2b,irow)  
            end if
         end do
      end do

      ncolo = ic2b-ic1b+1
      nrowo = ir2b-ir1b+1

      end subroutine

      subroutine pks7mpirowsum(a,ncol,nrow,ic1,ic2,ir1,ir2,
     &                         ncol2,rowsum,nodata)
      implicit none

      integer, intent(in) :: ncol, nrow, ic1, ic2, ir1, ir2, ncol2
      real, dimension(ncol,nrow) :: a
      real, dimension(ncol2) :: rowsum
      real, intent(in) :: nodata

      real, parameter :: tiny = 1.0e-20
      integer :: icol, irow
      real :: val

      rowsum = 0
      do irow = ir1, ir2
         do icol = ic1, ic2 
             val = a(icol,irow)
             if (abs(val-nodata).gt.tiny) 
     &          rowsum(icol-ic1+1) = rowsum(icol-ic1+1) + abs(val)
         end do
      end do

      end subroutine
 
      subroutine pks7mpicolsum(a,ncol,nrow, ic1, ic2, ir1, ir2,
     &                         nrow2,colsum,nodata)
      implicit none

      integer, intent(in) :: ncol, nrow, ic1, ic2, ir1, ir2, nrow2
      real, dimension(ncol,nrow) :: a
      real, dimension(nrow2) :: colsum
      real, intent(in) :: nodata

      real, parameter :: tiny = 1.0e-20
      integer :: icol, irow
      real :: val

      colsum = 0
      do irow = ir1, ir2
         do icol = ic1, ic2 
             val = a(icol,irow) 
             if (abs(val-nodata).gt.tiny)
     &          colsum(irow-ir1+1) = colsum(irow-ir1+1) + abs(val)
         end do
      end do  

      end subroutine

      subroutine pks7mpisplit(x, xn, y, yn)
      implicit none
    
      integer, intent(in) :: xn
      real, dimension(xn), intent(in) :: x
      integer, intent(in) :: yn
      integer, dimension(yn), intent(out) :: y 
     
      integer :: ix, iy
      real :: sum, tot 

      tot = 0
      do ix = 1, xn
         tot = tot + x(ix)   
      end do
      tot = tot/yn
  
      y = xn
      do iy = 1, yn-1
         sum = 0.
         do ix = 1, xn-1
            if ((sum + x(ix+1)) > tot*iy) then
               y(iy) = ix; exit       
            else
               sum = sum + x(ix)
            end if 
         end do
      end do
 
      end subroutine  

      !> MPP partitioning: load balance
      subroutine pks7mpipartuser( proc_icolmin, proc_icolmax, 
     &                         proc_irowmin, proc_irowmax )
      use pksmpi_mod, only: nrproc, gncol, gnrow,
     &                   nrproc_ncol, nrproc_nrow, 
     &                   nrproc_col, nrproc_row, inovl, myrank

      implicit none

c...     arguments
      integer, dimension(nrproc,2), intent(out) :: proc_icolmin
      integer, dimension(nrproc,2), intent(out) :: proc_icolmax
      integer, dimension(nrproc,2), intent(out) :: proc_irowmin
      integer, dimension(nrproc,2), intent(out) :: proc_irowmax     

c...     local variables
      integer, dimension(nrproc,2) :: proc_ncol   !< number of columns for each process
      integer, dimension(nrproc,2) :: proc_nrow   !< number of rows for each process
      integer :: i, j, iproc
c.......................................................................

      nrproc_col(nrproc_ncol+1) = gncol + 1
      nrproc_row(nrproc_nrow+1) = gnrow + 1
      
      do i = 1, nrproc_nrow
         do j = 1, nrproc_ncol
            iproc = j + (i-1)*nrproc_ncol
            proc_ncol(iproc,inovl) = nrproc_col(j+1)-
     &                           nrproc_col(j)
            proc_nrow(iproc,inovl) = nrproc_row(i+1)-
     &                           nrproc_row(i)
          
         end do
      end do
      
c fill partitions    
      if (myrank.eq.0) then
         write(*,*) '--->',nrproc_nrow, nrproc_ncol
         do iproc = 1, nrproc_ncol+1
            write(*,*) '==',nrproc_col(iproc)
         end do
         do iproc = 1, nrproc_nrow+1
            write(*,*) '==',nrproc_row(iproc)
         end do

         do iproc = 1, nrproc
            write(*,*) proc_ncol(iproc,inovl),proc_nrow(iproc,inovl)
         end do
      end if

      call pks7mpiminmax(nrproc_nrow, nrproc_ncol, 
     1   proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax,
     1   proc_ncol, proc_nrow )
      
      return
      end subroutine pks7mpipartuser

      !> MPP partitioning: user specifies partition
      subroutine pks7mpipartuser2( proc_icolmin, proc_icolmax, 
     &                          proc_irowmin, proc_irowmax )
      use pksmpi_mod, only: inovl, nrproc, partitionsminmax

      implicit none

c...     arguments
      integer, dimension(nrproc,2), intent(out) :: proc_icolmin
      integer, dimension(nrproc,2), intent(out) :: proc_icolmax
      integer, dimension(nrproc,2), intent(out) :: proc_irowmin
      integer, dimension(nrproc,2), intent(out) :: proc_irowmax     

c...     local variables
      integer :: iproc
c.......................................................................
       
      do iproc = 1, nrproc
         proc_icolmin(iproc,inovl) = partitionsminmax(iproc,1)
         proc_icolmax(iproc,inovl) = partitionsminmax(iproc,2)
         proc_irowmin(iproc,inovl) = partitionsminmax(iproc,3)
         proc_irowmax(iproc,inovl) = partitionsminmax(iproc,4)
      end do 
      
      return
      end subroutine pks7mpipartuser2

      subroutine pks7mpilxchini()
      
      use pksmpi_mod
      use pksmpiwrp_mod, only: sbufi, rbufi, sbufr, rbufr, sbufd, rbufd
      use pksmpiwrp_mod, only: lenbuf

      implicit none     

c functions
      logical :: pks7mpignodeinpart
      integer :: pks7mpig2lnode

c locals
      logical :: lin
      integer :: ic1, ic2, ir1, ir2, ilay, irow, icol, iovlap, i,
     1           xpproc, iproc, ixp, nmax, n, m, ma, ncolp, nrowp
      integer, dimension(:,:), allocatable :: wrk 
      integer, dimension(8) :: comm

c debug
      double precision :: mask
      real, dimension(:,:,:), allocatable :: tmp, tmp2 
      character(len=200) :: fname, fmt   
      integer :: jproc
      double precision, dimension(:,:,:), allocatable :: ddum

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

c determine number of exchange partners
      nrxp = 0
      allocate(xp(nrproc-1))
      do iproc = 1, nrproc
         if (topol(myproc,iproc).gt.0) then
            nrxp = nrxp + 1
            xp(nrxp)%xprnk = iproc - 1
         end if       
      end do 

      nmax = 2*gncol + 2*gnrow
      allocate(wrk(nmax,2)) 

c...      determine indices for send/receive
      do ixp = 1, nrxp
         xpproc = xp(ixp)%xprnk + 1 
         allocate( xp(ixp)%pckidx(novlapmax),
     &             xp(ixp)%upkidx(novlapmax) )

         do iovlap = 1, novlapmax

c--------receive cells
c           get cells in band
            call pks7mpigetoverlap(proc_icolmin(myproc,inovl), 
     &                          proc_icolmax(myproc,inovl),
     &                          proc_irowmin(myproc,inovl),
     &                          proc_irowmax(myproc,inovl),
     &                          stenimpsol, iovlap, wrk, nmax, n,
     &                          comm)

c           loop over cells
            m = 0
            do i = 1, n
                lin =  pks7mpignodeinpart(wrk(i,1), wrk(i,2), 
     &                          proc_icolmin(xpproc,inovl),
     &                          proc_icolmax(xpproc,inovl),
     &                          proc_irowmin(xpproc,inovl),
     &                          proc_irowmax(xpproc,inovl))
         
c               label        
                if (.not.lin) then
                   wrk(i,1) = -wrk(i,1)
                else
                   m = m  + 1
                end if
            end do

c           allocate
            if (m.eq.0) then
               write(*,*) 'WARNING: no communication',
     &            myrank,'<--',xp(ixp)%xprnk
            end if 
            ma = m*gnlay
            if (ma.gt.0) allocate( xp(ixp)%upkidx(iovlap)%idx(ma) )
            xp(ixp)%upkidx(iovlap)%nidx = ma

            m = 0 
            do ilay = 1, gnlay
               do i = 1, n
                  if (wrk(i,1).gt.0) then
                     m = m + 1
                     if (m.gt.ma) then
                        write(*,*) 'Error: memory corrupted!'
                     end if
                     xp(ixp)%upkidx(iovlap)%idx(m) = 
     1                  pks7mpig2lnode(wrk(i,1),wrk(i,2),ilay) 
                  end if
                end do
            end do             

c--------send cells
c           get cells in band
            call pks7mpigetoverlap(proc_icolmin(xpproc,inovl), 
     &                          proc_icolmax(xpproc,inovl),
     &                          proc_irowmin(xpproc,inovl),
     &                          proc_irowmax(xpproc,inovl),
     &                          stenimpsol, iovlap, wrk, nmax, n, 
     &                          comm)
c           loop over cells
            m = 0
            do i = 1, n
                lin =  pks7mpignodeinpart(wrk(i,1), wrk(i,2), 
     &                          proc_icolmin(myproc,inovl),
     &                          proc_icolmax(myproc,inovl),
     &                          proc_irowmin(myproc,inovl),
     &                          proc_irowmax(myproc,inovl))
         
c               label        
                if (.not.lin) then
                   wrk(i,1) = -wrk(i,1)
                else
                   m = m  + 1
                end if
            end do

c           allocate
            if (m.eq.0) then
               write(*,*) 'WARNING: no communication',
     &            myrank,'-->',xp(ixp)%xprnk
            end if 
            ma = m*gnlay
            if (ma.gt.0) allocate( xp(ixp)%pckidx(iovlap)%idx(ma) )
            xp(ixp)%pckidx(iovlap)%nidx = ma 
c            write(*,*) 'myrank: setting pck nidx',
c     &         xp(ixp)%pckidx(iovlap)%nidx

            m = 0 
            do ilay = 1, gnlay
               do i = 1, n
                  if (wrk(i,1).gt.0) then
                     m = m + 1
                     if (m.gt.ma) then
                        write(*,*) 'Error: memory corrupted!'
                     end if
                     xp(ixp)%pckidx(iovlap)%idx(m) = 
     1                  pks7mpig2lnode(wrk(i,1),wrk(i,2),ilay) 
                  end if
                end do
            end do
         end do
      end do

c... check for local indices
      iovlap = 1
      do ixp = 1, nrxp            

         do i = 1, xp(ixp)%upkidx(iovlap)%nidx 
            n = xp(ixp)%upkidx(iovlap)%idx(i)
            if (n.le.0) then
               write(*,*) 'Error, n < 0 for myank',myrank
             end if
         end do
         do i = 1, xp(ixp)%pckidx(iovlap)%nidx 
             n = xp(ixp)%pckidx(iovlap)%idx(i)
             if (n.le.0) then
                write(*,*) 'Error, n < 0 for myank',myrank
             end if
         end do
      end do

c...     allocate send and receive buffers
      ncolp = proc_ncol(myrank+1,inovl)
      nrowp = proc_nrow(myrank+1,inovl)
      lenbuf = gnlay*(2*nrowp*novlapmax +
     &         2*ncolp*novlapmax + 4*novlapmax*novlapmax)
      lenbuf = lenbuf*(maxlcx+maxlcix)
      lenbuf = max(lenbuf,6*nrproc) ! pks7mpigxchcnvg
      !n = 10*n      

      !write(*,*) 'Allocating sbuf and rbuf with length',n
      allocate( sbufi(lenbuf), rbufi(lenbuf),
     &          sbufr(lenbuf), rbufr(lenbuf),
     &          sbufd(lenbuf), rbufd(lenbuf) )

c deallocate
      deallocate(wrk)

      if (.false.) then
         if (myrank.eq.0) then
            write(*,*) 'topol:'
            write(fmt,'(a,i1,a)') '(',nrproc,'i2)'  
            do iproc = 1, nrproc 
               write(*,fmt)(topol(iproc,jproc),jproc=1,nrproc)
            end do
         end if

         ncolp = proc_ncol(myrank+1,iovl)
         nrowp = proc_nrow(myrank+1,iovl)  
c         write(*,*) 'myrank',myrank,'ncolp',ncolp,'nrowp',nrowp

         allocate(tmp(ncolp,nrowp,gnlay))
         allocate(tmp2(ncolp,nrowp,gnlay))
         
        
         do iovlap = 1, novlapmax
            tmp = -9999.
            tmp2 = 0.
            do ilay = 1, gnlay
               do irow = 1, nrowp
                  do icol = 1, ncolp
                     call pks7mpimask( mask, icol, irow, ilay )
                     if (mask.gt.0D0) 
     1                  tmp(icol,irow,ilay) = real(myrank+1)
                  end do
               end do
            end do

            do ixp = 1, nrxp            
               xpproc = xp(ixp)%xprnk + 1 
               write(*,*) myrank,'<---',xp(ixp)%xprnk,': ',
     &            xp(ixp)%upkidx(iovlap)%nidx 
               do i = 1, xp(ixp)%upkidx(iovlap)%nidx 
                  n = xp(ixp)%upkidx(iovlap)%idx(i)
                  call getircl( n,icol,irow,ilay,ncolp,nrowp,gnlay )
                  tmp(icol,irow,ilay) = real(xpproc)
                  tmp2(icol,irow,ilay) = real(i)
               end do
               write(*,*) myrank,'--->',xp(ixp)%xprnk,': ',
     &            xp(ixp)%pckidx(iovlap)%nidx 
               do i = 1, xp(ixp)%pckidx(iovlap)%nidx 
                  n = xp(ixp)%pckidx(iovlap)%idx(i)
                  call getircl( n,icol,irow,ilay,ncolp,nrowp,gnlay )
                  tmp(icol,irow,ilay) = -real(xpproc)
                  tmp2(icol,irow,ilay) = -real(i)
               end do
            end do

            do ilay = 1, gnlay
               write(fname,'(a,i2.2)') 'lxp_b',iovlap
c               call rwriteasc(tmp,ilay,ncolp,nrowp,gnlay,
c     1            fname,-9999.)
               write(fname,'(a,i2.2)') 'lxi_b',iovlap
c               call rwriteasc(tmp2,ilay,ncolp,nrowp,gnlay,
c     1            fname,-9999.)
            end do

            deallocate(tmp,tmp2)

         end do
      end if

      end subroutine    

      !> Set local nrow, ncol, nlay and clipping parameters.
      subroutine pks7mpisetrcl( nlay, nrow, ncol )

      use pksmpi_mod
#ifdef CLIP      
      use clipmodule
#endif
      implicit none
c
c...     Arguments
      integer, intent(out) :: nlay, !< number of local layers
     &                        ncol, !< number of local columns
     &                        nrow  !< number of local rows
c
c        Local variables
      integer :: i
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return
     
c...     set geometry
      nlay = gnlay
      nrow = proc_nrow(myproc,iovl)
      ncol = proc_ncol(myproc,iovl)
            
#ifdef CLIP
c...     switch on clipping read
      useclip = .true.

c...    clipping parameters columns
      srscol(1) = proc_icolmin(myproc,iovl) - 1
      srscol(2) = proc_icolmax(myproc,iovl) -
     &   proc_icolmin(myproc,iovl) + 1
      srscol(3) = gncol - proc_icolmax(myproc,iovl)

c...    clipping parameters rows
      srsrow(1) = proc_irowmin(myproc,iovl) - 1
      srsrow(2) = proc_irowmax(myproc,iovl) -
     &   proc_irowmin(myproc,iovl) + 1
      srsrow(3) = gnrow - proc_irowmax(myproc,iovl)

c...    clipping parameters layers
      srslay(1) = 0
      srslay(2) = nlay
      srslay(3) = 0
#endif
      return
      end subroutine pks7mpisetrcl

      !> Append output file name with rank ID for MPP.
      subroutine pks7mpifname( fname, len)

      use pksmpi_mod

      implicit none
c
c...     Arguments
      character(len=*), intent(inout) :: fname !< (I) file name
      integer, intent(inout) :: len            !< (I/O) length of file name
c
c...     Local variables
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

c...     add rank ID to file name
      write(fname,'(2a,i3.3)') fname(1:len), '.p', myrank
      len = len + 5
c
c...     normal return
      return
      end subroutine pks7mpifname
      
      !> Create partition string
      subroutine pks7mpipartstr( s )

      use pksmpi_mod

      implicit none
c
c...     Arguments
      character(len=*), intent(inout) :: s !< (I) file name
c
c...     Local variables
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) then
         s = ''    
         return
      end if   

c...     add rank ID to file name
      write(s,'(a,i3.3)') '_p', myrank
c
c...     normal return
      return
      end subroutine pks7mpipartstr      
      
      !> Read input for MPP package.
      subroutine pks7mpiar( iout, icbund, nodes )

      use pksmpi_mod

      implicit none
c
c...     Arguments
      integer, intent(in)  :: iout,                    !< lun list file
     &                        nodes                    !< number of nodes
      integer, dimension(nodes), intent(in) :: icbund  !< boundary array

c...     Local variables
      character(len=1) :: ch
      integer :: iproc
      real :: loadopt, loadinbal
c.......................................................................
c
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

c...     print data values just read
      write(iout,100)
  100 format(///47x,'solution by massive parallel processing',
     & ' method'/47x,53('-'))
c
c...     MPP load estimation
      call pks7mpiloadest(icbund, nodes)

c...     master process write partition information to out file
      if (myrank.eq.0) then
c         write(*,10)    nrproc
         write(iout,10) nrproc
   10    format(1x,'Number of MPI processes:',1x,i3)

c         if (stenadv.eq.1) then
c            write(ch,'(a)') '5'
c         else
c            write(ch,'(a)') '9'
c         end if
c         write(iout,15) 'advection',novlapadv,ch
         if (stenimpsol.eq.1) then
            write(ch,'(a)') '5'
         else
            write(ch,'(a)') '9'
         end if
         write(iout,15) 'implicit solver',novlapimpsol, ch

   15    format(1x,a,':',1x,'overlap of',1x,i2,1x,
     &          'cells using a ',1x,a,'-point communication stencil')

         write(iout,20)
   20    format(83x,'load',2x,'load opt')

         loadopt = 100./nrproc
         loadinbal = 0.

         do iproc = 1, nrproc
            write(iout,30) iproc-1,
     &               proc_ncol(iproc,inovl),
     &               proc_nrow(iproc,inovl),
     &               proc_nodes(iproc,inovl),
     &               proc_icolmin(iproc,inovl),
     &               proc_icolmax(iproc,inovl),
     &               proc_irowmin(iproc,inovl),
     &               proc_irowmax(iproc,inovl),
     &               proc_ncol(iproc,iovl),
     &               proc_nrow(iproc,iovl),
     &               proc_nodes(iproc,iovl),
     &               proc_icolmin(iproc,iovl),
     &               proc_icolmax(iproc,iovl),
     &               proc_irowmin(iproc,iovl),
     &               proc_irowmax(iproc,iovl),
     &               proc_load(iproc), loadopt

            loadinbal = loadinbal + abs(proc_load(iproc)-loadopt)

   30       format(1x,'p',i3.3,1x,':',
     &             1x,2(i3,1x),i7,4(1x,i4),1x,'|',
     &             1x,2(i3,1x),i7,4(1x,i4),1x,
     &             2f7.2)
         end do
      end if

      if (myrank.eq.0) then
      write(iout,'(1x,a,1x,f7.2,1x,a)') 'Load inbalance:',loadinbal,'%'
      end if

      if (partopt.lt.0) then
         if (myrank.eq.0) then
            write(*,40)
            write(iout,40)
         end if
   40    format(1x,'STOP. partopt < 0, running in load-balancing mode')
         call pks7mpiwrpfinalize()
         call pksstop('')
      end if
      
      if (verbose.eq.1) then
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if      
      
      return
      end subroutine pks7mpiar

      !> Get process rank ID for global (MT3DMS) node number.
      integer function pks7mpignode2rank( n )

      use pksmpi_mod

      implicit none
c
c...     Arguments
      integer, intent(in) :: n !< (I) global (MT3DMS) node number
c
c...     Local variables
      integer :: icol, irow, ilay, rank, iproc
c.......................................................................
c
c...     get column, row, lay
      call getircl( n, icol, irow, ilay, gncol, gnrow, gnlay )

      rank = -1
      do iproc = 1, nrproc
         if ( icol.ge.proc_icolmin(iproc,inovl) .and.
     &        icol.le.proc_icolmax(iproc,inovl) .and.
     &        irow.ge.proc_irowmin(iproc,inovl) .and.
     &        irow.le.proc_irowmax(iproc,inovl) ) rank = iproc - 1
      end do

      if (rank.eq.-1) then
         write(*,*) 'Error pks7mpignode2rank: rank not found!'
         write(*,*) 'n icol irow ilay gncol, gnrow, gnlay ',
     &    n,icol,irow,ilay,gncol, gnrow, gnlay
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if

      pks7mpignode2rank = rank

      return
      end function pks7mpignode2rank

      !> Local communication using non-blocking send and non-blocking receive (reals).
      subroutine pks7mpilxchi( x, lcx, nrlcx, commopt )

      use pksmpi_mod
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      integer, dimension(*), intent(inout) :: x       !< (I/O) array
      integer, dimension(*), intent(in)    :: lcx     !< (I) array with pointers (indices) to array
      integer              , intent(in)    :: nrlcx   !< (I) number of integer arrays to communicate
      integer              , intent(in)    :: commopt !< (I) option for overlap and stencil (1: advection; 2: implicit solver)
c
c...     Local variables
      logical :: commall
      integer :: ixp, iovlap, ilc, bufptr, buflen,
     &           nrlc, i, j, icol, irow, ilay, novlap, nrxp2,
     &           iovlapmin
c.......................................................................

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call timing_tic('MPI','LOCAL-pks7mpilxchi')                       !JV
      
c...     set overlap and stencil
      if (commopt.eq.1) then
         novlap  = novlapadv
         commall = commallovladv
      else
         novlap  = novlapimpsol
         commall = commallovlimp
      end if

      if (commall) then
         iovlapmin = 1
      else
         iovlapmin = novlap
      end if

      bufptr = 1
      nrlc = nrlcx

      nrxp2 = 0
      do ixp = 1, nrxp ! loop over exchange partners

         nrxp2 = nrxp2 + 1

         xp(ixp)%xpbuf = bufptr

         buflen = 0
         do iovlap = iovlapmin, novlap ! loop over overlap
            do ilc = 1, nrlc
               buflen = buflen + xp(ixp)%upkidx(iovlap)%nidx
            end do
         end do ! iovlap

         call pks7mpiwrpirecvi( rbufi(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      rreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp

      bufptr = 1

c...      pack
      do ixp = 1, nrxp

         do iovlap = iovlapmin, novlap ! loop over overlap
            do ilc = 1, nrlcx
               do i = 1, xp(ixp)%pckidx(iovlap)%nidx
                  j = xp(ixp)%pckidx(iovlap)%idx(i)
                  if (j.le.0) then
                     write(*,*) 'Program error pks7mpilxchr: pckidx!'
                     write(*,*) 'index ',j
                     call pksstop(' ')
                  end if
                  sbufi(bufptr) = x( lcx(ilc)+j-1 )
                  bufptr = bufptr + 1
               end do
            end do
        end do ! iovlap
      end do ! ixp

      bufptr = 1

      nrxp2 = 0
      do ixp = 1, nrxp

         nrxp2 = nrxp2 + 1

         buflen = 0
         do iovlap = iovlapmin, novlap
            do ilc = 1, nrlc
               buflen = buflen + xp(ixp)%pckidx(iovlap)%nidx
            end do
         end do ! iovlap

         call pks7mpiwrpisendi( sbufi(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      sreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp
   
      call pks7mpiwrpwaitall( nrxp2, rreq )

c...      unpacking
      do ixp = 1, nrxp

         bufptr = xp(ixp)%xpbuf

         do iovlap = iovlapmin, novlap
            do ilc = 1, nrlcx
               do i = 1, xp(ixp)%upkidx(iovlap)%nidx
                  j = xp(ixp)%upkidx(iovlap)%idx(i)
                  if (j.le.0) then
                     write(*,*) 'Program error pks7mpilxchr: upkidx!'
                     call pksstop(' ')
                  end if
                  x( lcx(ilc)+j-1 ) = rbufi(bufptr)
                  bufptr = bufptr + 1
               end do
            end do
         end do ! iovlap
      end do ! ixp
      
      call pks7mpiwrpwaitall( nrxp2, sreq )

      call timing_toc('MPI','LOCAL-pks7mpilxchi')                       !JV
      
      return
      end subroutine pks7mpilxchi

      !> Local communication using non-blocking send and non-blocking receive (reals).
      subroutine pks7mpilxchr( x, lcx, nrlcx, commopt )

      use pksmpi_mod
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      real   , dimension(*), intent(inout) :: x       !< (I/O) array
      integer, dimension(*), intent(in)    :: lcx     !< (I) array with pointers (indices) to array
      integer              , intent(in)    :: nrlcx   !< (I) number of integer arrays to communicate
      integer              , intent(in)    :: commopt !< (I) option for overlap and stencil (1: advection; 2: implicit solver)
c
c...     Local variables
      logical :: commall
      integer :: ixp, iovlap, ilc, bufptr, buflen,
     &           nrlc, i, j, icol, irow, ilay, novlap, nrxp2,
     &           iovlapmin
c.......................................................................

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      nlxchloosely = nlxchloosely + 1
      if (nlxchloosely.gt.1.and.mpiloosely.eq.1) return
      
      call timing_tic('MPI','LOCAL-pks7mpilxchr')                       !JV
      
c...     set overlap and stencil
      if (commopt.eq.1) then
         novlap  = novlapadv
         commall = commallovladv
      else
         novlap  = novlapimpsol
         commall = commallovlimp
      end if

      if (commall) then
         iovlapmin = 1
      else
         iovlapmin = novlap
      end if

      bufptr = 1
      nrlc = nrlcx

      nrxp2 = 0
      do ixp = 1, nrxp ! loop over exchange partners

         nrxp2 = nrxp2 + 1

         xp(ixp)%xpbuf = bufptr

         buflen = 0
         do iovlap = iovlapmin, novlap ! loop over overlap
            do ilc = 1, nrlc
               buflen = buflen + xp(ixp)%upkidx(iovlap)%nidx
            end do
         end do ! iovlap

         call pks7mpiwrpirecvr( rbufr(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      rreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp

      bufptr = 1

c...      pack
      do ixp = 1, nrxp

         do iovlap = iovlapmin, novlap ! loop over overlap
            do ilc = 1, nrlcx
               do i = 1, xp(ixp)%pckidx(iovlap)%nidx
                  j = xp(ixp)%pckidx(iovlap)%idx(i)
                  if (j.le.0) then
                     write(*,*) 'Program error pks7mpilxchr: pckidx!'
                     write(*,*) 'index ',j
                     call pksstop(' ')
                  end if
                  sbufr(bufptr) = x( lcx(ilc)+j-1 )
                  bufptr = bufptr + 1
               end do
            end do
        end do ! iovlap
      end do ! ixp

      bufptr = 1

      nrxp2 = 0
      do ixp = 1, nrxp

         nrxp2 = nrxp2 + 1

         buflen = 0
         do iovlap = iovlapmin, novlap
            do ilc = 1, nrlc
               buflen = buflen + xp(ixp)%pckidx(iovlap)%nidx
            end do
         end do ! iovlap

         call pks7mpiwrpisendr( sbufr(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      sreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp
   
      call pks7mpiwrpwaitall( nrxp2, rreq )

c...      unpacking
      do ixp = 1, nrxp

         bufptr = xp(ixp)%xpbuf

         do iovlap = iovlapmin, novlap
            do ilc = 1, nrlcx
               do i = 1, xp(ixp)%upkidx(iovlap)%nidx
                  j = xp(ixp)%upkidx(iovlap)%idx(i)
                  if (j.le.0) then
                     write(*,*) 'Program error pks7mpilxchr: upkidx!'
                     call pksstop(' ')
                  end if
                  x( lcx(ilc)+j-1 ) = rbufr(bufptr)
                  bufptr = bufptr + 1
               end do
            end do
         end do ! iovlap
      end do ! ixp
      
      call pks7mpiwrpwaitall( nrxp2, sreq )

      call timing_toc('MPI','LOCAL-pks7mpilxchr')                       ! PKS
 
      return
      end subroutine pks7mpilxchr

      !> Local communication using non-blocking send and non-blocking receive (doubles).
      subroutine pks7mpilxchd( x, lcx, nrlcx, commopt )

      use pksmpi_mod
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      double precision, dimension(*), intent(inout) :: x       !< (I/O) array
      integer, dimension(*), intent(in)    :: lcx     !< (I) array with pointers (indices) to array
      integer              , intent(in)    :: nrlcx   !< (I) number of integer arrays to communicate
      integer              , intent(in)    :: commopt !< (I) option for overlap and stencil (1: advection; 2: implicit solver)
c
c...     Local variables
      logical :: commall
      integer :: ixp, iovlap, ilc, bufptr, buflen,
     &           nrlc, i, j, icol, irow, ilay, novlap, nrxp2, 
     &           iovlapmin
c.......................................................................

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      nlxchloosely = nlxchloosely + 1
      if (nlxchloosely.gt.1.and.mpiloosely.eq.1) return
       
      call timing_tic('MPI','LOCAL-pks7mpilxchd')                       ! PKS

c...     set overlap and stencil
      if (commopt.eq.1) then
         !novlap  = novlapadv
         !commall = commallovladv
         novlap  = novlapimpsol
         commall = .false.         
      else
         novlap  = novlapimpsol
         commall = commallovlimp
      end if

      if (commall) then
         iovlapmin = 1
      else
         iovlapmin = novlap
      end if
      
      bufptr = 1
      nrlc = nrlcx

      nrxp2 = 0
      do ixp = 1, nrxp ! loop over exchange partners

         nrxp2 = nrxp2 + 1 
          
         xp(ixp)%xpbuf = bufptr

         buflen = 0
         do iovlap = iovlapmin, novlap ! loop over overlap
            do ilc = 1, nrlc
               buflen = buflen + xp(ixp)%upkidx(iovlap)%nidx
            end do
         end do ! iovlap
 
         call pks7mpiwrpirecvd( rbufd(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      rreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp

c...     check
      bufptr = bufptr - 1
      if (bufptr.gt.lenbuf) then
         write(*,*) 'Program error pks7mpilxchd: lenbuf!'
         call pksstop(' ')
      end if

      bufptr = 1
      
c...      pack
      do ixp = 1, nrxp

         do iovlap = iovlapmin, novlap ! loop over overlap
            do ilc = 1, nrlc
               do i = 1, xp(ixp)%pckidx(iovlap)%nidx
                  j = xp(ixp)%pckidx(iovlap)%idx(i)                  
                  if (j.le.0) then
                     write(*,*) 'Program error pks7mpilxchd: pckidx!',
     &                           myrank, ixp, nrxp, j
                     write(*,*) 'index ',j
                     call pksstop(' ')
                  end if
                  sbufd(bufptr) = x( lcx(ilc)+j-1 )
                  bufptr = bufptr + 1
               end do
            end do
        end do ! iovlap
      end do ! ixp

c...     check
      bufptr = bufptr - 1
      if (bufptr.gt.lenbuf) then
         write(*,*) 'Program error pks7mpilxchd: lenbuf!'
         call pksstop(' ')
      end if
      
      bufptr = 1

      nrxp2 = 0
      do ixp = 1, nrxp

         nrxp2 = nrxp2 + 1

         buflen = 0
         do iovlap = iovlapmin, novlap
            do ilc = 1, nrlc
               buflen = buflen + xp(ixp)%pckidx(iovlap)%nidx
            end do
         end do ! iovlap

         call pks7mpiwrpisendd( sbufd(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      sreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp

c...     check
      bufptr = bufptr - 1
      if (bufptr.gt.lenbuf) then
         write(*,*) 'Program error pks7mpilxchd: lenbuf!'
         call pksstop(' ')
      end if

      call pks7mpiwrpwaitall( nrxp2, rreq )

c...      unpacking
      do ixp = 1, nrxp

         bufptr = xp(ixp)%xpbuf

         do iovlap = iovlapmin, novlap
            do ilc = 1, nrlc
               do i = 1, xp(ixp)%upkidx(iovlap)%nidx
                  j = xp(ixp)%upkidx(iovlap)%idx(i)
                  if (j.le.0) then
                     write(*,*) 'Program error pks7mpilxchd: upkidx!',
     &                           myrank, ixp, nrxp, j
                     call pksstop(' ')
                  end if
                  x( lcx(ilc)+j-1 ) = rbufd(bufptr)
                  bufptr = bufptr + 1
               end do
            end do
         end do ! iovlap
      end do ! ixp
      
c...     check
      bufptr = bufptr - 1
      if (bufptr.gt.lenbuf) then
         write(*,*) 'Program error pks7mpilxchd: lenbuf!'
         call pksstop(' ')
      end if

      call pks7mpiwrpwaitall( nrxp2, sreq )

      call timing_toc('MPI','LOCAL-pks7mpilxchd')                       ! PKS
      
      return
      end subroutine pks7mpilxchd
 
      !> Local communication using non-blocking send and non-blocking receive (doubles).
      subroutine pks7mpilxchduns( x, nodec )

      use pksmpi_mod
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      double precision, dimension(*), intent(inout) :: x       !< (I/O) array
      integer, dimension(*), intent(in)             :: nodec
c
c...     Local variables
      logical :: commall
      integer :: ixp, iovlap, bufptr, buflen,
     &           i, j, icol, irow, ilay, novlap, nrxp2, 
     &           iovlapmin
      integer :: ieq
c.......................................................................

c...     return in the serial case or for MPP initialisation phase 1
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

#ifdef PKSUNS      
      call pks7mpilxchd(x, (/1/), 1, 2)
      return
#endif   
      
      nlxchloosely = nlxchloosely + 1
      if (nlxchloosely.gt.1 .and. mpiloosely.eq.1) return

      call timing_tic('MPI','LOCAL-pks7mpilxchduns')                    ! PKS
      
      novlap  = novlapimpsol
      commall = commallovlimp

      if (commall) then
         iovlapmin = 1
      else
         iovlapmin = novlap
      end if

      bufptr = 1
 
      nrxp2 = 0
      do ixp = 1, nrxp ! loop over exchange partners

         nrxp2 = nrxp2 + 1 
          
         xp(ixp)%xpbuf = bufptr

         buflen = 0
         do iovlap = iovlapmin, novlap ! loop over overlap
            buflen = buflen + xp(ixp)%upkidx(iovlap)%nidx
         end do ! iovlap

         call pks7mpiwrpirecvd( rbufd(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      rreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp

      bufptr = 1

c...      pack
      do ixp = 1, nrxp

         do iovlap = iovlapmin, novlap ! loop over overlap
            do i = 1, xp(ixp)%pckidx(iovlap)%nidx
               j = xp(ixp)%pckidx(iovlap)%idx(i)
               if (j.le.0) then
                  write(*,*) 'Program error pks7mpilxchd: pckidx!',
     &                        myrank, ixp, nrxp, j
                  write(*,*) 'index ',j
                  call pksstop(' ')
               end if
               ieq = nodec(j)
               !write(*,*) 'ieq=',ieq
               if (ieq.gt.0) then
                  sbufd(bufptr) = x( ieq )
                  !write(*,*) 'before',x(ieq)
               else
                  sbufd(bufptr) = -12345D0
               end if
               bufptr = bufptr + 1
            end do
        end do ! iovlap
      end do ! ixp

      bufptr = 1

      nrxp2 = 0
      do ixp = 1, nrxp

         nrxp2 = nrxp2 + 1

         buflen = 0
         do iovlap = iovlapmin, novlap
            buflen = buflen + xp(ixp)%pckidx(iovlap)%nidx
         end do ! iovlap

         call pks7mpiwrpisendd( sbufd(bufptr),
     &                      buflen,
     &                      xp(ixp)%xprnk,
     &                      0,
     &                      pks7mpiwrpcomm_world,
     &                      sreq(nrxp2) )

         bufptr = bufptr + buflen

      end do ! ixp

      call pks7mpiwrpwaitall( nrxp2, rreq )

c...      unpacking
      do ixp = 1, nrxp

         bufptr = xp(ixp)%xpbuf

         do iovlap = iovlapmin, novlap
            do i = 1, xp(ixp)%upkidx(iovlap)%nidx
               j = xp(ixp)%upkidx(iovlap)%idx(i)
               if (j.le.0) then
                  write(*,*) 'Program error pks7mpilxchd: upkidx!',
     &                        myrank, ixp, nrxp, j
                  call pksstop(' ')
               end if
               ieq = nodec(j)
               if (ieq.gt.0) then
                  !write(*,*) 'after', rbufd(bufptr)
                  x( ieq ) = rbufd(bufptr)
               end if
               bufptr = bufptr + 1
            end do
         end do ! iovlap
      end do ! ixp
      
      call pks7mpiwrpwaitall( nrxp2, sreq )

      call timing_toc('MPI','LOCAL-pks7mpilxchduns')                    ! PKS
      
      return
      end subroutine pks7mpilxchduns

      !> Global exchange of mass part 1.
      subroutine pks7mpilgxchmass1( temp )

      use pksmpi_mod
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      real, intent(inout), dimension(4) :: temp !< mass to add
c
c...     Local variables
      integer :: i
      real, dimension(4) :: grbuf
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call timing_tic('MPI','GLOBAL-pks7mpilgxchmass1')                 ! PKS
     
      call pks7mpiwrpallsumr( temp, grbuf, 4 )
      do i = 1, 4
         temp(i) = grbuf(i)
      end do

      call timing_toc('MPI','GLOBAL-pks7mpilgxchmass1')                 ! PKS
      
      return
      end subroutine pks7mpilgxchmass1

      !> Global exchange of mass part 2.
      subroutine pks7mpilgxchmass2( tmass, rmasio, ncomp )

      use pksmpi_mod
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      integer, intent(in) :: ncomp                          !< (I) number of components
      real, dimension(4,3,ncomp), intent(inout) :: tmass    !< (I) mass to add
      real, dimension(122,2,ncomp), intent(inout) :: rmasio !< (I) mass to add
c
c...     Local variables
      integer :: iq, icomp, i, bufptr, buflen
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call timing_tic('MPI','GLOBAL-pks7mpilgxchmass2')                 ! PKS
      
c...     Pack
      bufptr = 1

c...     tmass
      do iq = 1, 4
         i = 2
         do icomp = 1, ncomp
            bufptr = bufptr + 1
            sbufr(bufptr) = tmass(iq,i,icomp)
         end do
      end do
c...     rmasio
      do iq = 1, 122
         do i = 1, 2
            do icomp = 1, ncomp
               bufptr = bufptr + 1
               sbufr(bufptr) = rmasio(iq,i,icomp)
            end do
         end do
      end do

      buflen = bufptr

c...     all sum global reduce
      call pks7mpiwrpallsumr( sbufr, rbufr, buflen )

c...     Unpack
      bufptr = 1

c...     tmass
      do iq = 1, 4
         i = 2
         do icomp = 1, ncomp
            bufptr = bufptr + 1
            tmass(iq,i,icomp) = rbufr(bufptr)
         end do
      end do
c...     rmasio
      do iq = 1, 122
         do i = 1, 2
            do icomp = 1, ncomp
               bufptr = bufptr + 1
               rmasio(iq,i,icomp) = rbufr(bufptr)
            end do
         end do
      end do

      call timing_toc('MPI','GLOBAL-pks7mpilgxchmass2')                 ! PKS
      
      return
      end subroutine pks7mpilgxchmass2

      !> Return mask (real) in case this process is responsible for this
      !! node.
      subroutine pks7mpimask( maskval, icol, irow, ilay )

      use pksmpi_mod

      implicit none

c...     Arguments
      integer, intent(in) :: icol, !< (I) column index
     &                       irow, !< (I) row index
     &                       ilay  !< (I) layer index
      double precision, intent(inout) :: maskval  !< (O) mask (1 or 0)

c...     Local variables
      integer :: gicol, girow
c.......................................................................

      maskval = 1.

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      gicol = proc_icolmin(myrank+1,iovl)+icol-1
      girow = proc_irowmin(myrank+1,iovl)+irow-1

      if (gicol.lt.proc_icolmin(myrank+1,inovl) .or.
     &    gicol.gt.proc_icolmax(myrank+1,inovl) .or.
     &    girow.lt.proc_irowmin(myrank+1,inovl) .or.
     &    girow.gt.proc_irowmax(myrank+1,inovl) ) maskval = 0.

      return
      end subroutine pks7mpimask
      
      !> Return mask (real) in case this process is responsible for this
      !! node.
      subroutine pks7mpimaskbound( maskval, icol, irow, ilay,
     &                          ncol, nrow, nlay )

      use pksmpi_mod

      implicit none

c...     Arguments
      integer, intent(in) :: icol, !< (I) column index
     &                       irow, !< (I) row index
     &                       ilay, !< (I) layer index
     &                       ncol,
     &                       nrow,
     &                       nlay
      
      double precision, intent(inout) :: maskval  !< (O) mask (1 or 0)

c...     Local variables
      integer :: gicol, girow, n, i
c.......................................................................

      maskval = 1.

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      gicol = proc_icolmin(myrank+1,iovl)+icol-1
      girow = proc_irowmin(myrank+1,iovl)+irow-1

      if (gicol.lt.proc_icolmin(myrank+1,inovl) .or.
     &    gicol.gt.proc_icolmax(myrank+1,inovl) .or.
     &    girow.lt.proc_irowmin(myrank+1,inovl) .or.
     &    girow.gt.proc_irowmax(myrank+1,inovl) ) maskval = 0.
      
      n = icol + (irow-1)*ncol + (ilay-1)*nrow*ncol
      do i = 1, nbandnodes
          if (abs(bandnodes(i)).eq.n) maskval = -1.
      end do
 
      return
      end subroutine pks7mpimaskbound      

      !> Return mask (real) in case this process is responsible for this
      !! node. Global node number version.
      subroutine pks7mpimaskn( maskval, n, ncol, nrow, nlay )

      use pksmpi_mod
c
      implicit none
c
c...     Arguments
      integer, intent(in) :: n,    !< (I) node number
     &                       ncol, !< (I) number of columns
     &                       nrow, !< (I) number of rows
     &                       nlay  !< (I) number of layers
      double precision, intent(inout) :: maskval  !< (O) mask (1 or 0)

c...     Local variables
      integer :: gicol, girow, icol, irow, ilay
c.......................................................................

      maskval = 1.

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call getircl( n, icol, irow, ilay,
     &                 ncol, nrow, nlay )

      gicol = proc_icolmin(myproc,iovl)+icol-1
      girow = proc_irowmin(myproc,iovl)+irow-1

      if (gicol.lt.proc_icolmin(myproc,inovl) .or.
     &    gicol.gt.proc_icolmax(myproc,inovl) .or.
     &    girow.lt.proc_irowmin(myproc,inovl) .or.
     &    girow.gt.proc_irowmax(myproc,inovl) ) maskval = 0.

      return
      end subroutine pks7mpimaskn

      !> Restriction of the residual, required for Ristricted Additive Schwarz preconditioning.
      subroutine pks7mpisetresidual( res, ixmap, niac )

      use pksmpi_mod
c
      implicit none
c
c...     Arguments
      double precision, dimension(*), intent(inout) :: res
      integer, dimension(*), intent(in)             :: ixmap
      integer, intent(in)                           :: niac

c...     Local variables
      doubleprecision, parameter :: dzero = 0.0d0
      integer :: i
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      do i = 1, niac
         if (ixmap(i).lt.0) res(i) = dzero
      end do
 
      return
      end subroutine pks7mpisetresidual

      !> Check supported MPP feature.
      subroutine pks7mpicheck( name, optflag )
c
      use pksmpi_mod
c
      implicit none
c
c...     Arguments
      character(len=3), intent(in)           :: name    !< Package name
      integer         , intent(in), optional :: optflag !< Optional flag

c...    Locals
      logical :: supported
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      supported = .false.
      call cfn_s_upcase(name)

      select case (name)
         case ('ADV')
            if (optflag.ge.1) then ! mixelm
               supported = .false.
               if (myrank.eq.0)
     &            write(*,1000) name, '(MOC advection)'
            else
              supported = .true.
            end if
         case ('HSS')
            supported = .false.
            if (myrank.eq.0)
     &         write(*,1000) name, ' '
         case ('TOB')
            supported = .false.
            if (myrank.eq.0)
     &         write(*,1000) name, ' '
         case ('SIP')
            supported = .false.
            if (myrank.eq.0)
     &         write(*,1000) name, ' '
         case default
            supported = .true.
      end select

 1000 format(1x,'Unsupported MPP feature:',1x,a,1x,'package',1x,a)

      if (.not.supported) then
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if

      return
      end subroutine pks7mpicheck

      !> Check supported MPP feature.
      subroutine pks7mpicheckani()

      use pksmpi_mod
c
      implicit none
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      if (stenimpsol.ne.2) then
         write(*,*) 'Error, 9-point stencil required for ANI.'
         call pks7mpiwrpfinalize()
         call pksstop(' ')
      end if

      end subroutine pks7mpicheckani

      !> Finish for unsupported PKS feature.
      subroutine pks7mpinotsupported(name)

      use pksmpi_mod
c
      implicit none
c      
c...     Arguments      
      character(len=*) :: name
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      if (myrank.eq.0)write(*,*) 'Unsupported PKS feature : ',trim(name)
      call pks7mpiwrpfinalize()
      call pksstop(' ')
 
      end subroutine      
      
      !> Global exchange (minimum) of transport time step.
      subroutine pks7mpigxchtime( dtrans )

      use pksmpi_mod

      implicit none

c...     Arguments
      real, dimension(1), intent(inout) :: dtrans !< (I/O) transport time step
c...     Locals
      double precision, dimension(1) :: dval
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call timing_tic('MPI','GLOBAL-pks7mpigxchtime')                   ! PKS
      
      dval(1) = dble(dtrans(1))
      call pks7mpiwrpallmind( dval, 1 )
      dtrans(1) = real(dval(1))

      call timing_toc('MPI','GLOBAL-pks7mpigxchtime')                   ! PKS
      
      return
      end subroutine pks7mpigxchtime

      !> Global exchange (maximum) of transport time step.
      subroutine pks7mpigxchmax( x, n )

      use pksmpi_mod

      implicit none

c...     Arguments
      integer, intent(in) :: n
      real, dimension(n), intent(inout) :: x
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call timing_tic('MPI','GLOBAL-pks7mpigxchmax')                    ! PKS
      
      call pks7mpiwrpallmaxr( x, n )

       call timing_toc('MPI','GLOBAL-pks7mpigxchmax')                   ! PKS
      
      return
      end subroutine pks7mpigxchmax

      !> Global exchange (maximum) of one double precision.
      subroutine pks7mpigxchmaxd1( x ) 

      use pksmpi_mod

      implicit none

c...     Arguments
      double precision, intent(inout) :: x

c...    Local variables
      double precision, dimension(1) :: dbuf
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return
      if (mpiloosely.eq.1) return

      call timing_tic('MPI','GLOBAL-pks7mpigxchmaxd1')                  ! PKS
      
      dbuf(1) = x      

      call pks7mpiwrpallmaxd( dbuf, 1 )

      x = dbuf(1)   

      call timing_toc('MPI','GLOBAL-pks7mpigxchmaxd1')                  ! PKS
      
      return
      end subroutine pks7mpigxchmaxd1

      !> Global exchange (maximum) of two double precisions.
      subroutine pks7mpigxchmaxd2( x1, x2 ) 

      use pksmpi_mod

      implicit none

c...     Arguments
      double precision, intent(inout) :: x1
      double precision, intent(inout) :: x2

c...    Local variables
      double precision, dimension(2) :: dbuf
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call timing_tic('MPI','GLOBAL-pks7mpigxchmaxd2')                  ! PKS
      
      dbuf(1) = x1      
      dbuf(2) = x2   
   
      call pks7mpiwrpallmaxd( dbuf, 2 )

      x1 = dbuf(1)   
      x2 = dbuf(2)   
      
      call timing_toc('MPI','GLOBAL-pks7mpigxchmaxd2')                  ! PKS
      
      return
      end subroutine pks7mpigxchmaxd2

      !> Global exchange of internal product.
      subroutine pks7mpigxchip( ip1, ip2, n )

      use pksmpi_mod
      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer, intent(in) :: n    !< (I) number of items
      double precision, intent(inout) :: ip1, !< (I/O) interior product
     &                                   ip2  !< (I/O) interior product
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return
      if (mpiloosely.eq.1) return

      call timing_tic('MPI','GLOBAL-pks7mpigxchip')                     ! PKS
      
      if (n.eq.0) return
      if (n.eq.1) sbufd(1) = ip1
      if (n.eq.2) then
         sbufd(1) = ip1
         sbufd(2) = ip2
      end if

      call pks7mpiwrpallsumd( sbufd, rbufd, n )

      if (n.eq.0) return
      if (n.eq.1) ip1 = rbufd(1)
      if (n.eq.2) then
         ip1 = rbufd(1)
         ip2 = rbufd(2)
      end if

      call timing_toc('MPI','GLOBAL-pks7mpigxchip')                     ! PKS
      
      return
      end subroutine pks7mpigxchip

      !> Load estimation based on icbund.
      subroutine pks7mpiloadest( icbund, nodes)

      use pksmpi_mod

      implicit none

c...     Arguments
      integer, intent(in)  :: nodes                   !< (I) number of nodes
      integer, dimension(nodes), intent(in) :: icbund !< (I) boundary array

c...     Local variables
      integer :: iproc, gsbuf, grcnt(nrproc), offsets(nrproc),
     &          grbuf(nrproc), n, nact, nacttot
      real :: load
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return

      call timing_tic('MPI','GLOBAL-pks7mpiloadest')                    ! PKS
      
      allocate(proc_load(nrproc) )

      nact = 0
      do n = 1, nodes
         if (icbund(n).ne.0) nact = nact + 1
      end do

      gsbuf = nact
      do iproc = 1, nrproc
         grcnt(iproc) = 1
         offsets(iproc) = iproc-1
      end do

      call pks7mpiwrpgathervi( (/gsbuf/), 1, grbuf, grcnt, offsets, 0 )

      if (myrank.eq.0) then

         nacttot = 0
         do iproc = 1, nrproc
            nacttot = nacttot + grbuf(iproc)
         end do

         if (nacttot.ne.0) then
            do iproc = 1, nrproc
               proc_load(iproc) = 100*real(grbuf(iproc))/nacttot
            end do
         end if
      end if

      call timing_toc('MPI','GLOBAL-pks7mpiloadest')                    ! PKS
      
      return
      end subroutine pks7mpiloadest

      !> Master process reads observation nodes.
      subroutine pks7mpireadobs(locobs, mxobs, n, tn, nobs)

      use pksmpi_mod

      implicit none

c...     Arguments
      integer, intent(in) :: mxobs,                     !< (I) first dimension locobs array
     &                       n,                         !< (I) counter
     &                       tn,                        !< (I) counter
     &                       nobs                       !< (I) number of observation points
      integer, dimension(3,mxobs), intent(in) :: locobs !< (I) observation points

c...     Functions
      integer :: pks7mpignode2rank

c...     Local variables
      integer :: ilay, irow, icol, nrc, gn, rank
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1 .or.
     &    mergeobs.eq.0) then
         return
      end if

      if (nobs.eq.0) return

      if (tn.eq.1) then
         gnobs = nobs
         allocate( glocobs(4,mxobs) )
         if (myrank.eq.0) allocate( obscnew(gnobs) )
      end if

      ilay = locobs(1,n)
      irow = locobs(2,n)
      icol = locobs(3,n)

c...     copy global node numbers
      glocobs(1,tn) = ilay
      glocobs(2,tn) = irow
      glocobs(3,tn) = icol

      nrc = gnrow*gncol
      gn = icol+(irow-1)*gncol+(ilay-1)*nrc
      rank = pks7mpignode2rank( gn )

      glocobs(4,tn) = rank

      return
      end subroutine pks7mpireadobs

      !> Master process write header of observation file.
      subroutine pks7mpiwrtobshdr(iobs, writeobs)

      use pksmpi_mod

      implicit none

c...     arguments
      integer, intent(in)  :: iobs      !< (I) logical unit of observation output file
      logical, intent(out) :: writeobs  !< (O) logical indicating that other processes may write obs file

c...     local variables
      integer :: i, n
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1 .or.
     &    mergeobs.eq.0) then
         writeobs = .true.
         return
      end if

      write(iobs,1063) ((glocobs(i,n),i=1,3),n=1,gnobs)

 1063 FORMAT(1X,' STEP   TOTAL TIME',
     & '             LOCATION OF OBSERVATION POINTS (K,I,J)'
     & /1X,17X,16(1X,3I4,1X)/(1X,17X,16(1X,3I4,1X)))

      writeobs = .false.

      return
      end subroutine

       !> Master process gather observation concentrations from
       !! slave processes.
      subroutine pks7mpigxchobs(locobs, mxobs, nobs, cnew, ncol,
     &                       nrow, nlay, ncomp, icomp, ntrans,
     &                       nprobs, prtout, time2, iobs, writeobs)

      use pksmpi_mod
      use pksmpiwrp_mod, only: sbufr, rbufr

      implicit none

c...     arguments
      integer                                 , intent(in)  :: mxobs    !< (I) second array dimension
      integer                                 , intent(in)  :: nobs     !< (I) number of observation points
      integer                                 , intent(in)  :: ncol     !< (I) number of columns
      integer                                 , intent(in)  :: nrow     !< (I) number of rows
      integer                                 , intent(in)  :: nlay     !< (I) number of layers
      integer                                 , intent(in)  :: ncomp    !< (I) number of components
      integer                                 , intent(in)  :: icomp    !< (I) current component
      integer                                 , intent(in)  :: ntrans   !< (I) transport time step
      integer                                 , intent(in)  :: nprobs   !< (I) interval to write observation data
      integer                                 , intent(in)  :: iobs     !< (I) logical unit of observation output file
      logical                                 , intent(in)  :: prtout   !< (I) flag indicating output should be printed
      real                                    , intent(in)  :: time2    !< (I) current time
      integer, dimension(3,mxobs)             , intent(in)  :: locobs   !< (I) observation point locations
      real   , dimension(ncol,nrow,nlay,ncomp), intent(in)  :: cnew     !< (I) concentrations
      logical                                 , intent(out) :: writeobs !< (O) flag indicating if obs should be written

c...     local variables
      integer :: iproc, gscnt, grcnt(nrproc), offsets(nrproc),
     &           ilay, irow, icol, i, n
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1 .or.
     &    mergeobs.eq.0) then
         writeobs = .true.
         return
      end if

      call timing_tic('MPI','GLOBAL-pks7mpigxchobs')                    ! PKS
     
      grcnt(1:nrproc) = 0
      gscnt = 0
      do i = 1, gnobs
         iproc = glocobs(4,i) + 1
         grcnt(iproc) = grcnt(iproc) + 1
         if (iproc.eq.myrank+1) then
            gscnt = gscnt + 1
            ilay = locobs(1,gscnt)
            irow = locobs(2,gscnt)
            icol = locobs(3,gscnt)
            sbufr(gscnt) = cnew(icol,irow,ilay,icomp)
         end if
      end do

c...     offsets
      offsets(1) = 0
      do iproc = 1, nrproc-1
         offsets(iproc+1) = offsets(iproc) + grcnt(iproc)
      end do

      call pks7mpiwrpgathervr( sbufr, gscnt, rbufr, grcnt, offsets, 0 )

      if (myrank.eq.0) then

         do i = 1, gnobs
            obscnew(i) = rbufr(i)
         end do

c...        master process writes observation data
         if(gnobs.gt.0.and.
     &      (mod(ntrans-1,nprobs).eq.0.or.prtout)) then
            if(gnobs.le.16) then
              write(iobs+icomp,1000) ntrans,time2,
     &                               (obscnew(n),n=1,gnobs)
            else
              write(iobs+icomp,1010) ntrans,time2,
     &                               (obscnew(n),n=1,gnobs)
            endif
         endif
 1000 format(1x,i5,1x,1pg13.5,1x,16(g13.5,1x))
 1010 format(1x,i5,1x,1pg13.5,1x,16(g13.5,1x)/(1x,20x,16(g13.5,1x)))
C
      end if

      writeobs = .false.

      call timing_toc('MPI','GLOBAL-pks7mpigxchobs')                    ! PKS
      
      return
      end subroutine pks7mpigxchobs

      !>
      subroutine pks7mpigxchdiag( diagmat )

      use pksmpi_mod

      implicit none

c...     arguments
      logical, intent(inout) :: diagmat !< (I/O) flag indicating that the matrix is diagonal

c...     locals
      integer :: idiag
      real, dimension(1) :: sflg, rflg
c.......................................................................

      if (mpptyp.eq.mppser) return

      if (diagmat) then
         sflg(1) = 1.
      else
         sflg(1) = 0.
      end if

      call pks7mpiwrpallsumr( sflg, rflg, 1 )

      if (int(rflg(1)).eq.nrproc) then
         diagmat = .true.
      else
         diagmat = .false.
      end if

      return
      end subroutine pks7mpigxchdiag
      
      subroutine pks7mpibounds( icol, irow, ncol, nrow, 
     1                       lnorth, lsouth, least, lwest )

      use pksmpi_mod

      implicit none

c...     Arguments
      integer, intent(in) :: icol, !< (I) column index
     &                       irow, !< (I) row index
     &                       ncol,
     &                       nrow 
      logical, intent(out) :: lnorth, lsouth, least, lwest
c...     Local variables
      integer :: gicol, girow
c.......................................................................

      lnorth = .false.
      lsouth = .false.
      least  = .false.
      lwest  = .false.

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) then
          if (icol.eq.1)    lwest  = .true.
          if (icol.eq.ncol) least  = .true.
          if (irow.eq.1)    lnorth = .true.
          if (irow.eq.nrow) lsouth = .true.
          return
      end if
      
      gicol = proc_icolmin(myrank+1,iovl)+icol-1
      girow = proc_irowmin(myrank+1,iovl)+irow-1

      if (gicol.eq.proc_icolmin(myrank+1,inovl)) lwest  = .true.
      if (gicol.eq.proc_icolmax(myrank+1,inovl)) least  = .true.
      if (girow.eq.proc_irowmin(myrank+1,inovl)) lnorth = .true.
      if (girow.eq.proc_irowmax(myrank+1,inovl)) lsouth = .true.
      
      return
      end subroutine pks7mpibounds      
            
      !> Convert global node number to local node number
      integer function pks7mpig2lnode( gicol, girow, gilay )

      use pksmpi_mod

      implicit none
c
c...     Arguments
      integer, intent(in) :: gicol, girow, gilay 
c
c        Functions
      integer :: pks7mpignode2rank

c...     Local variables
      integer :: m, licol, lirow, lilay,
     &           lncol, lnrow, lnlay, nrc
c.......................................................................

c...     get local ncol, nrow, nlay
      lncol = proc_ncol(myproc,iovl)
      lnrow = proc_nrow(myproc,iovl)
      lnlay = gnlay
      nrc = lncol*lnrow

c...     determine local node number for the corresponding partition
      licol = (gicol-proc_icolmin(myproc,iovl)) + 1
      lirow = (girow-proc_irowmin(myproc,iovl)) + 1
      lilay = gilay

      m = licol + (lirow-1)*lncol + (lilay-1)*nrc
      pks7mpig2lnode = m

      return
      end function pks7mpig2lnode
     
      !> Convert local node number to global node number.
      integer function pks7mpil2gnode( n, rank )

      use pksmpi_mod
c
      implicit none

c...     Arguments
      integer, intent(in) :: n,   !< (I) local node number
     &                       rank !< (I) rank ID
c
c...     Local variables
      integer :: m, ncol, nrow, nlay,
     &           gicol, girow, gilay,
     &           icol, irow, ilay, nrc
c.......................................................................

      nrc = gnrow*gncol

      ncol = proc_ncol(rank+1,iovl)
      nrow = proc_nrow(rank+1,iovl)
      nlay = gnlay

      call getircl( n, icol, irow, ilay, ncol, nrow, nlay )

      gicol = proc_icolmin(myrank+1,iovl)+icol-1
      girow = proc_irowmin(myrank+1,iovl)+irow-1
      gilay = ilay

      m = gicol + (girow-1)*gncol + (ilay-1)*nrc

      pks7mpil2gnode = m

      return
      end function pks7mpil2gnode       
    
      !> Get indices for global node number.
      subroutine pks7mpigetgircl( n, icol, irow, ilay )

      use pksmpi_mod
c
      implicit none

c...     Arguments
      integer, intent(in)  :: n
      integer, intent(out) :: icol
      integer, intent(out) :: irow
      integer, intent(out) :: ilay
c
c...     Local variables
c.......................................................................

      call getircl( n, icol, irow, ilay, gncol, gnrow, gnlay )

      return
      end subroutine pks7mpigetgircl             
      
      !> Get flag for writing mass output.
      logical function pks7mpimasterwrite()

      use pksmpi_mod, only: mpptyp, mppser, myrank, mpiloosely

      implicit none

c...     locals
      logical :: writeflg
c.......................................................................

      if (mpptyp.eq.mppser.or.mpiloosely.eq.1) then
         writeflg = .true.
      else
         if (myrank.eq.0) then
            writeflg = .true.
         else
            writeflg = .false.
         end if
      end if
      pks7mpimasterwrite = writeflg

      return
      end function pks7mpimasterwrite

      !> Get flag for master processt.
      logical function pks7mpimaster()

      use pksmpi_mod, only: mpptyp, mppser, myrank, mpiloosely

      implicit none

c...     locals
      logical :: flg
c.......................................................................

      if (mpptyp.eq.mppser.or.mpiloosely.eq.1) then
         flg = .true.
      else
         if (myrank.eq.0) then
            flg = .true.
         else
            flg = .false.
         end if
      end if
      pks7mpimaster = flg

      return
      end function pks7mpimaster      
      
      !> Get flag for writing observation output.
      logical function pks7mpimasterwriteobs()

      use pksmpi_mod, only: mpptyp, mppser, myrank, mergeobs

      implicit none

c...     locals
      logical :: writeflg
c.......................................................................

      writeflg = .true. 
      if (mpptyp.ne.mppser .and. mergeobs.eq.1) then
         if (myrank.gt.0) writeflg = .false.
      end if
      pks7mpimasterwriteobs = writeflg

      return
      end function pks7mpimasterwriteobs

      !> Get number of processes.
      subroutine pks7mpigetnrproc(np)

      use pksmpi_mod, only: mpptyp, mppser, nrproc, serreadglob

      implicit none

      integer, intent(out) :: np
c.......................................................................

      if (mpptyp.eq.mppser.or.serreadglob) then
         np = 1
      else
         np = nrproc
      end if

      return
      end subroutine pks7mpigetnrproc 
 
      !> Get myrank
      subroutine pks7mpigetmyrank(rnk)

      use pksmpi_mod, only: mpptyp, mppser, myrank, serreadglob

      implicit none

      integer, intent(out) :: rnk
c.......................................................................

      if (mpptyp.eq.mppser.or.serreadglob) then
         rnk = 0
      else
         rnk = myrank
      end if

      return
      end subroutine pks7mpigetmyrank 
      
      !> Finialize MPI.
      subroutine pks7mpifinalize()
      use pksmpi_mod, only: mpiactive, myrank

      implicit none
c.......................................................................

      if (mpiactive) then      
         call pks7mpiwrpfinalize()
      end if
         
      return
      end subroutine pks7mpifinalize
      
      subroutine pks7mpigxchcnvg(dxmax,lxmax,pxmax,rmax,lrmax,prmax,
     1   l2norm0,l2norm,niac,ixmap,nodes,lfirst)
c      
      use pksmpi_mod, only: mpptyp, mppser, nrproc, myrank, l2gnod,
     1                      mpiloosely, timsec, itbeg, itend, itimmpig 
      use pksmpiwrp_mod, only: sbufd, rbufd
c
      implicit none
c
c...     Arguments
      double precision, intent(inout) :: dxmax 
      integer, intent(inout) :: lxmax 
      integer, intent(out) :: pxmax 
      double precision, intent(inout) :: rmax 
      integer, intent(inout) :: lrmax 
      integer, intent(out) :: prmax 
      double precision, intent(inout) :: l2norm0 
      double precision, intent(inout) :: l2norm 
      logical, intent(inout) :: lfirst 
c      
      integer, intent(in) :: niac
      integer, dimension(niac), intent(in) :: ixmap
      integer, intent(in) :: nodes     
c
c...     Functions
      integer :: pks7mpil2gnode
c
c...     Local variables
      integer :: iproc, gscnt, grcnt(nrproc), offsets(nrproc)
      integer :: glxmax, glrmax, n, l1, l2, i
      double precision :: d1, d2, d3, d4
c
c...     Parameters
      double precision, parameter :: dzero = 0.0d0
c.......................................................................

c...     return in the serial case
      if (mpptyp.eq.mppser) return
      if (mpiloosely.eq.1) return

      !call timing_date_and_time(itbeg)                                  !JV
      
c...     determine global nodes for maximum
      n = ixmap(lxmax)
      if (n.lt.-nodes) n = n + nodes
#ifdef PKSUNS      
      glxmax = l2gnod(abs(n))    
#else         
      glxmax = pks7mpil2gnode( abs(n), myrank)    
#endif      
c   
      n = ixmap(lrmax)
      if (n.lt.-nodes) n = n + nodes
#ifdef PKSUNS      
      glrmax = l2gnod(abs(n))  
#else         
      glrmax = pks7mpil2gnode( abs(n), myrank)    
#endif      
c
c...     pack
      gscnt = 6
      sbufd(1) = dxmax
      sbufd(2) = dble(glxmax)
      sbufd(3) = rmax
      sbufd(4) = dble(glrmax)
      sbufd(5) = l2norm0
      sbufd(6) = l2norm
c      
      grcnt(1:nrproc) = gscnt
c      
c...     offsets
      offsets(1) = 0
      do iproc = 1, nrproc-1
         offsets(iproc+1) = offsets(iproc) + grcnt(iproc)
      end do
c       
      call pks7mpiwrpallgathervd( sbufd, gscnt, rbufd, grcnt, offsets)
c 
c...    unpack
      d1 = dzero 
      d2 = dzero
      d3 = dzero
      d4 = dzero
      l1 = 0
      l2 = 0
      pxmax = -1
      prmax = -1      
      do iproc = 1, nrproc
         i = offsets(iproc)
         if ( rbufd(i+1).gt.d1 ) then
            d1    = rbufd(i+1)
            l1    = int(rbufd(i+2))
            pxmax = iproc
         end if 
         if ( rbufd(i+3).gt.d2 ) then
            d2    = rbufd(i+3)
            l2    = int(rbufd(i+4))
            prmax = iproc
         end if 
         d3 = d3 + rbufd(i+5)
         d4 = d4 + rbufd(i+6)
      end do
c      
c...     assign
      dxmax = d1
      lxmax = l1
      rmax  = d2
      lrmax = l2
      if (lfirst) l2norm0 = sqrt(d3)
      l2norm  = sqrt(d4)
c      
      lfirst = .false.
c      
      !call timing_date_and_time(itend)                                  !JV
      !call timing_add_sec(itbeg,itend,timsec(itimmpig))                 !JV

      return
      end subroutine pks7mpigxchcnvg     
      
      !> Check if MPI is active.
      subroutine pks7mpiactive(active)
      use pksmpi_mod

      implicit none

c        arguments
      logical, intent(out) :: active
      
c.......................................................................

      active = .false.
      if (mpptyp.ne.mppser) active = .true.

      return
      end subroutine pks7mpiactive          
      
      subroutine pks7mpisetserflg(flg)
      use pksmpi_mod, only: mpptyp, mppser, serreadglob
      
      implicit none
      
c...     arguments
      logical, intent(in) :: flg
c.......................................................................
      
      if (mpptyp.eq.mppser) then
         return
      end if    
      serreadglob = flg 
      
      end subroutine pks7mpisetserflg
          
      subroutine pks7mpiallociwrk(n)
      use pksmpi_mod, only: mpptyp, mppser, iwrk
 
      implicit none
       
c...     arguments
      integer, intent(in) :: n
c.......................................................................
      
      if (mpptyp.eq.mppser) then
         return
      end if  
      
      if (.not.allocated(iwrk)) then
         allocate(iwrk(n))
      else
         if (size(iwrk) < n) then 
            deallocate(iwrk)
            allocate(iwrk(n))
         end if
      end if

      end subroutine pks7mpiallociwrk
      
      subroutine pks7mpiallocrwrk(n)
      use pksmpi_mod, only: mpptyp, mppser, rwrk
 
      implicit none
       
c...     arguments
      integer, intent(in) :: n
c.......................................................................
      
      if (mpptyp.eq.mppser) then
         return
      end if  
      
      if (.not.allocated(rwrk)) then
         allocate(rwrk(n))
      else
         if (size(rwrk) < n) then 
            deallocate(rwrk)
            allocate(rwrk(n))
         end if
      end if

      end subroutine pks7mpiallocrwrk      
      
      subroutine pks7mpiallocdwrk(n)
      use pksmpi_mod, only: mpptyp, mppser, dwrk
 
      implicit none
       
c...     arguments
      integer, intent(in) :: n
c.......................................................................
      
      if (mpptyp.eq.mppser) then
         return
      end if  
      
      if (.not.allocated(dwrk)) then
         allocate(dwrk(n))
      else
         if (size(dwrk) < n) then 
            deallocate(dwrk)
            allocate(dwrk(n))
         end if
      end if

      end subroutine pks7mpiallocdwrk
      
      subroutine pks7mpisetixmap(ixmap,niac)
      use pksmpi_mod, only: mpptyp, mppser, xp, nrxp, novlapimpsol,
     1                      myrank
 
      implicit none
       
c...     arguments
      integer, intent(in)                   :: niac
      integer, dimension(niac), intent(out) :: ixmap      

c...    locals
      integer :: i, n, ixp, iovl
c.......................................................................
      
      if (mpptyp.eq.mppser) then
         do n = 1, niac
            ixmap(n) = n
         end do   
         return
      end if  
      
      do n = 1, niac
         ixmap(n) = n
      end do   
      do ixp = 1, nrxp
         do iovl = 1, novlapimpsol
            do i = 1, xp(ixp)%upkidx(iovl)%nidx
               n = xp(ixp)%upkidx(iovl)%idx(i)
               if (iovl == novlapimpsol) then
                  ixmap(n) = -niac-ixmap(n)
               else    
                  ixmap(n) = -ixmap(n)
               end if   
            end do 
         end do    
      end do
      
      end subroutine pks7mpisetixmap     
      

           
      subroutine pks7mpiskiplnod(n,lskip)
c   
      use pksmpi_mod, only: mpptyp, mppser, l2gnod

      implicit none
      
c...     arguments
      integer, intent(in) :: n !< (I) local node
      logical, intent(out):: lskip
c.......................................................................
      
c...     return in the serial case
      lskip = .false.
      if (mpptyp.eq.mppser) then
         return
      end if          

      if (l2gnod(n).le.0) then
         lskip = .true.
      end if    
      
      return
      end subroutine pks7mpiskiplnod

      subroutine pks7mpiskipgnod(n,lskip)
c   
      use pksmpi_mod, only: mpptyp, mppser, g2lnod

      implicit none
      
c...     arguments
      integer, intent(inout) :: n     !< (I/O) global node
      logical, intent(out)   :: lskip
c.......................................................................
      
c...     return in the serial case
      lskip = .false.
      if (mpptyp.eq.mppser) then
         return
      end if          

      if (g2lnod(n).eq.0) then
         lskip = .true.
         return
      end if    
      n = abs(g2lnod(n))
      
      return
      end subroutine pks7mpiskipgnod      
      
      subroutine pks7mpigetgnodes(n)
c   
      use pksmpi_mod, only: mpptyp, mppser, gnodes

      implicit none
      
c...     arguments
      integer, intent(inout) :: n     !< (I/O) global node
c.......................................................................
      
c...     return in the serial case
      if (mpptyp.eq.mppser) then
         return
      end if          

      n = gnodes 
      
      return
      end subroutine pks7mpigetgnodes           
      
      
      !> Global exchange timing results
      subroutine pks7mpilgxchtiming( temp, n )
      
      use pksmpi_mod
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      real, intent(inout), dimension(n) :: temp !< array with timings
      integer, intent(in) :: n
c
c...     Local variables
      integer :: i
c      real, dimension(n) :: grbuf
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return
    
      call pks7mpiwrpallmaxr( temp, n )
      !call timing_date_and_time(itend)

      return
      end subroutine pks7mpilgxchtiming      
      
      subroutine pks7mpisetibound(ibound,niac)
      use pksmpi_mod, only: mpptyp, mppser, xp, nrxp, novlapimpsol,
     1                      myrank, mpiloosely
 
      implicit none
       
c...     arguments
      integer, intent(in)                     :: niac
      integer, dimension(niac), intent(inout) :: ibound      

c...     locals
      integer :: i, n, ixp, iovl
c.......................................................................
      
      if (mpptyp.eq.mppser) return
      if (mpiloosely.eq.0) return
      
      do ixp = 1, nrxp
         do iovl = 1, novlapimpsol
            do i = 1, xp(ixp)%upkidx(iovl)%nidx
               n = xp(ixp)%upkidx(iovl)%idx(i)
               if (iovl == novlapimpsol) then
                  ibound(n) = -1
               else    
                  ! do nothing 
               end if   
            end do 
         end do    
      end do
      
      end subroutine pks7mpisetibound       
      
      !> Transform MODFLOW indices to local indices.
      subroutine pks7mpitrn(icol,irow,ilay,lused)
      
      use pksmpi_mod, only: mpptyp, mppser, mppini1,
     1                      gncol, gnrow, myproc, iovl,
     2                      proc_icolmin, proc_icolmax,
     3                      proc_irowmin, proc_irowmax
      implicit none
      
c...     arguments
      integer, intent(inout) :: icol ! (I): global column; (O): local column
      integer, intent(inout) :: irow ! (I): global row;    (O): local row
      integer, intent(inout) :: ilay
      logical, intent(out)   :: lused
      
c...     locals
      integer :: ic1, ic2, ir1, ir2
c.......................................................................

      lused = .true.
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return 
      
      ic1 = proc_icolmin(myproc,iovl)           
      ic2 = proc_icolmax(myproc,iovl)           
      ir1 = proc_irowmin(myproc,iovl)           
      ir2 = proc_irowmax(myproc,iovl)           
      
      if (icol.lt.ic1.or.icol.gt.ic2) lused = .false.
      if (irow.lt.ir1.or.irow.gt.ir2) lused = .false.
      if (.not.lused) then ! return is the node is not used
         icol = 0
         irow = 0
         return
      end if
      
      ! transform global --> local indices (ilay remains the same)
      icol = icol-proc_icolmin(myproc,iovl)+1       
      irow = irow-proc_irowmin(myproc,iovl)+1       
      
      end subroutine pks7mpitrn
      
      !> MPI barrier.
      subroutine pks7mpibarrier()
      
      use pksmpi_mod, only: mpptyp, mppser, mppini1            
      implicit none
      
c.......................................................................
      
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return 

      call pks7mpiwrpbarrier()     
      
      end subroutine pks7mpibarrier