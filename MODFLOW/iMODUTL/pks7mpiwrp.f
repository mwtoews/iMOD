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

      module pksmpiwrp_mod
c
      implicit none
c
#ifdef PKSMPI
c...     Include files
      include 'mpif.h'
#endif      
c
c...     Parameters
      integer, parameter :: pks7mpiwrplblk_size = 64,
     &                      pks7mpiwrprbuf_size = 256
c
#ifdef PKSMPI
      character(len=MPI_MAX_PROCESSOR_NAME) :: pks7mpiwrpmyname
      integer, dimension(MPI_STATUS_SIZE,pks7mpiwrplblk_size), save ::
     &   pks7mpiwrpstats
#endif      
c
      integer, save :: pks7mpiwrpcomm_world, pks7mpiwrpcomm_null
      integer, save :: pks7mpiwrpnrproc, pks7mpiwrpmyrank
      integer, dimension(pks7mpiwrprbuf_size), save          ::
     &    pks7mpiwrprbufi
      double precision, dimension(pks7mpiwrprbuf_size), save :: 
     &    pks7mpiwrprbufd

      integer, dimension(1000) :: rreq, sreq

      integer, save :: lenbuf
      integer         , dimension(:), allocatable :: sbufi, rbufi
      real            , dimension(:), allocatable :: sbufr, rbufr
      double precision, dimension(:), allocatable :: sbufd, rbufd
      
      end module pksmpiwrp_mod

      !> Wrapper subroutine around MPI_COMM_SIZE te determine number of
      !! processes.
      integer function pks7mpiwrpcomm_size( comm )
c
      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      integer, intent(in) :: comm !< (I) communicator
c
#ifdef PKSMPI
c
c...    Locals
      integer :: ierror, size
c.......................................................................
c
      call mpi_comm_size( comm, size, ierror )
c
      pks7mpiwrpcomm_size = size
c
#else
c
      pks7mpiwrpcomm_size = 1
c
#endif
c
      return
      end function pks7mpiwrpcomm_size
c
      !> Wrapper function around MPI_COMM_RANK to determine own
      !! process rank ID.
      !! @return rank ID of my own process.
      integer function pks7mpiwrpcomm_rank( comm )

      use pksmpiwrp_mod

      implicit none
c
c...     Arguments
      integer, intent(in) :: comm !< (I) communicator
c.......................................................................
c
#ifdef PKSMPI
c
      integer :: ierror, rank
c
      call mpi_comm_rank( comm, rank, ierror )
c
      pks7mpiwrpcomm_rank = rank
c
#else
c
      pks7mpiwrpcomm_rank = 0
c
#endif
c
      return
      end function

      !> Wrapper subroutine around MPI_INIT for initialization.
      subroutine pks7mpiwrpinit()

      use pksmpiwrp_mod

      implicit none
c
#ifdef PKSMPI
c
c...     Functions
      integer :: pks7mpiwrpcomm_rank, pks7mpiwrpcomm_size
c
c...     Local variables
      integer :: ierror, i, required, provided
c.......................................................................
c
      pks7mpiwrpcomm_world = MPI_COMM_WORLD
      pks7mpiwrpcomm_null  = MPI_COMM_NULL
c
c      call mpi_init( ierror )
      required = MPI_THREAD_FUNNELED   
      call mpi_init_thread(required,provided,ierror)
c
      do i = 1, pks7mpiwrprbuf_size
         pks7mpiwrprbufi(i) = 0
         pks7mpiwrprbufd(i) = 0.0d0
      end do
c
      pks7mpiwrpnrproc = pks7mpiwrpcomm_size( pks7mpiwrpcomm_world )
      pks7mpiwrpmyrank = pks7mpiwrpcomm_rank( pks7mpiwrpcomm_world )

      if (required.ne.provided) then
         if (pks7mpiwrpmyrank.eq.0) then
            write(*,*) 'Warning, could not guarantee thread safety.'
         end if
      end if
#else
c
      pks7mpiwrpnrproc = 1
      pks7mpiwrpmyrank = 0
c
#endif
      return
      end subroutine pks7mpiwrpinit

      !> Wrapper subroutine around MPI_FINALIZE termination.
      subroutine pks7mpiwrpfinalize()

      use pksmpiwrp_mod

      implicit none
c
#ifdef PKSMPI
c
c...     Local variables
      integer :: ierror
c.......................................................................
c
      call mpi_finalize( ierror )
C
#endif
C
      return
      end subroutine pks7mpiwrpfinalize

      !>  Wrapper subroutine around MPI_BARRIER barrier.
      subroutine pks7mpiwrpbarrier()
c
      use pksmpiwrp_mod
c
      implicit none
c
#ifdef PKSMPI
c
c...     Local variables
      integer :: ierror
c.......................................................................
c
      call mpi_barrier( pks7mpiwrpcomm_world, ierror )
C
#endif
C
      return
      end subroutine pks7mpiwrpbarrier

      !> Wrapper subroutine around MPI_GET_PROCESSOR_NAME to get
      !! host name.
      subroutine pks7mpiwrpget_processor_name( name )

      implicit none
c
c...     Arguments
      character(len=*), intent(out) :: name !< (O) host name
c
#ifdef PKSMPI
c
c...     Local variables
      integer :: namelen, ierror
c.......................................................................
c
      call mpi_get_processor_name( name, namelen, ierror )
c
#else
      name = ''
      call pks7mpiwrperror( 'pks7mpiwrpget_processor_name',
     1   'invalid operation' )
#endif
c
      return
      end subroutine pks7mpiwrpget_processor_name

      !> Waits for a given set of communication requests to complete.
      subroutine pks7mpiwrpwaitall( count, reqs )

      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      integer, intent(in) :: count                     !< (I)   number of request handles
      integer, dimension(count), intent(inout) :: reqs !< (I/O) array with request handles
c
#ifdef PKSMPI
c
c...     Local variables
      integer :: ierror, i, n
c.......................................................................
c
      if ( count .le. pks7mpiwrplblk_size ) then
c
         call mpi_waitall( count, reqs, pks7mpiwrpstats, ierror )
         return
c
      end if
c
      do i = 1, count, pks7mpiwrplblk_size
c
         n = min( pks7mpiwrplblk_size, count - i + 1 )
c
         call mpi_waitall( n, reqs(i), pks7mpiwrpstats, ierror )
c
      end do
c
#endif
c
      return
      end subroutine pks7mpiwrpwaitall
c
      !> Prints an error message and aborts the current program.
      subroutine pks7mpiwrperror( subname, message )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      character*(*), intent(in) :: subname, !< (I) the name of the subroutine in which the error occured
     &                             message  !< (I) the error message to be printed

c...     Local variables
      integer  ierror, i, idummy(1)
c.......................................................................
c
#ifdef PKSMPI
      write (6,*)
      write (6,*) '*** fatal runtime error in subroutine '//subname
      write (6,*)
      write (6,*) '    '//message
      write (6,*)
      write (6,*) '*** trying to terminate all processes...'
      write (6,*)
c
      call mpi_abort( mpi_comm_world, 1, ierror )
c
#else
c
      write (6,*)
      write (6,*) '*** fatal runtime error in subroutine '//subname
      write (6,*)
      write (6,*) '    '//message
      write (6,*)
      write (6,*) '*** aborting program...'
      write (6,*)
c
c...  try to force a core dump
c
      i         = -666666666
      idummy(i) =  666666666
c
      stop
c
#endif
c
      return
      end subroutine pks7mpiwrperror

      !> Begins a non-blocking send of an integer array to the
      !! process with rank dest.
      subroutine pks7mpiwrpisendi( buf, count, dest, tag, comm,  req )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer, intent(in)  :: buf(*), !< (I) integer buffer
     &                        count,  !< (I) number of integers to be send
     &                        dest,   !< (I) rank of destination process
     &                        tag,    !< (I) message tag
     &                        comm    !< (I) communicator
      integer, intent(out) :: req     !< (O) request handle
c
#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror
c.......................................................................
c
      call mpi_isend( buf, count, mpi_integer, dest, tag,
     &                comm,  req, ierror )
c
#else
      req = -1
      call pks7mpiwrperror( 'pks7mpiwrpisendi', 'invalid operation' )
#endif
c
      return
      end subroutine pks7mpiwrpisendi

      !> Begins a non-blocking send of a real array to the
      !! process with rankID dest.
      subroutine pks7mpiwrpisendr( buf, count, dest, tag, comm,  req )

      use pksmpiwrp_mod

      implicit none
c
c...     Arguments
      real,    intent(in)  :: buf(*) !< integer buffer to be sent
      integer, intent(in)  :: count  !< number of integers to be sent
      integer, intent(in)  :: dest   !< rank id of destination process
      integer, intent(in)  :: tag    !< message tag
      integer, intent(in)  :: comm   !< communicator
      integer, intent(out) :: req    !< request handle
c
#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror
c.......................................................................
c
      call mpi_isend( buf, count, mpi_real, dest, tag,
     &                comm,  req, ierror )
c
#else
      req = -1
      call pks7mpiwrperror( 'pks7mpiwrpisendr', 'invalid operation' )
#endif
c
      return
      end subroutine pks7mpiwrpisendr

      !> Begins a non-blocking send of a double array to the
      !! process with rankID dest.
      subroutine pks7mpiwrpisendd( buf, count, dest, tag, comm,  req )

      use pksmpiwrp_mod

      implicit none
c
c...     Arguments
      double precision,    intent(in)  :: buf(*) !< double buffer to be sent
      integer, intent(in)  :: count  !< number of integers to be sent
      integer, intent(in)  :: dest   !< rank id of destination process
      integer, intent(in)  :: tag    !< message tag
      integer, intent(in)  :: comm   !< communicator
      integer, intent(out) :: req    !< request handle
c
#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror
c.......................................................................
c
      call mpi_isend( buf, count, mpi_double_precision, dest, tag,
     &                comm,  req, ierror )
c
#else
      req = -1
      call pks7mpiwrperror( 'pks7mpiwrpisendd', 'invalid operation' )
#endif
c
      return
      end subroutine pks7mpiwrpisendd


      !> Begins a non-blocking receive of an integer array from
      !! the process with rank source.
      subroutine pks7mpiwrpirecvi( buf, count, source, tag, comm, req )

      use pksmpiwrp_mod

      implicit none
c
c...     Arguments
      integer :: buf(*)  !< (I) integer buffer
      integer :: count,  !< (I) number of integers to be received
     &           source, !< (I) rank of source process
     &           tag,    !< (I) message tag
     &           comm    !< (I) communicator
      integer :: req     !< (O) request handle
c
#ifdef PKSMPI
c
c...     Local variables
      integer  ierror
c.......................................................................
c
      call mpi_irecv( buf, count, mpi_integer, source, tag,
     &                comm,  req, ierror )
c
#else
      call pks7mpiwrperror( 'pks7mpiwrpirecvi', 'invalid operation' )
c
#endif
c
      return
      end subroutine pks7mpiwrpirecvi

      !> Begins a non-blocking receive of an real array from
      !! the process with rank source.
      subroutine pks7mpiwrpirecvr( buf, count, source, tag, comm, req )

      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      real    :: buf(*)  !< (I) double buffer
      integer :: count,  !< (I) number of integers to be received
     &           source, !< (I) rank of source process
     &           tag,    !< (I) message tag
     &           comm    !< (I) communicator
      integer :: req     !< (O) request handle
c
#ifdef PKSMPI
c
c...     Local variables
      integer  ierror
c.......................................................................
c
      call mpi_irecv( buf, count, mpi_real, source, tag,
     &                comm,  req, ierror )
c
#else
      call pks7mpiwrperror( 'pks7mpiwrpirecvr', 'invalid operation' )
c
#endif
c
      return
      end subroutine pks7mpiwrpirecvr

      !> Begins a non-blocking receive of an double array from
      !! the process with rank source.
      subroutine pks7mpiwrpirecvd( buf, count, source, tag, comm, req )

      use pksmpiwrp_mod
c
      implicit none
c
c...     Arguments
      double precision :: buf(*)  !< (I) double buffer
      integer :: count,  !< (I) number of integers to be received
     &           source, !< (I) rank of source process
     &           tag,    !< (I) message tag
     &           comm    !< (I) communicator
      integer :: req     !< (O) request handle
c
#ifdef PKSMPI
c
c...     Local variables
      integer  ierror
c.......................................................................
c
      call mpi_irecv( buf, count, mpi_double_precision, source, tag,
     &                comm,  req, ierror )
c
#else
      call pks7mpiwrperror( 'pks7mpiwrpirecvd', 'invalid operation' )
c
#endif
c
      return
      end subroutine pks7mpiwrpirecvd

      !> Communicates and minimizes integer value(s) across all processes.
      !! To minimize a scalar value, simply use gsbuf with size 1.
      !! For arrays, each index is minimized separately (across processes).
      subroutine pks7mpiwrpallmini( gsbuf, n )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer              , intent(in)    :: n     !< Number of elements in data array.
      integer, dimension(n), intent(inout) :: gsbuf !< Array with value(s) to be minimized.

#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror, i
      integer, dimension(n) :: grbuf
c.......................................................................
c
      call mpi_allreduce( gsbuf,
     &                    grbuf,
     &                    n,
     &                    mpi_integer,
     &                    mpi_min,
     &                    pks7mpiwrpcomm_world,
     &                    ierror )

c      gsbuf = grbuf

      do i = 1, n
         gsbuf(i) = grbuf(i)
      end do

#else
      call pks7mpiwrperror( 'pks7mpiwrpallmini', 'invalid operation' )
c
#endif
      return
      end subroutine pks7mpiwrpallmini      
      
      !> Communicates and minimizes double value(s) across all processes.
      !! To minimize a scalar value, simply use gsbuf with size 1.
      !! For arrays, each index is minimized separately (across processes).
      subroutine pks7mpiwrpallmind( gsbuf, n )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer           , intent(in)    :: n     !< Number of elements in data array.
      double precision, dimension(n), intent(inout) :: gsbuf !< Array with value(s) to be minimized.

#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror, i
      double precision, dimension(n) :: grbuf
c.......................................................................
c
      call mpi_allreduce( gsbuf,
     &                    grbuf,
     &                    n,
     &                    mpi_double_precision,
     &                    mpi_min,
     &                    pks7mpiwrpcomm_world,
     &                    ierror )

c      gsbuf = grbuf

      do i = 1, n
         gsbuf(i) = grbuf(i)
      end do

#else
      call pks7mpiwrperror( 'pks7mpiwrpallminr', 'invalid operation' )
c
#endif
      return
      end subroutine pks7mpiwrpallmind

      !> Communicates and maximizes real value(s) across all processes.
      !! To minimize a scalar value, simply use gsbuf with size 1.
      !! For arrays, each index is minimized separately (across processes).
      subroutine pks7mpiwrpallmaxr( gsbuf, n )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer           , intent(in)    :: n     !< Number of elements in data array.
      real, dimension(n), intent(inout) :: gsbuf !< Array with value(s) to be minimized.

#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror, i
      real, dimension(n) :: grbuf
c.......................................................................
c
      call mpi_allreduce( gsbuf,
     &                    grbuf,
     &                    n,
     &                    mpi_real,
     &                    mpi_max,
     &                    pks7mpiwrpcomm_world,
     &                    ierror )

      do i = 1, n
         gsbuf(i) = grbuf(i)
      end do
#else
      call pks7mpiwrperror( 'pks7mpiwrpallmaxr', 'invalid operation' )
c
#endif
      return
      end subroutine pks7mpiwrpallmaxr

      !> Communicates and maximizes integer value(s) across all processes.
      !! To minimize a scalar value, simply use gsbuf with size 1.
      !! For arrays, each index is minimized separately (across processes).
      subroutine pks7mpiwrpallmaxi( gsbuf, n )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer           , intent(in)       :: n     !< Number of elements in data array.
      integer, dimension(n), intent(inout) :: gsbuf !< Array with value(s) to be minimized.

#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror, i
      integer, dimension(n) :: grbuf
c.......................................................................
c
      call mpi_allreduce( gsbuf,
     &                    grbuf,
     &                    n,
     &                    mpi_integer,
     &                    mpi_max,
     &                    pks7mpiwrpcomm_world,
     &                    ierror )

c      gsbuf = grbuf

      do i = 1, n
         gsbuf(i) = grbuf(i)
      end do

#else
      call pks7mpiwrperror( 'pks7mpiwrpallmaxi', 'invalid operation' )
c
#endif
      return
      end subroutine pks7mpiwrpallmaxi

      !> Communicates and maximizes double value(s) across all processes.
      !! To minimize a scalar value, simply use gsbuf with size 1.
      !! For arrays, each index is minimized separately (across processes).
      subroutine pks7mpiwrpallmaxd( gsbuf, n )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer           , intent(in)    :: n     !< Number of elements in data array.
      double precision, dimension(n), intent(inout) :: gsbuf !< Array with value(s) to be minimized.

#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror, i
      double precision, dimension(n) :: gdbuf
c.......................................................................
c
      call mpi_allreduce( gsbuf,
     &                    gdbuf,
     &                    n,
     &                    mpi_double_precision,
     &                    mpi_max,
     &                    pks7mpiwrpcomm_world,
     &                    ierror )

      do i = 1, n
         gsbuf(i) = gdbuf(i)
      end do
#else
      call pks7mpiwrperror( 'pks7mpiwrpallmaxd', 'invalid operation' )
c
#endif
      return
      end subroutine pks7mpiwrpallmaxd

      !> Communicates and adds double value(s) across all processes.
      !! To add a scalar value, simply use gsbuf with size 1.
      !! For arrays, each index is minimized separately (across processes).
      subroutine pks7mpiwrpallsumr( gsbuf, grbuf, n )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer :: n                 !< Number of elements in data array.
      real, dimension(n) :: gsbuf, !< Array with value(s) to be minimized.
     &                      grbuf  !< Temporary receive buffer.

#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror, i
c.......................................................................
c
      call mpi_allreduce( gsbuf,
     &                    grbuf,
     &                    n,
     &                    mpi_real,
     &                    mpi_sum,
     &                    pks7mpiwrpcomm_world,
     &                    ierror )

c      gsbuf = grbuf

      do i = 1, n
         gsbuf(i) = grbuf(i)
      end do

#else
      call pks7mpiwrperror( 'pks7mpiwrpallsumr', 'invalid operation' )
c
#endif
      return
      end subroutine pks7mpiwrpallsumr

      !> Communicates and adds double value(s) across all processes.
      !! To add a scalar value, simply use gsbuf with size 1.
      !! For arrays, each index is minimized separately (across processes).
      subroutine pks7mpiwrpallsumd( gsbuf, grbuf, n )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer :: n                 !< Number of elements in data array.
      double precision, dimension(n) :: gsbuf, !< Array with value(s) to be minimized.
     &                                  grbuf  !< Temporary receive buffer.

#ifdef PKSMPI
c
c...     Local variables
      integer ::  ierror, i
c.......................................................................
c
      call mpi_allreduce( gsbuf,
     &                    grbuf,
     &                    n,
     &                    mpi_double_precision,
     &                    mpi_sum,
     &                    pks7mpiwrpcomm_world,
     &                    ierror )

c      gsbuf = grbuf

      do i = 1, n
         gsbuf(i) = grbuf(i)
      end do

#else
      call pks7mpiwrperror( 'pks7mpiwrpallsumd', 'invalid operation' )
c
#endif
      return
      end subroutine pks7mpiwrpallsumd

      !> Gathers integer values into specified memory
      !! locations from a group of processes and broadcasts the
      !! gathered data to the root process.
      subroutine pks7mpiwrpgathervi( gsbuf, gscnt, grbuf, grcnt,
     &                           offsets, root )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer ::  gsbuf(*), gscnt, grbuf(*), grcnt(*), offsets(*),
     &            root

c...     Local variables
      integer :: ierror, i, k

#ifdef PKSMPI
c.......................................................................
c
      call mpi_gatherv( gsbuf, gscnt, mpi_integer,
     &                  grbuf, grcnt, offsets, mpi_integer,
     &                  root, pks7mpiwrpcomm_world, ierror )
c
#else
c
      if ( grcnt(1) .lt. gscnt ) then
         call pks7mpiwrperror( 'mt_allgathervi',
     &                     'receive buffer too small' )
      end if
c
      k = offsets(1)
c
      do i = 1, gscnt
         grbuf(k + i) = gsbuf(i)
      enddo
#endif
c
      return
      end subroutine pks7mpiwrpgathervi

      !> Gathers real values into specified memory
      !! locations from a group of processes and broadcasts the
      !! gathered data to the root process.
      subroutine pks7mpiwrpgathervr( gsbuf, gscnt, grbuf, grcnt,
     &                           offsets, root )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer :: gscnt, grcnt(*), offsets(*), root
      real    :: gsbuf(*), grbuf(*)

c...     Local variables
      integer :: ierror, i, k

#ifdef PKSMPI
c.......................................................................
c
      call mpi_gatherv( gsbuf, gscnt, mpi_real,
     &                  grbuf, grcnt, offsets, mpi_real,
     &                  root, pks7mpiwrpcomm_world, ierror )
c
#else
c
      if ( grcnt(1) .lt. gscnt ) then
         call pks7mpiwrperror( 'mt_gathervr',
     &                     'receive buffer too small' )
      end if
c
      k = offsets(1)
c
      do i = 1, gscnt
         grbuf(k + i) = gsbuf(i)
      enddo
#endif
c
      return
      end subroutine pks7mpiwrpgathervr

      !> Gathers double values into specified memory
      !! locations from a group of processes and broadcasts the
      !! gathered data to all processes.
      subroutine pks7mpiwrpallgathervd( gsbuf, gscnt, grbuf, grcnt,
     &                                  offsets )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer :: gscnt, grcnt(*), offsets(*), root
      double precision :: gsbuf(*), grbuf(*)

c...     Local variables
      integer :: ierror, i, k

#ifdef PKSMPI
c.......................................................................
c
      call mpi_allgatherv( gsbuf, gscnt, mpi_double_precision,
     &                     grbuf, grcnt, offsets, mpi_double_precision,
     &                     pks7mpiwrpcomm_world, ierror )
c
#else
c
      if ( grcnt(1) .lt. gscnt ) then
         call pks7mpiwrperror( 'mt_gathervd',
     &                     'receive buffer too small' )
      end if
c
      k = offsets(1)
c
      do i = 1, gscnt
         grbuf(k + i) = gsbuf(i)
      enddo
#endif
c
      return
      end subroutine pks7mpiwrpallgathervd
     
      subroutine pks7mpiwrpreducersum( gsbuf, grbuf, gcnt, root )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer :: gcnt, root
      real    :: gsbuf(*), grbuf(*) 

c...     Local variables
      integer :: ierror, i, k

#ifdef PKSMPI
c.......................................................................
c
      call mpi_reduce( gsbuf, grbuf, gcnt,
     &                 mpi_real,
     &                 mpi_sum,
     &                 root,
     &                 pks7mpiwrpcomm_world,
     &                 ierror )
#else
      do i = 1, gcnt
         grbuf(i) = gsbuf(i)    
      end do
#endif
c
      return
      end subroutine pks7mpiwrpreducersum     
      
      subroutine pks7mpiwrpreducermax( gsbuf, grbuf, gcnt, root )

      use pksmpiwrp_mod

      implicit none

c...     Arguments
      integer :: gcnt, root
      real    :: gsbuf(*), grbuf(*) 

c...     Local variables
      integer :: ierror, i, k

#ifdef PKSMPI
c.......................................................................
c
      call mpi_reduce( gsbuf, grbuf, gcnt,
     &                 mpi_real,
     &                 mpi_max,
     &                 root,
     &                 pks7mpiwrpcomm_world,
     &                 ierror )
#else
      do i = 1, gcnt
         grbuf(i) = gsbuf(i)    
      end do
#endif
c
      return
      end subroutine pks7mpiwrpreducermax      
      
      