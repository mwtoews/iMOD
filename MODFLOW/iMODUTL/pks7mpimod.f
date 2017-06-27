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

      module pksmpitim_mod
      
      implicit none
      
        logical, parameter :: ltiming = .true.                          !JV
        integer, parameter :: itimmet1 = 1                              !JV
        integer, parameter :: itimmet2 = 2                              !JV
        integer, parameter :: itimmpil = 3                              !JV
        integer, parameter :: itimmpig = 4                              !JV
        integer, parameter :: itimar   = 5                              !JV
        integer, parameter :: itimrp   = 6                              !JV
        integer, parameter :: itimad   = 7                              !JV
        integer, parameter :: itimfm   = 8                              !JV
        integer, parameter :: itimsol  = 9                              !JV
        integer, parameter :: itimbd   = 10                             !JV
        integer, parameter :: itimot   = 11                             !JV
        integer, parameter :: itimda   = 12                             !JV
        integer, parameter :: itimtot  = 13                             !JV
        integer, parameter :: ntim = itimtot                            !JV
        real, save, dimension(ntim) :: timsec                           !JV
        character(len=20), dimension(ntim) :: timlabel                  !JV
c                    12345678901234567890                               !JV
        data timlabel/'METIS (AR)          ',                           !JV
     &                'Read partitions (AR)',                           !JV
     &                'MPI loc. comm. (SOL)',                           !JV
     &                'MPI glob.comm. (SOL)',                           !JV
     &                'Allocate and Read   ',                           !JV
     &                'Read and Prepare    ',                           !JV
     &                'ADvance             ',                           !JV
     &                'ForMulate           ',                           !JV
     &                'Solve               ',                           !JV
     &                'BuDget              ',                           !JV
     &                'Output              ',                           !JV
     &                'DeAllocate          ',                           !JV
     &                'Total               '/                           !JV
        integer, dimension(8) :: itbeg, itend                           !JV
      
      end module pksmpitim_mod
      
      module pksmpi_mod

      use pksmpitim_mod, only: timsec, itimmpil, itimmpig,
     &                         itimmet1, itimmet2

      implicit none

      logical, save :: mpiactive = .false.
      integer, save :: nlxchloosely = 0
c
c...     Parameters
      integer, parameter :: mppser    = 0,
     &                      mppini1   = 1,
     &                      mppini2   = 2,
     &                      mprun     = 3,
     &                      maxlcx    = 2,
c     &                      maxlcx    = 1,
     &                      maxlcix   = 0
      integer, parameter :: xpinw = 1,
     &                      xpin  = 2,
     &                      xpine = 3,
     &                      xpiw  = 4,
     &                      xpie  = 5,
     &                      xpisw = 6,
     &                      xpis  = 7,
     &                      xpise = 8,
     &                      xpsnd = 1,
     &                      xprec = 2
      integer, parameter :: inovl = 1,
     &                      iovl  = 2

      integer, parameter :: data_nmax = 500
      integer :: data_nfile
      integer           , dimension(data_nmax) :: data_iu
      integer           , dimension(data_nmax) :: data_iflen
      character(len=300), dimension(data_nmax) :: data_fname
      character(len=20) , dimension(data_nmax) :: data_fmtarg
      character(len=20) , dimension(data_nmax) :: data_accarg
      character(len=7)  , dimension(data_nmax) :: data_filstat
      character(len=20) , dimension(data_nmax) :: data_filact
      character(len=20) , dimension(data_nmax) :: data_filtyp

      integer, save :: partopt
      integer, save :: verbose
      integer, save :: nrprocmetis
      integer, save :: skipmetis
      integer, save :: mpiloosely
      integer, save :: metismethod
      integer, save :: readpartmetis
      character(len=300), save :: partmetisfile
      integer, parameter :: novlapadv    = 2
      integer, parameter :: stenadv      = 2
      logical, parameter :: commallovladv = .true.

c      integer, parameter :: novlapmax    = 1
c      integer, parameter :: novlapimpsol = 1
c      integer, parameter :: stenimpsol   = 2
      integer, save :: novlapmax
      integer, save :: novlapimpsol
      integer, save :: stenimpsol
      logical, parameter :: commallovlimp = .true.

      integer, save :: nrproc, myrank, myproc, mpptyp
      integer, save :: mergeobs,
     &                 ovlapopt, stenopt
      integer, save :: gncol = 0, gnrow = 0, gnlay = 0, gnobs,
     &                 nrproc_ncol, nrproc_nrow,
     &                 nrxp, licolmin, licolmax, lirowmin, lirowmax
      real, dimension(:), allocatable, save :: gdelr, gdelc
      integer, save :: nt
      real, save    :: dt1, dt2, dt3, dt4
      integer, save :: nbandnodes

      integer, save, dimension(:), allocatable :: nrproc_col,
     &                                            nrproc_row,
     &                                            nrproc_lay,
     &                                            bandnodes

      integer, save, dimension(:,:), allocatable :: proc_ncol,
     &                                              proc_nrow,
     &                                              proc_nodes,
     &                                              proc_icolmin,
     &                                              proc_icolmax,
     &                                              proc_irowmin,
     &                                              proc_irowmax,
     &                                              glocobs,
     &                                              topol,
     &                                              partitionsminmax
c
      real, save, dimension(:), allocatable :: proc_load, obscnew

      type :: commint
         integer :: xprnk, xpbuf
         type(commidx), pointer, dimension(:) :: pckidx => null()
         type(commidx), pointer, dimension(:) :: upkidx => null()
      end type commint

      type :: commidx
         integer :: nidx
         integer, pointer, dimension(:) :: idx => null()
         integer, pointer, dimension(:) :: gidx => null()
      end type commidx

      type(commint), pointer, dimension(:), save :: xp => null()

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c...     unstructured data structures
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      type partnodType
         integer :: partnr = 0, nja = 0, gnja = 0
         integer, dimension(:), pointer ::   ia=>null(),  ja=>null()
         integer, dimension(:), pointer ::  gia=>null(),  giac=>null(),
     1                                      gja=>null()
         type(nodType), pointer :: nodint=>null()
         integer :: novl = 0, nxp = 0
         integer, dimension(:), pointer :: xp=>null()
         type(nodType), dimension(:,:), pointer :: nodsnd=>null() ! for each exchange partner and overlap
         type(nodType), dimension(:,:), pointer :: nodrec=>null()
      end type partnodType
      type nodType
         integer :: n = 0
         integer, dimension(:), pointer :: lnod=>null(),
     1                                     gnod=>null(),
     2                                     ilay=>null()
      end type nodType
      type(partnodType), dimension(:), allocatable, save :: pnod
      type(nodType), pointer :: nodptr

      integer, save :: gnodes, gnjag, gnja, writegnodes
      integer, dimension(:), allocatable, save :: gnodlay,g2lnod,l2gnod
      integer, dimension(:), allocatable, save :: gia, giac, gja
      logical, save :: serreadglob = .false.
      logical, save :: njaflg = .false.

      integer, dimension(:), allocatable :: iwrk
      real, dimension(:), allocatable :: rwrk
      double precision, dimension(:), allocatable :: dwrk

      integer, dimension(8) :: itbeg, itend

      end module pksmpi_mod
