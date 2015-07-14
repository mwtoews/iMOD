c   Copyright (C) Stichting Deltares, 2005-2014.
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

      module lcdmodule

      use global, only: ncol, nrow

      implicit none

      logical, save :: lcdinit = .true.
      logical, save :: lqd
      real, save :: simcsize, xmin, ymin, xmax, ymax

      integer, dimension(:), allocatable, save :: genip
      integer, dimension(:,:), allocatable, save :: genpos

      end module lcdmodule

      module rdlcd_interface

      implicit none

      interface

      subroutine rdlcd(nlist,rlist,rlisttmp,lstbeg,ldim,mxlist,in,
     1                 iout,label,ncol,nrow,nlay)

      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
      real, dimension(:,:), pointer :: rlisttmp
      integer, intent(in) :: lstbeg
      integer, intent(in) :: in
      integer, intent(in) :: iout
      character(len=*), dimension(*), intent(in) :: label
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      integer, intent(in) :: nlay

      end subroutine

      end interface

      end module rdlcd_interface

      subroutine rdlcd(nlist,rlist,rlisttmp,lstbeg,ldim,mxlist,in,
     1                 iout,label,ncol,nrow,nlay)

c description:
c ------------------------------------------------------------------------------
c read data block of Line Column Data type.
c

c declaration section
c ------------------------------------------------------------------------------
      use imod_utl, only: imod_utl_openasc
      use gwfmetmodule, only: cdelr,cdelc
      use lcdmodule, only: genip, genpos

      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
      real, dimension(:,:), pointer :: rlisttmp
      integer, intent(in) :: lstbeg
      integer, intent(in) :: in
      integer, intent(in) :: iout
      character(len=*), dimension(*), intent(in) :: label
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      integer, intent(in) :: nlay

c local variables
      integer :: icol, irow, jcol, jrow, igen, iact, ngen, ii, kk, n, 
     1           ip1, ip2,ic1, ic2, ir1, ir2, j
      integer ::  il, is, ie, iline, jline, nline
      integer :: ilay
      real :: factor
      real, dimension(:,:), allocatable :: tmp
      integer(kind=1), dimension(:,:,:), allocatable :: ipc
      logical, dimension(:), allocatable :: writegen
      character(len=1024) :: fname
      integer :: lun
      character(len=1024) :: str

c parameters
      character(len=24) :: aname(1)
      data aname(1) /'                     LCD'/
      integer,parameter :: ineighbours=2
     
c program section
c ------------------------------------------------------------------------------
c init
      call initlcd()

c read number of hfb layers
      read(in,*) ngen

c allocate ipc
      allocate(tmp(ncol,nrow))
      
      !allocate(ipc(0:ncol+2,0:nrow+2,0:2))
      IF(ALLOCATED(IPC))DEALLOCATE(IPC); ALLOCATE(IPC(NCOL,NROW,2))

      allocate(writegen(max(1,ngen))); writegen = .false.

c count number of hfb and fill
      do iact = 1, 2
         if (iact == 2) then
            do while(.true.)
               backspace(in)
               read(in,'(a)') str
               str = adjustl(str)
               call upcase(str)
               if (str(1:3).eq.'LCD') then
                  exit
               else
                  backspace(in)
               end if
            end do
            read(in,*) ngen
         end if
         ii = lstbeg-1
         do igen = 1, ngen
            read(in,*) ilay, factor
            kk = ilay
            call u2drel(tmp,aname(1),
     1                  nrow,ncol,kk,in,iout) ! fill genpos list

            nline = size(genip)-1

            if (writegen(igen).and.iact.eq.2) then
               write(fname,'(a,i2.2,a,i2.2,a)')
     1            'hfb_',igen,'_l',ilay,'.gen'
               call imod_utl_openasc(lun,fname,'w')
               n = 0
            end if

            do j = 1, nline 
               
             ipc = int(0,1)
             is = genip(j-1)+1; ie = genip(j)

             !## line not in current model dimensions
             if(ie.eq.0)cycle
             if(is.gt.size(genpos,1).or.ie.gt.size(genpos,1))cycle
             
             jcol = genpos(is,1); jrow = genpos(is,2);

             !## startpoint
             JLINE=GENPOS(is,3)

             do il = is+1, ie
              writegen(igen) = .true.
!               icol = genpos(il,1)
!               irow = genpos(il,2)
!               jline = genpos(il,3)
!               if (iline.ne.jline) then
!                  write(*,*) 'ERROR, something went wrong in rdlcd'
!                  call ustop(' ')
!               end if          

              ILINE=GENPOS(il,3)
              IF(ILINE.EQ.JLINE)THEN
               IC1=GENPOS(il-1,1); IC2=GENPOS(il  ,1)
               IR1=GENPOS(il-1,2); IR2=GENPOS(il  ,2)
               IP1=GENPOS(il-1,4); IP2=GENPOS(il  ,4)
               CALL HFBGETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL)
              ENDIF
              
              IF(ILINE.NE.JLINE.OR.IL.EQ.IE)THEN
            
!               ipc(icol,irow,0) = int(1,1)
!               
!               if(icol.gt.jcol)then
!                  ipc(jcol,jrow,2)=int(1,1);     jcol=jcol+1
!               elseif(icol.lt.jcol)then
!                  ipc(jcol-1,jrow,2)=int(1,1);   jcol=jcol-1   
!               elseif(irow.gt.jrow)then
!                  ipc(jcol-1,jrow+1,1)=int(1,1); jrow=jrow+1
!               elseif(irow.lt.jrow)then
!                  ipc(jcol-1,jrow,1)=int(1,1);   jrow=jrow-1
!               endif
!            end do
            
               do irow = 1, nrow
                do icol = 1, ncol
                 !## place horizontal wall
                 if (irow.lt.nrow) then
                  if(ipc(icol,irow,2).eq.int(1,1)) then
                   ii = ii + 1
                   if (iact.eq.2) then
                    rlisttmp(1,ii) = ilay
                    rlisttmp(2,ii) = irow
                    rlisttmp(3,ii) = icol
                    rlisttmp(4,ii) = irow+1
                    rlisttmp(5,ii) = icol
                    rlisttmp(6,ii) = factor
                    rlisttmp(7,ii) = 0.0
                    if (writegen(igen)) then
                     n=n+1
                     write(lun,'(2i10)') n,igen
                     write(lun,'(2(f10.2,a1))') cdelr(icol-1),',',
     1 cdelc(irow)
                     write(lun,'(2(f10.2,a1))') cdelr(icol),',',
     1 cdelc(irow)
                     write(lun,'(a)') 'end'
                    end if
                   end if
                  end if
                 end if
                 !## place vertical wall
                 if (icol.lt.ncol) then
                  if(ipc(icol,irow,1).eq.int(1,1)) then
                   ii = ii + 1
                   if (iact.eq.2) then
                    rlisttmp(1,ii) = ilay
                    rlisttmp(2,ii) = irow
                    rlisttmp(3,ii) = icol
                    rlisttmp(4,ii) = irow
                    rlisttmp(5,ii) = icol+1
                    rlisttmp(6,ii) = factor
                    rlisttmp(7,ii) = 0.0
                    if (writegen(igen)) then
                     n=n+1
                     write(lun,'(2i10)') n,igen
                     write(lun,'(2(f10.2,a1))') cdelr(icol),',',
     1 cdelc(irow-1)
                     write(lun,'(2(f10.2,a1))') cdelr(icol),',',
     1 cdelc(irow)
                     write(lun,'(a)') 'end'
                    end if
                   end if
                  end if
                 end if
                end do ! icol
               end do ! irow

               !## reset for the next line   
               IPC=INT(0,1); JLINE=GENPOS(J,3)            

              endif  !if(iline.eq.jline)then
             end do ! do il = is, ie
            
            end do !  iline = 1, nline ! iline

            if (writegen(igen).and.iact.eq.2) close(lun)

         end do ! igen

         nlist = ii-lstbeg+1
         if (iact.eq.1) then
            if (.not.associated(rlisttmp)) then
               allocate(rlisttmp(ldim,2*nlist))
            end if
            if (nlist.gt.mxlist) then
               mxlist = 2*nlist
               deallocate(rlist)
               allocate(rlist(ldim,mxlist))
            end if
         end if ! iact = 1
      end do ! iact

c deallocate ipc
      if (allocated(ipc)) deallocate(ipc)
      if (allocated(tmp)) deallocate(tmp)
      if (allocated(writegen)) deallocate(writegen)

c end of program
      return
      end

      !###====================================================================
      SUBROUTINE HFBGETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL)
      !###====================================================================
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: IC1,IC2,IR1,IR2,IP1,IP2
      INTEGER,INTENT(IN) :: NROW,NCOL
      INTEGER(KIND=1),INTENT(INOUT),DIMENSION(NCOL,NROW,2) :: IPC
      INTEGER,DIMENSION(2) :: JPC,JPR,JC,JR,JP
      INTEGER :: I,IC,IR 

      JC(1)=IC1; JC(2)=IC2
      JR(1)=IR1; JR(2)=IR2
      JP(1)=IP1; JP(2)=IP2

      DO I=1,2
       IF(JP(I).EQ.2.OR.JP(I).EQ.3)JPC(I)=JC(I)
       IF(JP(I).EQ.1.OR.JP(I).EQ.4)JPC(I)=JC(I)-1
       IF(JP(I).EQ.1.OR.JP(I).EQ.2)JPR(I)=JR(I)-1
       IF(JP(I).EQ.3.OR.JP(I).EQ.4)JPR(I)=JR(I)
      ENDDO

      !## do nothing, is similar point
      IF(JPR(1).EQ.JPR(2).AND.JPC(1).EQ.JPC(2))RETURN 

      !## do nothing whenever jpc.eq.0 or jpr.eq.0
      IF(JPC(1).EQ.0.OR.JPC(2).EQ.0)RETURN
      IF(JPR(1).EQ.0.OR.JPR(2).EQ.0)RETURN

      !## horizontal fault ipc(,,1)=1
      IF(JPR(1).EQ.JPR(2).AND.JPC(1).NE.JPC(2))THEN
       IC=MAX(JPC(1),JPC(2)); IR=JPR(1); IPC(IC,IR,2)=INT(1,1)
      ENDIF
      !## vertical fault ipc(,,2)=1
      IF(JPC(1).EQ.JPC(2).AND.JPR(1).NE.JPR(2))THEN
       IC=JPC(1); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1)
      ENDIF
      !## diagonal, add two faults
      IF(JPR(1).NE.JPR(2).AND.JPC(1).NE.JPC(2))THEN
       !## goto to the west
       IF(JPC(1).GT.JPC(2))THEN
        !## goto to the north-west
        IF(JPR(1).GT.JPR(2))THEN
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal
        !## goto to the south-west
        ELSE
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal
        ENDIF
       !## goto to the east
       ELSE
        !## goto to the north-east
        IF(JPR(1).GT.JPR(2))THEN
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal   
        !## goto to the south-east
        ELSE
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal  
        ENDIF
       ENDIF
      ENDIF

      END SUBROUTINE HFBGETFACES

      subroutine initlcd()
c description:
c ------------------------------------------------------------------------------
c read file of LCD type
c

c declaration section
c ------------------------------------------------------------------------------
      use lcdmodule
      use global, only: iunit, delc, delr
      use gwfmetmodule
      use m_mf2005_iu, only: iumet

c arguments

c local variables
      integer :: icol, irow

c parameters

c program section
c ------------------------------------------------------------------------------
      if (lcdinit) then
         lcdinit = .false.
      else
         return
      end if

c check if metadata package is activated
      if (IUNIT(IUMET).le.0) then
         write(*,*) 'Error: lcd file, please activate met package.'
         call ustop(' ')
      end if
      if (.not.associated(coord_xll) .or.
     &    .not.associated(coord_yll) ) then
         write(*,*) 'Error: lcd file can only be used with xll and yll'
         call ustop(' ')
      end if

c check if grid is uniform
      lqd = .true.
      if ((maxval(delr).ne.minval(delr)).or.
     &    (maxval(delc).ne.minval(delc))) lqd = .false.
      if (lqd) then
         simcsize = delr(1)
      else
         write(*,*) 'Error: non-uniform grids not yet supported.'
         call ustop(' ')
      end if

      if (lqd) then
         xmin=cdelr(0)
         ymax=cdelc(0)
         xmax=xmin+(simcsize*ncol)
         ymin=ymax-(simcsize*nrow)
      endif

c end of program
      return
      end