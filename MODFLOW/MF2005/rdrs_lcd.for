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

      module lcdmodule


      implicit none

      logical, save :: lcdinit = .true.
      logical, save :: lqd
      real(kind=8), save :: simcsize, xmin, ymin, xmax, ymax

      integer, dimension(:), allocatable, save :: genip
      integer, dimension(:,:), allocatable, save :: genpos

      real(kind=8), dimension(:), allocatable, save :: lcdelr
      real(kind=8), dimension(:), allocatable, save :: lcdelc   
      
      integer, save :: lncol, lnrow
      
      end module lcdmodule

      module rdlcd_interface

      implicit none

      interface

      subroutine rdlcd(nlist,rlist,lstbeg,ldim,mxlist,in,
     1                 iout,label,ncol,nrow,nlay)

      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
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

      subroutine rdlcd(nlist,rlist,lstbeg,ldim,mxlist,in,
     1                 iout,label,ncol,nrow,nlay)

c description:
c ------------------------------------------------------------------------------
c read data block of Line Column Data type.
c

c declaration section
c ------------------------------------------------------------------------------
      use imod_utl, only: imod_utl_openasc
      use lcdmodule, only: genip, genpos, lncol, lnrow, lcdelr,lcdelc
      USE GLOBAL,      ONLY: IUNIT, BOTM,LBOTM
      USE M_MF2005_IU, ONLY : IULPF
      
      implicit none

c arguments
      integer, intent(inout) :: nlist
      integer, intent(in) :: ldim
      integer, intent(inout) :: mxlist
      real, dimension(:,:), pointer :: rlist
      integer, intent(in) :: lstbeg
      integer, intent(in) :: in
      integer, intent(in) :: iout
      character(len=*), dimension(*), intent(in) :: label
      integer, intent(in) :: ncol
      integer, intent(in) :: nrow
      integer, intent(in) :: nlay

c local variables
      real,parameter :: thickness=0.5
      real, dimension(:,:), allocatable :: rlisttmp                     ! DLT
      integer :: icol, irow, jcol, jrow, igen, iact, ngen, ii, kk,
     1           ip1, ip2,ic1, ic2, ir1, ir2, j, jj 
      integer ::  i,il, is, ie, iline, jline, nline
      integer :: ilay,il1,il2,jlay,isys
      real :: fct,HFB1EXPORT_GETDZ,c,z,c1,c2,zz,TFV,bfv,tpv,btv
      real, dimension(:,:), allocatable :: tmp,res,fdz,tf,bf
      integer(kind=1),allocatable,dimension(:,:) :: sys
      integer(kind=1), dimension(:,:,:), allocatable :: ipc
      integer,dimension(4) :: ICOUT,IROUT
      logical, dimension(:), allocatable :: writegen
      character(len=1024) :: fname
      integer,dimension(:),allocatable :: lun,dun,nlun
      character(len=1024) :: str
      integer :: iflen, myrank                                          ! PKS

c parameters
      character(len=24) :: aname(1)
      data aname(1) /'                     LCD'/
      integer,parameter :: ineighbours=2
      REAL :: ZF,NODATA
      LOGICAL :: LINV,ltb

c program section
c ------------------------------------------------------------------------------
c init
      call initlcd()

      NODATA=HUGE(1.0)

      ltb=.false.; if(iunit(iulpf).gt.0)ltb=.true.
      
c read number of hfb layers
      read(in,*) ngen

c allocate ipc
      allocate(tmp(lncol,lnrow))
      
      !allocate(ipc(0:ncol+2,0:nrow+2,0:2))
      IF(ALLOCATED(IPC))DEALLOCATE(IPC); ALLOCATE(IPC(LNCOL,LNROW,2))
      IF(ALLOCATED(TF))DEALLOCATE(TF); IF(ALLOCATED(BF))DEALLOCATE(BF)
      ALLOCATE(TF(LNCOL,LNROW),BF(LNCOL,LNROW))

      allocate(writegen(max(1,ngen))); writegen = .false.
      allocate(lun(nlay),dun(nlay),nlun(nlay)); lun=0; dun=0; nlun=0

      do ilay=1,nlay
       write(fname,'(a,i2.2)') 'hfb_l',ilay
       iflen = len_trim(fname)
       call pks7mpifname(fname,iflen)   
       fname = trim(fname)//'.gen'   
       call imod_utl_openasc(lun(ilay),fname,'w')
       write(fname,'(a,i2.2)') 'hfb_l',ilay
       iflen = len_trim(fname)
       call pks7mpifname(fname,iflen)   
       fname = trim(fname)//'.dat'   
       call imod_utl_openasc(dun(ilay),fname,'w')
       if(ltb)then
        WRITE(dun(ilay),'(A)') 'no,confined_resis,unconfined_resis,fract
     1ion,system'
       else
        WRITE(dun(ilay),'(A)') 'no,fraction,system'
       endif
      enddo
      
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
         !## process each genfile
         do igen = 1, ngen
            read(in,*) ilay, fct
            kk = ilay
            call u2drel(tmp,aname(1),
     1                  lnrow,lncol,kk,in,iout) ! fill genpos list

            NLINE = SIZE(GENIP)-1

            IPC = INT(0,1)
            IF(ILAY.EQ.0)THEN; TF=NODATA; BF=NODATA; ENDIF

            DO J = 1, NLINE 
             
             IS = GENIP(J-1)+1; IE = GENIP(J)

             !## line not in current model dimensions
             IF(IE.EQ.0)CYCLE
             IF(IS.GT.SIZE(GENPOS,1).OR.IE.GT.SIZE(GENPOS,1))CYCLE
             
              !## process line
              DO IL = IS+1, IE
               JLINE=GENPOS(IL-1,3)
               ILINE=GENPOS(il,3)
               !## similar line
               IF(ILINE.NE.JLINE)CYCLE

               IC1=GENPOS(IL-1,1); IC2=GENPOS(IL  ,1)
               IR1=GENPOS(IL-1,2); IR2=GENPOS(IL  ,2)
               IP1=GENPOS(IL-1,4); IP2=GENPOS(IL  ,4)

               CALL HFBGETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,LNROW,LNCOL,
     &ICOUT,IROUT)

               !## get top/bottom elevations on grid - only for display purposes
               IF(ILAY.EQ.0)THEN
                !## fill in z-coordinates
                ZF=(GENPOS(IL-1,5)+GENPOS(IL,5))/2.0
                DO I=1,4
                 ICOL=ICOUT(I); IROW=IROUT(I)
                 IF(ICOL.LE.0.OR.IROW.LE.0)CYCLE
                 IF(TF(ICOL,IROW).EQ.NODATA)THEN
                  TF(ICOL,IROW)=ZF/100.0; BF(ICOL,IROW)=ZF/100.0
                 ELSE
                  !## take maximal/minimal elevations
                  TF(ICOL,IROW)=MAX(TF(ICOL,IROW),ZF/100.0)
                  BF(ICOL,IROW)=MIN(BF(ICOL,IROW),ZF/100.0)
                 ENDIF
                ENDDO

               ENDIF
              ENDDO

            ENDDO

            !## determine what layer(s)
            IF(ilay.EQ.0)THEN
             IL1=1; IL2=NLAY
            ELSE
             IL1=ilay; IL2=IL1
            ENDIF

            do irow = 1, lnrow
             do icol = 1, lncol
              !## place horizontal wall
              if (irow.lt.lnrow) then
               if(ipc(icol,irow,2).eq.int(1,1)) then
                !## x-direction
                DO jLAY=IL1,IL2
                 Z=-1.0
                 IF(ilay.EQ.0)THEN   
                  Z=HFB1EXPORT_GETDZ(TF,BF,ICOL,IROW,ICOL,IROW+1,NODA
     1TA,jLAY,LNCOL,LNROW,TPV,BTV,TFV,BFV)                  
                 ELSE
                  TPV=0.0
                  BTV=0.0
                  TFV=0.0
                  BFV=0.0
                 ENDIF

                 !## skip fault on side of model or less than 0.0 fraction
                 IF(Z.LE.0.0.AND.ILAY.EQ.0)CYCLE

                 ii = ii + 1
                 !## store into memory second cycli
                 if (iact.eq.2) then

                  rlisttmp(1 ,ii) = jlay
                  rlisttmp(2 ,ii) = irow
                  rlisttmp(3 ,ii) = icol
                  rlisttmp(4 ,ii) = irow+1
                  rlisttmp(5 ,ii) = icol                  
                  rlisttmp(6 ,ii) = FCT
                  rlisttmp(7 ,ii) = Z
                  !## store system number
                  rlisttmp(8 ,ii) = igen
                  rlisttmp(9 ,ii) = tpv
                  rlisttmp(10,ii) = btv
                  rlisttmp(11,ii) = tfv
                  rlisttmp(12,ii) = bfv
                    
                 endif
                enddo
               end if
              end if
                 
              !## place vertical wall
              if (icol.lt.lncol) then
               if(ipc(icol,irow,1).eq.int(1,1)) then

                !## y-direction
                DO jLAY=IL1,IL2
 
                 Z=-1.0
                 IF(ilay.EQ.0)THEN  
                  Z=HFB1EXPORT_GETDZ(TF,BF,ICOL,IROW,ICOL+1,IROW,NODA
     1TA,jLAY,LNCOL,LNROW,TPV,BTV,TFV,BFV)                  
                 ELSE
                  TPV=0.0
                  BTV=0.0
                  TFV=0.0
                  BFV=0.0
                 ENDIF

                 !## skip fault on side of model or less than 0.0 fraction
                 IF(Z.LE.0.0.AND.ILAY.EQ.0)CYCLE

                 ii = ii + 1
                 if (iact.eq.2) then
                  rlisttmp(1 ,ii) = jlay
                  rlisttmp(2 ,ii) = irow
                  rlisttmp(3 ,ii) = icol
                  rlisttmp(4 ,ii) = irow
                  rlisttmp(5 ,ii) = icol+1
                  rlisttmp(6 ,ii) = FCT
                  rlisttmp(7 ,ii) = Z
                  !## store system number
                  rlisttmp(8 ,ii) = igen
                  rlisttmp(9 ,ii) = tpv
                  rlisttmp(10,ii) = btv
                  rlisttmp(11,ii) = tfv
                  rlisttmp(12,ii) = bfv
                     
                 end if
                enddo
               end if
              end if
             end do ! icol
            end do ! irow

         end do ! igen

         nlist = ii-lstbeg+1
         if (iact.eq.1) then
            if (.not.allocated(rlisttmp)) then
               !## allocate addition column to store system number and dimensions of fault
               allocate(rlisttmp(ldim+5,nlist))
            end if
         end if ! iact = 1
      end do ! iact

!    construct final fault list per model layer
      allocate(fdz(lncol,lnrow),res(lncol,lnrow))
      allocate(sys(lncol,lnrow))

      do iact=1,2      
       
       !## number of adjusted hfb-elements
       jj=0
       
       !## process each layer
       DO ILAY=1,NLAY
  
        IPC=INT(0,1)
        RES=0.0
        FDZ=0.0
        SYS=INT(0,1)
        LINV=.FALSE.
        TF=-10.0E10
        BF= 10.0E10
 
        DO ii=1,nlist
         jlay=int(rlisttmp(1 ,ii))
         !# not current modellayer
         if(jlay.ne.ilay)cycle
         ir1= int(rlisttmp(2 ,ii))
         ic1= int(rlisttmp(3 ,ii))
         ir2= int(rlisttmp(4 ,ii))
         ic2= int(rlisttmp(5 ,ii))
         c=       rlisttmp(6 ,ii)
         z=       rlisttmp(7 ,ii)
         isys=int(rlisttmp(8 ,ii))
         tpv=     rlisttmp(9 ,ii)
         btv=     rlisttmp(10,ii)
         tfv=     rlisttmp(11,ii)
         bfv=     rlisttmp(12,ii)
         
         !## skip c.lt.zero
         IF(C.LT.0.0)CYCLE

         IF(IC1.EQ.IC2)THEN
          IPC(IC1,IR1,2)=INT(1,1)
         ELSE
          IPC(IC1,IR1,1)=INT(1,1)
         ENDIF
      
         IF(Z.GT.0.0)LINV=.TRUE.
         
         !## still some space left in modellayer for an additional fault
         IF(Z.LT.0.0.OR.FDZ(IC1,IR1).LT.1.0)THEN
          !## available space
          ZZ=1.0-FDZ(IC1,IR1)
          !## net available space
          ZZ=MIN(ZZ,Z)
          !## confined system
          IF(Z.LT.0.0)ZZ=1.0
          !## take system number of largest contribution to c
          IF(RES(IC1,IR1).GT.0.0)THEN
           IF(Z.GT.0.0)THEN
            C2=1.0/RES(IC1,IR1)*FDZ(IC1,IR1)
            IF(C.GT.C2)SYS(IC1,IR1)=INT(ISYS,1)
           ELSE
            IF(C.GT.RES(IC1,IR1))SYS(IC1,IR1)=INT(ISYS,1)
           ENDIF
          ELSE
           SYS(IC1,IR1)=INT(ISYS,1)
          ENDIF
          !## resistance, sum conductances - ignore resistance of zero days
          IF(Z.GT.0.0)THEN
           IF(TPV-BTV.LE.THICKNESS)THEN
            C1=0.0
            IF(RES(IC1,IR1).GT.0.0)C1=1.0/RES(IC1,IR1)*FDZ(IC1,IR2)
            C2=C*ZZ
            RES(IC1,IR1)=1.0/((C1+C2)/(ZZ+FDZ(IC1,IR2)))
           !## add large fault using harmonic mean
           ELSE
            RES(IC1,IR1)=RES(IC1,IR1)+(1.0/C)*ZZ
           ENDIF
          ELSE
           !## get largest resistance
           RES(IC1,IR1)=MAX(RES(IC1,IR1),C)
          ENDIF
          !## occupation fraction
          FDZ(IC1,IR1)=MIN(1.0,FDZ(IC1,IR1)+ABS(Z))
         
          !## maximum top fault for display
          TF(IC1,IR1)=MAX(TF(IC1,IR1),TF(IC2,IR2),TFV)
          !## minimum bot fault for display
          BF(IC1,IR1)=MIN(BF(IC1,IR1),BF(IC2,IR2),BFV)
        
         ENDIF
         
        ENDDO
      
        DO IROW=1,LNROW; DO ICOL=1,LNCOL

         !## place vertical wall (block in y-direction)
         IF(IPC(ICOL,IROW,1).EQ.INT(1,1))THEN
          IF(ICOL.LT.LNCOL)THEN

           !## transform conductances to resistance
           IF(LINV)THEN
            C1=1.0/RES(ICOL,IROW)*FDZ(ICOL,IROW)
           ELSE
            C1=RES(ICOL,IROW)
           ENDIF
        
           !## get total resistance related to thickness of model layer
           IF(FDZ(ICOL,IROW).LT.1.0)THEN
            !## take harmonic mean in case of unsaturated thickness of fault 
            C2=1.0/((1.0/C1*FDZ(ICOL,IROW))+(1.0-FDZ(ICOL,IROW)))
           ELSE
            C2=C1
           ENDIF
           
           !## get systemnumber
           ISYS=SYS(ICOL,IROW)
           !## top fault for display purposes
           TFV=TF(ICOL,IROW)
           !## bottom fault for display purposes
           BFV=BF(ICOL,IROW)
           
           jj=jj+1
           if(iact.eq.2)then

            !## write fault to gen- and datfile
            call HFB1EXPORT_WRITEGEN(2,lun(ilay),dun(ilay),nlun(ilay),ic
     1ol,irow,C1,C2,FDZ(ICOL,IROW),ISYS,tfv,bfv,ltb)

            rlist(1,jj)=ilay
            rlist(2,jj)=irow
            rlist(3,jj)=icol
            rlist(4,jj)=irow
            rlist(5,jj)=icol+1
            rlist(6,jj)=c2
            !## system number
            rlist(7,jj)=isys 
           endif
           
          ENDIF 
         ENDIF

         !## place horizontal wall (block in x-direction)
         IF(IPC(ICOL,IROW,2).EQ.INT(1,1))THEN
          IF(IROW.LT.LNROW)THEN

           !## transform conductances to resistance
           IF(LINV)THEN
            C1=1.0/RES(ICOL,IROW)*FDZ(ICOL,IROW)
           ELSE
            C1=RES(ICOL,IROW)
           ENDIF
        
           !## get total resistance related to thickness of model layer
           IF(FDZ(ICOL,IROW).LT.1.0)THEN
            !## take harmonic mean in case of unsaturated thickness of fault 
            C2=1.0/((1.0/C1*FDZ(ICOL,IROW))+(1.0-FDZ(ICOL,IROW)))
!            C2=C1*FDZ(ICOL,IROW)**4.0
           ELSE
            C2=C1
           ENDIF

!           !## get total resistance related to thickness of model layer
!           C2=C1*FDZ(ICOL,IROW)**4.0
        
           !## get systemnumber
           ISYS=SYS(ICOL,IROW)
           !## top fault for display purposes
           TFV=TF(ICOL,IROW)
           !## bottom fault for display purposes
           BFV=BF(ICOL,IROW)

           jj=jj+1
           if(iact.eq.2)then

            !## write fault to gen- and datfile
            call HFB1EXPORT_WRITEGEN(1,lun(ilay),dun(ilay),nlun(ilay),ic
     1ol,irow,C1,C2,FDZ(ICOL,IROW),ISYS,tfv,bfv,ltb)

            rlist(1,jj)=ilay
            rlist(2,jj)=irow
            rlist(3,jj)=icol
            rlist(4,jj)=irow+1
            rlist(5,jj)=icol
            rlist(6,jj)=c2
            !## system number
            rlist(7,jj)=isys
           endif
           
          ENDIF
         ENDIF
     
        ENDDO; ENDDO
       ENDDO
      
       if(iact.eq.1)then
        mxlist=jj
        deallocate(rlist); allocate(rlist(ldim,mxlist))
       endif
       
      enddo

      !## get final number of elements
      nlist=mxlist

      call pks7mpigetmyrank(myrank)                                     ! PKS
      if (myrank.ne.0) then                                             ! PKS
       !## close files
       do ilay=1,nlay
        if(nlun(ilay).ne.0)then
         write(lun(ilay),'(a)') 'end'
        endif
        close(lun(ilay))
        close(dun(ilay))
       enddo
      end if                                                            ! PKS
      call pks7mpibarrier()                                             ! PKS
      if (myrank.eq.0) then                                             ! PKS
       !## close files
       do ilay=1,nlay
        if(nlun(ilay).eq.0)then
         close(lun(ilay),status='delete')
         close(dun(ilay),status='delete')
        else
         write(lun(ilay),'(a)') 'end'
         close(lun(ilay))
         close(dun(ilay))
        endif
       enddo
      end if                                                            ! PKS
c deallocate arrays
      if (allocated(rlisttmp)) deallocate(rlisttmp)
      if (allocated(ipc)) deallocate(ipc)
      if (allocated(tmp)) deallocate(tmp)
      IF(ALLOCATED(TF))DEALLOCATE(TF); IF(ALLOCATED(BF))DEALLOCATE(BF)
      deallocate(lun,dun,nlun)
      deallocate(res,fdz,sys)

c end of program
      return
      end

      !###====================================================================
      subroutine HFB1EXPORT_WRITEGEN(it,iu,ju,n,icol,irow,C,RES,FDZ,
     1ISYS,tfv,bfv,ltb)
      !###====================================================================
      use lcdmodule, only: lcdelr,lcdelc
      implicit none
      real,intent(in) :: C,RES,FDZ,tfv,bfv
      integer,intent(IN) :: IT,iu,ju,icol,irow,isys
      integer,intent(INOUT) :: n
      logical,intent(in) :: ltb
      real :: t1,b1
      
      if(it.eq.1)THEN
       n=n+1
       if(ltb)then
        write(ju,'(i10,3(1x,e15.7),i10)') n,c,res,fdz,isys
       else
        write(ju,'(i10,1x  ,e15.7 ,i10)') n,c,isys
       endif
       if(ltb.and.tfv.gt.bfv)then
        t1=tfv
        b1=bfv
        write(iu,'(i10)') n
        write(iu,'(3(f15.3,a1))') lcdelr(icol-1),',',lcdelc(irow),',',t1
        write(iu,'(3(f15.3,a1))') lcdelr(icol  ),',',lcdelc(irow),',',t1
        write(iu,'(3(f15.3,a1))') lcdelr(icol  ),',',lcdelc(irow),',',b1
        write(iu,'(3(f15.3,a1))') lcdelr(icol-1),',',lcdelc(irow),',',b1
        write(iu,'(3(f15.3,a1))') lcdelr(icol-1),',',lcdelc(irow),',',t1
        write(iu,'(a)') 'end'       
       else
        write(iu,'(i10)') n
        write(iu,'(2(f15.3,a1))') lcdelr(icol-1),',',lcdelc(irow)
        write(iu,'(2(f15.3,a1))') lcdelr(icol)  ,',',lcdelc(irow)
        write(iu,'(a)') 'end'
       endif
      endif

      if(it.eq.2)then
       n=n+1
       if(ltb)then
        write(ju,'(i10,3(1x,e15.7),i10)') n,c,res,fdz,isys
       else
        write(ju,'(i10,1x  ,e15.7 ,i10)') n,c,isys
       endif
       if(ltb.and.tfv.gt.bfv)then
        t1=tfv
        b1=bfv
        write(iu,'(i10)') n
        write(iu,'(3(f15.3,a1))') lcdelr(icol),',',lcdelc(irow-1),',',t1
        write(iu,'(3(f15.3,a1))') lcdelr(icol),',',lcdelc(irow)  ,',',t1
        write(iu,'(3(f15.3,a1))') lcdelr(icol),',',lcdelc(irow)  ,',',b1
        write(iu,'(3(f15.3,a1))') lcdelr(icol),',',lcdelc(irow-1),',',b1
        write(iu,'(3(g15.7,a1))') lcdelr(icol),',',lcdelc(irow-1),',',t1
        write(iu,'(a)') 'end'       
       else
        write(iu,'(i10)') n
        write(iu,'(2(f15.3,a1))') lcdelr(icol),',',lcdelc(irow-1)
        write(iu,'(2(f15.3,a1))') lcdelr(icol),',',lcdelc(irow  )
        write(iu,'(a)') 'end'
       endif
      end if

      end subroutine HFB1EXPORT_WRITEGEN

      !###====================================================================
      REAL FUNCTION HFB1EXPORT_GETDZ(TF,BF,IC1,IR1,IC2
     1,IR2,NODATA,ILAY,NCOL,NROW,TPV,BTV,TFV,BFV)
      !###====================================================================
      USE GLOBAL,ONLY : BOTM,lbotm,ibound
      IMPLICIT NONE     
      INTEGER,INTENT(IN) :: IC1,IR1,IC2,IR2,ILAY,NCOL,NROW
      real,intent(out) :: TPV,BTV,TFV,BFV
      REAL,INTENT(IN) :: NODATA
      REAL,INTENT(IN),DIMENSION(NCOL,NROW) :: TF,BF
      REAL :: DZ,C1,C2,CT,FFCT
      INTEGER :: IL1,IL2
      integer :: jc1,jr1,jc2,jr2,k                                      ! PKS
      logical :: lused1, lused2                                         ! PKS
      
      HFB1EXPORT_GETDZ=0.0

      !## determine values
      IF(TF(IC1,IR1).NE.NODATA.AND.TF(IC2,IR2).NE.NODATA)THEN
       TFV=(TF(IC1,IR1)+TF(IC2,IR2))/2.0
      ELSEIF(TF(IC1,IR1).NE.NODATA)THEN
       TFV=TF(IC1,IR1)
      ELSE
       TFV=TF(IC2,IR2)
      ENDIF
      IF(BF(IC1,IR1).NE.NODATA.AND.BF(IC2,IR2).NE.NODATA)THEN
       BFV=(BF(IC1,IR1)+BF(IC2,IR2))/2.0
      ELSEIF(BF(IC1,IR1).NE.NODATA)THEN
       BFV=BF(IC1,IR1)
      ELSE
       BFV=BF(IC2,IR2)
      ENDIF

      jc1 = ic1; jr1 = ir1; jc2 = ic2; jr2 = ir2; k = ilay              ! PKS
      call pks7mpitrn(jc1,jr1,k,lused1)                                 ! PKS
      call pks7mpitrn(jc2,jr2,k,lused2)                                 ! PKS
      if(.not.lused1.or..not.lused2) return                             ! PKS
      
      !## get internal layer number of vector of top/bottom information
      IL1=(ILAY*2)-1
      IL2=(ILAY*2)
      !## array starts at 0
      IL1=IL1-1
      IL2=IL2-1
      
!      il1=lbotm(ilay)-1
!      il2=lbotm(ilay)

      !## skip faults that passes inactive cells
      IF(IBOUND(JC1,JR1,ILAY).EQ.0.OR.IBOUND(JC2,JR2,ILAY).EQ.0)RETURN
      
      TPV=(BOTM(JC1,JR1,IL1)+BOTM(JC2,JR2,IL1))/2.0
      BTV=(BOTM(JC1,JR1,IL2)+BOTM(JC2,JR2,IL2))/2.0

      !## nett appearance of fault in modellayer
      DZ=MIN(TFV,TPV)-MAX(BFV,BTV)

      !## not in current modellayer
      IF(DZ.LT.0.0)RETURN

      IF(TPV-BTV.GT.0.0)THEN
       !## fraction of fault in modellayer
       DZ=DZ/(TPV-BTV)
      ELSE
       !## completely filled in model layer with thickness of zero
       DZ=1.0
      ENDIF
          
      HFB1EXPORT_GETDZ=DZ

      END FUNCTION HFB1EXPORT_GETDZ

!      !###====================================================================
!      REAL FUNCTION HFB1EXPORT_GETFACTOR(FCT,TF,BF,IC1,IR1,IC2,IR2,NODAT
!     1A,ILAY,NCOL,NROW)
!      !###====================================================================
!      USE GLOBAL,ONLY : BOTM
!      IMPLICIT NONE     
!      INTEGER,INTENT(IN) :: IC1,IR1,IC2,IR2,ILAY,NCOL,NROW
!      REAL,INTENT(IN) :: FCT,NODATA
!      REAL,INTENT(IN),DIMENSION(NCOL,NROW) :: TF,BF
!      REAL :: DZ,TFV,BFV,TPV,BTV,C1,C2,CT,FFCT
!      INTEGER :: IL1,IL2
!      
!      HFB1EXPORT_GETFACTOR=0.0
!
!      !## determine values
!      IF(TF(IC1,IR1).NE.NODATA.AND.TF(IC2,IR2).NE.NODATA)THEN
!       TFV=(TF(IC1,IR1)+TF(IC2,IR2))/2.0
!      ELSEIF(TF(IC1,IR1).NE.NODATA)THEN
!       TFV=TF(IC1,IR1)
!      ELSE
!       TFV=TF(IC2,IR2)
!      ENDIF
!      IF(BF(IC1,IR1).NE.NODATA.AND.BF(IC2,IR2).NE.NODATA)THEN
!       BFV=(BF(IC1,IR1)+BF(IC2,IR2))/2.0
!      ELSEIF(BF(IC1,IR1).NE.NODATA)THEN
!       BFV=BF(IC1,IR1)
!      ELSE
!       BFV=BF(IC2,IR2)
!      ENDIF
!
!      !## get internal layer number of vector of top/bottom information
!      IL1=(ILAY*2)-1
!      IL2=(ILAY*2)
!      !## array starts at 0
!      IL1=IL1-1
!      IL2=IL2-1
!      
!      TPV=(BOTM(IC1,IR1,IL1)+BOTM(IC2,IR2,IL1))/2.0
!      BTV=(BOTM(IC1,IR1,IL2)+BOTM(IC2,IR2,IL2))/2.0
!
!      !## nett appearance of fault in modellayer
!      DZ=MIN(TFV,TPV)-MAX(BFV,BTV)
!
!      !## not in current modellayer
!      IF(DZ.LT.0.0)RETURN
!
!      IF(TPV-BTV.GT.0.0)THEN
!       !## fraction of fault in modellayer
!       DZ=DZ/(TPV-BTV)
!      ENDIF
!      
!      !## if dz.eq.0, modellayer has thickness of zero, but fault to be retained
!      IF(DZ.EQ.0.0)DZ=1.0
!      
!      !## resistance of fault
!      FFCT=FCT; IF(FCT.EQ.0.0)FFCT=10.0E10
!
!      !## factor declines quadratically with layer occupation
!      HFB1EXPORT_GETFACTOR=FFCT*DZ**4.0
!
!      END FUNCTION HFB1EXPORT_GETFACTOR

      !###====================================================================
      SUBROUTINE HFBGETFACES(IC1,IC2,IR1,IR2,IP1,IP2,IPC,NROW,NCOL,
     &ICOUT,IROUT)
      !###====================================================================
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: IC1,IC2,IR1,IR2,IP1,IP2
      INTEGER,INTENT(IN) :: NROW,NCOL
      INTEGER,INTENT(OUT),DIMENSION(4) :: ICOUT,IROUT
      INTEGER(KIND=1),INTENT(INOUT),DIMENSION(NCOL,NROW,2) :: IPC
      INTEGER,DIMENSION(2) :: JPC,JPR,JC,JR,JP
      INTEGER :: I,IC,IR 

      !## cells capture faults
      ICOUT=0; IROUT=0
      
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
       ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
      ENDIF
      !## vertical fault ipc(,,2)=1
      IF(JPC(1).EQ.JPC(2).AND.JPR(1).NE.JPR(2))THEN
       IC=JPC(1); IR=MAX(JPR(1),JPR(2)); IPC(IC,IR,1)=INT(1,1)
       ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
      ENDIF
      !## diagonal, add two faults
      IF(JPR(1).NE.JPR(2).AND.JPC(1).NE.JPC(2))THEN
       !## goto to the west
       IF(JPC(1).GT.JPC(2))THEN
        !## goto to the north-west
        IF(JPR(1).GT.JPR(2))THEN
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
         IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal
         ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
        !## goto to the south-west
        ELSE
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
         IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal
         ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
        ENDIF
       !## goto to the east
       ELSE
        !## goto to the north-east
        IF(JPR(1).GT.JPR(2))THEN
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
         IC=MAX(JPC(1),JPC(2)); IR=MIN(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal   
         ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
        !## goto to the south-east
        ELSE
         IC=MIN(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,1)=INT(1,1) !## vertical
         ICOUT(3)=IC; IROUT(3)=IR; ICOUT(4)=IC+1; IROUT(4)=IR
         IC=MAX(JPC(1),JPC(2)); IR=MAX(JPR(1),JPR(2))
         IPC(IC,IR,2)=INT(1,1) !## horizontal  
         ICOUT(1)=IC; IROUT(1)=IR; ICOUT(2)=IC; IROUT(2)=IR+1
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
      use global, only: iunit, delc, delr, ncol, nrow
      use gwfmetmodule
      use m_mf2005_iu, only: iumet
      use pksmpi_mod,only: nrproc, gdelr, gdelc,
     1                     gncol, gnrow     

      implicit none
      
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

      simcsize = delr(1)
      
      if (nrproc.gt.1) then 
         lncol = gncol
         lnrow = gnrow
      else
         lncol = ncol
         lnrow = nrow
      end if   
      
      if(.not.allocated(lcdelr)) then
         allocate(lcdelr(0:lncol)) 
      end if    
      if(.not.allocated(lcdelc)) then
         allocate(lcdelc(0:lnrow)) 
      end if    
          
      if (nrproc.gt.1) then     
         lcdelr(0) = gcoord_xll
         do icol = 1, lncol
            lcdelr(icol) = gcoord_xll + real(icol)*simcsize
         end do
         lcdelc(0) = gcoord_yur
         do irow = 1, lnrow
            lcdelc(irow) = gcoord_yur - real(irow)*simcsize
         end do
      else
         lcdelc = cdelc    
         lcdelr = cdelr    
      end if
      
      if (ieq.eq.0) then
         lqd = .true.
      else
         lqd = .false.
      end if   
      
      if (lqd) then
         xmin=lcdelr(0)
         ymax=lcdelc(0)
         xmax=xmin+(simcsize*lncol)
         ymin=ymax-(simcsize*lnrow)
      endif

c end of program
      return
      end