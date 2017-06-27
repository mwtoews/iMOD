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

      !> Set the ID's for coupling
      subroutine pks7mpimssetids(x,n,gminid,gmaxid)

c...     modules 
      use pksmpi_mod, only: mpptyp, mppser, mppini1, myrank
      use pksmpims_mod, only: ids, nid
      use pks_iarmwp
      
      implicit none
      
c...     arguments
      integer, intent(in) :: n
      integer, intent(in) :: gminid
      integer, intent(in) :: gmaxid
      integer, dimension(*), intent(inout) :: x
      
c...     locals
      integer :: i, ixp, id
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return 

      if(allocated(ids)) deallocate(ids)
      if (gminid.le.0) then
         write(*,*) 'pks7mpimssetids: error, minimum ID <= 0'
         call pks7mpiwrpfinalize()
         call pksstop(' ')                  
      end if          
      
      if (gmaxid.le.0) then
         write(*,*) 'pks7mpimssetids: error, maximum ID <= 0'
         call pks7mpiwrpfinalize()
         call pksstop(' ')                  
      end if
      
      nid = gmaxid
      allocate(ids(nid))
      ids = 0
      do i = 1, n
         if (x(i).gt.0) then 
            ids(abs(x(i))) = 1 
         elseif(x(i).lt.0) then
            ids(abs(x(i))) = 2 
         end if   
         x(i) = abs(x(i))
      end do
      if (liarmwp) then
         do ixp = 1, nrxp
            do i = 1, xp(ixp)%nid 
               id = xp(ixp)%id(i)
               ! ID already exist in this partition
               if (ids(id).ne.0) then
                  if (ids(id).eq.1) then
                     ids(id) = 3
                  end if
                  if (ids(id).eq.2) then
                     ids(id) = 4
                  end if
               else ! ID does not yet exist: add
                  ids(id) = 3
               end if
            end do
         end do
      end if
      
      end subroutine      
      
      subroutine pks7mpimsid(id,lused,lmask)
      
c...     modules      
      use pksmpi_mod, only: mpptyp, mppser, mppini1
      use pksmpims_mod, only: ids, nid
      
      implicit none
      
c...     arguments
      integer, intent(in)  :: id
      logical, intent(out) :: lused 
      logical, intent(out) :: lmask
      
c...     locals

c.......................................................................

      lused = .true.
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return 
      if (nid.eq.0) return 

      if (id.eq.0) then
         write(*,*) 'pks7mpimsid: error, ID out of range',id
         call pksstop(' ')
      end if
      if (id.gt.nid) then
         write(*,*) 'pks7mpimsid: error, ID out of range',id
         call pksstop(' ')
      end if
      
      if (abs(ids(id)).le.0) then
         lused = .false.
      end if
      
      lmask = .true.
      if (ids(id).eq.2.or.ids(id).eq.4) lmask = .false.
      
      end subroutine pks7mpimsid
      
      subroutine pks7mpimsgsvat2id(iun,nuomigw)

c...     modules
      use pksmpi_mod, only: mpptyp, mppser, mppini1, myrank
      use pksmpims_mod, only: mxgsvatid, gsvat2id, mvid, ids
      use pks_iarmwp, only: liarmwp
      
      implicit none
      
c...     arguments
      integer, intent(in) :: iun
      integer, intent(inout) :: nuomigw
      
c...     locals
      logical :: lused, lmask, lused_1, lused_2, lstop
      integer :: ios, mf_r, k_r, ly_r, mf_1, mf_2, i
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) return           
      
      ! first, determine maximum svat
      mxgsvatid = 0
      ios = 0
      rewind(iun)
      do while(ios.eq.0)
         read(iun,'(i10,2x,i10,i5)',iostat=ios) mf_r, k_r, ly_r
         if (ios.eq.0) then
            mxgsvatid = max(mxgsvatid,k_r)
         end if   
      end do
      
      ! allocate
      allocate(gsvat2id(3,max(mxgsvatid,1)))
      
      ! step 1: label the svat layer 1
      ios = 0
      gsvat2id = mvid
      rewind(iun)
      do while(ios.eq.0)
         read(iun,'(i10,2x,i10,i5)',iostat=ios) mf_r, k_r, ly_r
         if (ios.ne.0) cycle
         if (ly_r.gt.1) cycle
         call pks7mpimsid(mf_r,lused_1,lmask) 
         if (lused_1) then
            gsvat2id(3,k_r) = 1
         end if        
      end do
       
      ! step 1: determine nuomigw
      ios = 0
      nuomigw = 0
      rewind(iun)
      do while(ios.eq.0)
         read(iun,'(i10,2x,i10,i5)',iostat=ios) mf_r, k_r, ly_r
         if (ios.ne.0) cycle
         if(gsvat2id(3,k_r).eq.mvid) cycle
         if(ly_r.gt.1) then
            i = ids(mf_r)
            if(i.eq.3.or.i.eq.4) then ! ID belong to other partition        
               mf_r = -mf_r
            end if
         end if
         if (ly_r.eq.1) then 
            gsvat2id(1,k_r) = mf_r
         else   
            gsvat2id(2,k_r) = mf_r
         end if                
         nuomigw = nuomigw + 1  
      end do
      rewind(iun)
          
      ! checks
      lstop = .false.
      do i = 1, mxgsvatid
         if(gsvat2id(3,k_r).eq.mvid) cycle 
         mf_1 = gsvat2id(1,i)    
         mf_2 = gsvat2id(2,i)
         ! check for svats that are connected to multiple partitions
         if (mf_1.ne.mvid.and.mf_2.ne.mvid) then
            call pks7mpimsid(abs(mf_1),lused_1,lmask)    
            call pks7mpimsid(abs(mf_2),lused_2,lmask)
            if(.not.lused_1) then
               write(*,*) 'Error, modflow cell for layer = 1 does'//
     1                    ' not belong to this partition! Svat=',
     1                    myrank, i, mf_1
               stop
            end if    
            if(.not.lused_2) then
               write(*,*) 'Error, modflow cell for layer > 1 does'//
     1                    ' not belong to this partition! Svat=',
     1                    myrank, i, mf_2
               stop
            end if    
         end if   
      end do
      
      !write(*,*) '@@@nuomigw',myrank,nuomigw
      !stop
      
      end subroutine
      
      subroutine pks7mpimsg2lsvat(nm,ke,nxe)

c...     modules
      use pksmpi_mod, only: mpptyp, mppser, mppini1, myrank
      use pksmpims_mod, only: mxgsvatid, gsvat2id, mvid
      
      implicit none
      
c...     arguments
      integer, intent(in) :: nm
      integer, intent(in) :: nxe
      integer, intent(out), dimension(nxe) :: ke 
      
c...     locals
      logical :: lused, lmask, lcoupled
      integer :: ios, r_nnex, id1, id2, lsvat, i, r_nund_tmp, n
c.......................................................................

      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) then
         ke = 1
         return
      end if    

      ke = 0
      
      ! fill g2lsvat (ke) mapping
      ios = 0
      rewind(nm)
      n = 0
      do while(ios.eq.0)
          read(nm,'(i10)',iostat=ios) r_nnex
          if(gsvat2id(3,r_nnex).eq.mvid) cycle 
          if (ios.eq.0) then
             lcoupled = .false. 
             if (r_nnex.le.nxe) then
                id1 = gsvat2id(1,r_nnex)
                if (id1.ne.mvid) then ! check if svat is coupled to MODFLOW
                   lcoupled = .true.
                   call pks7mpimsid(abs(id1),lused,lmask) 
                   if (lused) then ! belongs to my partion 
                      if (ke(r_nnex).eq.0) then
                         n = n + 1 
                         if (lmask) then
                            ke(r_nnex) = 1
                         else
                            ke(r_nnex) = 2
                         end if                             
                      end if
                   end if
                end if   
                id2 = gsvat2id(2,r_nnex)
                if (id2.ne.mvid) then ! check if svat is coupled to MODFLOW
                   lcoupled = .true.
                   call pks7mpimsid(abs(id2),lused,lmask)       
                   if (lused) then ! belongs to my partion 
                      if (ke(r_nnex).eq.0) then
                         n = n + 1 
                         if (lmask) then
                            ke(r_nnex) = 1
                         else
                            ke(r_nnex) = 2
                         end if                             
                     end if
                   end if
                end if   
             end if
             if (.not.lcoupled) then ! add non-coupled svats to my partition ==> duplicate over MPI processes
                write(*,*) 'Warning, non-coupled svat:', r_nnex
                n = n + 1
                ke(r_nnex) = 1
             end if
          end if
      end do
c      call pks7mpiwrpfinalize()
c      stop 
      
      end subroutine
      
      subroutine pks7mpimsiarmwpmask(id,gsvat,rmask)
c...     modules
      use pks_iarmwp, only: liarmwp
      use pksmpi_mod, only: mpptyp, mppser, mppini1, myrank   
      use pksmpims_mod, only: gsvat2id
      
      implicit none
c...     arguments
      integer, intent(in) :: id
      integer, intent(in) :: gsvat
      real, intent(out) :: rmask
c...     locals
      integer :: id1
      logical :: lused, lmask
c.......................................................................
     
      rmask = 1.
      if(.not.liarmwp) return
      if (mpptyp.eq.mppser .or. mpptyp.eq.mppini1) then
         return
      end if
      
      id1 = gsvat2id(1,gsvat)
      call pks7mpimsid(id1,lused,lmask)
      if(.not.lmask)then
         rmask = 0.
      end if
      
      return
      end subroutine