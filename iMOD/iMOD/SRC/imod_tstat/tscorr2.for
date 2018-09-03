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

      subroutine tscorr2(x1,y1,ts1w,nval1,x2,y2,ts2w,nval2,              !*URT*
     1                  result,dtmp,nlag,ncol,minlag,maxlag,mv,          !*URT*
     1                  lp501,lp502)                                     !*URT*


c declaration section
c ------------------------------------------------------------------------------

      implicit none


c arguments
      integer   nval1,            
     1          nval2,            
     1          nlag,             
     1          ncol              

      REAL(KIND=DP_KIND), intent(out)  :: result(nlag,ncol)
      
      REAL(KIND=DP_KIND) dtmp(nlag,ncol)

      real      x1(nval1),y1(nval1), 
     1          x2(nval2),y2(nval2), 
                                     
                                     
     1          ts1w(nval1,nlag), 
     1          ts2w(nval2,nlag), 
     1          minlag,           
     1          maxlag,           
     1          mv                

      logical   lp501             
      logical   lp502             


c local variables
      integer   i,i1b,j,j2b,j2e,lag

      real      n,rlag,perc,
     1          lowint,uppint         
                                      

      REAL(KIND=DP_KIND) val1,val2,dval,dmv

      logical   continue

      include 'tscorr2.inc'

c functions


c include files


c program section
c ------------------------------------------------------------------------------


c init
      dmv=mv          !  REAL(KIND=DP_KIND) version of nodata value


      do j=1,ncol
         do i=1,nlag
            dtmp(i,j)=0.0D0
         enddo
      enddo


      do j=1,nlag
         do i=1,nval1
            ts1w(i,j)=mv
         enddo
         do i=1,nval2
            ts2w(i,j)=mv
         enddo
      enddo


      if (nval1.gt.0 .and. nval2.gt.0) then

         j2b = 1  
         j2e = 1  

         i1b = 1
         continue=.true.
         do while(continue)
            if (i1b.le.nval1) then

              ! lowint=x1(i1b)+minlag
               uppint=x1(i1b)+maxlag

c            if ((ts2(1,1)-ts1(i1b,1)).gt.maxlag) then
               if (uppint.lt.x2(1)) then
                  i1b=i1b+1
               else
                  continue=.false.
               endif
            else
               continue=.false.
            endif
         enddo

         do i=i1b,nval1
            val1=y1(i)
            if (val1.ne.dmv) then

               lowint=x1(i)+minlag
               uppint=x1(i)+maxlag

               continue=.true.
               do while(continue)
                  if (j2b.le.nval2) then
                     if (x2(j2b).lt.lowint) then
                        j2b=j2b+1
                        else
                        continue=.false.
                     endif
                  else
                     continue=.false.
                  endif
               enddo

               j2e=max(j2b,j2e)
               j2e=min(j2e,nval2)
               continue=.true.
               do while(continue)
                  if (j2e.lt.nval2) then
                     if (x2(j2e+1).lt.uppint) then
                        j2e=j2e+1
                     else
                        continue=.false.
                     endif
                  else
                     continue=.false.
                  endif
               enddo
               if (x2(j2e).ge.uppint) then
                  j2e=j2e-1
               endif
               j2e=min(j2e,nval2)

               do j=j2b,j2e
                  val2=y2(j)
                  if (val2.ne.dmv) then
                     rlag=nlag*(x2(j)-x1(i)-minlag)/
     1                        (maxlag-minlag)+1
                     lag=int(rlag)
                     if (lag.lt.1 .or. lag.gt.nlag) then
                     endif
                     lag=max(1,min(lag,nlag)) 
                                              
                     dval=val1-val2
                     dtmp(lag,igmlg) = dtmp(lag,igmlg)+x2(j)-x1(i)
                     dtmp(lag,icov)   = dtmp(lag,icov)   +val1*val2
                     dtmp(lag,ivar1)  = dtmp(lag,ivar1)  +val1*val1
                     dtmp(lag,ivar2)  = dtmp(lag,ivar2)  +val2*val2
                     dtmp(lag,irmse)  = dtmp(lag,irmse)  +dval*dval
                     dtmp(lag,igem1)  = dtmp(lag,igem1)  +val1
                     dtmp(lag,igem2)  = dtmp(lag,igem2)  +val2
                     dtmp(lag,in)     = dtmp(lag,in)     +1.
                     ts1w(i,lag)=val1
                     ts2w(j,lag)=val2

                  endif

               enddo

            endif
         enddo

      endif


      do i=1,nlag
         if (dtmp(i,in).eq.0.0D0) then
            do j=1,ncol
               dtmp(i,j)=dmv
            enddo
            dtmp(i,in)=0.0D0  
         else
            n=dtmp(i,in)
            dtmp(i,igmlg) = dtmp(i,igmlg)/n                        
            dtmp(i,igem1) = dtmp(i,igem1)/n                        
            dtmp(i,ivar1) = dtmp(i,ivar1)/n - dtmp(i,igem1)**2.0   
            dtmp(i,igem2) = dtmp(i,igem2)/n                        
            dtmp(i,ivar2) = dtmp(i,ivar2)/n - dtmp(i,igem2)**2.0   
            dtmp(i,icov)  = dtmp(i,icov)/n                         
            dtmp(i,irmse) = sqrt(dtmp(i,irmse)/n)                  

            dtmp(i,icov)  = dtmp(i,icov) - dtmp(i,igem1)*dtmp(i,igem2)

            if ((dtmp(i,ivar1)*dtmp(i,ivar2)).gt.0.0D0) then
               dtmp(i,icc)   =
     1                 dtmp(i,icov)/sqrt(dtmp(i,ivar1)*dtmp(i,ivar2))
            else
               dtmp(i,icc)   = dmv
            endif
            
            perc=dmv
            if (lp501) call calperc(ts1w(1,i),nval1,mv,50.,perc)
            dtmp(i,ip50_1)=perc

            perc=dmv
            if (lp502) call calperc(ts2w(1,i),nval2,mv,50.,perc)
            dtmp(i,ip50_2)=perc

         endif

         dtmp(i,ilag) = (i-0.5)*(maxlag-minlag)/nlag+minlag  
                                                             
         do j=1,ncol
            result(i,j)=dtmp(i,j)
         enddo
      enddo

      return
      end
