!!  Copyright (C) Stichting Deltares, 2005-2016.
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

!> description
!! read file of different formats
subroutine rdfile4(infile,string,mv,sn,coldate,colval,exitcode)


! declaration section
! ------------------------------------------------------------------------------
 use m_cross

 implicit none


! arguments
 integer  , intent(in)          :: sn              !> series number to store the data

 real     , intent(in)          :: mv              !> missing value

 character (len=*), intent(in)  :: infile          !> input file name
 character (len=*), intent(out) :: string          !> description of data set
 character (len=*), intent(in)  :: coldate         !> column name for date values
 character (len=*), intent(in)  :: colval          !> column name for values
                                                   !! when colnames are given the input files
                                                   !! are interpeted as iModflow text files
 integer  , intent(out)         :: exitcode        !> exit status of this routine: 0=OK


! local variables
 integer   lun,ios,l,lc1,lc2,ftype,nd,i
 integer   icoldate,icolval

 character record*128,del*1
 character (len=32) :: cdate,cval,cname

 real      vmv,r


! functions
 integer   cfn_getlun,osd_open2,cfn_length,osd_rindex

 character osd_basename*64


! program section
! ------------------------------------------------------------------------------


! init
 exitcode = 0
 lun      = -1
 vmv      = mv


! initialise m_cross module
 call m_cross_init()


! disassociate pointers when pointing to the same memory
if (associated(ser(1)%x,ser(2)%x)) then
   nullify(ser(2)%x)
   nullify(ser(2)%y)
endif


! use pointers
 if (sn.ge.1 .and. sn.le.2) then
    psr=>ser(sn)
    if (.not.associated(psr%n)) allocate(psr%n)
    n=>psr%n
 else
    write(*,'(/a/)') ' ERROR, wrong data series number for reading file '//infile(1:cfn_length(infile))
    exitcode = -3
 endif


! open file
 if (exitcode.eq.0) then
    lun=cfn_getlun(10,99)
    ios=osd_open2(lun,0,infile,'old,readonly,shared,formatted')
    if (ios.ne.0) then
       write(*,'(/2a/)') ' ERROR, could not open file ',infile(1:cfn_length(infile))
       lun=-1
       exitcode=-1
    endif
 endif


! determine file type
 if (exitcode.eq.0) then
    cdate=coldate
    cval =colval
    call cfn_token(cdate,'tl')
    call cfn_token(cval ,'tl')
    lc1=cfn_length(cdate)
    lc2=cfn_length(cval)
    if (lc1.le.0 .and. lc2.le.0) then
       ! type 1
       ftype=1
    else
       ! type 2
       ftype=2
    endif
 endif


! read header
 if (exitcode.eq.0) then

    if (ftype.eq.1) then
       ! type 1
       ! record 1   : identification
       !        2   : number of data records
       !        3...: date (yyyymmdd), value

       ! data columns
       icoldate=1
       icolval =2

       ! record 1: data identification string
       call rdrecord(lun,record,ios)
       string=record

       ! record 2: number data records
       n=-1
       call rdrecordi(lun,record,n,ios)

       if (ios.ne.0) then
          write(*,'(/2a/)') ' ERROR, reading header of file ',infile(1:cfn_length(infile))
          exitcode=-2
       endif
       
       if (n.le.0) then
          write(*,'(/2a/)') ' ERROR, zero length data file ',infile(1:cfn_length(infile))
          exitcode=-4
       endif
    else
       ! type 2
       ! record 1   : number of data records
       !        2   : number of data columns
       !        3...: <column name>, <missing value code>
       !        ....: value , value, value, ......

       ! record 1: number of data records
       n=-1
       call rdrecordi(lun,record,n,ios)
       if (ios.ne.0) exitcode=-2

       ! record 2: number of data columns
       nd=-1
       call rdrecordi(lun,record,nd,ios)
       if (ios.ne.0) exitcode=-2

       ! record 3: column names

       !    try to read integer values from column names
       read(coldate,*,iostat=ios) icoldate
       if (ios.ne.0) icoldate=0
       read(colval ,*,iostat=ios) icolval
       if (ios.ne.0) icolval =0

       do i=1,nd
!          read(lun,*) cname,vmv
          
          call rdrecord(lun,record,ios)
          call cfn_par_ext(cname,record,' ,',2,del)
          call cfn_token(cname,'tl')

          if (icoldate.eq.0) then
             if (cname.eq.cdate) icoldate=i
          endif

          if (icolval.eq.0) then
             if (cname.eq.cval ) then
                icolval=i
                ! try also to get missing value code
                read(record,*,iostat=ios) r
                if (ios.eq.0) then
                   vmv=r
                endif
             endif
          else
           if(i.eq.icolval)then
                read(record,*,iostat=ios) r
                if (ios.eq.0) then
                   vmv=r
                endif
           endif
          endif

       enddo
       ! default column names
       if (icoldate.le.0) icoldate=1
       if (icolval .le.0) icolval =2


       if (exitcode.ne.0) then
          write(*,'(/2a/)') ' ERROR, reading header of file ',infile(1:cfn_length(infile))
          exitcode=-2
       endif

       ! set identification string, take filename without extension
       string=osd_basename(infile,' ')
       i=osd_rindex(string,'.')
       if (i.gt.0) string=string(1:i)
    endif

 endif


! allocate data arrays
 if (exitcode.eq.0) then
   if (associated(psr%x)) then
      if (size(psr%x).lt.n) then
         ! current allocated array too small
         deallocate(psr%x,psr%y)
         ! reallocate
         allocate(psr%x(n),psr%y(n))
      endif
   else
      allocate(psr%x(n),psr%y(n))
   endif

   ! pointer
   x=>psr%x
   y=>psr%y
 endif


! read data
 if (exitcode.eq.0) then
    call rdseries4(lun,x,y,n,vmv,icoldate,icolval,exitcode)
!    call rdseries4(lun,x,y,n,mv,icoldate,icolval,exitcode)
 endif


! close file
 if (lun.gt.0) then
    close(lun)
 endif


! end of program
 return
end

! ******************************************************************************

!> description
!! read time series
!! missing values will be removed from the data
subroutine rdseries4(lun,x,y,n,mv,icoldate,icolval,exitcode)


! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(in)     :: lun          !> unit number
 integer  , intent(inout)  :: n            !> number of data values

 real     , intent(out)    :: x(*),y(*)    !> values

 integer  , intent(in)     :: icoldate     !> column number for date values
 integer  , intent(in)     :: icolval      !> column number for values

 real     , intent(in)     :: mv           !> missing value code

 integer  , intent(out)    :: exitcode     !> exit status: 0=OK


! local variables
 integer   i,j,idat,ios,l,nval,mc

 real      v

 double precision ddat

 integer, parameter :: mxcol=20
 double precision tmpv(mxcol)

 character record*128


! functions
 integer   cfn_length,cfn_dat2cen


! program section
! ------------------------------------------------------------------------------

! init
 exitcode = 0


! check column  numbers
 mc=max(icoldate,icolval)
 if (mc.gt.mxcol) then
    ! oops, temporary array to small
    write(*,*) ' ERROR, temporary array TMPV in routine rdseries4 to small! Increase to at least ',mc
    call exit(21)
 endif


! read data records
 nval=0
 do i=1,n
    call rdrecord(lun,record,ios)

    read(record,*,iostat=ios) (tmpv(j),j=1,mc)
    if (ios.eq.0) then
       ! FR 20140206 
       ddat=tmpv(icoldate)
       ddat=int(FLOOR(ddat/1000000.))
       v   =tmpv(icolval)
       if (v.ne.mv) then
          nval=nval+1
          idat=int(ddat)
          x(nval)=cfn_dat2cen(idat)
          y(nval)=v
       endif
    else
! ERROR
       write(*,'(/a,i8,a,i8/)') ' ERROR reading data. record ',i,'  iostat=',ios
       exitcode=-10
       ! leave do-loop
       exit
    endif
 enddo


! set new value for n
 n=nval


! end of program
 return
end

! ******************************************************************************

!> description
!! series 1 and 2 are equal, let point series 2 to the arrays of series 1
subroutine rdequal()


! declaration section
! ------------------------------------------------------------------------------
 use m_cross

 implicit none


! arguments


! local variables


! program section
! ------------------------------------------------------------------------------


! initialise m_cross module
 call m_cross_init()


! set pointers
 n1=>ser(1)%n
 x1=>ser(1)%x
 y1=>ser(1)%y

 x2=>ser(2)%x
 y2=>ser(2)%y


! check or series 2 has to be deallocated
 if (associated(x2)) then
    ! be sure it is not pointing to x1 already
    if (.not.associated(x2,x1)) then
       ! aha, different memory locations. Deallocate series 2
       deallocate(x2,y2)
    endif
 endif

! check n2
 if (.not.associated(ser(2)%n)) allocate(ser(2)%n)

! set pointers
 ser(2)%x=>x1
 ser(2)%y=>y1
! set value for n
 ser(2)%n= n1


! end of program
 return
end

! ******************************************************************************

 subroutine rdrecord(lun,record,ios)
 
 ! read one record from file lun and remove a trailing ^M
 implicit none

! arguments 
 integer,           intent(in)   :: lun     !> unit number
 character (len=*), intent(out)  :: record  !> output record
 integer,           intent(out)  :: ios     !> I/O status

! local variables
 integer    l


 read(lun,'(a)',iostat=ios) record
 l=index(record,char(13))
 if (l.gt.0) record(l:l)=' '

 return
end

! ******************************************************************************

 subroutine rdrecordi(lun,record,n,ios)
 
 ! read one record from file lun and remove a trailing ^M
 ! try to read an integer from record and store the value into n

 implicit none

! arguments 
 integer,           intent(in)   :: lun     !> unit number
 character (len=*), intent(out)  :: record  !> output record
 integer,           intent(out)  :: ios     !> I/O status
 integer,           intent(out)  :: n       !> integer 

! local variables
 integer    l


 call rdrecord(lun,record,ios)
 if (ios.eq.0) then
    read(record,*,iostat=ios) l
    if (ios.eq.0) n=l
 endif

 return
end

