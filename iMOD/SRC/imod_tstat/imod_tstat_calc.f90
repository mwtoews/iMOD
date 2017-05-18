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
!> description
!! subroutine to be called from imod to calculate the wanted statistics
subroutine imod_tstat_calc(infile1,infile2,coldate,colval,options,mv,     & ! input
                           result,                                        & ! results
                           lag,dlag,                                      & ! correlations
                           ibegdat,ienddat,slen,wfc,xstep,                & ! period
                           gbegdat,genddat,gnpy,                          & ! GxG
                           perc1,perc2,                                   & ! percentile
                           exitcode)



! declaration section
! ------------------------------------------------------------------------------
 use m_cross

 implicit none


! arguments
 integer  , intent(in)          :: options(*)       !> options which parameters will be calculated
                                                    !! When 1: switched on, otherwise: switched off
                                                    !! options(1): auto correlation of timeseries 1
                                                    !!         2 : auto correlation of timeseries 2
                                                    !!         3 : cross correlation between timesries 1 and 2
                                                    !!         4 : Percentile time series 1
                                                    !!         5 : Percentile time series 2
                                                    !!         6 : GxG time series 1
                                                    !!         7 : GxG time series 2
                                                    !!         8 : period timeseries 1
                                                    !!         9 : period timeseries 2
                                                    !!        10 : 

 real     , intent(out)         :: result(*)        !> Result of the calculations
                                                    !! Depending on options() the next data will be
                                                    !! assigned to results
                                                    !! results(1): auto correlation timeseries 1
                                                    !!         2 :    mean lag
                                                    !!         3 :    number of data pairs used
                                                    !!         4 : auto correlation timeseries 2
                                                    !!         5 :    mean lag
                                                    !!         6 :    number of data pairs used
                                                    !!         7 : cross correlation
                                                    !!         8 :     mean lag
                                                    !!         9 :     number of data pairs used
                                                    !!        10  : Percentile time series 1
                                                    !!        11  : Percentile time series 2
                                                    !!        12  : GHG time series 1
                                                    !!        13  :    GLG
                                                    !!        14  :    nGxG number of years used to calculate GxG
                                                    !!        15  : GHG time series 2
                                                    !!        16  :    GLG
                                                    !!        17  :    nGxG number of years used to calculate GxG
                                                    !!        18  : date  top period time series 1
                                                    !!        19  :    value top
                                                    !!        20  :    date  bot
                                                    !!        21  :    value bot
                                                    !!        22  : date  top period time series 2
                                                    !!        23  :    value top
                                                    !!        24  :    date  bot
                                                    !!        25  :    value bot


 character (len=*), intent(in)  :: infile1          !> input data filename for time series 1
 character (len=*), intent(in)  :: infile2          !> input data filename for time series 2
                                                    !! this one may be empty
 character (len=*), DIMENSION(2), intent(in)  :: coldate          !> column name for date values
                                                    !! if numeric it's the column number
 character (len=*), DIMENSION(2), intent(in)  :: colval           !> column name for the values
                                                    !! if numeric it's the column number
 real   , intent(in)            :: mv               !> missing value code

 real   , intent(in)            :: lag              !> lag distance
 real   , intent(in)            :: dlag             !> lag width (half)

 integer, intent(in)            :: ibegdat          !> begin date finding top/bot period (yyyymmdd)
 integer, intent(in)            :: ienddat          !> end   date finding top/bot period (yyyymmdd)
 real   , intent(in)            :: slen             !> 
 real   , intent(in)            :: wfc              !> 
 real   , intent(in)            :: xstep            !> 

 integer, intent(in)            :: gbegdat          !> begin date (yyyymmdd)
 integer, intent(in)            :: genddat          !> end   date (yyyymmdd)
 integer, intent(in)            :: gnpy             !> minimal number of measurments in a year to calc Gx3

 real   , intent(in)            :: perc1            !> percentile value for time series 1
 real   , intent(in)            :: perc2            !> percentile value for time series 2

 INTEGER,INTENT(OUT) :: EXITCODE

! local variables
 integer   i !,exitcode

 ! corr
 integer   nlag
 real      minlag,maxlag
 integer, parameter :: ncol = 12   ! number of result variabeles of correlation calculation

 ! period
 real      begdat,enddat

 ! gxg
 integer  datgxg(3)

 ! offset
 integer   noff

 character (len=64) :: id1,id2

 logical  lauto1,lauto2,lcor,lperc1,lperc2,lgxg1,lgxg2,lper1,lper2

 real     tresult(ncol)

 include 'tscorr2.inc'


! functions
 integer   cfn_dat2cen


! program section
! ------------------------------------------------------------------------------

! init
 exitcode = 0
 nlag     = 1       ! only one lag for crosscorrelation will be calculated
 noff     = 1       ! not used, only for size of temporary arrays
 do i=1,25
    result(i)=mv
 enddo


! get settings
 lauto1=(options(1).eq.1)
 lauto2=(options(2).eq.1)
 lcor  =(options(3).eq.1)
 lperc1=(options(4).eq.1)
 lperc2=(options(5).eq.1)
 lgxg1 =(options(6).eq.1)
 lgxg2 =(options(7).eq.1)
 lper1 =(options(8).eq.1)
 lper2 =(options(9).eq.1)

 if (lauto1 .or. lauto2 .or. lcor) then
    minlag=lag-dlag
    maxlag=lag+dlag
 endif

 if (lgxg1 .or. lgxg2) then
    datgxg(1)=gbegdat
    datgxg(2)=genddat
    datgxg(3)=gnpy
 endif

 if (lper1 .or. lper2) then
    begdat=cfn_dat2cen(ibegdat)
    enddat=cfn_dat2cen(ienddat)
 endif


! read files
 call rdfiles(infile1,infile2,coldate,colval,mv,id1,id2,exitcode)


! set pointers
 if (exitcode.eq.0) then
    n1=>ser(1)%n
    x1=>ser(1)%x
    y1=>ser(1)%y

    n2=>ser(2)%n
    x2=>ser(2)%x
    y2=>ser(2)%y
 endif


! allocate temporary arrys
 if (exitcode.eq.0) then
    call createtmp(nlag,ncol,noff)
 endif


! calculate correlation
 if (exitcode.eq.0) then
    if (lauto1) then
       call tscorr2(x1,y1,tsw1,n1,x1,y1,tsw2,n1,tresult,tsw4,nlag,ncol,minlag,maxlag,mv,.false.,.false.)
       result(1)=tresult(icc)          ! correlation
       result(2)=tresult(igmlg)        ! average lag
       result(3)=tresult(in)           ! number of pairs used for correlation
    endif
    if (lauto2) then
       call tscorr2(x2,y2,tsw1,n2,x2,y2,tsw2,n2,tresult,tsw4,nlag,ncol,minlag,maxlag,mv,.false.,.false.)
       result(4)=tresult(icc)          ! correlation
       result(5)=tresult(igmlg)        ! average lag
       result(6)=tresult(in)           ! number of pairs used for correlation
    endif
    if (lcor) then
       call tscorr2(x1,y1,tsw1,n1,x2,y2,tsw2,n2,tresult,tsw4,nlag,ncol,minlag,maxlag,mv,.false.,.false.)
       result(7)=tresult(icc)          ! correlation
       result(8)=tresult(igmlg)        ! average lag
       result(9)=tresult(in)           ! number of pairs used for correlation
    endif
 endif


! calculate percentile
 if (exitcode.eq.0) then
    if (lperc1) then
       call cfn_cp_r2r(y1,tsw1,n1)
       call calperc(tsw1,n1,mv,perc1,result(10))
    endif
    if (lperc2) then
       call cfn_cp_r2r(y2,tsw2,n2)
       call calperc(tsw2,n2,mv,perc2,result(11))
    endif
 endif
 

! calculate period
 if (exitcode.eq.0) then
    ! ipres=ires   ! save position for calculate offset
    if (lper1) call findperiod(x1,y1,n1,begdat,enddat,slen,wfc,xstep,mv,result(18))
    if (lper2) call findperiod(x2,y2,n2,begdat,enddat,slen,wfc,xstep,mv,result(22))
 endif

! calculate GxG
 if (exitcode.eq.0) then
    if (lgxg1) call calcgxg(x1,y1,n1,datgxg,result(12))
    if (lgxg2) call calcgxg(x2,y2,n2,datgxg,result(15))
 endif


! end of program
 return
end
