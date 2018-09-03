
      ! Parmeter settings for correlation results

      ! WARNING: this include file is used by f77 and f90 formatted source
      !          DON'T USE CONTINUATION LINES

      integer   igmlg    ! average lag found for auro/cross correlation
      integer   icc      ! auto/cross correlation
      integer   icov     ! covariance
      integer   ivar1    ! variance ts 1
      integer   ivar2    ! variance ts 2
      integer   igem1    ! average ts 1
      integer   igem2    ! average ts 2
      integer   ip50_1   ! P50 ts 1 (only for the measurements used for correlation!)
      integer   ip50_2   ! P50 ts 2 (only for the measurements used for correlation!)
      integer   irmse    ! RMSE
      integer   ilag     ! lag
      integer   in       ! number of pairs used for calculations

      parameter (igmlg=1,icc=2,icov=3,ivar1=4,ivar2=5,igem1=6,igem2=7)
      parameter (ip50_1=8,ip50_2=9,irmse=10,ilag=11,in=12)

