C-------ROUTINES FROM SPARSKIT TO PERMUTATE A LINEAR SYSTEM OF EQUATIONS
C       IN ORDER TO REORDER THE MATRIX TO MINIMIZE THE BANDWIDTH USING
C       THE REVERSE CUTHILL MCKEE ALGORITHM
      subroutine dperm (nrow,a,ja,ia,ao,jao,iao,perm,qperm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),
     +        qperm(*),job
      real*8 a(*),ao(*)
!-----------------------------------------------------------------------
! This routine permutes the rows and columns of a matrix stored in CSR
! format. i.e., it computes P A Q, where P, Q are permutation matrices.
! P maps row i into row perm(i) and Q maps column j into column qperm(j):
!      a(i,j)    becomes   a(perm(i),qperm(j)) in new matrix
! In the particular case where Q is the transpose of P (symmetric
! permutation of A) then qperm is not needed.
! note that qperm should be of length ncol (number of columns) but this
! is not checked.
!-----------------------------------------------------------------------
! Y. Saad, Sep. 21 1989 / recoded Jan. 28 1991.
!-----------------------------------------------------------------------
! on entry:
!----------
! n       = dimension of the matrix
! a, ja,
!    ia = input matrix in a, ja, ia format
! perm       = integer array of length n containing the permutation arrays
!        for the rows: perm(i) is the destination of row i in the
!         permuted matrix -- also the destination of column i in case
!         permutation is symmetric (job .le. 2)
!
! qperm      = same thing for the columns. This should be provided only
!         if job=3 or job=4, i.e., only in the case of a nonsymmetric
!        permutation of rows and columns. Otherwise qperm is a dummy
!
! job      = integer indicating the work to be done:
! * job = 1,2 permutation is symmetric  Ao :== P * A * transp(P)
!             job = 1      permute a, ja, ia into ao, jao, iao
!             job = 2 permute matrix ignoring real values.
! * job = 3,4 permutation is non-symmetric  Ao :== P * A * Q
!             job = 3      permute a, ja, ia into ao, jao, iao
!             job = 4 permute matrix ignoring real values.
!
! on return:
!-----------
! ao, jao, iao = input matrix in a, ja, ia format
!
! in case job .eq. 2 or job .eq. 4, a and ao are never referred to
! and can be dummy arguments.
! Notes:
!-------
!  1) algorithm is in place
!  2) column indices may not be sorted on return even  though they may be
!     on entry.
!----------------------------------------------------------------------c
! local variables
      integer locjob, mod
!
!     locjob indicates whether or not real values must be copied.
!
      locjob = mod(job,2)
!
! permute rows first
!
      call rperm (nrow,a,ja,ia,ao,jao,iao,perm,locjob)
!
! then permute columns
!
      locjob = 0
!
      if (job .le. 2) then
         call cperm (nrow,ao,jao,iao,ao,jao,iao,perm,locjob)
      else
         call cperm (nrow,ao,jao,iao,ao,jao,iao,qperm,locjob)
      endif
!
      return
!-------end-of-dperm----------------------------------------------------
      end subroutine
!-----------------------------------------------------------------------
      subroutine rperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),job
      real*8 a(*),ao(*)
!-----------------------------------------------------------------------
! this subroutine permutes the rows of a matrix in CSR format.
! rperm  computes B = P A  where P is a permutation matrix.
! the permutation P is defined through the array perm: for each j,
! perm(j) represents the destination row number of row number j.
! Youcef Saad -- recoded Jan 28, 1991.
!-----------------------------------------------------------------------
! on entry:
!----------
! n       = dimension of the matrix
! a, ja, ia = input matrix in csr format
! perm       = integer array of length nrow containing the permutation arrays
!        for the rows: perm(i) is the destination of row i in the
!         permuted matrix.
!         ---> a(i,j) in the original matrix becomes a(perm(i),j)
!         in the output  matrix.
!
! job      = integer indicating the work to be done:
!             job = 1      permute a, ja, ia into ao, jao, iao
!                       (including the copying of real values ao and
!                       the array iao).
!             job .ne. 1 :  ignore real values.
!                     (in which case arrays a and ao are not needed nor
!                      used).
!
!------------
! on return:
!------------
! ao, jao, iao = input matrix in a, ja, ia format
! note :
!        if (job.ne.1)  then the arrays a and ao are not used.
!----------------------------------------------------------------------c
!           Y. Saad, May  2, 1990                                      c
!----------------------------------------------------------------------c
      logical values
      values = (job .eq. 1)
!
!     determine pointers for output matix.
!
      do 50 j=1,nrow
         i = perm(j)
         iao(i+1) = ia(j+1) - ia(j)
 50   continue
!
! get pointers from lengths
!
      iao(1) = 1
      do 51 j=1,nrow
         iao(j+1)=iao(j+1)+iao(j)
 51   continue
!
! copying
!
      do 100 ii=1,nrow
!
! old row = ii  -- new row = iperm(ii) -- ko = new pointer
!
         ko = iao(perm(ii))
         do 60 k=ia(ii), ia(ii+1)-1
            jao(ko) = ja(k)
            if (values) ao(ko) = a(k)
            ko = ko+1
 60      continue
 100  continue
!
      return
!---------end-of-rperm -------------------------------------------------
!-----------------------------------------------------------------------
      end subroutine

!-----------------------------------------------------------------------
      subroutine cperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(*), job
      real*8 a(*), ao(*)
!-----------------------------------------------------------------------
! this subroutine permutes the columns of a matrix a, ja, ia.
! the result is written in the output matrix  ao, jao, iao.
! cperm computes B = A P, where  P is a permutation matrix
! that maps column j into column perm(j), i.e., on return
!      a(i,j) becomes a(i,perm(j)) in new matrix
! Y. Saad, May 2, 1990 / modified Jan. 28, 1991.
!-----------------------------------------------------------------------
! on entry:
!----------
! nrow       = row dimension of the matrix
!
! a, ja, ia = input matrix in csr format.
!
! perm      = integer array of length ncol (number of columns of A
!         containing the permutation array  the columns:
!         a(i,j) in the original matrix becomes a(i,perm(j))
!         in the output matrix.
!
! job      = integer indicating the work to be done:
!             job = 1      permute a, ja, ia into ao, jao, iao
!                       (including the copying of real values ao and
!                       the array iao).
!             job .ne. 1 :  ignore real values ao and ignore iao.
!
!------------
! on return:
!------------
! ao, jao, iao = input matrix in a, ja, ia format (array ao not needed)
!
! Notes:
!-------
! 1. if job=1 then ao, iao are not used.
! 2. This routine is in place: ja, jao can be the same.
! 3. If the matrix is initially sorted (by increasing column number)
!    then ao,jao,iao  may not be on return.
!
!----------------------------------------------------------------------c
! local parameters:
      integer k, i, nnz
!
      nnz = ia(nrow+1)-1
      do 100 k=1,nnz
         jao(k) = perm(ja(k))
 100  continue
!
!     done with ja array. return if no need to touch values.
!
      if (job .ne. 1) return
!
! else get new pointers -- and copy values too.
!
      do 1 i=1, nrow+1
         iao(i) = ia(i)
 1    continue
!
      do 2 k=1, nnz
         ao(k) = a(k)
 2    continue
!
      return
!---------end-of-cperm--------------------------------------------------
!-----------------------------------------------------------------------
      end subroutine
!-----------------------------------------------------------------------
      subroutine vperm (n, x, perm)
      integer n, perm(n)
      real*8 x(n)
!-----------------------------------------------------------------------
! this subroutine performs an in-place permutation of a real vector x
! according to the permutation array perm(*), i.e., on return,
! the vector x satisfies,
!
!	x(perm(j)) :== x(j), j=1,2,.., n
!
!-----------------------------------------------------------------------
! on entry:
!---------
! n 	= length of vector x.
! perm 	= integer array of length n containing the permutation  array.
! x	= input vector
!
! on return:
!----------
! x	= vector x permuted according to x(perm(*)) :=  x(*)
!
!----------------------------------------------------------------------c
!           Y. Saad, Sep. 21 1989                                      c
!----------------------------------------------------------------------c
! local variables
      real*8 tmp, tmp1
!
      init      = 1
      tmp       = x(init)
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
!
! loop
!
 6    k = k+1
!
! save the chased element --
!
      tmp1      = x(ii)
      x(ii)     = tmp
      next      = perm(ii)
      if (next .lt. 0 ) goto 65
!
! test for end
!
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next
!
! end loop
!
      goto 6
!
! reinitilaize cycle --
!
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp       = x(init)
      ii        = perm(init)
      perm(init)=-perm(init)
      goto 6
!
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue
!
      return
!-------------------end-of-vperm---------------------------------------
!-----------------------------------------------------------------------
      end subroutine
