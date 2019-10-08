MODULE MOD_LHC
 
 CONTAINS
 
 SUBROUTINE LHC(N,M,SEED,TABLE)

!*****************************************************************************80
!
!! MAIN IS THE MAIN PROGRAM FOR LATIN_RANDOM_DATASET.
!
!  DISCUSSION:
!
!    LATIN_RANDOM_DATASET GENERATES AND SAVES A LATIN RANDOM SQUARE DATASET.
!
!    A "LATIN RANDOM SQUARE" DATASET IS CREATED BY DIVIDING EACH SIDE
!    OF THE UNIT HYPERCUBE INTO N SUBINTERVALS, AND THEN CHOOSING
!    A SET OF N SUBCUBES.  THE SUBCUBES ARE SELECTED IN SUCH A WAY
!    THAT IF WE PROJECT THE SUBCUBES ONTO ANY COORDINATE DIRECTION,
!    THERE IS EXACTLY ONE SUBCUBE IN EACH SUBINTERVAL.
!
!    THAT'S THE "LATIN SQUARE" PART OF THE DATASET.
!
!    THEN WE CHOOSE, UNIFORMLY AT RANDOM, ONE POINT WITHIN EACH SUBCUBE
!    AS A "REPRESENTATIVE".  THIS SET OF POINTS CONSTITUTES THE
!    "LATIN RANDOM SQUARE" DATASET, A TERMINOLOGY I MADE UP TO
!    DISTINGUISH IT FROM THE "LATIN RANDOM SQUARE" AND "LATIN EDGE SQUARE"
!    DATASETS.
!
!  USAGE:
!
!    LATIN_RANDOM_DATASET M N SEED
!
!    WHERE
!
!    * M, THE SPATIAL DIMENSION,
!    * N, THE NUMBER OF POINTS TO GENERATE,
!    * SEED, THE SEED, A POSITIVE INTEGER.
!
!    CREATES AN M BY N DATASET AND WRITES IT TO THE
!    FILE "LATIN_RANDOM_M_N.TXT".
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE. 
!
!  MODIFIED:
!
!    12 NOVEMBER 2014
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: N,M
  INTEGER,INTENT(INOUT) :: SEED
  REAL(KIND=8),INTENT(OUT),DIMENSION(M,N) :: TABLE
  
!  INTEGER ( KIND = 4 ) ARG_NUM
!  INTEGER ( KIND = 4 ) IARG
!  INTEGER ( KIND = 4 ) IARGC
!  INTEGER ( KIND = 4 ) IERROR
!  INTEGER ( KIND = 4 ) IOS
!  INTEGER ( KIND = 4 ) LAST
!  INTEGER ( KIND = 4 ) M
!  INTEGER ( KIND = 4 ) N
!  CHARACTER ( LEN = 255 ) OUTPUT_FILENAME
  REAL ( KIND = 8 ), ALLOCATABLE, DIMENSION ( :, : ) :: R
!  INTEGER ( KIND = 4 ) SEED
!  CHARACTER ( LEN = 255 ) STRING

  CALL TIMESTAMP ( )

  WRITE ( *, '(A)' ) ' '
  WRITE ( *, '(A)' ) 'LATIN_RANDOM_DATASET'
  WRITE ( *, '(A)' ) '  FORTRAN90 VERSION'
  WRITE ( *, '(A)' ) '  GENERATE A LATIN RANDOM SQUARE DATASET.'
!
!  GET THE NUMBER OF COMMAND LINE ARGUMENTS.
!
!  ARG_NUM = IARGC ( )
!!
!!  GET THE SPATIAL DIMENSION M.
!!
!  IF ( 1 <= ARG_NUM ) THEN
!    IARG = 1
!    CALL GETARG ( IARG, STRING )
!    CALL S_TO_I4 ( STRING, M, IERROR, LAST )
!  ELSE
!    WRITE ( *, '(A)' ) ' '
!    WRITE ( *, '(A)' ) '  ENTER THE SPATIAL DIMENSION M (1 OR GREATER)'
!    READ ( *, * ) M
!  END IF

!M=2

  WRITE ( *, '(A)' ) ' '
  WRITE ( *, '(A,I8)' ) '  SPATIAL DIMENSION M = ', M
!!
!!  GET THE NUMBER OF POINTS N.
!!
!  IF ( 2 <= ARG_NUM ) THEN
!    IARG = 2
!    CALL GETARG ( IARG, STRING )
!    CALL S_TO_I4 ( STRING, N, IERROR, LAST )
!  ELSE
!    WRITE ( *, '(A)' ) ' '
!    WRITE ( *, '(A)' ) '  ENTER THE NUMBER OF POINTS N (1 OR GREATER)'
!    READ ( *, * ) N
!  END IF

!N=2

  WRITE ( *, '(A,I8)' ) '  NUMBER OF POINTS N = ', N
!
!  GET THE SEED, SEED
!
!  IF ( 3 <= ARG_NUM ) THEN
!    IARG = 3
!    CALL GETARG ( IARG, STRING )
!    CALL S_TO_I4 ( STRING, SEED, IERROR, LAST )
!  ELSE
!    WRITE ( *, '(A)' ) ' '
!    WRITE ( *, '(A)' ) '  ENTER THE SEED SEED (1 OR GREATER)'
!    READ ( *, * ) SEED
!  END IF

!SEED=2

  WRITE ( *, '(A,I12)' ) '  SEED SEED = ', SEED

  IF ( SEED == 0 ) THEN
    CALL GET_SEED ( SEED )
    WRITE ( *, '(A,I12)' ) '  RANDOMIZED SEED = ', SEED
  END IF
!
!  COMPUTE THE DATA
!
  ALLOCATE ( R(1:M,1:N) )

  CALL LATIN_RANDOM ( M, N, SEED, R )

  TABLE=R
!!
!!  WRITE IT TO A FILE.
!!
!  WRITE ( OUTPUT_FILENAME, '(A,I2.2,A,I5.5,A)' ) &
!    'LATIN_RANDOM_', M, '_', N, '.TXT'
!
!  CALL R8MAT_WRITE ( OUTPUT_FILENAME, M, N, R )
!
!  WRITE ( *, '(A)' ) ' '
!  WRITE ( *, '(A)' ) &
!    '  THE DATA WAS WRITTEN TO "' // TRIM ( OUTPUT_FILENAME ) // '".'
!!
!!  FREE MEMORY.
!!
  DEALLOCATE ( R )
!
!  TERMINATE.
!
!  WRITE ( *, '(A)' ) ' '
!  WRITE ( *, '(A)' ) 'LATIN_RANDOM_DATASET'
!  WRITE ( *, '(A)' ) '  NORMAL END OF EXECUTION.'
!  WRITE ( *, '(A)' ) ' '
!  CALL TIMESTAMP ( )
!
!  STOP
 END
 SUBROUTINE GET_SEED ( SEED )

!*****************************************************************************80
!
!! GET_SEED RETURNS A SEED FOR THE RANDOM NUMBER GENERATOR.
!
!  DISCUSSION:
!
!    THE SEED DEPENDS ON THE CURRENT TIME, AND OUGHT TO BE (SLIGHTLY)
!    DIFFERENT EVERY MILLISECOND.  ONCE THE SEED IS OBTAINED, A RANDOM
!    NUMBER GENERATOR SHOULD BE CALLED A FEW TIMES TO FURTHER PROCESS
!    THE SEED.
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE.
!
!  MODIFIED:
!
!    02 AUGUST 2004
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  PARAMETERS:
!
!    OUTPUT, INTEGER ( KIND = 4 ) SEED, A PSEUDORANDOM SEED VALUE.
!
  IMPLICIT NONE

  INTEGER(KIND=4),INTENT(OUT) :: SEED
  REAL ( KIND = 8 ) TEMP
  CHARACTER ( LEN = 10 ) TIME
  CHARACTER ( LEN = 8 ) TODAY
  INTEGER ( KIND = 4 ) VALUES(8)
  CHARACTER ( LEN = 5 ) ZONE

  CALL DATE_AND_TIME ( TODAY, TIME, ZONE, VALUES )

  TEMP = 0.0D+00

  TEMP = TEMP + REAL ( VALUES(2) - 1, KIND = 8 ) /  11.0D+00
  TEMP = TEMP + REAL ( VALUES(3) - 1, KIND = 8 ) /  30.0D+00
  TEMP = TEMP + REAL ( VALUES(5),     KIND = 8 ) /  23.0D+00
  TEMP = TEMP + REAL ( VALUES(6),     KIND = 8 ) /  59.0D+00
  TEMP = TEMP + REAL ( VALUES(7),     KIND = 8 ) /  59.0D+00
  TEMP = TEMP + REAL ( VALUES(8),     KIND = 8 ) / 999.0D+00
  TEMP = TEMP                                    /   6.0D+00

  DO WHILE ( TEMP <= 0.0D+00 )
    TEMP = TEMP + 1.0D+00
  END DO

  DO WHILE ( 1.0D+00 < TEMP )
    TEMP = TEMP - 1.0D+00
  END DO

  SEED = INT ( REAL ( HUGE ( 1 ), KIND = 8 ) * TEMP )
!
!  NEVER USE A SEED OF 0 OR MAXIMUM INTEGER ( KIND = 4 ).
!
  IF ( SEED == 0 ) THEN
    SEED = 1
  END IF

  IF ( SEED == HUGE ( 1 ) ) THEN
    SEED = SEED - 1
  END IF

  RETURN
END
SUBROUTINE GET_UNIT ( IUNIT )

!*****************************************************************************80
!
!! GET_UNIT RETURNS A FREE FORTRAN UNIT NUMBER.
!
!  DISCUSSION:
!
!    A "FREE" FORTRAN UNIT NUMBER IS A VALUE BETWEEN 1 AND 99 WHICH
!    IS NOT CURRENTLY ASSOCIATED WITH AN I/O DEVICE.  A FREE FORTRAN UNIT
!    NUMBER IS NEEDED IN ORDER TO OPEN A FILE WITH THE OPEN COMMAND.
!
!    IF IUNIT = 0, THEN NO FREE FORTRAN UNIT COULD BE FOUND, ALTHOUGH
!    ALL 99 UNITS WERE CHECKED (EXCEPT FOR UNITS 5, 6 AND 9, WHICH
!    ARE COMMONLY RESERVED FOR CONSOLE I/O).
!
!    OTHERWISE, IUNIT IS A VALUE BETWEEN 1 AND 99, REPRESENTING A
!    FREE FORTRAN UNIT.  NOTE THAT GET_UNIT ASSUMES THAT UNITS 5 AND 6
!    ARE SPECIAL, AND WILL NEVER RETURN THOSE VALUES.
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE.
!
!  MODIFIED:
!
!    18 SEPTEMBER 2005
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  PARAMETERS:
!
!    OUTPUT, INTEGER ( KIND = 4 ) IUNIT, THE FREE UNIT NUMBER.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  LOGICAL LOPEN

  IUNIT = 0

  DO I = 1, 99

    IF ( I /= 5 .AND. I /= 6 .AND. I /= 9 ) THEN

      INQUIRE ( UNIT = I, OPENED = LOPEN, IOSTAT = IOS )

      IF ( IOS == 0 ) THEN
        IF ( .NOT. LOPEN ) THEN
          IUNIT = I
          RETURN
        END IF
      END IF

    END IF

  END DO

  RETURN
END
FUNCTION I4_UNIFORM_AB ( A, B, SEED )

!*****************************************************************************80
!
!! I4_UNIFORM_AB RETURNS A SCALED PSEUDORANDOM I4 BETWEEN A AND B.
!
!  DISCUSSION:
!
!    AN I4 IS AN INTEGER ( KIND = 4 ) VALUE.
!
!    THE PSEUDORANDOM NUMBER WILL BE SCALED TO BE UNIFORMLY DISTRIBUTED
!    BETWEEN A AND B.
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE. 
!
!  MODIFIED:
!
!    02 OCTOBER 2012
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  REFERENCE:
!
!    PAUL BRATLEY, BENNETT FOX, LINUS SCHRAGE,
!    A GUIDE TO SIMULATION,
!    SECOND EDITION,
!    SPRINGER, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    BENNETT FOX,
!    ALGORITHM 647:
!    IMPLEMENTATION AND RELATIVE EFFICIENCY OF QUASIRANDOM
!    SEQUENCE GENERATORS,
!    ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!    VOLUME 12, NUMBER 4, DECEMBER 1986, PAGES 362-376.
!
!    PIERRE L'ECUYER,
!    RANDOM NUMBER GENERATION,
!    IN HANDBOOK OF SIMULATION,
!    EDITED BY JERRY BANKS,
!    WILEY, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    PETER LEWIS, ALLEN GOODMAN, JAMES MILLER,
!    A PSEUDO-RANDOM NUMBER GENERATOR FOR THE SYSTEM/360,
!    IBM SYSTEMS JOURNAL,
!    VOLUME 8, NUMBER 2, 1969, PAGES 136-143.
!
!  PARAMETERS:
!
!    INPUT, INTEGER ( KIND = 4 ) A, B, THE LIMITS OF THE INTERVAL.
!
!    INPUT/OUTPUT, INTEGER ( KIND = 4 ) SEED, THE "SEED" VALUE, WHICH
!    SHOULD NOT BE 0.  ON OUTPUT, SEED HAS BEEN UPDATED.
!
!    OUTPUT, INTEGER ( KIND = 4 ) I4_UNIFORM_AB, A NUMBER BETWEEN A AND B.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) A
  INTEGER ( KIND = 4 ) B
  INTEGER ( KIND = 4 ), PARAMETER :: I4_HUGE = 2147483647
  INTEGER ( KIND = 4 ) I4_UNIFORM_AB
  INTEGER ( KIND = 4 ) K
  REAL ( KIND = 4 ) R
  INTEGER ( KIND = 4 ) SEED
  INTEGER ( KIND = 4 ) VALUE

  IF ( SEED == 0 ) THEN
    WRITE ( *, '(A)' ) ' '
    WRITE ( *, '(A)' ) 'I4_UNIFORM_AB - FATAL ERROR!'
    WRITE ( *, '(A)' ) '  INPUT VALUE OF SEED = 0.'
    STOP 1
  END IF

  K = SEED / 127773

  SEED = 16807 * ( SEED - K * 127773 ) - K * 2836

  IF ( SEED < 0 ) THEN
    SEED = SEED + I4_HUGE
  END IF

  R = REAL ( SEED, KIND = 4 ) * 4.656612875E-10
!
!  SCALE R TO LIE BETWEEN A-0.5 AND B+0.5.
!
  R = ( 1.0E+00 - R ) * ( REAL ( MIN ( A, B ), KIND = 4 ) - 0.5E+00 ) & 
    +             R   * ( REAL ( MAX ( A, B ), KIND = 4 ) + 0.5E+00 )
!
!  USE ROUNDING TO CONVERT R TO AN INTEGER BETWEEN A AND B.
!
  VALUE = NINT ( R, KIND = 4 )

  VALUE = MAX ( VALUE, MIN ( A, B ) )
  VALUE = MIN ( VALUE, MAX ( A, B ) )

  I4_UNIFORM_AB = VALUE

  RETURN
END
SUBROUTINE LATIN_RANDOM ( DIM_NUM, POINT_NUM, SEED, X )

!*****************************************************************************80
!
!! LATIN_RANDOM RETURNS POINTS IN A LATIN RANDOM SQUARE.
!
!  DISCUSSION:
!
!    IN EACH SPATIAL DIMENSION, THERE WILL BE EXACTLY ONE
!    POINT WHOSE COORDINATE VALUE LIES BETWEEN CONSECUTIVE
!    VALUES IN THE LIST:
!
!      ( 0, 1, 2, ..., POINT_NUM ) / POINT_NUM
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE.
!
!  MODIFIED:
!
!    08 APRIL 2003
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  PARAMETERS:
!
!    INPUT, INTEGER ( KIND = 4 ) DIM_NUM, THE SPATIAL DIMENSION.
!
!    INPUT, INTEGER ( KIND = 4 ) POINT_NUM, THE NUMBER OF POINTS.
!
!    INPUT/OUTPUT, INTEGER ( KIND = 4 ) SEED, A SEED FOR THE RANDOM
!    NUMBER GENERATOR.
!
!    OUTPUT, REAL ( KIND = 8 ) X(DIM_NUM,POINT_NUM), THE POINTS.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) DIM_NUM
  INTEGER ( KIND = 4 ) POINT_NUM

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) PERM(POINT_NUM)
!  REAL ( KIND = 8 ) R8_UNIFORM_01
  INTEGER ( KIND = 4 ) SEED
  REAL ( KIND = 8 ) X(DIM_NUM,POINT_NUM)
!
!  PICK DIM_NUM * POINT_NUM RANDOM NUMBERS BETWEEN 0 AND 1.
!
  CALL R8MAT_UNIFORM_01 ( DIM_NUM, POINT_NUM, SEED, X )
!
!  FOR SPATIAL DIMENSION I,
!    PICK A RANDOM PERMUTATION OF 1 TO POINT_NUM,
!    FORCE THE CORRESPONDING I-TH COMPONENTS OF X TO LIE IN THE
!    INTERVAL ( PERM(J)-1, PERM(J) ) / POINT_NUM.
!
  DO I = 1, DIM_NUM

    CALL PERM_UNIFORM ( POINT_NUM, SEED, PERM )

    DO J = 1, POINT_NUM
      X(I,J) = ( REAL ( PERM(J) - 1, KIND = 8 ) + X(I,J) ) &
               / REAL ( POINT_NUM, KIND = 8 )
    END DO

  END DO

  RETURN
END
SUBROUTINE PERM_UNIFORM ( N, SEED, P )

!*****************************************************************************80
!
!! PERM_UNIFORM SELECTS A RANDOM PERMUTATION OF N OBJECTS.
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE.
!
!  MODIFIED:
!
!    18 NOVEMBER 2008
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  REFERENCE:
!
!    ALBERT NIJENHUIS, HERBERT WILF,
!    COMBINATORIAL ALGORITHMS FOR COMPUTERS AND CALCULATORS,
!    ACADEMIC PRESS, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  PARAMETERS:
!
!    INPUT, INTEGER ( KIND = 4 ) N, THE NUMBER OF OBJECTS TO BE PERMUTED.
!
!    INPUT/OUTPUT, INTEGER ( KIND = 4 ) SEED, A SEED FOR THE RANDOM
!    NUMBER GENERATOR.
!
!    OUTPUT, INTEGER ( KIND = 4 ) P(N), THE PERMUTATION.  P(I) IS THE "NEW"
!    LOCATION OF THE OBJECT ORIGINALLY AT I.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
!  INTEGER ( KIND = 4 ) I4_UNIFORM_AB
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) K
  INTEGER ( KIND = 4 ) P(N)
  INTEGER ( KIND = 4 ) SEED

  DO I = 1, N
    P(I) = I
  END DO

  DO I = 1, N - 1
    J = I4_UNIFORM_AB ( I, N, SEED )
    K    = P(I)
    P(I) = P(J)
    P(J) = K
  END DO

  RETURN
END
SUBROUTINE R8MAT_UNIFORM_01 ( M, N, SEED, R )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_01 FILLS AN R8MAT WITH UNIT PSEUDORANDOM NUMBERS.
!
!  DISCUSSION:
!
!    AN R8MAT IS AN MXN ARRAY OF R8'S, STORED BY (I,J) -> [I+J*M].
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE.
!
!  MODIFIED:
!
!    11 AUGUST 2004
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  REFERENCE:
!
!    PAUL BRATLEY, BENNETT FOX, LINUS SCHRAGE,
!    A GUIDE TO SIMULATION,
!    SPRINGER VERLAG, PAGES 201-202, 1983.
!
!    BENNETT FOX,
!    ALGORITHM 647:
!    IMPLEMENTATION AND RELATIVE EFFICIENCY OF QUASIRANDOM
!    SEQUENCE GENERATORS,
!    ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!    VOLUME 12, NUMBER 4, PAGES 362-376, 1986.
!
!    PETER LEWIS, ALLEN GOODMAN, JAMES MILLER,
!    A PSEUDO-RANDOM NUMBER GENERATOR FOR THE SYSTEM/360,
!    IBM SYSTEMS JOURNAL,
!    VOLUME 8, PAGES 136-143, 1969.
!
!  PARAMETERS:
!
!    INPUT, INTEGER ( KIND = 4 ) M, N, THE NUMBER OF ROWS AND COLUMNS IN
!    THE ARRAY.
!
!    INPUT/OUTPUT, INTEGER ( KIND = 4 ) SEED, THE "SEED" VALUE, WHICH
!    SHOULD NOT BE 0.  ON OUTPUT, SEED HAS BEEN UPDATED.
!
!    OUTPUT, REAL ( KIND = 8 ) R(M,N), THE ARRAY OF PSEUDORANDOM VALUES.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) M
  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ), PARAMETER :: I4_HUGE = 2147483647
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) K
  INTEGER ( KIND = 4 ) SEED
  REAL ( KIND = 8 ) R(M,N)

  DO J = 1, N

    DO I = 1, M

      K = SEED / 127773

      SEED = 16807 * ( SEED - K * 127773 ) - K * 2836

      IF ( SEED < 0 ) THEN
        SEED = SEED + I4_HUGE
      END IF

      R(I,J) = REAL ( SEED, KIND = 8 ) * 4.656612875D-10

    END DO
  END DO

  RETURN
END
SUBROUTINE R8MAT_WRITE ( OUTPUT_FILENAME, M, N, TABLE )

!*****************************************************************************80
!
!! R8MAT_WRITE WRITES AN R8MAT FILE.
!
!  DISCUSSION:
!
!    AN R8MAT IS AN ARRAY OF R8 VALUES.
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE.
!
!  MODIFIED:
!
!    31 MAY 2009
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  PARAMETERS:
!
!    INPUT, CHARACTER ( LEN = * ) OUTPUT_FILENAME, THE OUTPUT FILE NAME.
!
!    INPUT, INTEGER ( KIND = 4 ) M, THE SPATIAL DIMENSION.
!
!    INPUT, INTEGER ( KIND = 4 ) N, THE NUMBER OF POINTS.
!
!    INPUT, REAL ( KIND = 8 ) TABLE(M,N), THE TABLE DATA.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) M
  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = * ) OUTPUT_FILENAME
  INTEGER ( KIND = 4 ) OUTPUT_STATUS
  INTEGER ( KIND = 4 ) OUTPUT_UNIT
  CHARACTER ( LEN = 30 ) STRING
  REAL ( KIND = 8 ) TABLE(M,N)
!
!  OPEN THE FILE.
!
  CALL GET_UNIT ( OUTPUT_UNIT )

  OPEN ( UNIT = OUTPUT_UNIT, FILE = OUTPUT_FILENAME, &
    STATUS = 'REPLACE', IOSTAT = OUTPUT_STATUS )

  IF ( OUTPUT_STATUS /= 0 ) THEN
    WRITE ( *, '(A)' ) ' '
    WRITE ( *, '(A)' ) 'R8MAT_WRITE - FATAL ERROR!'
    WRITE ( *, '(A,I8)' ) '  COULD NOT OPEN THE OUTPUT FILE "' // &
      TRIM ( OUTPUT_FILENAME ) // '" ON UNIT ', OUTPUT_UNIT
    OUTPUT_UNIT = -1
    STOP
  END IF
!
!  CREATE A FORMAT STRING.
!
!  FOR LESS PRECISION IN THE OUTPUT FILE, TRY:
!
!                                            '(', M, 'G', 14, '.', 6, ')'
!
  IF ( 0 < M .AND. 0 < N ) THEN

    WRITE ( STRING, '(A1,I8,A1,I8,A1,I8,A1)' ) '(', M, 'G', 24, '.', 16, ')'
!
!  WRITE THE DATA.
!
    DO J = 1, N
      WRITE ( OUTPUT_UNIT, STRING ) TABLE(1:M,J)
    END DO

  END IF
!
!  CLOSE THE FILE.
!
  CLOSE ( UNIT = OUTPUT_UNIT )

  RETURN
END
SUBROUTINE S_TO_I4 ( S, VALUE, IERROR, LENGTH )

!*****************************************************************************80
!
!! S_TO_I4 READS AN INTEGER VALUE FROM A STRING.
!
!  DISCUSSION:
!
!    INSTEAD OF ICHAR, WE NOW USE THE IACHAR FUNCTION, WHICH
!    GUARANTEES THE ASCII COLLATING SEQUENCE.
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE. 
!
!  MODIFIED:
!
!    12 JANUARY 2009
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  PARAMETERS:
!
!    INPUT, CHARACTER ( LEN = * ) S, A STRING TO BE EXAMINED.
!
!    OUTPUT, INTEGER ( KIND = 4 ) VALUE, THE INTEGER VALUE READ FROM THE STRING.
!    IF THE STRING IS BLANK, THEN VALUE WILL BE RETURNED 0.
!
!    OUTPUT, INTEGER ( KIND = 4 ) IERROR, AN ERROR FLAG.
!    0, NO ERROR.
!    1, AN ERROR OCCURRED.
!
!    OUTPUT, INTEGER ( KIND = 4 ) LENGTH, THE NUMBER OF CHARACTERS 
!    OF S USED TO MAKE THE INTEGER.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) STATE
  CHARACTER :: TAB = ACHAR ( 9 )
  INTEGER ( KIND = 4 ) VALUE

  VALUE = 0
  IERROR = 0
  LENGTH = 0

  STATE = 0
  ISGN = 1

  DO I = 1, LEN_TRIM ( S )

    C = S(I:I)
!
!  STATE = 0, HAVEN'T READ ANYTHING.
!
    IF ( STATE == 0 ) THEN

      IF ( C == ' ' .OR. C == TAB ) THEN

      ELSE IF ( C == '-' ) THEN
        STATE = 1
        ISGN = -1
      ELSE IF ( C == '+' ) THEN
        STATE = 1
        ISGN = +1
      ELSE IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN
        STATE = 2
        VALUE = IACHAR ( C ) - IACHAR ( '0' )
      ELSE
        IERROR = 1
        RETURN
      END IF
!
!  STATE = 1, HAVE READ THE SIGN, EXPECTING DIGITS OR SPACES.
!
    ELSE IF ( STATE == 1 ) THEN

      IF ( C == ' ' .OR. C == TAB ) THEN

      ELSE IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN
        STATE = 2
        VALUE = IACHAR ( C ) - IACHAR ( '0' )
      ELSE
        IERROR = 1
        RETURN
      END IF
!
!  STATE = 2, HAVE READ AT LEAST ONE DIGIT, EXPECTING MORE.
!
    ELSE IF ( STATE == 2 ) THEN

      IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN

        VALUE = 10 * VALUE + IACHAR ( C ) - IACHAR ( '0' )

      ELSE

        VALUE = ISGN * VALUE
        IERROR = 0
        LENGTH = I - 1
        RETURN

      END IF

    END IF

  END DO
!
!  IF WE READ ALL THE CHARACTERS IN THE STRING, SEE IF WE'RE OK.
!
  IF ( STATE == 2 ) THEN

    VALUE = ISGN * VALUE
    IERROR = 0
    LENGTH = LEN_TRIM ( S )

  ELSE

    VALUE = 0
    IERROR = 1
    LENGTH = 0

  END IF

  RETURN
END
SUBROUTINE TIMESTAMP ( )

!*****************************************************************************80
!
!! TIMESTAMP PRINTS THE CURRENT YMDHMS DATE AS A TIME STAMP.
!
!  EXAMPLE:
!
!    31 MAY 2001   9:45:54.872 AM
!
!  LICENSING:
!
!    THIS CODE IS DISTRIBUTED UNDER THE GNU LGPL LICENSE.
!
!  MODIFIED:
!
!    18 MAY 2013
!
!  AUTHOR:
!
!    JOHN BURKARDT
!
!  PARAMETERS:
!
!    NONE
!
  IMPLICIT NONE

  CHARACTER ( LEN = 8 ) AMPM
  INTEGER ( KIND = 4 ) D
  INTEGER ( KIND = 4 ) H
  INTEGER ( KIND = 4 ) M
  INTEGER ( KIND = 4 ) MM
  CHARACTER ( LEN = 9 ), PARAMETER, DIMENSION(12) :: MONTH = (/ &
    'JANUARY  ', 'FEBRUARY ', 'MARCH    ', 'APRIL    ', &
    'MAY      ', 'JUNE     ', 'JULY     ', 'AUGUST   ', &
    'SEPTEMBER', 'OCTOBER  ', 'NOVEMBER ', 'DECEMBER ' /)
  INTEGER ( KIND = 4 ) N
  INTEGER ( KIND = 4 ) S
  INTEGER ( KIND = 4 ) VALUES(8)
  INTEGER ( KIND = 4 ) Y

  CALL DATE_AND_TIME ( VALUES = VALUES )

  Y = VALUES(1)
  M = VALUES(2)
  D = VALUES(3)
  H = VALUES(5)
  N = VALUES(6)
  S = VALUES(7)
  MM = VALUES(8)

  IF ( H < 12 ) THEN
    AMPM = 'AM'
  ELSE IF ( H == 12 ) THEN
    IF ( N == 0 .AND. S == 0 ) THEN
      AMPM = 'NOON'
    ELSE
      AMPM = 'PM'
    END IF
  ELSE
    H = H - 12
    IF ( H < 12 ) THEN
      AMPM = 'PM'
    ELSE IF ( H == 12 ) THEN
      IF ( N == 0 .AND. S == 0 ) THEN
        AMPM = 'MIDNIGHT'
      ELSE
        AMPM = 'AM'
      END IF
    END IF
  END IF

  WRITE ( *, '(I2.2,1X,A,1X,I4,2X,I2,A1,I2.2,A1,I2.2,A1,I3.3,1X,A)' ) &
    D, TRIM ( MONTH(M) ), Y, H, ':', N, ':', S, '.', MM, TRIM ( AMPM )

  RETURN
END

END MODULE MOD_LHC