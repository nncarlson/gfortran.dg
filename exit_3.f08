! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! PR fortran/44602
! Check for correct behavior of EXIT / CYCLE combined with non-loop
! constructs at run-time.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

  TYPE :: t
  END TYPE t

  INTEGER :: i
  CLASS(t), ALLOCATABLE :: var

  ! EXIT and CYCLE without names always refer to innermost *loop*.  This
  ! however is checked at run-time already in exit_1.f08.

  ! Basic EXITs from different non-loop constructs.

  i = 2
  myif: IF (i == 1) THEN
    stop 1
    EXIT myif
  ELSE IF (i == 2) THEN
    EXIT myif
    stop 1
  ELSE
    stop 1
    EXIT myif
  END IF myif

  mysel: SELECT CASE (i)
    CASE (1)
      stop 1
      EXIT mysel
    CASE (2)
      EXIT mysel
      stop 1
    CASE DEFAULT
      stop 1
      EXIT mysel
  END SELECT mysel

  mycharsel: SELECT CASE ("foobar")
    CASE ("abc")
      stop 1
      EXIT mycharsel
    CASE ("xyz")
      stop 1
      EXIT mycharsel
    CASE DEFAULT
      EXIT mycharsel
      stop 1
  END SELECT mycharsel

  myblock: BLOCK
    EXIT myblock
    stop 1
  END BLOCK myblock

  myassoc: ASSOCIATE (x => 5 + 2)
    EXIT myassoc
    stop 1
  END ASSOCIATE myassoc

  ALLOCATE (t :: var)
  mytypesel: SELECT TYPE (var)
    TYPE IS (t)
      EXIT mytypesel
      stop 1
    CLASS DEFAULT
      stop 1
      EXIT mytypesel
  END SELECT mytypesel

  ! Check EXIT with nested constructs.
  outer: BLOCK
    inner: IF (.TRUE.) THEN
      EXIT outer
      stop 1
    END IF inner
    stop 1
  END BLOCK outer
END PROGRAM main
