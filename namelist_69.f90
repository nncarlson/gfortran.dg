! { dg-do run }
!
! PR fortran/47339
! PR fortran/43062
!
! Run-time test for Fortran 2003 NAMELISTS
! Version for non-strings
!
program nml_test
  implicit none

  character(len=1000) :: str

  integer, allocatable :: a(:)
  integer, allocatable :: b
  integer, pointer :: ap(:)
  integer, pointer :: bp
  integer :: c
  integer :: d(3)

  type t
    integer :: c1
    integer :: c2(3)
  end type t
  type(t) :: e,f(2)
  type(t),allocatable :: g,h(:)
  type(t),pointer :: i,j(:)

  namelist /nml/ a, b, c, d, ap, bp,e,f,g,h,i,j

  a = [1,2]
  allocate(b,ap(2),bp)
  ap = [98, 99]
  b = 7
  bp = 101
  c = 8
  d = [-1, -2, -3]

  e%c1 = -701
  e%c2 = [-702,-703,-704]
  f(1)%c1 = 33001
  f(2)%c1 = 33002
  f(1)%c2 = [44001,44002,44003]
  f(2)%c2 = [44011,44012,44013]

  allocate(g,h(2),i,j(2))

  g%c1 = -601
  g%c2 = [-602,6703,-604]
  h(1)%c1 = 35001
  h(2)%c1 = 35002
  h(1)%c2 = [45001,45002,45003]
  h(2)%c2 = [45011,45012,45013]

  i%c1 = -501
  i%c2 = [-502,-503,-504]
  j(1)%c1 = 36001
  j(2)%c1 = 36002
  j(1)%c2 = [46001,46002,46003]
  j(2)%c2 = [46011,46012,46013]

  ! SAVE NAMELIST
  str = repeat('X', len(str))
  write(str,nml=nml)

  ! RESET NAMELIST
  a = [-1,-1]
  ap = [-1, -1]
  b = -1
  bp = -1
  c = -1
  d = [-1, -1, -1]

  e%c1 = -1
  e%c2 = [-1,-1,-1]
  f(1)%c1 = -1
  f(2)%c1 = -1
  f(1)%c2 = [-1,-1,-1]
  f(2)%c2 = [-1,-1,-1]

  g%c1 = -1
  g%c2 = [-1,-1,-1]
  h(1)%c1 = -1
  h(2)%c1 = -1
  h(1)%c2 = [-1,-1,-1]
  h(2)%c2 = [-1,-1,-1]

  i%c1 = -1
  i%c2 = [-1,-1,-1]
  j(1)%c1 = -1
  j(2)%c1 = -1
  j(1)%c2 = [-1,-1,-1]
  j(2)%c2 = [-1,-1,-1]

  ! Read back
  read(str,nml=nml)

  ! Check result
  if (any (a /= [1,2])) stop 1
  if (any (ap /= [98, 99])) stop 1
  if (b /= 7) stop 1
  if (bp /= 101) stop 1
  if (c /= 8) stop 1
  if (any (d /= [-1, -2, -3])) stop 1

  if (e%c1 /= -701) stop 1
  if (any (e%c2 /= [-702,-703,-704])) stop 1
  if (f(1)%c1 /= 33001) stop 1
  if (f(2)%c1 /= 33002) stop 1
  if (any (f(1)%c2 /= [44001,44002,44003])) stop 1
  if (any (f(2)%c2 /= [44011,44012,44013])) stop 1

  if (g%c1 /= -601) stop 1
  if (any(g%c2 /= [-602,6703,-604])) stop 1
  if (h(1)%c1 /= 35001) stop 1
  if (h(2)%c1 /= 35002) stop 1
  if (any (h(1)%c2 /= [45001,45002,45003])) stop 1
  if (any (h(2)%c2 /= [45011,45012,45013])) stop 1

  if (i%c1 /= -501) stop 1
  if (any (i%c2 /= [-502,-503,-504])) stop 1
  if (j(1)%c1 /= 36001) stop 1
  if (j(2)%c1 /= 36002) stop 1
  if (any (j(1)%c2 /= [46001,46002,46003])) stop 1
  if (any (j(2)%c2 /= [46011,46012,46013])) stop 1

  ! Check argument passing (dummy processing)
  call test2(a,b,c,d,ap,bp,e,f,g,h,i,j,2) 

contains
  subroutine test2(x1,x2,x3,x4,x1p,x2p,x6,x7,x8,x9,x10,x11,n)
    integer, allocatable :: x1(:)
    integer, allocatable :: x2
    integer, pointer :: x1p(:)
    integer, pointer :: x2p
    integer :: x3
    integer :: x4(3)
    integer :: n
    integer :: x5(n)
    type(t) :: x6,x7(2)
    type(t),allocatable :: x8,x9(:)
    type(t),pointer :: x10,x11(:)
    type(t) :: x12(n)

    namelist /nml2/ x1, x2, x3, x4,x5,x1p,x2p,x6,x7,x8,x9,x10,x11,x12

    x5 = [ 42, 53 ]

    x12(1)%c1 = 37001
    x12(2)%c1 = 37002
    x12(1)%c2 = [47001,47002,47003]
    x12(2)%c2 = [47011,47012,47013]

    ! SAVE NAMELIST
    str = repeat('X', len(str))
    write(str,nml=nml2)

    ! RESET NAMELIST
    x1 = [-1,-1]
    x1p = [-1, -1]
    x2 = -1
    x2p = -1
    x3 = -1
    x4 = [-1, -1, -1]

    x6%c1 = -1
    x6%c2 = [-1,-1,-1]
    x7(1)%c1 = -1
    x7(2)%c1 = -1
    x7(1)%c2 = [-1,-1,-1]
    x7(2)%c2 = [-1,-1,-1]

    x8%c1 = -1
    x8%c2 = [-1,-1,-1]
    x9(1)%c1 = -1
    x9(2)%c1 = -1
    x9(1)%c2 = [-1,-1,-1]
    x9(2)%c2 = [-1,-1,-1]

    x10%c1 = -1
    x10%c2 = [-1,-1,-1]
    x11(1)%c1 = -1
    x11(2)%c1 = -1
    x11(1)%c2 = [-1,-1,-1]
    x11(2)%c2 = [-1,-1,-1]

    x5 = [ -1, -1 ]

    x12(1)%c1 = -1
    x12(2)%c1 = -1
    x12(1)%c2 = [-1,-1,-1]
    x12(2)%c2 = [-1,-1,-1]

    ! Read back
    read(str,nml=nml2)

    ! Check result
    if (any (x1 /= [1,2])) stop 1
    if (any (x1p /= [98, 99])) stop 1
    if (x2 /= 7) stop 1
    if (x2p /= 101) stop 1
    if (x3 /= 8) stop 1
    if (any (x4 /= [-1, -2, -3])) stop 1

    if (x6%c1 /= -701) stop 1
    if (any (x6%c2 /= [-702,-703,-704])) stop 1
    if (x7(1)%c1 /= 33001) stop 1
    if (x7(2)%c1 /= 33002) stop 1
    if (any (x7(1)%c2 /= [44001,44002,44003])) stop 1
    if (any (x7(2)%c2 /= [44011,44012,44013])) stop 1

    if (x8%c1 /= -601) stop 1
    if (any(x8%c2 /= [-602,6703,-604])) stop 1
    if (x9(1)%c1 /= 35001) stop 1
    if (x9(2)%c1 /= 35002) stop 1
    if (any (x9(1)%c2 /= [45001,45002,45003])) stop 1
    if (any (x9(2)%c2 /= [45011,45012,45013])) stop 1

    if (x10%c1 /= -501) stop 1
    if (any (x10%c2 /= [-502,-503,-504])) stop 1
    if (x11(1)%c1 /= 36001) stop 1
    if (x11(2)%c1 /= 36002) stop 1
    if (any (x11(1)%c2 /= [46001,46002,46003])) stop 1
    if (any (x11(2)%c2 /= [46011,46012,46013])) stop 1

    if (any (x5 /= [ 42, 53 ])) stop 1

    if (x12(1)%c1 /= 37001) stop 1
    if (x12(2)%c1 /= 37002) stop 1
    if (any (x12(1)%c2 /= [47001,47002,47003])) stop 1
    if (any (x12(2)%c2 /= [47011,47012,47013])) stop 1
  end subroutine test2
end program nml_test
