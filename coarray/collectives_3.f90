! { dg-do run }
!
! CO_BROADCAST
!
program test
  implicit none
  intrinsic co_broadcast

  type t
    integer :: i
    character(len=1) :: c
    real(8) :: x(3), y(3)
  end type t

  integer :: i, j(10), stat
  complex :: a(5,5)
  character(kind=1, len=5) :: str1, errstr
  character(kind=4, len=8) :: str2(2)
  type(t) :: dt(4)

  i = 1
  j = 55
  a = 99.0
  str1 = 1_"XXXXX"
  str2 = 4_"YYYYYYYY"
  dt = t(1, 'C', [1.,2.,3.], [3,3,3])
  errstr = "ZZZZZ"

  if (this_image() == num_images()) then
    i = 2
    j = 66
    a = -99.0
    str1 = 1_"abcd"
    str2 = 4_"12 3 4 5"
    dt = t(-1, 'a', [3.,1.,8.], [99,24,5])
  end if
  sync all

  call co_broadcast(i, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (i /= 2) stop 1

  call co_broadcast(j, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (any (j /= 66)) stop 1

  call co_broadcast(a, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (any (a /= -99.0)) stop 1

  call co_broadcast(str1, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (str1 /= "abcd") stop 1

  call co_broadcast(str2, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (any (str2 /= 4_"12 3 4 5")) stop 1

  call co_broadcast(dt, source_image=num_images(), stat=stat, errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (any (dt(:)%i /= -1)) stop 1
  if (any (dt(:)%c /= 'a')) stop 1
  if (any (dt(:)%x(1) /= 3.)) stop 1
  if (any (dt(:)%x(2) /= 1.)) stop 1
  if (any (dt(:)%x(3) /= 8.)) stop 1
  if (any (dt(:)%y(1) /= 99.)) stop 1
  if (any (dt(:)%y(2) /= 24.)) stop 1
  if (any (dt(:)%y(3) /= 5.)) stop 1

  sync all
  dt = t(1, 'C', [1.,2.,3.], [3,3,3])
  sync all
  if (this_image() == num_images()) then
    str2 = 4_"001122"
    dt(2:4) = t(-2, 'i', [9.,2.,3.], [4,44,321])
  end if

  call co_broadcast(str2(::2), source_image=num_images(), stat=stat, &
                    errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (str2(1) /= 4_"001122") stop 1
  if (this_image() == num_images()) then
    if (str2(1) /= 4_"001122") stop 1
  else
    if (str2(2) /= 4_"12 3 4 5") stop 1
  end if

  call co_broadcast(dt(2::2), source_image=num_images(), stat=stat, &
                    errmsg=errstr)
  if (stat /= 0) stop 1
  if (errstr /= "ZZZZZ") stop 1
  if (this_image() == num_images()) then
    if (any (dt(1:1)%i /= 1)) stop 1
    if (any (dt(1:1)%c /= 'C')) stop 1
    if (any (dt(1:1)%x(1) /= 1.)) stop 1
    if (any (dt(1:1)%x(2) /= 2.)) stop 1
    if (any (dt(1:1)%x(3) /= 3.)) stop 1
    if (any (dt(1:1)%y(1) /= 3.)) stop 1
    if (any (dt(1:1)%y(2) /= 3.)) stop 1
    if (any (dt(1:1)%y(3) /= 3.)) stop 1

    if (any (dt(2:)%i /= -2)) stop 1
    if (any (dt(2:)%c /= 'i')) stop 1
    if (any (dt(2:)%x(1) /= 9.)) stop 1
    if (any (dt(2:)%x(2) /= 2.)) stop 1
    if (any (dt(2:)%x(3) /= 3.)) stop 1
    if (any (dt(2:)%y(1) /= 4.)) stop 1
    if (any (dt(2:)%y(2) /= 44.)) stop 1
    if (any (dt(2:)%y(3) /= 321.)) stop 1
  else
    if (any (dt(1::2)%i /= 1)) stop 1
    if (any (dt(1::2)%c /= 'C')) stop 1
    if (any (dt(1::2)%x(1) /= 1.)) stop 1
    if (any (dt(1::2)%x(2) /= 2.)) stop 1
    if (any (dt(1::2)%x(3) /= 3.)) stop 1
    if (any (dt(1::2)%y(1) /= 3.)) stop 1
    if (any (dt(1::2)%y(2) /= 3.)) stop 1
    if (any (dt(1::2)%y(3) /= 3.)) stop 1

    if (any (dt(2::2)%i /= -2)) stop 1
    if (any (dt(2::2)%c /= 'i')) stop 1
    if (any (dt(2::2)%x(1) /= 9.)) stop 1
    if (any (dt(2::2)%x(2) /= 2.)) stop 1
    if (any (dt(2::2)%x(3) /= 3.)) stop 1
    if (any (dt(2::2)%y(1) /= 4.)) stop 1
    if (any (dt(2::2)%y(2) /= 44.)) stop 1
    if (any (dt(2::2)%y(3) /= 321.)) stop 1
  endif
end program test
