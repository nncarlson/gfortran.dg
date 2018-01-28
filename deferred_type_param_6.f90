! { dg-do run }
!
! PR fortran/51055
! PR fortran/49110
! PR fortran/60334

subroutine test()
  implicit none
  integer :: i = 5
  character(len=:), allocatable :: s1
  character(len=:), pointer :: s2
  character(len=5), target :: fifeC = 'FIVEC'
  call sub(s1, i)
  if (len(s1) /= 5) stop 1
  if (s1 /= "ZZZZZ") stop 1
  s2 => subfunc()
  if (len(s2) /= 5) stop 1
  if (s2 /= "FIVEC") stop 1
  s1 = addPrefix(subfunc())
  if (len(s1) /= 7) stop 1
  if (s1 /= "..FIVEC") stop 1
contains
  subroutine sub(str,j)
    character(len=:), allocatable :: str
    integer :: j
    str = REPEAT("Z",j)
    if (len(str) /= 5) stop 1
    if (str /= "ZZZZZ") stop 1
  end subroutine sub
  function subfunc() result(res)
    character(len=:), pointer :: res
    res => fifec
    if (len(res) /= 5) stop 1
    if (res /= "FIVEC") stop 1
  end function subfunc
  function addPrefix(str) result(res)
    character(len=:), pointer :: str
    character(len=:), allocatable :: res
    res = ".." // str
  end function addPrefix
end subroutine test

program a
 character(len=:),allocatable :: s
 integer :: j=2
 s = repeat ('x', j)
 if (len(repeat(' ',j)) /= 2) stop 1
 if (repeat('y',j) /= "yy") stop 1
 if (len(s) /= 2) stop 1
 if (s /= "xx") stop 1
 call test()
end program a
