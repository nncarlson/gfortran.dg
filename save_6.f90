! { dg-do run }
! { dg-require-effective-target lto }
! { dg-options "-fno-automatic -flto -g" }
!
! PR fortran/55733
!
! Check that -fno-automatic makes the local variable SAVEd
! Check that -flto -g works
!

! Scalar allocatable
subroutine foo(i)
  integer :: i
  integer, allocatable :: j
  if (i == 1) j = 42
  if (.not. allocated (j)) stop 1
  if (j /= 42) stop 1
end

! Deferred-length string scalar
subroutine bar()
  logical, save :: first = .true.
  character(len=:), allocatable :: str
  if (first) then
    first = .false.
    if (allocated (str)) stop 1
    str = "ABCDEF"
  end if
  if (.not. allocated (str)) stop 1
  if (len (str) /= 6) stop 1
  if (str(1:6) /= "ABCDEF") stop 1
end subroutine bar

! Deferred-length string array
subroutine bar_array()
  logical, save :: first = .true.
  character(len=:), allocatable :: str
  if (first) then
    first = .false.
    if (allocated (str)) stop 1
    str = "ABCDEF"
  end if
  if (.not. allocated (str)) stop 1
  if (len (str) /= 6) stop 1
  if (str(1:6) /= "ABCDEF") stop 1
end subroutine bar_array

call foo(1)
call foo(2)
call bar()
call bar_array()
call bar()
call bar_array()
end
