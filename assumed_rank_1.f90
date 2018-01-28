! { dg-do run }
! { dg-additional-sources assumed_rank_1_c.c }
!
! PR fortran/48820
!
! Assumed-rank tests
!

implicit none

interface
  subroutine check_value(b, n, val)
    integer :: b(..)
    integer, value :: n
    integer :: val(n)
  end subroutine
end interface

integer, target :: x(2:5,4:7), y(-4:4)
integer, allocatable, target :: z(:,:,:,:)
integer, allocatable :: val(:)
integer :: i

allocate(z(1:4, -2:5, 4, 10:11))

if (rank(x) /= 2) stop 1
val = [(2*i+3, i = 1, size(x))]
x = reshape (val, shape(x))
call foo(x, rank(x), lbound(x), ubound(x), val)
call foo2(x, rank(x), lbound(x), ubound(x), val)
call bar(x,x,.true.)
call bar(x,prsnt=.false.)

if (rank(y) /= 1) stop 1
val = [(2*i+7, i = 1, size(y))]
y = reshape (val, shape(y))
call foo(y, rank(y), lbound(y), ubound(y), val)
call foo2(y, rank(y), lbound(y), ubound(y), val)
call bar(y,y,.true.)
call bar(y,prsnt=.false.)

if (rank(z) /= 4) stop 1
val = [(2*i+5, i = 1, size(z))]
z(:,:,:,:) = reshape (val, shape(z))
call foo(z, rank(z), lbound(z), ubound(z), val)
call foo(z, rank(z), lbound(z), ubound(z), val)
call foo2(z, rank(z), lbound(z), ubound(z), val)
call bar(z,z,.true.)
call bar(z,prsnt=.false.)

contains
  subroutine bar(a,b, prsnt)
    integer, pointer, optional, intent(in) :: a(..),b(..)
    logical, value :: prsnt
    if (.not. associated(a)) stop 1
    if (present(b)) then
       ! The following is not valid.
       ! Technically, it could be allowed and might be in Fortran 2015:
       ! if (.not. associated(a,b)) stop 1
    else
      if (.not. associated(a)) stop 1
    end if
    if (.not. present(a)) stop 1
    if (prsnt .neqv. present(b)) stop 1
  end subroutine

  ! POINTER argument - bounds as specified before
  subroutine foo(a, rnk, low, high, val)
    integer,pointer, intent(in) :: a(..)
    integer, value :: rnk
    integer, intent(in) :: low(:), high(:), val(:)
    integer :: i



    if (rank(a) /= rnk) stop 1
    if (size(low) /= rnk .or. size(high) /= rnk) stop 1
    if (size(a) /= product (high - low +1)) stop 1

    if (rnk > 0) then
      if (low(1) /= lbound(a,1)) stop 1
      if (high(1) /= ubound(a,1)) stop 1
      if (size (a,1) /= high(1)-low(1)+1) stop 1
    end if

    do i = 1, rnk
      if (low(i) /= lbound(a,i)) stop 1
      if (high(i) /= ubound(a,i)) stop 1
      if (size (a,i) /= high(i)-low(i)+1) stop 1
    end do
    call check_value (a, rnk, val)
    call foo2(a, rnk, low, high, val)
  end subroutine

  ! Non-pointer, non-allocatable bounds. lbound == 1
  subroutine foo2(a, rnk, low, high, val)
    integer, intent(in) :: a(..)
    integer, value :: rnk
    integer, intent(in) :: low(:), high(:), val(:)
    integer :: i

    if (rank(a) /= rnk) stop 1
    if (size(low) /= rnk .or. size(high) /= rnk) stop 1
    if (size(a) /= product (high - low +1)) stop 1

    if (rnk > 0) then
      if (1 /= lbound(a,1)) stop 1
      if (high(1)-low(1)+1 /= ubound(a,1)) stop 1
      if (size (a,1) /= high(1)-low(1)+1) stop 1
    end if

    do i = 1, rnk
      if (1 /= lbound(a,i)) stop 1
      if (high(i)-low(i)+1 /= ubound(a,i)) stop 1
      if (size (a,i) /= high(i)-low(i)+1) stop 1
    end do
    call check_value (a, rnk, val)
  end subroutine foo2

  ! ALLOCATABLE argument - bounds as specified before
  subroutine foo3 (a, rnk, low, high, val)
    integer, allocatable, intent(in), target :: a(..)
    integer, value :: rnk
    integer, intent(in) :: low(:), high(:), val(:)
    integer :: i

    if (rank(a) /= rnk) stop 1
    if (size(low) /= rnk .or. size(high) /= rnk) stop 1
    if (size(a) /= product (high - low +1)) stop 1

    if (rnk > 0) then
      if (low(1) /= lbound(a,1)) stop 1
      if (high(1) /= ubound(a,1)) stop 1
      if (size (a,1) /= high(1)-low(1)+1) stop 1
    end if

    do i = 1, rnk
      if (low(i) /= lbound(a,i)) stop 1
      if (high(i) /= ubound(a,i)) stop 1
      if (size (a,i) /= high(i)-low(i)+1) stop 1
    end do
    call check_value (a, rnk, val)
    call foo(a, rnk, low, high, val)
  end subroutine
end
