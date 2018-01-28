! { dg-do compile }
! { dg-options "-std=f2003" }
!
! TARGET actual to POINTER dummy with INTENT(IN)
!
program test
  implicit none
  integer, target :: a
  a = 66
  call foo(a) ! { dg-error "Fortran 2008: Non-pointer actual argument" }
  if (a /= 647) stop 1
contains
  subroutine foo(p)
    integer, pointer, intent(in) :: p
    if (a /= 66) stop 1
    if (p /= 66) stop 1
    p = 647
    if (p /= 647) stop 1
    if (a /= 647) stop 1
  end subroutine foo
end program test
