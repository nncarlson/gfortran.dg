! { dg-do run }
!
! NULL() initialization for PROCEDURE POINTERS
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program main
implicit none
call test(.true.)
call test(.false.)

contains

integer function hello()
 hello = 42
end function hello

subroutine test(first)
 logical :: first
 integer :: i
 procedure(integer), pointer :: x => null()

 if(first) then
  if(associated(x)) stop 1
  x => hello
 else
  if(.not. associated(x)) stop 1
  i = x()
  if(i /= 42) stop 1
 end if
 end subroutine test

end program main
