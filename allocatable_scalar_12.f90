! { dg-do run }
!
! PR fortran/47421
!
! Don't auto-deallocatable scalar character allocatables.
!
implicit none
character(len=5), allocatable :: str
allocate(str)
str = '1bcde'
if(str /= '1bcde') stop 1
call sub(str,len(str))
if(str /= '1bcde') stop 1
call subOUT(str,len(str))
if (len(str) /= 5) stop 1
if(allocated(str)) stop 1
contains
  subroutine sub(x,n)
     integer :: n
     character(len=n), allocatable :: x
     if(len(x) /= 5) stop 1
     if(x /= '1bcde') stop 1
  end subroutine sub
  subroutine subOUT(x,n)
     integer :: n
     character(len=n), allocatable,intent(out) :: x
     if(allocated(x)) stop 1
     if(len(x) /= 5) stop 1
  end subroutine subOUT
end
