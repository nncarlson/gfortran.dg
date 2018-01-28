! { dg-do run }
! Test passing character strings by-value.
! PR fortran/32732
program test
  implicit none
  character(len=13) :: chr
  chr =  'Fortran       '
  call sub1(chr)
  if(chr /= 'Fortran       ') stop 1
contains
  subroutine sub1(a)
    character(len=13), VALUE :: a
    a = trim(a)//" rules"
    call sub2(a)        
  end subroutine sub1
  subroutine sub2(a)
    character(len=13), VALUE :: a
    print *, a          
    if(a /= 'Fortran rules') stop 1
  end subroutine sub2
end program test

