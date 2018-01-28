! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! Optimize unnecessary TRIMs in contained namespaces too.
module faz
  implicit none
contains
  subroutine bar
    character(len=3) :: a
    character(len=4) :: b,c
    b = 'abcd'
    a = trim(b)
    c = trim(trim(a))
    if (a /= 'abc') stop 1
    if (c /= 'abc') stop 1
  end subroutine bar
end module faz

program main
  use faz
  implicit none
  call foo
  call bar
contains
  subroutine foo
    character(len=3) :: a
    character(len=4) :: b,c
    b = 'abcd'
    a = trim(b)
    c = trim(trim(a))
    if (a /= 'abc') stop 1
    if (c /= 'abc') stop 1
  end subroutine foo
end program main

! { dg-final { scan-tree-dump-times "memmove" 6 "original" } }
! { dg-final { scan-tree-dump-times "string_trim" 0 "original" } }
