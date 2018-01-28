! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! PR 40628 - optimize unnecessary TRIMs on assignment
program main
  character(len=3) :: a
  character(len=4) :: b,c
  b = 'abcd'
  a = trim(b)
  c = trim(trim(a))
  if (a /= 'abc') stop 1
  if (c /= 'abc') stop 1
end program main

! { dg-final { scan-tree-dump-times "memmove" 3 "original" } }
! { dg-final { scan-tree-dump-times "string_trim" 0 "original" } }
