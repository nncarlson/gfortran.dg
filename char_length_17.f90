! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR 34145 - the length of the string should be simplified to one,
! no library call for string comparison is necessary.
program main
  character (len=5) :: c
  integer(kind=8) :: i
  i = 3
  c(i:i) = 'a'
  c(i+1:i+1) = 'b'
  if (c(i:i) /= 'a') stop 1
  if (c(i+1:i+1) /= 'b') stop 1
end program main
! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }
