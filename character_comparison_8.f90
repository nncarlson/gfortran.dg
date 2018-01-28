! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! Check for compile-time optimization of LLE and friends.
program main
  character(3) :: a
  a = 'ab'
  if (.not. LLE(a,a)) stop 1
  if (LLT(a,a)) stop 1
  if (.not. LGE(a,a)) stop 1
  if (LGT(a,a)) stop 1
end program main
! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

