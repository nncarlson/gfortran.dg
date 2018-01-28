! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
program main
  implicit none
  character(len=4) :: c, d
  integer :: n
  integer :: i
  common /foo/ i

  n = 0
  i = 0
  c = 'abcd'
  d = 'efgh'

  n = n + 1 ; if ('a' // c == 'a' // c) call yes
  n = n + 1 ; if (c // 'a' == c // 'a') call yes
  n = n + 1; if ('b' // c > 'a' // d) call yes
  n = n + 1; if (c // 'b' > c // 'a') call yes

  if ('a' // c /= 'a' // c) stop 1
  if ('a' // c // 'b' == 'a' // c // 'a') stop 1
  if ('b' // c == 'a' // c) stop 1
  if (c // 'a' ==  c // 'b') stop 1
  if (c // 'a ' /=  c // 'a') stop 1
  if (c // 'b' /=  c // 'b ') stop 1

  if (n /= i) stop 1
end program main

subroutine yes
  implicit none
  common /foo/ i
  integer :: i
  i = i + 1
end subroutine yes

! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

