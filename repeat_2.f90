! REPEAT intrinsic
!
! { dg-do run }
subroutine foo(i, j, s, t)
  implicit none
  integer, intent(in) :: i, j
  character(len=i), intent(in) :: s
  character(len=i*j), intent(in) :: t

  if (repeat(s,j) /= t) stop 1
  call bar(j,s,t)
end subroutine foo

subroutine bar(j, s, t)
  implicit none
  integer, intent(in) :: j
  character(len=*), intent(in) :: s
  character(len=len(s)*j), intent(in) :: t

  if (repeat(s,j) /= t) stop 1
end subroutine bar

program test
  implicit none
  character(len=0), parameter :: s0 = "" 
  character(len=1), parameter :: s1 = "a"
  character(len=2), parameter :: s2 = "ab"
  character(len=0) :: t0 
  character(len=1) :: t1
  character(len=2) :: t2
  integer :: i

  t0 = ""
  t1 = "a"
  t2 = "ab"

  if (repeat(t0, 0) /= "") stop 1
  if (repeat(t1, 0) /= "") stop 1
  if (repeat(t2, 0) /= "") stop 1
  if (repeat(t0, 1) /= "") stop 1
  if (repeat(t1, 1) /= "a") stop 1
  if (repeat(t2, 1) /= "ab") stop 1
  if (repeat(t0, 2) /= "") stop 1
  if (repeat(t1, 2) /= "aa") stop 1
  if (repeat(t2, 2) /= "abab") stop 1

  if (repeat(s0, 0) /= "") stop 1
  if (repeat(s1, 0) /= "") stop 1
  if (repeat(s2, 0) /= "") stop 1
  if (repeat(s0, 1) /= "") stop 1
  if (repeat(s1, 1) /= "a") stop 1
  if (repeat(s2, 1) /= "ab") stop 1
  if (repeat(s0, 2) /= "") stop 1
  if (repeat(s1, 2) /= "aa") stop 1
  if (repeat(s2, 2) /= "abab") stop 1

  i = 0
  if (repeat(t0, i) /= "") stop 1
  if (repeat(t1, i) /= "") stop 1
  if (repeat(t2, i) /= "") stop 1
  i = 1
  if (repeat(t0, i) /= "") stop 1
  if (repeat(t1, i) /= "a") stop 1
  if (repeat(t2, i) /= "ab") stop 1
  i = 2
  if (repeat(t0, i) /= "") stop 1
  if (repeat(t1, i) /= "aa") stop 1
  if (repeat(t2, i) /= "abab") stop 1

  i = 0
  if (repeat(s0, i) /= "") stop 1
  if (repeat(s1, i) /= "") stop 1
  if (repeat(s2, i) /= "") stop 1
  i = 1
  if (repeat(s0, i) /= "") stop 1
  if (repeat(s1, i) /= "a") stop 1
  if (repeat(s2, i) /= "ab") stop 1
  i = 2
  if (repeat(s0, i) /= "") stop 1
  if (repeat(s1, i) /= "aa") stop 1
  if (repeat(s2, i) /= "abab") stop 1

  call foo(0,0,"","")
  call foo(0,1,"","")
  call foo(0,2,"","")
  call foo(1,0,"a","")
  call foo(1,1,"a","a")
  call foo(1,2,"a","aa")
  call foo(2,0,"ab","")
  call foo(2,1,"ab","ab")
  call foo(2,2,"ab","abab")
end program test
