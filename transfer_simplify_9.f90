! { dg-do run }
!
! Various checks on simplification of TRANSFER of substrings
  character(len=4), parameter :: t = "xyzt"
  integer, parameter :: w = transfer(t,0)
  integer :: i = 1
  if (transfer(t,0) /= w) stop 1
  if (transfer(t(:),0) /= w) stop 1
  if (transfer(t(1:4),0) /= w) stop 1
  if (transfer(t(i:i+3),0) /= w) stop 1

  if (transfer(t(1:1), 0_1) /= transfer("x", 0_1)) stop 1
  if (transfer(t(2:2), 0_1) /= transfer("y", 0_1)) stop 1
  if (transfer(t(i:i), 0_1) /= transfer("x", 0_1)) stop 1
  if (transfer(t(i+1:i+1), 0_1) /= transfer("y", 0_1)) stop 1
  if (transfer(t(1:2), 0_2) /= transfer("xy", 0_2)) stop 1
  if (transfer(t(3:4), 0_2) /= transfer("zt", 0_2)) stop 1

  if (transfer(transfer(-1, t), 0) /= -1) stop 1
  if (transfer(transfer(-1, t(:)), 0) /= -1) stop 1
  if (any (transfer(transfer(-1, (/t(1:1)/)), (/0_1/)) /= -1)) stop 1
  if (transfer(transfer(-1, t(1:1)), 0_1) /= -1) stop 1
  end
