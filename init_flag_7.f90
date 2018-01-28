! { dg-do run }
! { dg-options "-finit-integer=101" }

program init_flag_7
  call save_test1 (.true.)
  call save_test1 (.false.) 
  call save_test2 (.true.)
  call save_test2 (.false.)
end program init_flag_7

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine save_test1 (first)
  logical first
  integer :: i1 = -100
  integer i2
  integer i3
  save i2
  if (first) then
     if (i1 .ne. -100) stop 1
     if (i2 .ne. 101) stop 1
     if (i3 .ne. 101) stop 1
  else
     if (i1 .ne. 1001) stop 1
     if (i2 .ne. 1002) stop 1
     if (i3 .ne. 101) stop 1
  end if
  i1 = 1001
  i2 = 1002
  i3 = 1003
end subroutine save_test1
        
subroutine save_test2 (first)
  logical first
  integer :: i1 = -100
  integer i2
  save
  if (first) then
     if (i1 .ne. -100) stop 1
     if (i2 .ne. 101) stop 1
  else
     if (i1 .ne. 1001) stop 1
     if (i2 .ne. 1002) stop 1
  end if
  i1 = 1001
  i2 = 1002
end subroutine save_test2
