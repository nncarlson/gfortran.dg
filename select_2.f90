! { dg-do run }
!  Simple test program to see if gfortran eliminates the 'case (3:2)'
!  statement.  This is an unreachable CASE because the range is empty.
!
program select_3
  integer i
  do i = 1, 4
     select case(i)
     case (1)
       if (i /= 1) stop 1
     case (3:2)
       stop 1
     case (4)
       if (i /= 4) stop 1
     case default
       if (i /= 2 .and. i /= 3) stop 1
     end select
  end do
end program select_3
