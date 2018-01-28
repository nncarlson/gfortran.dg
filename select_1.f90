! { dg-do run }
!  Simple test for SELECT CASE
!
program select_2
  integer i
  do i = 1, 5
     select case(i)
     case (1)
       if (i /= 1) stop 1
     case (2:3)
       if (i /= 2 .and. i /= 3) stop 1
     case (4)
       if (i /= 4) stop 1
     case default
       if (i /= 5) stop 1
     end select
  end do
end program select_2
