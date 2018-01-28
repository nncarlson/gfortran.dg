! { dg-do run }
module z
   integer :: i
   character(6) :: a(2) = (/ ('main  ' , i = 1, 2) /)
   character(6) :: b(2) = (/ 'abcd  ' , 'efghij' /)
end module

program y
  use z
  if (a(1) /= 'main  ') stop 1
  if (a(2) /= 'main  ') stop 1
  if (b(1) /= 'abcd  ') stop 1
  if (b(2) /= 'efghij') stop 1
end program y
