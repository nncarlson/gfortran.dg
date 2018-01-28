! { dg-do run }
! { dg-options "-O2" }
! Tests the fix PR29451, in which the negative size of the
! automatic array 'jello' was not detected and the
! runtime error: Attempt to allocate a negative amount of memory
! resulted.
!
! Contributed by Philip Mason  <pmason@ricardo.com> 
!
program fred
  call jackal (1, 0)
  call jackal (2, 1)
  call jackal (3, 0)
end

subroutine jackal (b, c)
  integer :: b, c
  integer :: jello(b:c), cake(1:2, b:c), soda(b:c, 1:2)
  if (lbound (jello, 1) <= ubound (jello, 1)) stop 1
  if (size (jello) /= 0) stop 1

  if (.not.any(lbound (cake) <= ubound (cake))) stop 1
  if (size (cake) /= 0) stop 1

  if ((lbound (soda, 1) > ubound (soda, 1)) .and. &
      (lbound (soda, 2) > ubound (soda, 2))) stop 1
  if (size (soda) /= 0) stop 1

end subroutine jackal
