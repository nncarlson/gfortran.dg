! Check that lbound() and ubound() work correctly for assumed shapes.
! { dg-do run }
program main
  integer, dimension (40, 80) :: a = 1
  call test (a)
contains
  subroutine test (b)
    integer, dimension (11:, -8:), target :: b
    integer, dimension (:, :), pointer :: ptr

    if (lbound (b, 1) .ne. 11) stop 1
    if (ubound (b, 1) .ne. 50) stop 1
    if (lbound (b, 2) .ne. -8) stop 1
    if (ubound (b, 2) .ne. 71) stop 1

    if (lbound (b (:, :), 1) .ne. 1) stop 1
    if (ubound (b (:, :), 1) .ne. 40) stop 1
    if (lbound (b (:, :), 2) .ne. 1) stop 1
    if (ubound (b (:, :), 2) .ne. 80) stop 1

    if (lbound (b (20:30:3, 40), 1) .ne. 1) stop 1
    if (ubound (b (20:30:3, 40), 1) .ne. 4) stop 1

    ptr => b
    if (lbound (ptr, 1) .ne. 11) stop 1
    if (ubound (ptr, 1) .ne. 50) stop 1
    if (lbound (ptr, 2) .ne. -8) stop 1
    if (ubound (ptr, 2) .ne. 71) stop 1
  end subroutine test
end program main
