! { dg-do run }
!
! Check that PR50221 comment #4 is fixed.
!
! Contributed by Arjen Makus  <arjen.markus895@gmail.com>
!
program chk_alloc_string
    implicit none

    character(len=:), dimension(:), allocatable :: strings
    character(20) :: buffer
    integer :: i

    allocate( character(10):: strings(1:3) )

    strings = [ "A   ", "C   ", "ABCD", "V   " ]

    if (len(strings) .ne. 4) stop 1
    if (size(strings, 1) .ne. 4) stop 1
    if (any (strings .ne. [character(len=4) :: "A", "C", "ABCD", "V"])) stop 1

    strings = [character(len=4) :: "A", "C", "ABCDE", "V", "zzzz"]

    if (len(strings) .ne. 4) stop 1
    if (size(strings, 1) .ne. 5) stop 1
    if (any (strings .ne. [character(len=4) :: "A", "C", "ABCD", "V", "zzzz"])) stop 1

    write (buffer, "(5a4)") strings
    if (buffer .ne. "A   C   ABCDV   zzzz") stop 1
end program chk_alloc_string
