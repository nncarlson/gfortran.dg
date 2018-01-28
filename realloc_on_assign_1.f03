! { dg-do run }
! Tests the patch that implements F2003 automatic allocation and
! reallocation of allocatable arrays on assignment.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  integer(4), allocatable :: a(:), b(:), c(:,:)
  integer(4) :: j
  integer(4) :: src(2:5) = [11,12,13,14]
  integer(4) :: mat(2:3,5:6)
  character(4), allocatable :: chr1(:)
  character(4) :: chr2(2) = ["abcd", "wxyz"]

  allocate(a(1))
  mat = reshape (src, [2,2])

  a = [4,3,2,1]
  if (size(a, 1) .ne. 4) stop 1
  if (any (a .ne. [4,3,2,1])) stop 1

  a = [((42 - i), i = 1, 10)]
  if (size(a, 1) .ne. 10) stop 1
  if (any (a .ne. [((42 - i), i = 1, 10)])) stop 1

  b = a
  if (size(b, 1) .ne. 10) stop 1
  if (any (b .ne. a)) stop 1

  a = [4,3,2,1]
  if (size(a, 1) .ne. 4) stop 1
  if (any (a .ne. [4,3,2,1])) stop 1

  a = b
  if (size(a, 1) .ne. 10) stop 1
  if (any (a .ne. [((42 - i), i = 1, 10)])) stop 1

  j = 20
  a = [(i, i = 1, j)]
  if (size(a, 1) .ne. j) stop 1
  if (any (a .ne. [(i, i = 1, j)])) stop 1

  a = foo (15)
  if (size(a, 1) .ne. 15) stop 1
  if (any (a .ne. [((i + 15), i = 1, 15)])) stop 1

  a = src
  if (lbound(a, 1) .ne. lbound(src, 1)) stop 1
  if (ubound(a, 1) .ne. ubound(src, 1)) stop 1
  if (any (a .ne. [11,12,13,14])) stop 1

  k = 7
  a = b(k:8)
  if (lbound(a, 1) .ne. lbound (b(k:8), 1)) stop 1
  if (ubound(a, 1) .ne. ubound (b(k:8), 1)) stop 1
  if (any (a .ne. [35,34])) stop 1

  c = mat
  if (any (lbound (c) .ne. lbound (mat))) stop 1
  if (any (ubound (c) .ne. ubound (mat))) stop 1
  if (any (c .ne. mat)) stop 1

  deallocate (c)
  c = mat(2:,:)
  if (any (lbound (c) .ne. lbound (mat(2:,:)))) stop 1

  chr1 = chr2(2:1:-1)
  if (lbound(chr1, 1) .ne. 1) stop 1
  if (any (chr1 .ne. chr2(2:1:-1))) stop 1

  b = c(1, :) + c(2, :)
  if (lbound(b, 1) .ne. lbound (c(1, :) + c(2, :), 1)) stop 1
  if (any (b .ne. c(1, :) + c(2, :))) stop 1
contains
  function foo (n) result(res)
    integer(4), allocatable, dimension(:) :: res
    integer(4) :: n
    allocate (res(n))
    res = [((i + 15), i = 1, n)]
  end function foo
end
