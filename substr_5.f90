! { dg-do run }
!
  character(*), parameter  :: chrs = '-+.0123456789eEdD'
  character(*), parameter  :: expr = '-+.0123456789eEdD'
  integer :: i

  if (index(chrs(:), expr) /= 1) stop 1
  if (index(chrs(14:), expr) /= 0) stop 1
  if (index(chrs(:12), expr) /= 0) stop 1
  if (index(chrs, expr(:)) /= 1) stop 1
  if (index(chrs, expr(1:)) /= 1) stop 1
  if (index(chrs, expr(:1)) /= 1) stop 1

  if (foo(expr) /= 1) stop 1
  if (foo(expr) /= 1) stop 1
  if (foo(expr) /= 1) stop 1
  if (foo(expr(:)) /= 1) stop 1
  if (foo(expr(1:)) /= 1) stop 1
  if (foo(expr(:1)) /= 1) stop 1

  call bar(expr)

contains
  subroutine bar(expr)
    character(*), intent(in) :: expr
    character(*), parameter  :: chrs = '-+.0123456789eEdD'
    integer :: foo

    if (index(chrs(:), expr) /= 1) stop 1
    if (index(chrs(14:), expr) /= 0) stop 1
    if (index(chrs(:12), expr) /= 0) stop 1
    if (index(chrs, expr(:)) /= 1) stop 1
    if (index(chrs, expr(1:)) /= 1) stop 1
    if (index(chrs, expr(:1)) /= 1) stop 1
  end subroutine bar

  integer function foo(expr)
    character(*), intent(in) :: expr
    character(*), parameter  :: chrs = '-+.0123456789eEdD'

    foo = index(chrs, expr)
  end function foo

end
