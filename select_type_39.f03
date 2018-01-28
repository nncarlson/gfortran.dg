! { dg-do run }
!
! Tests the fix for PR67564 comment #9.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
class(*), allocatable :: val(:)
call get_value (val)
select type (val)
type is (character(*))
  if (size (val) .ne. 2) stop 1
  if (len(val) .ne. 3) stop 1
  if (any (val .ne. ['foo','bar'])) stop 1
end select
contains
  subroutine get_value (value)
    class(*), allocatable, intent(out) :: value(:)
    allocate(value, source=['foo','bar'])
  end subroutine
end
