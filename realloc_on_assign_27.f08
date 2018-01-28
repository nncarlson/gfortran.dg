! { dg-do run }

  type :: t
    integer :: i
  end type

  type, extends(t) :: r
    real :: r
  end type

  class(t), allocatable :: x
  type(r) :: y = r (3, 42)

  x = y
  if (x%i /= 3) stop 1
  select type(x)
    class is (r)
      if (x%r /= 42.0) stop 1
    class default
      stop 1
  end select
end

