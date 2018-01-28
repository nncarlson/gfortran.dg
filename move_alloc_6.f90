! { dg-do run }
!
! Test move_alloc for polymorphic scalars
!
!
module myalloc
  implicit none

  type :: base_type
     integer :: i  =2
  end type base_type

  type, extends(base_type) :: extended_type
     integer :: j = 77
  end type extended_type
contains
  subroutine myallocate (a)
    class(base_type), allocatable, intent(inout) :: a
    class(base_type), allocatable :: tmp

    allocate (extended_type :: tmp)

    select type(tmp)
      type is(base_type)
        stop 1
      type is(extended_type)
        if (tmp%i /= 2 .or. tmp%j /= 77) stop 1
        tmp%i = 5
        tmp%j = 88
    end select

    select type(a)
      type is(base_type)
        if (a%i /= -44) stop 1
        a%i = -99
      class default
        stop 1
    end select

    call move_alloc (from=tmp, to=a)

    select type(a)
      type is(extended_type)
        if (a%i /= 5) stop 1
        if (a%j /= 88) stop 1
        a%i = 123
        a%j = 9498
      class default
        stop 1
    end select

    if (allocated (tmp)) stop 1
  end subroutine myallocate
end module myalloc

program main
  use myalloc
  implicit none
  class(base_type), allocatable :: a

  allocate (a)

  select type(a)
    type is(base_type)
      if (a%i /= 2) stop 1
      a%i = -44
    class default
      stop 1
  end select

  call myallocate (a)

  select type(a)
    type is(extended_type)
      if (a%i /= 123) stop 1
      if (a%j /= 9498) stop 1
    class default
      stop 1
  end select
end program main
