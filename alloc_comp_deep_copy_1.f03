! { dg-do run }
!
! Check fix for correctly deep copying allocatable components.
! PR fortran/59678
! Contributed by Andre Vehreschild  <vehre@gmx.de>
!
program alloc_comp_copy_test

  type InnerT
    integer :: ii
    integer, allocatable :: ai
    integer, allocatable :: v(:)
  end type InnerT

  type T
    integer :: i
    integer, allocatable :: a_i
    type(InnerT), allocatable :: it
    type(InnerT), allocatable :: vec(:)
  end type T

  type(T) :: o1, o2
  class(T), allocatable :: o3, o4
  o1%i = 42

  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (allocated(o2%a_i)) stop 1
  if (allocated(o2%it)) stop 1
  if (allocated(o2%vec)) stop 1

  allocate (o1%a_i, source=2)
  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (.not. allocated(o2%a_i)) stop 1
  if (o2%a_i /= 2) stop 1
  if (allocated(o2%it)) stop 1
  if (allocated(o2%vec)) stop 1

  allocate (o1%it)
  o1%it%ii = 3
  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (.not. allocated(o2%a_i)) stop 1
  if (o2%a_i /= 2) stop 1
  if (.not. allocated(o2%it)) stop 1
  if (o2%it%ii /= 3) stop 1
  if (allocated(o2%it%ai)) stop 1
  if (allocated(o2%it%v)) stop 1
  if (allocated(o2%vec)) stop 1

  allocate (o1%it%ai)
  o1%it%ai = 4
  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (.not. allocated(o2%a_i)) stop 1
  if (o2%a_i /= 2) stop 1
  if (.not. allocated(o2%it)) stop 1
  if (o2%it%ii /= 3) stop 1
  if (.not. allocated(o2%it%ai)) stop 1
  if (o2%it%ai /= 4) stop 1
  if (allocated(o2%it%v)) stop 1
  if (allocated(o2%vec)) stop 1

  allocate (o1%it%v(3), source= 5)
  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (.not. allocated(o2%a_i)) stop 1
  if (o2%a_i /= 2) stop 1
  if (.not. allocated(o2%it)) stop 1
  if (o2%it%ii /= 3) stop 1
  if (.not. allocated(o2%it%ai)) stop 1
  if (o2%it%ai /= 4) stop 1
  if (.not. allocated(o2%it%v)) stop 1
  if (any (o2%it%v /= 5) .or. size (o2%it%v) /= 3) stop 1
  if (allocated(o2%vec)) stop 1

  allocate (o1%vec(2))
  o1%vec(:)%ii = 6
  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (.not. allocated(o2%a_i)) stop 1
  if (o2%a_i /= 2) stop 1
  if (.not. allocated(o2%it)) stop 1
  if (o2%it%ii /= 3) stop 1
  if (.not. allocated(o2%it%ai)) stop 1
  if (o2%it%ai /= 4) stop 1
  if (.not. allocated(o2%it%v)) stop 1
  if (size (o2%it%v) /= 3) stop 1
  if (any (o2%it%v /= 5)) stop 1
  if (.not. allocated(o2%vec)) stop 1
  if (size(o2%vec) /= 2) stop 1
  if (any(o2%vec(:)%ii /= 6)) stop 1
  if (allocated(o2%vec(1)%ai) .or. allocated(o2%vec(2)%ai)) stop 1
  if (allocated(o2%vec(1)%v) .or. allocated(o2%vec(2)%v)) stop 1

  allocate (o1%vec(2)%ai)
  o1%vec(2)%ai = 7
  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (.not. allocated(o2%a_i)) stop 1
  if (o2%a_i /= 2) stop 1
  if (.not. allocated(o2%it)) stop 1
  if (o2%it%ii /= 3) stop 1
  if (.not. allocated(o2%it%ai)) stop 1
  if (o2%it%ai /= 4) stop 1
  if (.not. allocated(o2%it%v)) stop 1
  if (size (o2%it%v) /= 3) stop 1
  if (any (o2%it%v /= 5)) stop 1
  if (.not. allocated(o2%vec)) stop 1
  if (size(o2%vec) /= 2) stop 1
  if (any(o2%vec(:)%ii /= 6)) stop 1
  if (allocated(o2%vec(1)%ai)) stop 1
  if (.not. allocated(o2%vec(2)%ai)) stop 1
  if (o2%vec(2)%ai /= 7) stop 1
  if (allocated(o2%vec(1)%v) .or. allocated(o2%vec(2)%v)) stop 1

  allocate (o1%vec(1)%v(3))
  o1%vec(1)%v = [8, 9, 10]
  call copyO(o1, o2)
  if (o2%i /= 42) stop 1
  if (.not. allocated(o2%a_i)) stop 1
  if (o2%a_i /= 2) stop 1
  if (.not. allocated(o2%it)) stop 1
  if (o2%it%ii /= 3) stop 1
  if (.not. allocated(o2%it%ai)) stop 1
  if (o2%it%ai /= 4) stop 1
  if (.not. allocated(o2%it%v)) stop 1
  if (size (o2%it%v) /= 3) stop 1
  if (any (o2%it%v /= 5)) stop 1
  if (.not. allocated(o2%vec)) stop 1
  if (size(o2%vec) /= 2) stop 1
  if (any(o2%vec(:)%ii /= 6)) stop 1
  if (allocated(o2%vec(1)%ai)) stop 1
  if (.not. allocated(o2%vec(2)%ai)) stop 1
  if (o2%vec(2)%ai /= 7) stop 1
  if (.not. allocated(o2%vec(1)%v)) stop 1
  if (any (o2%vec(1)%v /= [8,9,10])) stop 1
  if (allocated(o2%vec(2)%v)) stop 1

  ! Now all the above for class objects.
  allocate (o3, o4)
  o3%i = 42

  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (allocated(o4%a_i)) stop 1
  if (allocated(o4%it)) stop 1
  if (allocated(o4%vec)) stop 1

  allocate (o3%a_i, source=2)
  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (.not. allocated(o4%a_i)) stop 1
  if (o4%a_i /= 2) stop 1
  if (allocated(o4%it)) stop 1
  if (allocated(o4%vec)) stop 1

  allocate (o3%it)
  o3%it%ii = 3
  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (.not. allocated(o4%a_i)) stop 1
  if (o4%a_i /= 2) stop 1
  if (.not. allocated(o4%it)) stop 1
  if (o4%it%ii /= 3) stop 1
  if (allocated(o4%it%ai)) stop 1
  if (allocated(o4%it%v)) stop 1
  if (allocated(o4%vec)) stop 1

  allocate (o3%it%ai)
  o3%it%ai = 4
  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (.not. allocated(o4%a_i)) stop 1
  if (o4%a_i /= 2) stop 1
  if (.not. allocated(o4%it)) stop 1
  if (o4%it%ii /= 3) stop 1
  if (.not. allocated(o4%it%ai)) stop 1
  if (o4%it%ai /= 4) stop 1
  if (allocated(o4%it%v)) stop 1
  if (allocated(o4%vec)) stop 1

  allocate (o3%it%v(3), source= 5)
  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (.not. allocated(o4%a_i)) stop 1
  if (o4%a_i /= 2) stop 1
  if (.not. allocated(o4%it)) stop 1
  if (o4%it%ii /= 3) stop 1
  if (.not. allocated(o4%it%ai)) stop 1
  if (o4%it%ai /= 4) stop 1
  if (.not. allocated(o4%it%v)) stop 1
  if (any (o4%it%v /= 5) .or. size (o4%it%v) /= 3) stop 1
  if (allocated(o4%vec)) stop 1

  allocate (o3%vec(2))
  o3%vec(:)%ii = 6
  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (.not. allocated(o4%a_i)) stop 1
  if (o4%a_i /= 2) stop 1
  if (.not. allocated(o4%it)) stop 1
  if (o4%it%ii /= 3) stop 1
  if (.not. allocated(o4%it%ai)) stop 1
  if (o4%it%ai /= 4) stop 1
  if (.not. allocated(o4%it%v)) stop 1
  if (size (o4%it%v) /= 3) stop 1
  if (any (o4%it%v /= 5)) stop 1
  if (.not. allocated(o4%vec)) stop 1
  if (size(o4%vec) /= 2) stop 1
  if (any(o4%vec(:)%ii /= 6)) stop 1
  if (allocated(o4%vec(1)%ai) .or. allocated(o4%vec(2)%ai)) stop 1
  if (allocated(o4%vec(1)%v) .or. allocated(o4%vec(2)%v)) stop 1

  allocate (o3%vec(2)%ai)
  o3%vec(2)%ai = 7
  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (.not. allocated(o4%a_i)) stop 1
  if (o4%a_i /= 2) stop 1
  if (.not. allocated(o4%it)) stop 1
  if (o4%it%ii /= 3) stop 1
  if (.not. allocated(o4%it%ai)) stop 1
  if (o4%it%ai /= 4) stop 1
  if (.not. allocated(o4%it%v)) stop 1
  if (size (o4%it%v) /= 3) stop 1
  if (any (o4%it%v /= 5)) stop 1
  if (.not. allocated(o4%vec)) stop 1
  if (size(o4%vec) /= 2) stop 1
  if (any(o4%vec(:)%ii /= 6)) stop 1
  if (allocated(o4%vec(1)%ai)) stop 1
  if (.not. allocated(o4%vec(2)%ai)) stop 1
  if (o4%vec(2)%ai /= 7) stop 1
  if (allocated(o4%vec(1)%v) .or. allocated(o4%vec(2)%v)) stop 1

  allocate (o3%vec(1)%v(3))
  o3%vec(1)%v = [8, 9, 10]
  call copyO(o3, o4)
  if (o4%i /= 42) stop 1
  if (.not. allocated(o4%a_i)) stop 1
  if (o4%a_i /= 2) stop 1
  if (.not. allocated(o4%it)) stop 1
  if (o4%it%ii /= 3) stop 1
  if (.not. allocated(o4%it%ai)) stop 1
  if (o4%it%ai /= 4) stop 1
  if (.not. allocated(o4%it%v)) stop 1
  if (size (o4%it%v) /= 3) stop 1
  if (any (o4%it%v /= 5)) stop 1
  if (.not. allocated(o4%vec)) stop 1
  if (size(o4%vec) /= 2) stop 1
  if (any(o4%vec(:)%ii /= 6)) stop 1
  if (allocated(o4%vec(1)%ai)) stop 1
  if (.not. allocated(o4%vec(2)%ai)) stop 1
  if (o4%vec(2)%ai /= 7) stop 1
  if (.not. allocated(o4%vec(1)%v)) stop 1
  if (any (o4%vec(1)%v /= [8,9,10])) stop 1
  if (allocated(o4%vec(2)%v)) stop 1

contains

  subroutine copyO(src, dst)
    type(T), intent(in) :: src
    type(T), intent(out) :: dst

    dst = src
  end subroutine copyO

end program alloc_comp_copy_test

