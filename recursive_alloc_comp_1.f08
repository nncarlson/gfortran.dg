! { dg-do run }
!
! Tests functionality of recursive allocatable derived types.
!
  type :: recurses
    type(recurses), allocatable :: c
    integer, allocatable :: ia
  end type

  type(recurses), allocatable, target :: a, d
  type(recurses), pointer :: b

  integer :: total = 0

! Check chained allocation.
  allocate(a)
  a%ia = 1
  allocate (a%c)
  a%c%ia = 2

! Check move_alloc.
  allocate (d)
  d%ia = 3
  call move_alloc (d, a%c%c)

  if (a%ia .ne. 1)  stop 1
  if (a%c%ia .ne. 2)  stop 1
  if (a%c%c%ia .ne. 3)  stop 1

! Check that we can point anywhere in the chain
  b => a%c%c
  if (b%ia .ne. 3) stop 1
  b => a%c
  if (b%ia .ne. 2) stop 1

! Check that the pointer can be used as if it were an element in the chain.
  if (.not.allocated (b%c)) stop 1
  b => a%c%c
  if (.not.allocated (b%c)) allocate (b%c)
  b%c%ia = 4
  if (a%c%c%c%ia .ne. 4) stop 1

! A rudimentary iterator.
  b => a
  do while (associated (b))
    total = total + b%ia
    b => b%c
  end do
  if (total .ne. 10) stop 1

! Take one element out of the chain.
  call move_alloc (a%c%c, d)
  call move_alloc (d%c, a%c%c)
  if (d%ia .ne. 3) stop 1
  deallocate (d)

! Checkcount of remaining chain.
  total = 0
  b => a
  do while (associated (b))
    total = total + b%ia
    b => b%c
  end do
  if (total .ne. 7) stop 1

! Deallocate to check that there are no memory leaks.
  deallocate (a%c%c)
  deallocate (a%c)
  deallocate (a)
end
