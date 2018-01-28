! { dg-do run }
!
! Check whether registering allocatable coarrays works
!
type position
  real :: x, y, z
end type position

integer, allocatable :: a[:]
type(position), allocatable :: p[:]

allocate(a[*])
a = 7

allocate(p[*])
p%x = 11
p%y = 13
p%z = 15

if (a /= 7) stop 1
a = 88
if (a /= 88) stop 1

if (p%x /= 11) stop 1
p%x = 17
if (p%x /= 17) stop 1

 block
   integer, allocatable :: b[:]

   allocate(b[*])
   b = 8494
   
   if (b /= 8494) stop 1
 end block

if (a /= 88) stop 1
call test ()
end

subroutine test()
  type velocity
    real :: x, y, z
  end type velocity

  real, allocatable :: z[:]
  type(velocity), allocatable :: v[:]

  allocate(z[*])
  z = sqrt(2.0)

  allocate(v[*])
  v%x = 21
  v%y = 23
  v%z = 25

  if (z /= sqrt(2.0)) stop 1
  if (v%x /= 21) stop 1

end subroutine test
