! { dg-do run }
! { dg-options "-finline-matmul-limit=0 -fdump-tree-original -fdump-tree-optimized -Warray-temporaries -fbounds-check" }

  implicit none

  integer :: i, j

  integer, parameter :: nx=3, ny=4
  integer, parameter, dimension(nx,ny) :: p = &
    & reshape ((/ (i**2, i=1,size(p)) /), shape(p))
  integer, parameter, dimension(ny,nx) :: q = &
    & reshape ((/ (((nx*(i-1)+j)**2, i=1,ny), j=1,nx) /), (/ ny, nx /))

  integer, parameter, dimension(nx,nx) :: r = &
    & reshape ((/ (i*i, i=1,size(r)) /), shape(r))
  integer, parameter, dimension(nx,nx) :: s = &
    & reshape ((/ (((nx*(i-1)+j)**2, i=1,nx), j=1,nx) /), (/ nx, nx /))



  integer, dimension(nx,ny) :: a, b
  integer, dimension(ny,nx) :: c
  integer, dimension(nx,nx) :: e, f, g

  character(144) :: u, v

  a = p

  c = transpose(a)
  if (any(c /= q)) stop 1

  write(u,*) transpose(a)
  write(v,*) q
  if (u /= v) stop 1


  e = r
  f = s

  g = transpose(e+f)
  if (any(g /= r + s)) stop 1

  write(u,*) transpose(e+f)
  write(v,*) r + s
  if (u /= v) stop 1


  e = transpose(e)      ! { dg-warning "Creating array temporary" }
  if (any(e /= s)) stop 1

  write(u,*) transpose(transpose(e))
  write(v,*) s
  if (u /= v) stop 1


  e = transpose(e+f)     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*r)) stop 1

  write(u,*) transpose(transpose(e+f))-f
  write(v,*) 2*r
  if (u /= v) stop 1


  a = foo(transpose(c))
  if (any(a /= p+1)) stop 1

  write(u,*) foo(transpose(c))    ! { dg-warning "Creating array temporary" }
  write(v,*) p+1
  if (u /= v) stop 1


  c = transpose(foo(a))      ! Unnecessary { dg-warning "Creating array temporary" }
  if (any(c /= q+2)) stop 1

  write(u,*) transpose(foo(a))     ! { dg-warning "Creating array temporary" }
  write(v,*) q+2
  if (u /= v) stop 1


  e = foo(transpose(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*s+1)) stop 1

  write(u,*) transpose(foo(transpose(e))-1)     ! { dg-warning "Creating array temporary" }
  write(v,*) 2*s+1
  if (u /= v) stop 1


  e = transpose(foo(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*r+2)) stop 1

  write(u,*) transpose(foo(transpose(e)-1))     ! 2 temps { dg-warning "Creating array temporary" }
  write(v,*) 2*r+2
  if (u /= v) stop 1


  a = bar(transpose(c))
  if (any(a /= p+4)) stop 1

  write(u,*) bar(transpose(c))
  write(v,*) p+4
  if (u /= v) stop 1


  c = transpose(bar(a))
  if (any(c /= q+6)) stop 1

  write(u,*) transpose(bar(a))
  write(v,*) q+6
  if (u /= v) stop 1


  e = bar(transpose(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*s+4)) stop 1

  write(u,*) transpose(bar(transpose(e)))-2
  write(v,*) 2*s+4
  if (u /= v) stop 1


  e = transpose(bar(e))     ! { dg-warning "Creating array temporary" }
  if (any(e /= 2*r+6)) stop 1

  write(u,*) transpose(transpose(bar(e))-2)
  write(v,*) 2*r+6
  if (u /= v) stop 1


  if (any(a /= transpose(transpose(a)))) stop 1     ! optimized away

  write(u,*) a
  write(v,*) transpose(transpose(a))
  if (u /= v) stop 1


  b = a * a

  if (any(transpose(a+b) /= transpose(a)+transpose(b))) stop 1      ! optimized away

  write(u,*) transpose(a+b)
  write(v,*) transpose(a) + transpose(b)
  if (u /= v) stop 1


  if (any(transpose(matmul(a,c)) /= matmul(transpose(c), transpose(a)))) stop 1      ! 2 temps { dg-warning "Creating array temporary" }

  write(u,*) transpose(matmul(a,c))     ! { dg-warning "Creating array temporary" }
  write(v,*) matmul(transpose(c), transpose(a))     ! { dg-warning "Creating array temporary" }
  if (u /= v) stop 1


  if (any(transpose(matmul(e,a)) /= matmul(transpose(a), transpose(e)))) stop 1     ! 2 temps { dg-warning "Creating array temporary" }

  write(u,*) transpose(matmul(e,a))     ! { dg-warning "Creating array temporary" }
  write(v,*) matmul(transpose(a), transpose(e))     ! { dg-warning "Creating array temporary" }
  if (u /= v) stop 1


  call baz (transpose(a))


  call toto1 (a, transpose (c))
  if (any (a /= 2 * p + 12)) stop 1

  call toto1 (e, transpose (e))          ! { dg-warning "Creating array temporary" }
  if (any (e /= 4 * s + 12)) stop 1


  call toto2 (c, transpose (a))
  if (any (c /= 2 * q + 13)) stop 1

  call toto2 (e, transpose(e))           ! { dg-warning "Creating array temporary" }
  if (any (e /= 4 * r + 13)) stop 1

  call toto2 (e, transpose(transpose(e)))           ! { dg-warning "Creating array temporary" }
  if (any (e /= 4 * r + 14)) stop 1


  call toto3 (e, transpose(e))
  if (any (e /= 4 * r + 14)) stop 1


  call titi (nx, e, transpose(e))           ! { dg-warning "Creating array temporary" }
  if (any (e /= 4 * s + 17)) stop 1

  contains

  function foo (x)
    integer, intent(in) :: x(:,:)
    integer :: foo(size(x,1), size(x,2))
    foo = x + 1
  end function foo

  elemental function bar (x)
    integer, intent(in) :: x
    integer :: bar
    bar = x + 2
  end function bar

  subroutine baz (x)
    integer, intent(in) :: x(:,:)
  end subroutine baz

  elemental subroutine toto1 (x, y)
    integer, intent(out) :: x
    integer, intent(in)  :: y
    x = y + y
  end subroutine toto1

  subroutine toto2 (x, y)
    integer, dimension(:,:), intent(out) :: x
    integer, dimension(:,:), intent(in)  :: y
    x = y + 1
  end subroutine toto2

  subroutine toto3 (x, y)
    integer, dimension(:,:), intent(in) :: x, y
  end subroutine toto3

end

subroutine titi (n, x, y)
  integer :: n, x(n,n), y(n,n)
  x = y + 3
end subroutine titi

! No call to transpose
! { dg-final { scan-tree-dump-times "_gfortran_transpose" 0 "original" } }
!
! 24 temporaries
! { dg-final { scan-tree-dump-times "struct\[^\\n\]*atmp" 24 "original" } }
!
! 2 tests optimized out
! { dg-final { scan-tree-dump-times "_gfortran_abort" 39 "original" } }
! { # Commented out as failing at -O0: dg-final { scan-tree-dump-times "_gfortran_abort" 37 "optimized" } }
!
! cleanup
