! { dg-do run }
! { dg-options "-fdump-tree-original -fdump-tree-original -fmax-stack-var-size=1" }
!
! PR fortran/56845
!
type t
end type t
type, extends(t) :: t2
end type t2
type(t) :: y
call foo()
call bar()
contains
  subroutine foo()
    class(t), allocatable :: x
    if(allocated(x)) stop 1
    if(.not.same_type_as(x,y)) stop 1
    allocate (t2 :: x)
  end
  subroutine bar()
    class(t), allocatable :: x(:)
    if(allocated(x)) stop 1
    if(.not.same_type_as(x,y)) stop 1
    allocate (t2 :: x(4))
  end
end
! { dg-final { scan-tree-dump-times "__builtin_free" 2 "original" } }
