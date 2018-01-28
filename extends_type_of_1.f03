! { dg-do run }
!
! Verifying the runtime behavior of the intrinsic function EXTENDS_TYPE_OF.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 implicit none

 intrinsic :: extends_type_of

 type :: t1
   integer :: i = 42
 end type

 type, extends(t1) :: t2
   integer :: j = 43
 end type

 type, extends(t2) :: t3
   class(t1),pointer :: cc
 end type

 class(t1), pointer :: c1,c2
 type(t1), target :: x
 type(t2), target :: y
 type(t3), target :: z
 
 c1 => x
 c2 => y
 z%cc => y

 if (.not. extends_type_of (c1, c1)) stop 1
 if (      extends_type_of (c1, c2)) stop 1
 if (.not. extends_type_of (c2, c1)) stop 1

 if (.not. extends_type_of (x, x)) stop 1
 if (      extends_type_of (x, y)) stop 1
 if (.not. extends_type_of (y, x)) stop 1

 if (.not. extends_type_of (c1, x)) stop 1
 if (      extends_type_of (c1, y)) stop 1
 if (.not. extends_type_of (x, c1)) stop 1
 if (.not. extends_type_of (y, c1)) stop 1

 if (.not. extends_type_of (z,   c1)) stop 1
 if (      extends_type_of (z%cc, z)) stop 1

end
