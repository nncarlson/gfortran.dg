! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! PR fortran/50981
! PR fortran/54618
!

  implicit none
  type t
   integer, allocatable :: i
  end type t
  type, extends (t):: t2
   integer, allocatable :: j
  end type t2

  class(t), allocatable :: xa, xa2(:), xac[:], xa2c(:)[:]
  class(t), pointer :: xp, xp2(:)

  xp => null()
  xp2 => null()

  call suba(alloc=.false., prsnt=.false.)
  call suba(xa, alloc=.false., prsnt=.true.)
  if (.not. allocated (xa)) stop 1
  if (.not. allocated (xa%i)) stop 1
  if (xa%i /= 5) stop 1
  xa%i = -3
  call suba(xa, alloc=.true., prsnt=.true.)
  if (allocated (xa)) stop 1

  call suba2(alloc=.false., prsnt=.false.)
  call suba2(xa2, alloc=.false., prsnt=.true.)
  if (.not. allocated (xa2)) stop 1
  if (size (xa2) /= 1) stop 1
  if (.not. allocated (xa2(1)%i)) stop 1
  if (xa2(1)%i /= 5) stop 1
  xa2(1)%i = -3
  call suba2(xa2, alloc=.true., prsnt=.true.)
  if (allocated (xa2)) stop 1

  call subp(alloc=.false., prsnt=.false.)
  call subp(xp, alloc=.false., prsnt=.true.)
  if (.not. associated (xp)) stop 1
  if (.not. allocated (xp%i)) stop 1
  if (xp%i /= 5) stop 1
  xp%i = -3
  call subp(xp, alloc=.true., prsnt=.true.)
  if (associated (xp)) stop 1

  call subp2(alloc=.false., prsnt=.false.)
  call subp2(xp2, alloc=.false., prsnt=.true.)
  if (.not. associated (xp2)) stop 1
  if (size (xp2) /= 1) stop 1
  if (.not. allocated (xp2(1)%i)) stop 1
  if (xp2(1)%i /= 5) stop 1
  xp2(1)%i = -3
  call subp2(xp2, alloc=.true., prsnt=.true.)
  if (associated (xp2)) stop 1

  call subac(alloc=.false., prsnt=.false.)
  call subac(xac, alloc=.false., prsnt=.true.)
  if (.not. allocated (xac)) stop 1
  if (.not. allocated (xac%i)) stop 1
  if (xac%i /= 5) stop 1
  xac%i = -3
  call subac(xac, alloc=.true., prsnt=.true.)
  if (allocated (xac)) stop 1

  call suba2c(alloc=.false., prsnt=.false.)
  call suba2c(xa2c, alloc=.false., prsnt=.true.)
  if (.not. allocated (xa2c)) stop 1
  if (size (xa2c) /= 1) stop 1
  if (.not. allocated (xa2c(1)%i)) stop 1
  if (xa2c(1)%i /= 5) stop 1
  xa2c(1)%i = -3
  call suba2c(xa2c, alloc=.true., prsnt=.true.)
  if (allocated (xa2c)) stop 1

contains
 subroutine suba2c(x, prsnt, alloc)
   class(t), optional, allocatable :: x(:)[:]
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) stop 1
   if (prsnt) then
     if (alloc .neqv. allocated(x)) stop 1
     if (.not. allocated (x)) then
       allocate (x(1)[*])
       x(1)%i = 5
     else
       if (x(1)%i /= -3) stop 1
       deallocate (x)
     end if
   end if
 end subroutine suba2c

 subroutine subac(x, prsnt, alloc)
   class(t), optional, allocatable :: x[:]
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) stop 1
   if (present (x)) then
     if (alloc .neqv. allocated(x)) stop 1
     if (.not. allocated (x)) then
       allocate (x[*])
       x%i = 5
     else
       if (x%i /= -3) stop 1
       deallocate (x)
     end if
   end if
 end subroutine subac

 subroutine suba2(x, prsnt, alloc)
   class(t), optional, allocatable :: x(:)
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) stop 1
   if (prsnt) then
     if (alloc .neqv. allocated(x)) stop 1
     if (.not. allocated (x)) then
       allocate (x(1))
       x(1)%i = 5
     else
       if (x(1)%i /= -3) stop 1
       deallocate (x)
     end if
   end if
 end subroutine suba2

 subroutine suba(x, prsnt, alloc)
   class(t), optional, allocatable :: x
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) stop 1
   if (present (x)) then
     if (alloc .neqv. allocated(x)) stop 1
     if (.not. allocated (x)) then
       allocate (x)
       x%i = 5
     else
       if (x%i /= -3) stop 1
       deallocate (x)
     end if
   end if
 end subroutine suba

 subroutine subp2(x, prsnt, alloc)
   class(t), optional, pointer :: x(:)
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) stop 1
   if (present (x)) then
     if (alloc .neqv. associated(x)) stop 1
     if (.not. associated (x)) then
       allocate (x(1))
       x(1)%i = 5
     else
       if (x(1)%i /= -3) stop 1
       deallocate (x)
     end if
   end if
 end subroutine subp2

 subroutine subp(x, prsnt, alloc)
   class(t), optional, pointer :: x
   logical prsnt, alloc
   if (present (x) .neqv. prsnt) stop 1
   if (present (x)) then
     if (alloc .neqv. associated(x)) stop 1
     if (.not. associated (x)) then
       allocate (x)
       x%i = 5
     else
       if (x%i /= -3) stop 1
       deallocate (x)
     end if
   end if
 end subroutine subp
end
