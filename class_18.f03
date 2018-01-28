! { dg-do run }
!
! PR 43207: [OOP] ICE for class pointer => null() initialization
!
! Original test case by Tobias Burnus <burnus@gcc.gnu.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

  implicit none
  type :: parent
  end type
  type(parent), target :: t
  class(parent), pointer :: cp => null()

  if (associated(cp)) stop 1
  cp => t
  if (.not. associated(cp)) stop 1

end
