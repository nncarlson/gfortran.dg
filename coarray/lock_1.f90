! { dg-do run }
!
! LOCK/UNLOCK check
!
! PR fortran/18918
!

use iso_fortran_env
implicit none

type(lock_type) :: lock[*]
integer :: stat
logical :: acquired

LOCK(lock)
UNLOCK(lock)

stat = 99
LOCK(lock, stat=stat)
if (stat /= 0) stop 1
stat = 99
UNLOCK(lock, stat=stat)
if (stat /= 0) stop 1

if (this_image() == 1) then
  acquired = .false.
  LOCK (lock[this_image()], acquired_lock=acquired)
  if (.not. acquired) stop 1
  UNLOCK (lock[1])
end if
end

