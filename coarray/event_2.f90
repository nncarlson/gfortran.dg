! { dg-do run }
!
! Run-time test for EVENT_TYPE
!
use iso_fortran_env, only: event_type
implicit none

type(event_type), save, allocatable :: var(:)[:]
integer :: count, stat

allocate(var(3)[*])

count = -42
call event_query (var(1), count)
if (count /= 0) stop 1
call event_query (var(1), count)
if (count /= 0) stop 1
call event_query (var(2), count)
if (count /= 0) stop 1
call event_query (var(3), count)
if (count /= 0) stop 1

stat = 99
event post (var(2), stat=stat)
if (stat /= 0) stop 1
call event_query (var(1), count)
if (count /= 0) stop 1
call event_query(var(2), count, stat=stat)
if (count /= 1 .or. stat /= 0) stop 1
call event_query (var(3), count)
if (count /= 0) stop 1

stat = 99
event post (var(2)[this_image()])
call event_query(var(1), count)
if (count /= 0) stop 1
call event_query(var(2), count)
if (count /= 2) stop 1
call event_query(var(2), count)
if (count /= 2) stop 1
call event_query(var(3), count)
if (count /= 0) stop 1

stat = 99
event wait (var(2))
call event_query(var(1), count)
if (count /= 0) stop 1
call event_query(var(2), count)
if (count /= 1) stop 1
call event_query(var(3), count)
if (count /= 0) stop 1

stat = 99
event post (var(2))
call event_query(var(1), count)
if (count /= 0) stop 1
call event_query(var(2), count)
if (count /= 2) stop 1
call event_query(var(3), count)
if (count /= 0) stop 1

stat = 99
event post (var(2))
call event_query(var(1), count)
if (count /= 0) stop 1
call event_query(var(2), count)
if (count /= 3) stop 1
call event_query(var(3), count)
if (count /= 0) stop 1

stat = 99
event wait (var(2), until_count=2)
call event_query(var(1), count)
if (count /= 0) stop 1
call event_query(var(2), count)
if (count /= 1) stop 1
call event_query(var(3), count)
if (count /= 0) stop 1

stat = 99
event wait (var(2), stat=stat, until_count=1)
if (stat /= 0) stop 1
call event_query(event=var(1), stat=stat, count=count)
if (count /= 0 .or. stat /= 0) stop 1
call event_query(event=var(2), stat=stat, count=count)
if (count /= 0 .or. stat /= 0) stop 1
call event_query(event=var(3), stat=stat, count=count)
if (count /= 0 .or. stat /= 0) stop 1
end
