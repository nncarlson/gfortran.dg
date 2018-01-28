! { dg-do run }
  integer(kind=8) o, o2

  open (10, status="scratch")
  call ftell (10, o)
  if (o /= 0) stop 1
  write (10,"(A)") "1234567"
  call ftell (10, o)
  if (o /= 8 .and. o /= 9) stop 1
  write (10,"(A)") "1234567"
  call ftell (10, o2)
  if (o2 /= 2 * o) stop 1
  close (10)
  call ftell (10, o)
  if (o /= -1) stop 1
  end
