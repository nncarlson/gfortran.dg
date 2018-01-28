! { dg-do run }
  integer(kind=8) o
  open (10, status="scratch")
  if (ftell(10) /= 0) stop 1
  write (10,"(A)") "1234567"
  if (ftell(10) /= 8 .and. ftell(10) /= 9) stop 1
  o = ftell(10)
  write (10,"(A)") "1234567"
  if (ftell(10) /= 2 * o) stop 1
  close (10)
  if (ftell(10) /= -1) stop 1
  end
