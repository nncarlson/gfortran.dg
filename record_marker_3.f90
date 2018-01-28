! { dg-do run }
! { dg-options "-frecord-marker=8" }

program main
  implicit none
  integer (kind=8) :: i1, i2, i3

  open(15,form="UNFORMATTED")
  write (15) 1_8
  close (15)
  open (15,form="UNFORMATTED",access="DIRECT",recl=8)
  i1 = 1
  i2 = 2
  i3 = 3
  read (15,rec=1) i1
  read (15,rec=2) i2
  read (15,rec=3) i3
  close (15, status="DELETE")
  if (i1 /= 8) stop 1
  if (i2 /= 1) stop 1
  if (i3 /= 8) stop 1

  open(15,form="UNFORMATTED",convert="SWAP")
  write (15) 1_8
  close (15)
  open (15,form="UNFORMATTED",access="DIRECT",convert="SWAP",recl=8)
  i1 = 1
  i2 = 2
  i3 = 3
  read (15,rec=1) i1
  read (15,rec=2) i2
  read (15,rec=3) i3
  close(15,status="DELETE")
  if (i1 /= 8) stop 1
  if (i2 /= 1) stop 1
  if (i3 /= 8) stop 1

end program main
