! { dg-do run { target fd_truncate } }
! PR43605 FTELL intrinsic returns incorrect position
! Contributed by Janne Blomqvist, Manfred Schwarb
! and Dominique d'Humieres.
program ftell_3
  integer :: i, j
  character(1) :: ch
  character(len=99) :: buffer
  open(10, form='formatted', position='rewind')
  write(10, '(a)') '123456'
  write(10, '(a)') '789'
  write(10, '(a)') 'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
  write(10, '(a)') 'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
  rewind(10)
  read(10, '(a)') buffer
  call ftell(10, i)
! Expected: On '\n' systems: 7, on \r\n systems: 8
  if(i /= 7 .and. i /= 8) then
    stop 1
  end if
  read(10,'(a)') buffer
  if (trim(buffer) /= "789") then
     stop 1
  end if
  call ftell(10,j)
  close(10)
  open(10, access="stream")
! Expected: On '\n' systems: 11, on \r\n systems: 13
  if (i == 7) then
    read(10, pos=7) ch
    if (ch /= char(10)) stop 1
    if (j /= 11) stop 1
  end if
  if (i == 8) then
    read(10, pos=7) ch
    if (ch /= char(13)) stop 1
    read(10) ch
    if (ch /= char(10)) stop 1
    if (j /= 13) stop 1
  end if
  close(10, status="delete")
end program ftell_3
