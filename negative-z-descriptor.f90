! { dg-do run }
! PR 22217:  Z edit descriptor with negative numbers used to give lots of *

program main
  character(len=70) line
  character(len=20) fmt
  write(unit=line,fmt='(Z4)') -1_1
  if (line(1:4) .ne. '  FF') stop 1
  write(unit=line,fmt='(Z5)') -1_2
  if (line(1:5) .ne. ' FFFF') stop 1
  write(unit=line,fmt='(Z9)') -1_4
  if (line(1:9) .ne. ' FFFFFFFF') stop 1
  write(unit=line,fmt='(Z17)') -2_8
  if (line(1:17) .ne. ' FFFFFFFFFFFFFFFE') stop 1
  write(unit=line,fmt='(Z2)') 10_8
  if (line(1:2) .ne. ' A') stop 1

  write(unit=line,fmt='(Z8)') -43_8
  if (line(1:1) .ne. '*') stop 1

  write(unit=line,fmt='(B65)') -1_8
  if (line(1:2) .ne. ' 1') stop 1
  if (line(64:66) .ne. '11 ') stop 1

  write(unit=line,fmt='(O4)') -2_1
  if (line(1:4) .ne. ' 376') stop 1
end
