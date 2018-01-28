! { dg-do run }
! PR43409 I/O: INQUIRE for SIZE does not work.
integer :: i
character(30) :: aname = "noname"
logical :: is_named

open(25, file="testfile_inquire_size", status="replace", access="stream", form="unformatted")
do i=1,100
  write(25) i, "abcdefghijklmnopqrstuvwxyz"
enddo
! Gfortran implicitly flushes the buffer when doing a file size
! inquire on an open file.
! flush(25)

inquire(unit=25, named=is_named, name=aname, size=i)
if (.not.is_named) stop 1
if (aname /= "testfile_inquire_size") stop 1
if (i /= 3000) stop 1

inquire(file="testfile_inquire_size", size=i)
if (.not.is_named) stop 1
if (aname /= "testfile_inquire_size") stop 1
if (i /= 3000) stop 1

close(25, status="delete")
inquire(file="testfile_inquire_size", size=i)
if (i /= -1)  stop 1
end


