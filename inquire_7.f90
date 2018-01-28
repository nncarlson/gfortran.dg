! { dg-do run }
! pr 19647 / segfault on inquire(..pad=..)
!   Thomas.Koenig@online.de
!   bdavis9659@comcast.net
       program main
       character(len=10) delim
! quote
       open(10,delim='quote',status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'QUOTE') stop 1
! apostrophe
       open(10,delim='apostrophe',status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'APOSTROPHE') stop 1
! none
       open(10,status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'NONE') stop 1
! undefined
       open(10,form='UNFORMATTED',status='SCRATCH')
       inquire(10,delim=delim)
       close(10)
       if (delim .ne. 'UNDEFINED') stop 1
       end program main
