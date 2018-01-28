! { dg-do run { xfail *-*-freebsd* *-*-dragonfly* hppa*-*-hpux* powerpc-ibm-aix* } }
! PR67367
program bug
   implicit none
   character(len=1) :: c
   character(len=256) :: message
   integer ios
   call system('[ -d junko.dir ] || mkdir junko.dir')
   open(unit=10, file='junko.dir',iostat=ios,action='read',access='stream')
   if (ios.ne.0) then
      call system('rmdir junko.dir')
      stop 1
   end if
   read(10, iostat=ios) c
   if (ios.ne.21) then 
      close(10, status='delete')
      stop 1
   end if
   close(10, status='delete')
end program bug
