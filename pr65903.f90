! { dg-do run }
! { dg-options "-std=gnu" }
! 
character(20) :: astring

100 format ("& notblank !")
200 format ("&          !")
300 format ("&!")

write(astring,100)
if (astring.ne."& notblank !") stop 1
!print *, astring
write(astring,200)
if (astring.ne."&          !") stop 1
!print *, astring
write(astring,300)
if (astring.ne."&!") stop 1
!print *, astring

end
