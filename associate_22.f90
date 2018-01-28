! { dg-do run }
program foo

   implicit none

   character(len=4) :: s
   character(len=10) :: a

   ! This works.
   s = 'abc'
   associate(t => s)
      if (trim(t) /= 'abc') stop 1
   end associate

   ! This failed.
   associate(u => 'abc')
      if (trim(u) /= 'abc') stop 1
   end associate

   ! This failed.
   a = s // 'abc'
   associate(v => s // 'abc')
      if (trim(v) /= trim(a)) stop 1
   end associate

   ! This failed.
   ! This still doesn't work correctly, see PR 83344
!   a = trim(s) // 'abc'
!   associate(w => trim(s) // 'abc')
!      if (trim(w) /= trim(a)) stop 1
!   end associate

   ! This failed.
   associate(x => trim('abc'))
      if (trim(x) /= 'abc') stop 1
   end associate

end program foo
