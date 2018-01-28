! { dg-do run }
! PR 25292: Check that the intrinsic associated works with functions returning
! pointers as arguments
program test
   real, pointer :: a, b

   nullify(a,b)
   if(associated(a,b).or.associated(a,a)) stop 1
   allocate(a)
   if(associated(b,a)) stop 1
   if (.not.associated(x(a))) stop 1
   if (.not.associated(a, x(a))) stop 1

   nullify(b)
   if (associated(x(b))) stop 1
   allocate(b)
   if (associated(x(b), x(a))) stop 1

contains

  function x(a) RESULT(b)
    real, pointer :: a,b
    b => a
  end function x

end program test
