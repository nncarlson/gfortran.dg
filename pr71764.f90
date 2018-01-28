! { dg-do run }
! PR71764 
program p
   use iso_c_binding, only: c_ptr, c_null_ptr, c_ptr, c_associated, c_loc
   logical, target :: rls
   real, target :: t = 3.14
   type(c_ptr) :: nullptr,c
   real, pointer :: k
   nullptr = c_null_ptr
   c = nullptr
   rls = c_associated(c)
   if (rls) stop 1
   if (c_associated(c)) stop 1
   c = c_loc(rls)
   if (.not. c_associated(c)) stop 1
   c = nullptr
   if (c_associated(c)) stop 1
   c = c_loc(t)
   k => t
   call association_test(k, c)
contains
  subroutine association_test(a,b)
    use iso_c_binding, only: c_associated, c_loc, c_ptr
    implicit none
    real, pointer :: a
    type(c_ptr) :: b
    if(c_associated(b, c_loc(a))) then
       return
    else
       stop 1
    end if
  end subroutine association_test
end

