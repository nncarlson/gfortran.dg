! { dg-do run }
! { dg-options "-fdefault-integer-8" }
! PR fortran/32968
program selected

  if (selected_int_kind (1)  /= 1) stop 1
  if (selected_int_kind (3)  /= 2) stop 1
  if (selected_int_kind (5)  /= 4) stop 1
  if (selected_int_kind (10) /= 8) stop 1
  if (selected_real_kind (1)  /= 4) stop 1
  if (selected_real_kind (2)  /= 4) stop 1
  if (selected_real_kind (9)  /= 8) stop 1
  if (selected_real_kind (10) /= 8) stop 1

end program selected

