! { dg-do run }
module iso_fortran_env
  real :: x
end module iso_fortran_env

subroutine bar
  use , intrinsic :: iso_fortran_env
  implicit none

  if (file_storage_size /= 8) stop 1
  if (character_storage_size /= 8) stop 1
  if (all (numeric_storage_size /= [ 8, 16, 32, 64, 128])) stop 1
  if (input_unit /= 5) stop 1
  if (output_unit /= 6) stop 1
  if (error_unit /= 0) stop 1
  if (iostat_end /= -1) stop 1
  if (iostat_eor /= -2) stop 1
end

subroutine bar2
  use , intrinsic :: iso_fortran_env, only : file_storage_size, &
    character_storage_size, numeric_storage_size, input_unit, output_unit, &
    error_unit, iostat_end, iostat_eor
  implicit none

  if (file_storage_size /= 8) stop 1
  if (character_storage_size /= 8) stop 1
  if (all (numeric_storage_size /= [ 8, 16, 32, 64, 128])) stop 1
  if (input_unit /= 5) stop 1
  if (output_unit /= 6) stop 1
  if (error_unit /= 0) stop 1
  if (iostat_end /= -1) stop 1
  if (iostat_eor /= -2) stop 1
end

program test
  use , intrinsic :: iso_fortran_env, uu => output_unit
  implicit none

  if (input_unit /= 5 .or. uu /= 6) stop 1
  call bar
  call bar2
end
