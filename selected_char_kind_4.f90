! { dg-do run }
!
! Check that runtime result values of SELECTED_CHAR_KIND agree with
! front-end simplification results.
!
  implicit none
  character(len=20) :: s

  s = "ascii"
  if (selected_char_kind(s) /= selected_char_kind("ascii")) stop 1

  s = "default"
  if (selected_char_kind(s) /= selected_char_kind("default")) stop 1

  s = "iso_10646"
  if (selected_char_kind(s) /= selected_char_kind("iso_10646")) stop 1

  s = ""
  if (selected_char_kind(s) /= selected_char_kind("")) stop 1

  s = "invalid"
  if (selected_char_kind(s) /= selected_char_kind("invalid")) stop 1

end
