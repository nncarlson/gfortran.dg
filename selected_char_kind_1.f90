! { dg-do run }
! 
! Checks for the SELECTED_CHAR_KIND intrinsic
!
  integer, parameter :: ascii = selected_char_kind ("ascii")
  integer, parameter :: default = selected_char_kind ("default")

  character(kind=ascii) :: s1
  character(kind=default) :: s2
  character(kind=selected_char_kind ("ascii")) :: s3
  character(kind=selected_char_kind ("default")) :: s4

  if (kind (s1) /= selected_char_kind ("ascii")) stop 1
  if (kind (s2) /= selected_char_kind ("default")) stop 1
  if (kind (s3) /= ascii) stop 1
  if (kind (s4) /= default) stop 1

  if (selected_char_kind("ascii") /= 1) stop 1
  if (selected_char_kind("default") /= 1) stop 1
  if (selected_char_kind("defauLt") /= 1) stop 1
  if (selected_char_kind("foo") /= -1) stop 1
  if (selected_char_kind("asciiiii") /= -1) stop 1
  if (selected_char_kind("default       ") /= 1) stop 1

  call test("ascii", 1)
  call test("default", 1)
  call test("defauLt", 1)
  call test("asciiiiii", -1)
  call test("foo", -1)
  call test("default     ", 1)
  call test("default     x", -1)

  call test(ascii_"ascii", 1)
  call test(ascii_"default", 1)
  call test(ascii_"defauLt", 1)
  call test(ascii_"asciiiiii", -1)
  call test(ascii_"foo", -1)
  call test(ascii_"default     ", 1)
  call test(ascii_"default     x", -1)

  call test(default_"ascii", 1)
  call test(default_"default", 1)
  call test(default_"defauLt", 1)
  call test(default_"asciiiiii", -1)
  call test(default_"foo", -1)
  call test(default_"default     ", 1)
  call test(default_"default     x", -1)

  if (kind (selected_char_kind ("")) /= kind(0)) stop 1
end

subroutine test(s,i)
  character(len=*,kind=selected_char_kind("ascii")) s
  integer i

  call test2(s,i)
  if (selected_char_kind (s) /= i) stop 1
end subroutine test

subroutine test2(s,i)
  character(len=*,kind=selected_char_kind("default")) s
  integer i

  if (selected_char_kind (s) /= i) stop 1
end subroutine test2
