! { dg-do run }
! { dg-options "-fbackslash" }

  character(kind=1,len=20) :: s1, t1
  character(kind=4,len=20) :: s4, t4

  call test (4_"ccc  ", 4_"bbb", 4_"ccc", 4_"ddd")
  call test (4_" \xACp  ", 4_" \x900000 ", 4_" \xACp  ", 4_"ddd")
  call test (4_" \xACp  ", 4_" \x900000 ", 4_" \xACp  ", 4_"ddd")

  call test2 (4_" \x900000 ", 4_" \xACp  ", 4_"ddd")

contains

  subroutine test(s4, t4, u4, v4)
    character(kind=4,len=*) :: s4, t4, u4, v4

    if (.not. (s4 >= t4)) stop 1
    if (.not. (s4 > t4)) stop 1
    if (.not. (s4 .ge. t4)) stop 1
    if (.not. (s4 .gt. t4)) stop 1
    if (      (s4 == t4)) stop 1
    if (.not. (s4 /= t4)) stop 1
    if (      (s4 .eq. t4)) stop 1
    if (.not. (s4 .ne. t4)) stop 1
    if (      (s4 <= t4)) stop 1
    if (      (s4 < t4)) stop 1
    if (      (s4 .le. t4)) stop 1
    if (      (s4 .lt. t4)) stop 1

    if (.not. (s4 >= u4)) stop 1
    if (      (s4 > u4)) stop 1
    if (.not. (s4 .ge. u4)) stop 1
    if (      (s4 .gt. u4)) stop 1
    if (.not. (s4 == u4)) stop 1
    if (      (s4 /= u4)) stop 1
    if (.not. (s4 .eq. u4)) stop 1
    if (      (s4 .ne. u4)) stop 1
    if (.not. (s4 <= u4)) stop 1
    if (      (s4 < u4)) stop 1
    if (.not. (s4 .le. u4)) stop 1
    if (      (s4 .lt. u4)) stop 1

    if (      (s4 >= v4)) stop 1
    if (      (s4 > v4)) stop 1
    if (      (s4 .ge. v4)) stop 1
    if (      (s4 .gt. v4)) stop 1
    if (      (s4 == v4)) stop 1
    if (.not. (s4 /= v4)) stop 1
    if (      (s4 .eq. v4)) stop 1
    if (.not. (s4 .ne. v4)) stop 1
    if (.not. (s4 <= v4)) stop 1
    if (.not. (s4 < v4)) stop 1
    if (.not. (s4 .le. v4)) stop 1
    if (.not. (s4 .lt. v4)) stop 1

  end subroutine test

  subroutine test2(t4, u4, v4)
    character(kind=4,len=*) :: t4, u4, v4

    if (.not. (4_" \xACp  " >= t4)) stop 1
    if (.not. (4_" \xACp  " > t4)) stop 1
    if (.not. (4_" \xACp  " .ge. t4)) stop 1
    if (.not. (4_" \xACp  " .gt. t4)) stop 1
    if (      (4_" \xACp  " == t4)) stop 1
    if (.not. (4_" \xACp  " /= t4)) stop 1
    if (      (4_" \xACp  " .eq. t4)) stop 1
    if (.not. (4_" \xACp  " .ne. t4)) stop 1
    if (      (4_" \xACp  " <= t4)) stop 1
    if (      (4_" \xACp  " < t4)) stop 1
    if (      (4_" \xACp  " .le. t4)) stop 1
    if (      (4_" \xACp  " .lt. t4)) stop 1

    if (.not. (4_" \xACp  " >= u4)) stop 1
    if (      (4_" \xACp  " > u4)) stop 1
    if (.not. (4_" \xACp  " .ge. u4)) stop 1
    if (      (4_" \xACp  " .gt. u4)) stop 1
    if (.not. (4_" \xACp  " == u4)) stop 1
    if (      (4_" \xACp  " /= u4)) stop 1
    if (.not. (4_" \xACp  " .eq. u4)) stop 1
    if (      (4_" \xACp  " .ne. u4)) stop 1
    if (.not. (4_" \xACp  " <= u4)) stop 1
    if (      (4_" \xACp  " < u4)) stop 1
    if (.not. (4_" \xACp  " .le. u4)) stop 1
    if (      (4_" \xACp  " .lt. u4)) stop 1

    if (      (4_" \xACp  " >= v4)) stop 1
    if (      (4_" \xACp  " > v4)) stop 1
    if (      (4_" \xACp  " .ge. v4)) stop 1
    if (      (4_" \xACp  " .gt. v4)) stop 1
    if (      (4_" \xACp  " == v4)) stop 1
    if (.not. (4_" \xACp  " /= v4)) stop 1
    if (      (4_" \xACp  " .eq. v4)) stop 1
    if (.not. (4_" \xACp  " .ne. v4)) stop 1
    if (.not. (4_" \xACp  " <= v4)) stop 1
    if (.not. (4_" \xACp  " < v4)) stop 1
    if (.not. (4_" \xACp  " .le. v4)) stop 1
    if (.not. (4_" \xACp  " .lt. v4)) stop 1

  end subroutine test2

  subroutine test3(t4, u4, v4)
    character(kind=4,len=*) :: t4, u4, v4

    if (.not. (4_" \xACp  " >= 4_" \x900000 ")) stop 1
    if (.not. (4_" \xACp  " > 4_" \x900000 ")) stop 1
    if (.not. (4_" \xACp  " .ge. 4_" \x900000 ")) stop 1
    if (.not. (4_" \xACp  " .gt. 4_" \x900000 ")) stop 1
    if (      (4_" \xACp  " == 4_" \x900000 ")) stop 1
    if (.not. (4_" \xACp  " /= 4_" \x900000 ")) stop 1
    if (      (4_" \xACp  " .eq. 4_" \x900000 ")) stop 1
    if (.not. (4_" \xACp  " .ne. 4_" \x900000 ")) stop 1
    if (      (4_" \xACp  " <= 4_" \x900000 ")) stop 1
    if (      (4_" \xACp  " < 4_" \x900000 ")) stop 1
    if (      (4_" \xACp  " .le. 4_" \x900000 ")) stop 1
    if (      (4_" \xACp  " .lt. 4_" \x900000 ")) stop 1

    if (.not. (4_" \xACp  " >= 4_" \xACp  ")) stop 1
    if (      (4_" \xACp  " > 4_" \xACp  ")) stop 1
    if (.not. (4_" \xACp  " .ge. 4_" \xACp  ")) stop 1
    if (      (4_" \xACp  " .gt. 4_" \xACp  ")) stop 1
    if (.not. (4_" \xACp  " == 4_" \xACp  ")) stop 1
    if (      (4_" \xACp  " /= 4_" \xACp  ")) stop 1
    if (.not. (4_" \xACp  " .eq. 4_" \xACp  ")) stop 1
    if (      (4_" \xACp  " .ne. 4_" \xACp  ")) stop 1
    if (.not. (4_" \xACp  " <= 4_" \xACp  ")) stop 1
    if (      (4_" \xACp  " < 4_" \xACp  ")) stop 1
    if (.not. (4_" \xACp  " .le. 4_" \xACp  ")) stop 1
    if (      (4_" \xACp  " .lt. 4_" \xACp  ")) stop 1

    if (      (4_" \xACp  " >= 4_"ddd")) stop 1
    if (      (4_" \xACp  " > 4_"ddd")) stop 1
    if (      (4_" \xACp  " .ge. 4_"ddd")) stop 1
    if (      (4_" \xACp  " .gt. 4_"ddd")) stop 1
    if (      (4_" \xACp  " == 4_"ddd")) stop 1
    if (.not. (4_" \xACp  " /= 4_"ddd")) stop 1
    if (      (4_" \xACp  " .eq. 4_"ddd")) stop 1
    if (.not. (4_" \xACp  " .ne. 4_"ddd")) stop 1
    if (.not. (4_" \xACp  " <= 4_"ddd")) stop 1
    if (.not. (4_" \xACp  " < 4_"ddd")) stop 1
    if (.not. (4_" \xACp  " .le. 4_"ddd")) stop 1
    if (.not. (4_" \xACp  " .lt. 4_"ddd")) stop 1

  end subroutine test3

end
