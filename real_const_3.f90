!{ dg-do run }
!{ dg-options "-fno-range-check" }
!{ dg-add-options ieee }
!{ dg-skip-if "NaN not supported" { spu-*-* } }
! PR19310 and PR19904, allow disabling range check during compile.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program main
  character(len=80) str
  real, parameter :: zero=0, nan=0/zero
  complex :: z = (-0.1,-2.2)/(0.0,0.0)
  complex :: z2 = (0.1,1)/0
  complex :: z3 = (1e35, -2e3)/1.234e-37
  complex :: z4 = (1e-35, -2e-35)/1234e34
  real :: a
  a = exp(1000.0)
  b = 1/exp(1000.0)

  write(str,*) a
  if (trim(adjustl(str)) .ne. 'Infinity') stop 1

  if (b .ne. 0.) stop 1

  write(str,*) -1.0/b
  if (trim(adjustl(str)) .ne. '-Infinity') stop 1

  write(str,*) b/0.0
  if (trim(adjustl(str)) .ne. 'NaN') stop 1

  write(str,*) 0.0/0.0
  if (trim(adjustl(str)) .ne. 'NaN') stop 1

  write(str,*) 1.0/(-0.)
  if (trim(adjustl(str)) .ne. '-Infinity') stop 1

  write(str,*) -2.0/0.
  if (trim(adjustl(str)) .ne. '-Infinity') stop 1

  write(str,*) 3.0/0.
  if (trim(adjustl(str)) .ne. 'Infinity') stop 1

  write(str,*)  nan
  if (trim(adjustl(str)) .ne. 'NaN') stop 1

  write(str,*) z
  if (trim(adjustl(str)) .ne. '(NaN,NaN)') stop 1

  write(str,*) z2
  if (trim(adjustl(str)) .ne. '(NaN,NaN)') stop 1

  write(str,*) z3
  if (trim(adjustl(str)) .ne. '(Inf,-Inf)') stop 1

  write(str,*) z4
  if (trim(adjustl(str)) .ne. '(0.00000000,-0.00000000)') stop 1
end program main
