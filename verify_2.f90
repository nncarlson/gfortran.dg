! { dg-do run }
program verify_2
  character(len=3) s1, s2
  s1 = 'abc'
  s2 = ''
  if (verify('ab', '') /= 1) stop 1
  if (verify(s1, s2)   /= 1) stop 1
  if (verify('abc', '', .true.) /= 3) stop 1
  if (verify(s1, s2, .true.) /= 3) stop 1
end program verify_2

