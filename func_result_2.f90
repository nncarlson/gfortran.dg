! { dg-do run }
! Character functions with a result clause were broken
program testch
  if (ch().ne."hello     ") stop 1
contains
  function ch () result(str)
    character(len = 10)  :: str
    str ="hello"
  end function ch
end program testch
