! { dg-do run }
! PR34325 Wrong error message for syntax error
program aa
implicit none
real(kind=8)::r1=0
character(25) :: a
a = 'I am not a )))))'')''.'
if ((((((a /= "I am not a )))))')'.")))))) stop 1
if ((((((a /= 'I am not a )))))'')''.')))))) stop 1
a = "I am not a )))))"")""."
if ((((((a /= "I am not a )))))"")"".")))))) stop 1
if (((3*r1)**2)>= 0) a = "good"
if (a /= "good") stop 1
end
