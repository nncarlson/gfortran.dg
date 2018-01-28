! { dg-do run }
! { dg-options "-w" }
! PR fortran/82884
! Original code contributed by Gerhard Steinmetz
program p
   character :: c(4) = [1h(, 1hi, 1h4, 1h)]
   if (c(1) /= '(') stop 1
   if (c(2) /= 'i') stop 1
   if (c(3) /= '4') stop 1
   if (c(4) /= ')') stop 1
end
