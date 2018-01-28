! { dg-do run }
!
! PR fortran/37400
!
module mod 
   implicit character(len=*,kind=kind('A')) (Q) 
   parameter(Q1 = '12345678')     ! len=8
   parameter(Q2 = 'abcdefghijkl') ! len=12
   contains 
      subroutine sub(Q3) 
         if(len('#'//Q3//'#') /= 15) stop 1
         if('#'//Q3//'#' /= '#ABCDEFGHIJKLM#') stop 1
      end subroutine sub 
end module mod 
program startest 
   use mod 
   implicit none
   if(len('#'//Q1//'#') /= 10) stop 1
   if(len('#'//Q2//'#') /= 14) stop 1
   if('#'//Q1//'#' /='#12345678#') stop 1
   if('#'//Q2//'#' /='#abcdefghijkl#') stop 1
   call sub('ABCDEFGHIJKLM') ! len=13
end program startest
