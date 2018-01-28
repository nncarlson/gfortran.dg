! { dg-do run }
program p
 implicit none

 real   , parameter :: arr(3) = [ real    :: 2, 2.5, (1.5, 2.5) ]
 real   , parameter :: ari(3) = [ integer :: 2, 2.5, (1.5, 2.5) ]
 real   , parameter :: arc(3) = [ complex :: 2, 2.5, (1.5, 2.5) ]
 integer, parameter :: air(3) = [ real    :: 2, 2.5, (1.5, 2.5) ]
 integer, parameter :: aii(3) = [ integer :: 2, 2.5, (1.5, 2.5) ]
 integer, parameter :: aic(3) = [ complex :: 2, 2.5, (1.5, 2.5) ]
 complex, parameter :: acr(3) = [ real    :: 2, 2.5, (1.5, 2.5) ]
 complex, parameter :: aci(3) = [ integer :: 2, 2.5, (1.5, 2.5) ]
 complex, parameter :: acc(3) = [ complex :: 2, 2.5, (1.5, 2.5) ]

 real   , parameter :: mrr(3) =  4.5       * [ real    :: 2, 2.5, (3.5, 4.0) ]
 real   , parameter :: mri(3) =  4.5       * [ integer :: 2, 2.5, (3.5, 4.0) ]
 real   , parameter :: mrc(3) =  4.5       * [ complex :: 2, 2.5, (3.5, 4.0) ]
 integer, parameter :: mir(3) =  4         * [ real    :: 2, 2.5, (3.5, 4.0) ]
 integer, parameter :: mii(3) =  4         * [ integer :: 2, 2.5, (3.5, 4.0) ]
 integer, parameter :: mic(3) =  4         * [ complex :: 2, 2.5, (3.5, 4.0) ]
 complex, parameter :: mcr(3) = (4.5, 5.5) * [ real    :: 2, 2.5, (3.5, 4.0) ]
 complex, parameter :: mci(3) = (4.5, 5.5) * [ integer :: 2, 2.5, (3.5, 4.0) ]
 complex, parameter :: mcc(3) = (4.5, 5.5) * [ complex :: 2, 2.5, (3.5, 4.0) ]

 if (any(arr /= [2.00, 2.50, 1.50])) stop 1
 if (any(ari /= [2.00, 2.00, 1.00])) stop 1
 if (any(arc /= [2.00, 2.50, 1.50])) stop 1

 if (any(air /= [2, 2, 1])) stop 1
 if (any(aii /= [2, 2, 1])) stop 1
 if (any(aic /= [2, 2, 1])) stop 1

 if (any(acr /= [(2.00, 0.00), (2.50, 0.00), (1.50, 0.00)])) stop 1
 if (any(aci /= [(2.00, 0.00), (2.00, 0.00), (1.00, 0.00)])) stop 1
 if (any(acc /= [(2.00, 0.00), (2.50, 0.00), (1.50, 2.50)])) stop 1

 if (any(mrr /= [9.00, 11.25, 15.75])) stop 1
 if (any(mri /= [9.00,  9.00, 13.50])) stop 1
 if (any(mrc /= [9.00, 11.25, 15.75])) stop 1

 if (any(mir /= [8, 10, 14])) stop 1
 if (any(mii /= [8,  8, 12])) stop 1
 if (any(mic /= [8, 10, 14])) stop 1

 if (any(mcr /= [(9.00, 11.00), (11.25, 13.75), (15.75, 19.25)])) stop 1
 if (any(mci /= [(9.00, 11.00), ( 9.00, 11.00), (13.50, 16.50)])) stop 1
 if (any(mcc /= [(9.00, 11.00), (11.25, 13.75), (-6.25, 37.25)])) stop 1

end program p
