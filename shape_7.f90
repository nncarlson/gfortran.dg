! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/52093
!
! Contributed by Mohammad Rahmani
!

Program Main
 Implicit None
 Integer:: X(2,2)
 Integer:: X2(7:11,8:9)

 if (size((X)) /= 4) stop 1
 if (any (Shape((X))  /= [2,2])) stop 1
 if (any (lbound((X)) /= [1,1])) stop 1
 if (any (ubound((X)) /= [2,2])) stop 1

 if (size(X2) /= 10) stop 1
 if (any (Shape(X2)  /= [5,2])) stop 1
 if (any (lbound(X2) /= [7,8]))  stop 1
 if (any (ubound(X2) /= [11,9])) stop 1

 if (size((X2)) /= 10) stop 1
 if (any (Shape((X2))  /= [5,2])) stop 1
 if (any (lbound((X2)) /= [1,1])) stop 1
 if (any (ubound((X2)) /= [5,2])) stop 1
End Program Main

! { dg-final { scan-tree-dump-times "abort" 0 "original" } }

