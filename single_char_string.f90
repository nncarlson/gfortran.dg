! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR12456 - Optimize string(k:k) as single character.

Program pr12456
character a
character b
character (len=5) :: c
integer i

b = 'a'
a = b
if (a .ne. 'a') stop 1
if (a .ne. b) stop 1
c (3:3) = 'a'
if (c (3:3) .ne. b) stop 1
if (c (3:3) .ne. 'a') stop 1
if (LGT (a, c (3:3))) stop 1
if (LGT (a, 'a')) stop 1

i = 3
c (i:i) = 'a'
if (c (i:i) .ne. b) stop 1
if (c (i:i) .ne. 'a') stop 1
if (LGT (a, c (i:i))) stop 1

if (a .gt. char (255)) stop 1
end

! There should not be _gfortran_compare_string and _gfortran_copy_string in
! the dumped file.

! { dg-final { scan-tree-dump-times "_gfortran_compare_string" 0 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_copy_string" 0 "original" } }

