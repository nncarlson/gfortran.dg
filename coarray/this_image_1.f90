! { dg-do run }
!
! PR fortran/18918
!
! this_image(coarray) run test,
! expecially for num_images > 1
!
! Tested are values up to num_images == 8,
! higher values are OK, but not tested for
!
implicit none
integer :: a(1)[2:2, 3:4, 7:*]
integer :: b(:)[:, :,:]
allocatable :: b
integer :: i

if (this_image(A, dim=1) /= 2) stop 1
i = 1
if (this_image(A, dim=i) /= 2) stop 1

select case (this_image())
  case (1)
    if (this_image(A, dim=2) /= 3) stop 1
    if (this_image(A, dim=3) /= 7) stop 1
    i = 2
    if (this_image(A, dim=i) /= 3) stop 1
    i = 3
    if (this_image(A, dim=i) /= 7) stop 1
    if (any (this_image(A) /= [2,3,7])) stop 1

  case (2)
    if (this_image(A, dim=2) /= 4) stop 1
    if (this_image(A, dim=3) /= 7) stop 1
    i = 2
    if (this_image(A, dim=i) /= 4) stop 1
    i = 3
    if (this_image(A, dim=i) /= 7) stop 1
    if (any (this_image(A) /= [2,4,7])) stop 1

  case (3)
    if (this_image(A, dim=2) /= 3) stop 1
    if (this_image(A, dim=3) /= 8) stop 1
    i = 2
    if (this_image(A, dim=i) /= 3) stop 1
    i = 3
    if (this_image(A, dim=i) /= 8) stop 1
    if (any (this_image(A) /= [2,3,8])) stop 1

  case (4)
    if (this_image(A, dim=2) /= 4) stop 1
    if (this_image(A, dim=3) /= 8) stop 1
    i = 2
    if (this_image(A, dim=i) /= 4) stop 1
    i = 3
    if (this_image(A, dim=i) /= 8) stop 1
    if (any (this_image(A) /= [2,4,8])) stop 1

  case (5)
    if (this_image(A, dim=2) /= 3) stop 1
    if (this_image(A, dim=3) /= 9) stop 1
    i = 2
    if (this_image(A, dim=i) /= 3) stop 1
    i = 3
    if (this_image(A, dim=i) /= 9) stop 1
    if (any (this_image(A) /= [2,3,9])) stop 1

  case (6)
    if (this_image(A, dim=2) /= 4) stop 1
    if (this_image(A, dim=3) /= 9) stop 1
    i = 2
    if (this_image(A, dim=i) /= 4) stop 1
    i = 3
    if (this_image(A, dim=i) /= 9) stop 1
    if (any (this_image(A) /= [2,4,9])) stop 1

  case (7)
    if (this_image(A, dim=2) /= 3) stop 1
    if (this_image(A, dim=3) /= 10) stop 1
    i = 2
    if (this_image(A, dim=i) /= 3) stop 1
    i = 3
    if (this_image(A, dim=i) /= 10) stop 1
    if (any (this_image(A) /= [2,3,10])) stop 1

  case (8)
    if (this_image(A, dim=2) /= 4) stop 1
    if (this_image(A, dim=3) /= 10) stop 1
    i = 2
    if (this_image(A, dim=i) /= 4) stop 1
    i = 3
    if (this_image(A, dim=i) /= 10) stop 1
    if (any (this_image(A) /= [2,4,10])) stop 1
end select


allocate (b(3)[-1:0,2:4,*])

select case (this_image())
  case (1)
    if (this_image(B, dim=1) /= -1) stop 1
    if (this_image(B, dim=2) /= 2) stop 1
    if (this_image(B, dim=3) /= 1) stop 1
    i = 1
    if (this_image(B, dim=i) /= -1) stop 1
    i = 2
    if (this_image(B, dim=i) /= 2) stop 1
    i = 3
    if (this_image(B, dim=i) /= 1) stop 1
    if (any (this_image(B) /= [-1,2,1])) stop 1

  case (2)
    if (this_image(B, dim=1) /= 0) stop 1
    if (this_image(B, dim=2) /= 2) stop 1
    if (this_image(B, dim=3) /= 1) stop 1
    i = 1
    if (this_image(B, dim=i) /= 0) stop 1
    i = 2
    if (this_image(B, dim=i) /= 2) stop 1
    i = 3
    if (this_image(B, dim=i) /= 1) stop 1
    if (any (this_image(B) /= [0,2,1])) stop 1

  case (3)
    if (this_image(B, dim=1) /= -1) stop 1
    if (this_image(B, dim=2) /= 3) stop 1
    if (this_image(B, dim=3) /= 1) stop 1
    i = 1
    if (this_image(B, dim=i) /= -1) stop 1
    i = 2
    if (this_image(B, dim=i) /= 3) stop 1
    i = 3
    if (this_image(B, dim=i) /= 1) stop 1
    if (any (this_image(B) /= [-1,3,1])) stop 1

  case (4)
    if (this_image(B, dim=1) /= 0) stop 1
    if (this_image(B, dim=2) /= 3) stop 1
    if (this_image(B, dim=3) /= 1) stop 1
    i = 1
    if (this_image(B, dim=i) /= 0) stop 1
    i = 2
    if (this_image(B, dim=i) /= 3) stop 1
    i = 3
    if (this_image(B, dim=i) /= 1) stop 1
    if (any (this_image(B) /= [0,3,1])) stop 1

  case (5)
    if (this_image(B, dim=1) /= -1) stop 1
    if (this_image(B, dim=2) /= 4) stop 1
    if (this_image(B, dim=3) /= 1) stop 1
    i = 1
    if (this_image(B, dim=i) /= -1) stop 1
    i = 2
    if (this_image(B, dim=i) /= 4) stop 1
    i = 3
    if (this_image(B, dim=i) /= 1) stop 1
    if (any (this_image(B) /= [-1,4,1])) stop 1

  case (6)
    if (this_image(B, dim=1) /= 0) stop 1
    if (this_image(B, dim=2) /= 4) stop 1
    if (this_image(B, dim=3) /= 1) stop 1
    i = 1
    if (this_image(B, dim=i) /= 0) stop 1
    i = 2
    if (this_image(B, dim=i) /= 4) stop 1
    i = 3
    if (this_image(B, dim=i) /= 1) stop 1
    if (any (this_image(B) /= [0,4,1])) stop 1

  case (7)
    if (this_image(B, dim=1) /= -1) stop 1
    if (this_image(B, dim=2) /= 2) stop 1
    if (this_image(B, dim=3) /= 2) stop 1
    i = 1
    if (this_image(B, dim=i) /= -1) stop 1
    i = 2
    if (this_image(B, dim=i) /= 2) stop 1
    i = 3
    if (this_image(B, dim=i) /= 2) stop 1
    if (any (this_image(B) /= [-1,2,2])) stop 1

  case (8)
    if (this_image(B, dim=1) /= 0) stop 1
    if (this_image(B, dim=2) /= 2) stop 1
    if (this_image(B, dim=3) /= 2) stop 1
    i = 1
    if (this_image(B, dim=i) /= 0) stop 1
    i = 2
    if (this_image(B, dim=i) /= 2) stop 1
    i = 3
    if (this_image(B, dim=i) /= 2) stop 1
    if (any (this_image(B) /= [0,2,2])) stop 1
end select

end
