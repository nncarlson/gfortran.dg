! { dg-do run }
!
! PR fortran/33197
!
! Check implementation of PARITY
!
implicit none

integer :: i
logical :: Lt(1) = [ .true. ]
logical :: Lf(1) = [ .false.]
logical :: Ltf(2) = [ .true., .false. ]
logical :: Ltftf(4) = [.true., .false., .true.,.false.]

if (parity([logical ::]) .neqv. .false.) stop 1
if (parity([.true., .false.]) .neqv. .true.) stop 1
if (parity([.true.]) .neqv. .true.) stop 1
if (parity([.false.]) .neqv. .false.) stop 1
if (parity([.true., .false., .true.,.false.]) .neqv. .false.) stop 1
if (parity(reshape([.true., .false., .true.,.false.],[2,2])) &
    .neqv. .false.) stop 1
if (any (parity(reshape([.true., .false., .true.,.false.],[2,2]),dim=1) &
         .neqv. [.true., .true.])) stop 1
if (any (parity(reshape([.true., .false., .true.,.false.],[2,2]),dim=2) &
         .neqv. [.false., .false.])) stop 1

i = 0
if (parity(Lt(1:i)) .neqv. .false.) stop 1
if (parity(Ltf) .neqv. .true.) stop 1
if (parity(Lt) .neqv. .true.) stop 1
if (parity(Lf) .neqv. .false.) stop 1
if (parity(Ltftf) .neqv. .false.) stop 1
if (parity(reshape(Ltftf,[2,2])) &
    .neqv. .false.) stop 1
if (any (parity(reshape(Ltftf,[2,2]),dim=1) &
         .neqv. [.true., .true.])) stop 1
if (any (parity(reshape(Ltftf,[2,2]),dim=2) &
         .neqv. [.false., .false.])) stop 1

end
