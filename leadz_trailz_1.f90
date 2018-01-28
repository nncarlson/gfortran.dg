! { dg-do run }

  integer(kind=1) :: i1
  integer(kind=2) :: i2
  integer(kind=4) :: i4
  integer(kind=8) :: i8

  i1 = -1
  i2 = -1
  i4 = -1
  i8 = -1

  if (leadz(i1) /= 0) stop 1
  if (leadz(i2) /= 0) stop 1
  if (leadz(i4) /= 0) stop 1
  if (leadz(i8) /= 0) stop 1

  if (trailz(i1) /= 0) stop 1
  if (trailz(i2) /= 0) stop 1
  if (trailz(i4) /= 0) stop 1
  if (trailz(i8) /= 0) stop 1

  if (leadz(-1_1) /= 0) stop 1
  if (leadz(-1_2) /= 0) stop 1
  if (leadz(-1_4) /= 0) stop 1
  if (leadz(-1_8) /= 0) stop 1

  if (trailz(-1_1) /= 0) stop 1
  if (trailz(-1_2) /= 0) stop 1
  if (trailz(-1_4) /= 0) stop 1
  if (trailz(-1_8) /= 0) stop 1

  i1 = -64
  i2 = -64
  i4 = -64
  i8 = -64

  if (leadz(i1) /= 0) stop 1
  if (leadz(i2) /= 0) stop 1
  if (leadz(i4) /= 0) stop 1
  if (leadz(i8) /= 0) stop 1

  if (trailz(i1) /= 6) stop 1
  if (trailz(i2) /= 6) stop 1
  if (trailz(i4) /= 6) stop 1
  if (trailz(i8) /= 6) stop 1

  if (leadz(-64_1) /= 0) stop 1
  if (leadz(-64_2) /= 0) stop 1
  if (leadz(-64_4) /= 0) stop 1
  if (leadz(-64_8) /= 0) stop 1

  if (trailz(-64_1) /= 6) stop 1
  if (trailz(-64_2) /= 6) stop 1
  if (trailz(-64_4) /= 6) stop 1
  if (trailz(-64_8) /= 6) stop 1

  i1 = -108
  i2 = -108
  i4 = -108
  i8 = -108

  if (leadz(i1) /= 0) stop 1
  if (leadz(i2) /= 0) stop 1
  if (leadz(i4) /= 0) stop 1
  if (leadz(i8) /= 0) stop 1

  if (trailz(i1) /= 2) stop 1
  if (trailz(i2) /= 2) stop 1
  if (trailz(i4) /= 2) stop 1
  if (trailz(i8) /= 2) stop 1

  if (leadz(-108_1) /= 0) stop 1
  if (leadz(-108_2) /= 0) stop 1
  if (leadz(-108_4) /= 0) stop 1
  if (leadz(-108_8) /= 0) stop 1

  if (trailz(-108_1) /= 2) stop 1
  if (trailz(-108_2) /= 2) stop 1
  if (trailz(-108_4) /= 2) stop 1
  if (trailz(-108_8) /= 2) stop 1

  i1 = 1
  i2 = 1
  i4 = 1
  i8 = 1

  if (leadz(i1) /= bit_size(i1) - 1) stop 1
  if (leadz(i2) /= bit_size(i2) - 1) stop 1
  if (leadz(i4) /= bit_size(i4) - 1) stop 1
  if (leadz(i8) /= bit_size(i8) - 1) stop 1

  if (trailz(i1) /= 0) stop 1
  if (trailz(i2) /= 0) stop 1
  if (trailz(i4) /= 0) stop 1
  if (trailz(i8) /= 0) stop 1

  if (leadz(1_1) /= bit_size(1_1) - 1) stop 1
  if (leadz(1_2) /= bit_size(1_2) - 1) stop 1
  if (leadz(1_4) /= bit_size(1_4) - 1) stop 1
  if (leadz(1_8) /= bit_size(1_8) - 1) stop 1

  if (trailz(1_1) /= 0) stop 1
  if (trailz(1_2) /= 0) stop 1
  if (trailz(1_4) /= 0) stop 1
  if (trailz(1_8) /= 0) stop 1

  i1 = 64
  i2 = 64
  i4 = 64
  i8 = 64

  if (leadz(i1) /= 1) stop 1
  if (leadz(i2) /= 9) stop 1
  if (leadz(i4) /= 25) stop 1
  if (leadz(i8) /= 57) stop 1

  if (trailz(i1) /= 6) stop 1
  if (trailz(i2) /= 6) stop 1
  if (trailz(i4) /= 6) stop 1
  if (trailz(i8) /= 6) stop 1

  if (leadz(64_1) /= 1) stop 1
  if (leadz(64_2) /= 9) stop 1
  if (leadz(64_4) /= 25) stop 1
  if (leadz(64_8) /= 57) stop 1

  if (trailz(64_1) /= 6) stop 1
  if (trailz(64_2) /= 6) stop 1
  if (trailz(64_4) /= 6) stop 1
  if (trailz(64_8) /= 6) stop 1

end
