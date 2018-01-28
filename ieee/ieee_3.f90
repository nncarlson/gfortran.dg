! { dg-do run }

  use :: ieee_arithmetic
  implicit none

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  integer, parameter :: s = kind(sx1), d = kind(dx1)
  type(ieee_round_type) :: mode

  ! Test IEEE_IS_FINITE

  if (ieee_support_datatype(0._s)) then
    if (.not. ieee_is_finite(0.2_s)) stop 1
    if (.not. ieee_is_finite(-0.2_s)) stop 1
    if (.not. ieee_is_finite(0._s)) stop 1
    if (.not. ieee_is_finite(-0._s)) stop 1
    if (.not. ieee_is_finite(tiny(0._s))) stop 1
    if (.not. ieee_is_finite(tiny(0._s)/100)) stop 1
    if (.not. ieee_is_finite(huge(0._s))) stop 1
    if (.not. ieee_is_finite(-huge(0._s))) stop 1
    sx1 = huge(sx1)
    if (ieee_is_finite(2*sx1)) stop 1
    if (ieee_is_finite(2*(-sx1))) stop 1
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_finite(sx1)) stop 1
  end if

  if (ieee_support_datatype(0._d)) then
    if (.not. ieee_is_finite(0.2_d)) stop 1
    if (.not. ieee_is_finite(-0.2_d)) stop 1
    if (.not. ieee_is_finite(0._d)) stop 1
    if (.not. ieee_is_finite(-0._d)) stop 1
    if (.not. ieee_is_finite(tiny(0._d))) stop 1
    if (.not. ieee_is_finite(tiny(0._d)/100)) stop 1
    if (.not. ieee_is_finite(huge(0._d))) stop 1
    if (.not. ieee_is_finite(-huge(0._d))) stop 1
    dx1 = huge(dx1)
    if (ieee_is_finite(2*dx1)) stop 1
    if (ieee_is_finite(2*(-dx1))) stop 1
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_finite(dx1)) stop 1
  end if

  ! Test IEEE_IS_NAN

  if (ieee_support_datatype(0._s)) then
    if (ieee_is_nan(0.2_s)) stop 1
    if (ieee_is_nan(-0.2_s)) stop 1
    if (ieee_is_nan(0._s)) stop 1
    if (ieee_is_nan(-0._s)) stop 1
    if (ieee_is_nan(tiny(0._s))) stop 1
    if (ieee_is_nan(tiny(0._s)/100)) stop 1
    if (ieee_is_nan(huge(0._s))) stop 1
    if (ieee_is_nan(-huge(0._s))) stop 1
    sx1 = huge(sx1)
    if (ieee_is_nan(2*sx1)) stop 1
    if (ieee_is_nan(2*(-sx1))) stop 1
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(sx1)) stop 1
    sx1 = -1
    if (.not. ieee_is_nan(sqrt(sx1))) stop 1
  end if

  if (ieee_support_datatype(0._d)) then
    if (ieee_is_nan(0.2_d)) stop 1
    if (ieee_is_nan(-0.2_d)) stop 1
    if (ieee_is_nan(0._d)) stop 1
    if (ieee_is_nan(-0._d)) stop 1
    if (ieee_is_nan(tiny(0._d))) stop 1
    if (ieee_is_nan(tiny(0._d)/100)) stop 1
    if (ieee_is_nan(huge(0._d))) stop 1
    if (ieee_is_nan(-huge(0._d))) stop 1
    dx1 = huge(dx1)
    if (ieee_is_nan(2*dx1)) stop 1
    if (ieee_is_nan(2*(-dx1))) stop 1
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(dx1)) stop 1
    dx1 = -1
    if (.not. ieee_is_nan(sqrt(dx1))) stop 1
  end if

  ! IEEE_IS_NEGATIVE

  if (ieee_support_datatype(0._s)) then
    if (ieee_is_negative(0.2_s)) stop 1
    if (.not. ieee_is_negative(-0.2_s)) stop 1
    if (ieee_is_negative(0._s)) stop 1
    if (.not. ieee_is_negative(-0._s)) stop 1
    if (ieee_is_negative(tiny(0._s))) stop 1
    if (ieee_is_negative(tiny(0._s)/100)) stop 1
    if (.not. ieee_is_negative(-tiny(0._s))) stop 1
    if (.not. ieee_is_negative(-tiny(0._s)/100)) stop 1
    if (ieee_is_negative(huge(0._s))) stop 1
    if (.not. ieee_is_negative(-huge(0._s))) stop 1
    sx1 = huge(sx1)
    if (ieee_is_negative(2*sx1)) stop 1
    if (.not. ieee_is_negative(2*(-sx1))) stop 1
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_negative(sx1)) stop 1
    sx1 = -1
    if (ieee_is_negative(sqrt(sx1))) stop 1
  end if

  if (ieee_support_datatype(0._d)) then
    if (ieee_is_negative(0.2_d)) stop 1
    if (.not. ieee_is_negative(-0.2_d)) stop 1
    if (ieee_is_negative(0._d)) stop 1
    if (.not. ieee_is_negative(-0._d)) stop 1
    if (ieee_is_negative(tiny(0._d))) stop 1
    if (ieee_is_negative(tiny(0._d)/100)) stop 1
    if (.not. ieee_is_negative(-tiny(0._d))) stop 1
    if (.not. ieee_is_negative(-tiny(0._d)/100)) stop 1
    if (ieee_is_negative(huge(0._d))) stop 1
    if (.not. ieee_is_negative(-huge(0._d))) stop 1
    dx1 = huge(dx1)
    if (ieee_is_negative(2*dx1)) stop 1
    if (.not. ieee_is_negative(2*(-dx1))) stop 1
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_negative(dx1)) stop 1
    dx1 = -1
    if (ieee_is_negative(sqrt(dx1))) stop 1
  end if

  ! Test IEEE_IS_NORMAL

  if (ieee_support_datatype(0._s)) then
    if (.not. ieee_is_normal(0.2_s)) stop 1
    if (.not. ieee_is_normal(-0.2_s)) stop 1
    if (.not. ieee_is_normal(0._s)) stop 1
    if (.not. ieee_is_normal(-0._s)) stop 1
    if (.not. ieee_is_normal(tiny(0._s))) stop 1
    if (ieee_is_normal(tiny(0._s)/100)) stop 1
    if (.not. ieee_is_normal(-tiny(0._s))) stop 1
    if (ieee_is_normal(-tiny(0._s)/100)) stop 1
    if (.not. ieee_is_normal(huge(0._s))) stop 1
    if (.not. ieee_is_normal(-huge(0._s))) stop 1
    sx1 = huge(sx1)
    if (ieee_is_normal(2*sx1)) stop 1
    if (ieee_is_normal(2*(-sx1))) stop 1
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (ieee_is_normal(sx1)) stop 1
    sx1 = -1
    if (ieee_is_normal(sqrt(sx1))) stop 1
  end if

  if (ieee_support_datatype(0._d)) then
    if (.not. ieee_is_normal(0.2_d)) stop 1
    if (.not. ieee_is_normal(-0.2_d)) stop 1
    if (.not. ieee_is_normal(0._d)) stop 1
    if (.not. ieee_is_normal(-0._d)) stop 1
    if (.not. ieee_is_normal(tiny(0._d))) stop 1
    if (ieee_is_normal(tiny(0._d)/100)) stop 1
    if (.not. ieee_is_normal(-tiny(0._d))) stop 1
    if (ieee_is_normal(-tiny(0._d)/100)) stop 1
    if (.not. ieee_is_normal(huge(0._d))) stop 1
    if (.not. ieee_is_normal(-huge(0._d))) stop 1
    dx1 = huge(dx1)
    if (ieee_is_normal(2*dx1)) stop 1
    if (ieee_is_normal(2*(-dx1))) stop 1
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (ieee_is_normal(dx1)) stop 1
    dx1 = -1
    if (ieee_is_normal(sqrt(dx1))) stop 1
  end if

end
