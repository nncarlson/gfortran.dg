! { dg-do run { xfail i386-*-freebsd* } }

  use :: ieee_arithmetic
  implicit none

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3
  integer, parameter :: s = kind(sx1), d = kind(dx1)
  type(ieee_round_type) :: mode

  ! Test IEEE_CLASS

  if (ieee_support_datatype(0._s)) then
    sx1 = 0.1_s
    if (ieee_class(sx1) /= ieee_positive_normal) stop 1
    if (ieee_class(-sx1) /= ieee_negative_normal) stop 1
    sx1 = huge(sx1)
    if (ieee_class(sx1) /= ieee_positive_normal) stop 1
    if (ieee_class(-sx1) /= ieee_negative_normal) stop 1
    if (ieee_class(2*sx1) /= ieee_positive_inf) stop 1
    if (ieee_class(2*(-sx1)) /= ieee_negative_inf) stop 1
    sx1 = tiny(sx1)
    if (ieee_class(sx1) /= ieee_positive_normal) stop 1
    if (ieee_class(-sx1) /= ieee_negative_normal) stop 1
    if (ieee_class(sx1 / 2) /= ieee_positive_denormal) stop 1
    if (ieee_class((-sx1) / 2) /= ieee_negative_denormal) stop 1
    sx1 = -1
    if (ieee_class(sqrt(sx1)) /= ieee_quiet_nan) stop 1
    sx1 = 0
    if (ieee_class(sx1) /= ieee_positive_zero) stop 1
    if (ieee_class(-sx1) /= ieee_negative_zero) stop 1
  end if

  if (ieee_support_datatype(0._d)) then
    dx1 = 0.1_d
    if (ieee_class(dx1) /= ieee_positive_normal) stop 1
    if (ieee_class(-dx1) /= ieee_negative_normal) stop 1
    dx1 = huge(dx1)
    if (ieee_class(dx1) /= ieee_positive_normal) stop 1
    if (ieee_class(-dx1) /= ieee_negative_normal) stop 1
    if (ieee_class(2*dx1) /= ieee_positive_inf) stop 1
    if (ieee_class(2*(-dx1)) /= ieee_negative_inf) stop 1
    dx1 = tiny(dx1)
    if (ieee_class(dx1) /= ieee_positive_normal) stop 1
    if (ieee_class(-dx1) /= ieee_negative_normal) stop 1
    if (ieee_class(dx1 / 2) /= ieee_positive_denormal) stop 1
    if (ieee_class((-dx1) / 2) /= ieee_negative_denormal) stop 1
    dx1 = -1
    if (ieee_class(sqrt(dx1)) /= ieee_quiet_nan) stop 1
    dx1 = 0
    if (ieee_class(dx1) /= ieee_positive_zero) stop 1
    if (ieee_class(-dx1) /= ieee_negative_zero) stop 1
  end if

  ! Test IEEE_VALUE and IEEE_UNORDERED

  if (ieee_support_datatype(0._s)) then
    sx1 = ieee_value(sx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(sx1)) stop 1
    if (.not. ieee_unordered(sx1, sx1)) stop 1
    if (.not. ieee_unordered(sx1, 0._s)) stop 1
    if (.not. ieee_unordered(sx1, 0._d)) stop 1
    if (.not. ieee_unordered(0._s, sx1)) stop 1
    if (.not. ieee_unordered(0._d, sx1)) stop 1
    if (ieee_unordered(0._s, 0._s)) stop 1

    sx1 = ieee_value(sx1, ieee_positive_inf)
    if (ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (ieee_is_negative(sx1)) stop 1
    if (ieee_is_normal(sx1)) stop 1

    sx1 = ieee_value(sx1, ieee_negative_inf)
    if (ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (.not. ieee_is_negative(sx1)) stop 1
    if (ieee_is_normal(sx1)) stop 1

    sx1 = ieee_value(sx1, ieee_positive_normal)
    if (.not. ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (ieee_is_negative(sx1)) stop 1
    if (.not. ieee_is_normal(sx1)) stop 1

    sx1 = ieee_value(sx1, ieee_negative_normal)
    if (.not. ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (.not. ieee_is_negative(sx1)) stop 1
    if (.not. ieee_is_normal(sx1)) stop 1

    sx1 = ieee_value(sx1, ieee_positive_denormal)
    if (.not. ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (ieee_is_negative(sx1)) stop 1
    if (ieee_is_normal(sx1)) stop 1
    if (sx1 <= 0) stop 1
    if (sx1 >= tiny(sx1)) stop 1

    sx1 = ieee_value(sx1, ieee_negative_denormal)
    if (.not. ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (.not. ieee_is_negative(sx1)) stop 1
    if (ieee_is_normal(sx1)) stop 1
    if (sx1 >= 0) stop 1
    if (sx1 <= -tiny(sx1)) stop 1

    sx1 = ieee_value(sx1, ieee_positive_zero)
    if (.not. ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (ieee_is_negative(sx1)) stop 1
    if (.not. ieee_is_normal(sx1)) stop 1
    if (sx1 /= 0) stop 1

    sx1 = ieee_value(sx1, ieee_negative_zero)
    if (.not. ieee_is_finite(sx1)) stop 1
    if (ieee_is_nan(sx1)) stop 1
    if (.not. ieee_is_negative(sx1)) stop 1
    if (.not. ieee_is_normal(sx1)) stop 1
    if (sx1 /= 0) stop 1

  end if

  if (ieee_support_datatype(0._d)) then
    dx1 = ieee_value(dx1, ieee_quiet_nan)
    if (.not. ieee_is_nan(dx1)) stop 1
    if (.not. ieee_unordered(dx1, dx1)) stop 1
    if (.not. ieee_unordered(dx1, 0._s)) stop 1
    if (.not. ieee_unordered(dx1, 0._d)) stop 1
    if (.not. ieee_unordered(0._s, dx1)) stop 1
    if (.not. ieee_unordered(0._d, dx1)) stop 1
    if (ieee_unordered(0._d, 0._d)) stop 1

    dx1 = ieee_value(dx1, ieee_positive_inf)
    if (ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (ieee_is_negative(dx1)) stop 1
    if (ieee_is_normal(dx1)) stop 1

    dx1 = ieee_value(dx1, ieee_negative_inf)
    if (ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (.not. ieee_is_negative(dx1)) stop 1
    if (ieee_is_normal(dx1)) stop 1

    dx1 = ieee_value(dx1, ieee_positive_normal)
    if (.not. ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (ieee_is_negative(dx1)) stop 1
    if (.not. ieee_is_normal(dx1)) stop 1

    dx1 = ieee_value(dx1, ieee_negative_normal)
    if (.not. ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (.not. ieee_is_negative(dx1)) stop 1
    if (.not. ieee_is_normal(dx1)) stop 1

    dx1 = ieee_value(dx1, ieee_positive_denormal)
    if (.not. ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (ieee_is_negative(dx1)) stop 1
    if (ieee_is_normal(dx1)) stop 1
    if (dx1 <= 0) stop 1
    if (dx1 >= tiny(dx1)) stop 1

    dx1 = ieee_value(dx1, ieee_negative_denormal)
    if (.not. ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (.not. ieee_is_negative(dx1)) stop 1
    if (ieee_is_normal(dx1)) stop 1
    if (dx1 >= 0) stop 1
    if (dx1 <= -tiny(dx1)) stop 1

    dx1 = ieee_value(dx1, ieee_positive_zero)
    if (.not. ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (ieee_is_negative(dx1)) stop 1
    if (.not. ieee_is_normal(dx1)) stop 1
    if (dx1 /= 0) stop 1

    dx1 = ieee_value(dx1, ieee_negative_zero)
    if (.not. ieee_is_finite(dx1)) stop 1
    if (ieee_is_nan(dx1)) stop 1
    if (.not. ieee_is_negative(dx1)) stop 1
    if (.not. ieee_is_normal(dx1)) stop 1
    if (dx1 /= 0) stop 1

  end if

end
