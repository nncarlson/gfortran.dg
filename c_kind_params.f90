! { dg-do run }
! { dg-require-effective-target stdint_types }
! { dg-additional-sources c_kinds.c }
! { dg-options "-w -std=c99" }
! the -w option is needed to make f951 not report a warning for 
! the -std=c99 option that the C file needs.
!
module c_kind_params
  use, intrinsic :: iso_c_binding
  implicit none

contains
  subroutine param_test(my_short, my_int, my_long, my_long_long, &
       my_int8_t, my_int_least8_t, my_int_fast8_t, &
       my_int16_t, my_int_least16_t, my_int_fast16_t, &
       my_int32_t, my_int_least32_t, my_int_fast32_t, &
       my_int64_t, my_int_least64_t, my_int_fast64_t, &
       my_intmax_t, my_intptr_t, my_float, my_double, my_long_double, &
       my_char, my_bool) bind(c)
    integer(c_short), value :: my_short
    integer(c_int), value :: my_int
    integer(c_long), value :: my_long
    integer(c_long_long), value :: my_long_long
    integer(c_int8_t), value :: my_int8_t
    integer(c_int_least8_t), value :: my_int_least8_t
    integer(c_int_fast8_t), value :: my_int_fast8_t
    integer(c_int16_t), value :: my_int16_t
    integer(c_int_least16_t), value :: my_int_least16_t
    integer(c_int_fast16_t), value :: my_int_fast16_t
    integer(c_int32_t), value :: my_int32_t
    integer(c_int_least32_t), value :: my_int_least32_t
    integer(c_int_fast32_t), value :: my_int_fast32_t
    integer(c_int64_t), value :: my_int64_t
    integer(c_int_least64_t), value :: my_int_least64_t
    integer(c_int_fast64_t), value :: my_int_fast64_t
    integer(c_intmax_t), value :: my_intmax_t
    integer(c_intptr_t), value :: my_intptr_t
    real(c_float), value :: my_float
    real(c_double), value :: my_double
    real(c_long_double), value :: my_long_double
    character(c_char), value :: my_char
    logical(c_bool), value :: my_bool

    if(my_short     /= 1_c_short)     stop 1
    if(my_int       /= 2_c_int)       stop 1
    if(my_long      /= 3_c_long)      stop 1
    if(my_long_long /= 4_c_long_long) stop 1

    if(my_int8_t      /= 1_c_int8_t)        stop 1
    if(my_int_least8_t  /= 2_c_int_least8_t ) stop 1
    if(my_int_fast8_t  /= 3_c_int_fast8_t ) stop 1

    if(my_int16_t     /= 1_c_int16_t)       stop 1
    if(my_int_least16_t /= 2_c_int_least16_t) stop 1
    if(my_int_fast16_t  /= 3_c_int_fast16_t ) stop 1

    if(my_int32_t     /= 1_c_int32_t)       stop 1
    if(my_int_least32_t /= 2_c_int_least32_t) stop 1
    if(my_int_fast32_t  /= 3_c_int_fast32_t ) stop 1

    if(my_int64_t     /= 1_c_int64_t)       stop 1
    if(my_int_least64_t /= 2_c_int_least64_t) stop 1
    if(my_int_fast64_t  /= 3_c_int_fast64_t ) stop 1

    if(my_intmax_t /= 1_c_intmax_t) stop 1
    if(my_intptr_t /= 0_c_intptr_t) stop 1

    if(my_float       /= 1.0_c_float) stop 1
    if(my_double      /= 2.0_c_double) stop 1
    if(my_long_double /= 3.0_c_long_double) stop 1

    if(my_char        /= c_char_'y') stop 1
    if(my_bool      .neqv. .true._c_bool) stop 1
  end subroutine param_test
    
end module c_kind_params
