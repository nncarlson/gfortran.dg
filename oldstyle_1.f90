! { dg-do run }
      integer i, j /1/, g/2/, h ! { dg-warning "" }
      integer k, l(3) /2*2,1/   ! { dg-warning "" }
      real pi /3.1416/, e       ! { dg-warning "" }

      if (j /= 1) stop 1
      if (g /= 2) stop 1
      if (any(l /= (/2,2,1/))) stop 1
      if (pi /= 3.1416) stop 1
      end
