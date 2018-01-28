c { dg-do run }
      DO I = 0, 255
         IF (ICHAR(CHAR(I)) .NE. I) stop 1
      ENDDO
      END
