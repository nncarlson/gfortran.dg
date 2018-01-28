! { dg-do run }
program b
   integer w
   character(len=2) s, t
   s = 'xi'

   w = scan(s, 'iI')
   if (w /= 2) stop 1
   w = scan(s, 'xX', .true.)
   if (w /= 1) stop 1
   w = scan(s, 'ab')
   if (w /= 0) stop 1
   w = scan(s, 'ab', .true.)
   if (w /= 0) stop 1

   s = 'xi'
   t = 'iI'
   w = scan(s, t)
   if (w /= 2) stop 1
   t = 'xX'
   w = scan(s, t, .true.)
   if (w /= 1) stop 1
   t = 'ab'
   w = scan(s, t)
   if (w /= 0) stop 1
   w = scan(s, t, .true.)
   if (w /= 0) stop 1

end program b
   

   
