! { dg-do run }
program L
   if (and(.TRUE._1, .TRUE._1) .neqv. .true.) stop 1
   if (or(.TRUE._1, .TRUE._1) .neqv. .true.) stop 1
   if (xor(.TRUE._1, .TRUE._1) .neqv. .false.) stop 1
end program L

