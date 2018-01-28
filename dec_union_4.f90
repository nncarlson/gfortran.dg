! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test nested UNIONs.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  stop 1
end subroutine

! Nested unions
structure /s4/
  union ! U0                ! rax
    map
      character(16) rx
    end map
    map
      character(8) rh         ! rah
      union ! U1
        map
          character(8) rl     ! ral
        end map
        map
          character(8) ex     ! eax
        end map
        map
          character(4) eh     ! eah
          union ! U2
            map
              character(4) el ! eal
            end map
            map
              character(4) x  ! ax
            end map
            map
              character(2) h  ! ah
              character(2) l  ! al
            end map
          end union
        end map
      end union
    end map
  end union
end structure
record /s4/ r4


! Nested unions
r4.rx     =     'AAAAAAAA.BBB.C.D'

if ( r4.rx .ne. 'AAAAAAAA.BBB.C.D' ) stop 1s ("rax")
if ( r4.rh .ne. 'AAAAAAAA'         ) stop 1s ("rah")
if ( r4.rl .ne.         '.BBB.C.D' ) stop 1s ("ral")
if ( r4.ex .ne.         '.BBB.C.D' ) stop 1s ("eax")
if ( r4.eh .ne.         '.BBB'     ) stop 1s ("eah")
if ( r4.el .ne.             '.C.D' ) stop 1s ("eal")
if ( r4.x  .ne.             '.C.D' ) stop 1s ("ax")
if ( r4.h  .ne.             '.C'   ) stop 1s ("ah")
if ( r4.l  .ne.               '.D' ) stop 1s ("al")

end
