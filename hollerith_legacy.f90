! { dg-do compile }
! { dg-options "-std=legacy" }
! PR15966, PR18781 & PR16531
implicit none
complex(kind=8) x(2) 
complex a(2,2)
character*4 z
character z1(4)
character*4 z2(2,2)
character*80 line
integer i
logical l
real r
character*8 c

data x /16Habcdefghijklmnop, 16Hqrstuvwxyz012345/
data a /8H(i3),abc, 0, 4H(i4), 8H    (i9)/
data z/4h(i5)/
data z1/1h(,1hi,1h6,1h)/
data z2/4h(i7),'xxxx','xxxx','xxxx'/

z2 (1,2) = 4h(i8)
i = 4hHell
l = 4Ho wo	! { dg-warning "has undefined result" }
r = 4Hrld! 
write (line, '(3A4)') i, l, r
if (line .ne. 'Hello world!') stop 1
i = 2Hab
r = 2Hab
l = 2Hab	! { dg-warning "has undefined result" }
c = 2Hab
write (line, '(3A4, 8A)') i, l, r, c
if (line .ne. 'ab  ab  ab  ab      ') stop 1

write(line, '(4A8, "!")' ) x
if (line .ne. 'abcdefghijklmnopqrstuvwxyz012345!') stop 1

write (line, a) 3
if (line .ne. '  3') stop 1
write (line, a (1,2)) 4
if (line .ne. '   4') stop 1
write (line, z) 5
if (line .ne. '    5') stop 1
write (line, z1) 6
if (line .ne. '     6') stop 1
write (line, z2) 7
if (line .ne. '      7') stop 1
write (line, z2 (1,2)) 8
if (line .ne. '       8') stop 1
write (line, '(16A)') z2
if (line .ne. '(i7)xxxx(i8)xxxx') stop 1
call test (8h   hello)
end

subroutine test (h)
integer(kind=8) h
character*80 line

write (line, '(8a)') h
if (line .ne. '   hello') stop 1
end subroutine
