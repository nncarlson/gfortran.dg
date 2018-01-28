!{ dg-do run }
!{ dg-options -std=gnu }
! Tests namelist read when more data is provided then specified by 
! array qualifier in list.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>.
      program pr24459
      implicit none
      integer nd, ier, i, j
      parameter ( nd = 5 )
      character*(8) names(nd,nd)
      character*(8) names2(nd,nd)
      character*(8) names3(nd,nd)
      namelist / mynml /  names, names2, names3
      open(unit=20,status='scratch', delim='apostrophe')
      write (20, '(a)') "&MYNML"
      write (20, '(a)') "NAMES = 25*'0'"
      write (20, '(a)') "NAMES2 = 25*'0'"
      write (20, '(a)') "NAMES3 = 25*'0'"
      write (20, '(a)') "NAMES(2,2) = 'frogger'"
      write (20, '(a)') "NAMES(1,1) = 'E123' 'E456' 'D789' 'P135' 'P246'"
      write (20, '(a)') "NAMES2(1:5:2,2) = 'abcde' 'fghij' 'klmno'"
      write (20, '(a)') "NAMES3 = 'E123' 'E456' 'D789' 'P135' 'P246' '0' 'frogger'"
      write (20, '(a)') "/"
      rewind(20)
      read(20,nml=mynml, iostat=ier)
      if (ier.ne.0) stop 1
      if (any(names(:,3:5).ne."0")) stop 1
      if (names(2,2).ne."frogger") stop 1
      if (names(1,1).ne."E123") stop 1
      if (names(2,1).ne."E456") stop 1
      if (names(3,1).ne."D789") stop 1
      if (names(4,1).ne."P135") stop 1
      if (names(5,1).ne."P246") stop 1
      if (any(names2(:,1).ne."0")) stop 1
      if (any(names2(:,3:5).ne."0")) stop 1
      if (names2(1,2).ne."abcde") stop 1
      if (names2(2,2).ne."0") stop 1
      if (names2(3,2).ne."fghij") stop 1
      if (names2(4,2).ne."0") stop 1
      if (names2(5,2).ne."klmno") stop 1
      if (any(names3.ne.names)) stop 1
      end
