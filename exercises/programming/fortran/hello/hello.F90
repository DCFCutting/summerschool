program hello
  implicit none
  real :: x=3.0
  real :: y=5.2
  write (*,*) 'Hello world from Fortran!'
  write (*,'(F8.3)') x*y
end program hello
