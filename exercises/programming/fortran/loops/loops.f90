program loops
  implicit none
  ! TODO define parameters nx and ny
  ! TODO: define real-valued array A
  integer, parameter :: nx=10
  integer, parameter :: ny=10
  integer :: i, j
  real :: A(0:nx+1,0:ny+1)
  ! TODO initialize array A here
  do i=0,ny+1
     do j=0,nx+1
        if (i==0 .and. j==0) then
           A(j,i)=20.0   
	else if (i==ny+1 .and. j==0) then
           A(j,i)=85.0
        else if (i==0 .and. j==nx+1) then
           A(j,i)=70.0
        else if (i==nx+1 .and. j==ny+1) then
           A(j,i)=5.0
        else 
           A(j,i)=65.0
        end if
     end do
  end do

  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 0, nx+1
     write(*, '(12F6.1)') A(i,:)
  end do

end program loops
