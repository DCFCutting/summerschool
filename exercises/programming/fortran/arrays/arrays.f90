program arrays
  implicit none
  integer :: nx, ny
  integer :: i, alloc_stat
  real , allocatable :: A(:)
  real , allocatable :: B(:,:)
  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  ! TODO allocate A now that we know nx and ny
  allocate(A((nx)*(ny)),stat=alloc_stat)
  if (alloc_stat /= 0) call abort()
  A(:)=65.0
  allocate(B(nx,ny),stat=alloc_stat)
  if (alloc_stat /= 0) call abort()
  B=reshape(A,shape(B))
  B(1,1)=70.0
  B(nx,1)=85.0
  B(1,ny)=60.0
  B(nx,ny)=5.0
 
  !--------------------------------------------------
  ! Print out the array
  do i = 1, nx
    write(*,'(*(F6.1))') B(i,:)
  end do


end program arrays
