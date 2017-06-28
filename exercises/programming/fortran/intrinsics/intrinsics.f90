program intrinsics
  implicit none
  integer :: nx, ny
  integer :: i,j,k, alloc_stat
  real, dimension(:,:), allocatable :: A
  real, dimension(:), allocatable:: a1
  real :: b1
  integer :: b2
  real, dimension (:,:), allocatable :: b3
  real :: c
  logical :: d
  integer :: e
  
  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  allocate(A(nx,ny), stat = alloc_stat)
  if (alloc_stat /= 0) call abort()

  ! Initializing array
  A(:,:)  = 65.0 ! middle
  A(:,1)  = 20.0 ! left
  A(:,ny) = 70.0 ! right
  A(1,:)  = 85.0 ! top
  A(nx,:) = 5.0  ! bottom

  !--------------------------------------------------
  ! Print out the array
  do i = 1, nx
    write(*,'(*(F6.1))') A(i,:)
  end do

  
  !--------------------------------------------------
  ! TODO: use array intrinsics to probe elements of A
  allocate(a1(nx),stat=alloc_stat)
  if (alloc_stat /= 0) call abort()

  a1=sum(A,2)

  b1=maxval(A)

  b2=count(A==b1)

  allocate(b3(b2,2),stat=alloc_stat)
  if (alloc_stat /= 0) call abort()
  k=0
  do j=1,ny
     do i=1,nx
        if(A(i,j)==b1) then
           b3(k,:)=[i,j]
           k=k+1
        end if
     end do
  end do
  
  c=minval(A)

  d=all(A>=0.)
  
  e=count(A>=0.5)

  write(*,*) 'a) Sum of all elements across 2nd dim of A ' , a1

  write(*,*) 'b) location of maximum elements ' 
  do i = 1, b2
    write(*,'(*(F6.1))') b3(i,:)
  end do


  write(*,*) 'c) Minimum value of A ', c

  if(d) then 
     write(*,*) 'All elements in A are more than 0'
  else 
     write(*,*) 'Not all elements in A are more than 0'
  end if
  
  write(*,*) 'Amount of elements greater than or equal to 0.5 is ', e
  

end program intrinsics
