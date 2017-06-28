module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
  interface initialize
     module procedure initialize_read
     module procedure initialize_specify
  end interface

contains
  
  subroutine initialize_read(field0)
    ! field0 has to be an allocatable, real martrix with 2 dimensions. 
    ! Initialize then will then read from the user the number of rows and columns
    ! and then the boundary values, and central value.

    integer :: nx, ny,i , alloc_stat
    
    real , allocatable :: A(:)
    real , allocatable :: field0(:,:)
    real :: left_bound
    real :: right_bound
    real :: bot_bound
    real :: top_bound
    real :: centre_value
    write (*,*) 'Give number of rows and columns for matrix initial matrix:'
    read (*,*) nx, ny
    write (*,*) 'Give left, right, bottom, and top boundary values:'
    read (*,*) left_bound, right_bound, bot_bound,top_bound
    write (*,*) 'Give central value:'
    read (*,*) centre_value

    ! Allocate matrix now we know its size
    allocate(A((nx)*(ny)),stat=alloc_stat)
    if (alloc_stat /= 0) call abort()
    A(:)=centre_value
    allocate(field0(nx,ny),stat=alloc_stat)
    if (alloc_stat /= 0) call abort()
    field0=reshape(A,shape(field0))
    field0(1,:)=bot_bound
    field0(nx,:)=top_bound
    field0(:,ny)=right_bound
    field0(:,1)=left_bound


  end subroutine initialize_read


  subroutine initialize_specify(field0,nx,ny,left_bound,right_bound,bot_bound,top_bound,centre_value)
    ! field0 has to be an allocatable, real martrix with 2 dimensions. 
    ! Here we supply all the values as part of the subroutine instead!

    integer :: nx, ny , alloc_stat
    
    real , allocatable :: A(:)
    real , allocatable :: field0(:,:)
    real :: left_bound
    real :: right_bound
    real :: bot_bound
    real :: top_bound
    real :: centre_value

    ! Allocate matrix now we know its size
    allocate(A((nx)*(ny)),stat=alloc_stat)
    if (alloc_stat /= 0) call abort()
    A(:)=centre_value
    allocate(field0(nx,ny),stat=alloc_stat)
    if (alloc_stat /= 0) call abort()
    field0=reshape(A,shape(field0))
    field0(1,:)=bot_bound
    field0(nx,:)=top_bound
    field0(:,ny)=right_bound
    field0(:,1)=left_bound


  end subroutine initialize_specify


   
  subroutine laplacian(curr, prev)
! inserts a subroutine that computes a laplacian of the
! array "prev" and returns it as an array "curr"  end subroutine laplacian
    real , dimension(:,:), allocatable :: curr, prev
    integer, dimension(2) :: prev_shape, curr_shape
    integer :: i,j

    prev_shape=shape(prev)
    if(.not. allocated(curr)) allocate(curr(prev_shape(1),prev_shape(2)))
    curr_shape=shape(curr)
    if (all(curr_shape/=prev_shape)) then
       write(*,*) 'Error, previous array different shape from current array!'
       call abort()
    end if
    
    curr(:,:)=0.0

    do j=2,prev_shape(2)-1
       do i=2,prev_shape(1)-1
           curr(i,j)=(prev(i-1,j)-2*prev(i,j)+prev(i+1,j))/(dx*dx)+ &
               (prev(i,j-1)-2*prev(i,j)+prev(i,j+1))/(dy*dy)
       end do
    end do

  end subroutine laplacian

  subroutine write_field(array)
    real, dimension(:,:) :: array
! TODO: write a subroutine that prints "array" on screen
    integer, dimension(2) :: array_shape
    integer :: i

    array_shape(:)=shape(array)
    do i = 1 , array_shape(1)
       write(*,*) array(i,:)
    end do
  end subroutine write_field

end module laplacian_mod
