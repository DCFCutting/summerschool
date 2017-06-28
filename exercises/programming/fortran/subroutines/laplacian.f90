module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(field0)
    integer :: nx, ny
    integer :: i, alloc_stat
    real , allocatable :: A(:)
    real , allocatable :: B(:,:)
    real :: left_bound
    real :: right_bound
    real :: bot_bound
    real :: top_bound
    real :: default_value
    write (*,*) 'Give number of rows and columns for matrix A:'
    read (*,*) nx, ny
    write (*,*) 'Give left, right, bottom, and top boundary values:'
    read (*,*) left_bound, right_bound, bot_bound,top_bound
    write (*,*) 'Give middle value:'
    read (*,*) default_value

    ! TODO allocate A now that we know nx and ny
    allocate(A((nx)*(ny)),stat=alloc_stat)
    if (alloc_stat /= 0) call abort()
    A(:)=default_value
    allocate(B(nx,ny),stat=alloc_stat)
    if (alloc_stat /= 0) call abort()
    B=reshape(A,shape(B))
    B(1,:)=bot_bound
    B(nx,:)=top_bound
    B(:,ny)=right_bound
    B(:,1)=left_bound


  end subroutine initialize
   
  subroutine laplacian(curr, prev)
! TODO: insert a subroutine that computes a laplacian of the
! array "prev" and returns it as an array "curr"
  end subroutine laplacian

  subroutine write_field(array)
! TODO: write a subroutine that prints "array" on screen
  end subroutine write_field

end module laplacian_mod
