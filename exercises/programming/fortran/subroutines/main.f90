program subroutines
  use laplacian_mod
  implicit none
  real, allocatable :: previous(:,:), current(:,:)
  
  ! initialize the array
  call initialize(previous)

  write(*,*) 'Previous array'
  call write_field(previous)

  ! compute the Laplacian
  call laplacian(current, previous)

  ! print the result array
  write(*,*) 'Current array'
  call write_field(current)
 
end program subroutines

