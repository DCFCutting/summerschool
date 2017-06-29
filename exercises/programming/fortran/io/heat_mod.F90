module heat
  use iso_fortran_env, only : REAL64
  type temperature_field
     integer :: nx , ny
     real(kind=REAL64) :: dx, dy
     real(kind=REAL64), allocatable:: field(:,:)
  end type temperature_field


contains

  subroutine initialise_temp_field(field,nx,ny)
    integer, intent(in):: nx,ny
    real :: grid_spacing=0.01
    type (temperature_field), intent(out) :: field
    field % nx=nx
    field % ny=ny
    field % dx=grid_spacing
    field % dy=grid_spacing
    allocate(field%field(nx,ny))
  end subroutine initialise_temp_field

  
end module heat
