program main
  use io
  use heat
  implicit none
  real, dimension(:,:), allocatable :: field
  type (temperature_field) :: temp_field
  integer,dimension(2) :: field_shape
  
  call read_field(field, 'bottle.dat')

  field_shape=shape(field)

  call initialise_temp_field(temp_field,field_shape(1),field_shape(2))
  temp_field%field=field
  
  call write_field(temp_field%field, 0)

end program main
