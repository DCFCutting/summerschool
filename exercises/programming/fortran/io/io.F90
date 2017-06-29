module io

contains

  ! Reads the temperature distribution from an input file
  subroutine read_field(field, filename)
    implicit none
    logical :: file_exist
    real, dimension(:,:), allocatable, intent(out) :: field
    character(len=*), intent(in) :: filename
    integer :: nx, ny, i
    character(len=2):: dummy 
    integer :: iu=10
    logical :: iu_opened
    ! TODO: implement function that will:
    ! open the file
    inquire(file=filename,exist=file_exist)
    if (.not. file_exist) then
       write(*,*) 'The file does not exist'
       call abort()
    end if
    inquire(iu,opened=iu_opened)
    do while (iu_opened)
       iu=iu+1
       inquire(iu,opened=iu_opened)
    end do
    open(iu,file=filename,status='old',action='read')    
    ! read the first header line to get nx and ny
    read(iu,*) dummy,nx,ny
    ! allocate matrix called field
    allocate(field(nx,ny))
    
    ! read rest of the file into field
    do i=1,nx
       read(iu,*) field(i,:)
    end do
    
    
    ! close the file

    close(iu)


  end subroutine read_field

  ! Output routine, saves the temperature distribution as a png image
  subroutine write_field(field, iter)
    use iso_fortran_env, only : REAL64
    use pngwriter
    implicit none

    integer, parameter :: dp = REAL64
    real(kind=REAL64), intent(in) :: field(:,:)
    integer, intent(in) :: iter

    character(len=85) :: filename
    integer :: nx, ny, stat

    nx = size(field, 1)
    ny = size(field, 2)


    write(filename,'(A5,I4.4,A4,A)')  'heat_', iter, '.png'
    stat = save_png(real(field, kind=dp), nx, ny, filename)

  end subroutine write_field

end module io
