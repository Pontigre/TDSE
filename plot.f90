module plot

  use plplot

  implicit none

  call plparseopts(PL_PARSE_FULL)

  call plscol0(0, 255, 255, 255) ! white
  call plscol0(1, 255, 0, 0) ! red
  call plscol0(2, 0, 255, 0) ! green
  call plscol0(3, 0, 0, 255) ! blue
  call plscol0(4, 255, 0, 255) ! magenta
  call plscol0(5, 0, 255, 255) ! cyan
  call plscol0(6, 255, 255, 0) ! yellow
  call plscol0(7, 0, 0, 0) ! black
  call plscol0(8, 255, 76, 0) ! orange
  call plscol0(9, 128, 128, 128) ! gray

  call plinit()

  call plot_x2(0d0, 1d0, 21)

  call plend()

contains

  subroutine linspace(x1, x2, n, x)  !sets up the linearly spaced array

    real(8), intent(in) :: x1, x2
    integer, intent(in) :: n
    real(8), intent(out) :: x(n)

    real(8) :: dx
    integer :: i

    dx = (x2 - x1) / (n - 1)
    do i = 1, n
       x(i) = x1 + (i - 1) * dx
    end do


  end subroutine linspace


  subroutine plot_x2(x1, x2, n) !plots function

    real(8), intent(in) :: x1, x2
    integer, intent(in) :: n
    real(8) :: x(n), y(n)

    integer :: i


    call linspace(x1, x2, n, x)
    do i = 1, n
       y(i) = x(i)**2
    end do


    call plcol0(7)
    call plenv(x(1), x(n), y(1), y(n), 0, 0)
    call pllab("x", "y", "y=x#u2")
    
    call plcol0(1)
    call plline(x, y)
    
    call plcol0(2)
    call plpoin(x, y, 2)

 
end subroutine plot_x2


end module plot
