module plot

  use plplot

  implicit none

  public plot_init
  public plot_init_2D
  private linspace
  public plot_1D
  public plot_2D
  public plot_close

contains 
  subroutine plot_init(N)
    integer, intent(in) :: n
    real(8) :: xmin, xmax
    call plscol0(0, 255, 255, 255) ! white
    call plscol0(1, 255, 0, 0) ! red
    call plscol0(2, 0, 255, 0) ! green
    call plscol0(3, 0, 0, 255) ! blue
    call plscol0(7, 0, 0, 0) ! black

    call plsdev("xcairo")
    call plinit()
    xmin = 45
    xmax = 90
    call plcol0(7)
    call plenv(xmin, xmax, 0d0, 2d0, 0, 0)
  end subroutine plot_init

  subroutine plot_init_2D(N)
    integer, intent(in) :: N
    real(8) :: xmax, ymax
    call plscol0(0, 255, 255, 255) ! white
    call plscol0(1, 255, 0, 0) ! red
    call plscol0(2, 0, 255, 0) ! green
    call plscol0(3, 0, 0, 255) ! blue
    call plscol0(7, 0, 0, 0) ! black

    call plsdev("xcairo")
    call plinit()
    xmax = N-1
    ymax = N-1
    call plcol0(7)
    call plenv(0d0, xmax, 0d0, ymax, 0, 0)

  end subroutine plot_init_2D

  subroutine linspace(n, x)  !sets up the linearly spaced array

    integer, intent(in) :: n
    real(8), intent(out) :: x(n)

    real(8) :: dx=1
    integer :: i

    do i = 1, n
       x(i) = (i - 1) * dx
    end do


  end subroutine linspace


  subroutine plot_1D(N, psi, pot) !plots function
    real(8) :: xmax
    integer, intent(in) :: N
    complex(8), intent(in) :: psi(N)
    real(8) :: x(N), y(N), pot(N)
    
    integer :: i

    xmax = N-1

!    call plcol0(7)
!    call plenv(0d0, xmax, 0d0, 2d0, 0, 0)

    call plclear()

    call linspace(N, x)
    do i = 1, N
       y(i) = CDABS(psi(i))**2
    end do

    call plcol0(1)
    call plline(x, y)

    call plcol0(3)
    call plline(x, pot)

    call plcol0(2)
    call plpoin(x, y, 1)

    call plflush()


end subroutine plot_1D

subroutine plot_2D(N,psi,pot)
integer, intent(in) :: N
complex(8), intent(in) :: psi(N,N)
real(8), intent(in) :: pot(N,N)

call plimage(f, 1._plflt, 1._plflt*cDim, 1._plflt, 1._plflt*rDim, &
      -10._plflt, 10._plflt, 1._plflt, 1._plflt*cDim, 1._plflt, 1._plflt*rDim)
call plflush()
end subroutine plot_2D


subroutine plot_close()

  call plspause(.false.)
  call plend()

end subroutine plot_close

end module plot
