module plot

  use plplot

  implicit none

  public plot_init
  public plot_init_2D
  public plot_1D
  public plot_2D
  public plot_close

contains 
  subroutine plot_init()
    real(8) :: xmin, xmax
    call plscol0(0, 255, 255, 255) ! white
    call plscol0(1, 255, 0, 0) ! red
    call plscol0(2, 0, 255, 0) ! green
    call plscol0(3, 0, 0, 255) ! blue
    call plscol0(7, 0, 0, 0) ! black

    call plsdev("xcairo")
    call plinit()
    xmin = 20
    xmax = 90
    call plcol0(7)
    call plenv(xmin, xmax, -1d0, 2d0, 0, 0)
  end subroutine plot_init

  subroutine plot_init_2D()
    real(8) :: xmin, xmax, ymin, ymax
    call plscol0(0, 255, 255, 255) ! white
    call plscol0(1, 255, 0, 0) ! red
    call plscol0(2, 0, 255, 0) ! green
    call plscol0(3, 0, 0, 255) ! blue
    call plscol0(7, 0, 0, 0) ! black

    call plsdev("xcairo")
    call plinit()
    xmin = 20
    xmax = 90
    ymin = 20
    ymax = 90
    call plcol0(7)
    call plenv(xmin, xmax, ymin, ymax, 0, 0)

  end subroutine plot_init_2D

  subroutine plot_1D(N, psi, pot) !plots function
    integer, intent(in) :: N
    complex(8), intent(in) :: psi(N)
    real(8) :: x(N), y(N), pot(N)
    real(8) :: dx=1
    integer :: i

    call plclear()

    do i = 1, n
       x(i) = (i - 1) * dx
    end do

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

subroutine plot_2D(N,psi)
integer, intent(in) :: N
complex(8), intent(in) :: psi(N,N)
real(8) :: size, xmin, xmax, ymin, ymax, zmin, zmax, rpsi(N,N)
size = N
xmin = 20
xmax = 90
ymin = 20
ymax = 90
zmin = 0d0
zmax = 2d0
rpsi(:,:) = CDABS(psi(:,:))**2

!call plclear()
!call plimage(idata, xmin, xmax, ymin, ymax, zmin, zmax, Dxmin, Dxmax, Dymin, Dymax)
call plimage(rpsi, xmin, xmax, ymin, ymax, 0d0, 2d0, xmin, xmax, ymin, ymax)
call plflush()

end subroutine plot_2D


subroutine plot_close()

  call plspause(.false.)
  call plend()

end subroutine plot_close

end module plot
