module initialization

  implicit none

  public init_psi
  public init_sys

contains

  subroutine init_psi(N,psi,flag)
    integer, intent(in) :: N, flag
    complex(8), intent(inout) :: psi(N)
    integer :: N_0, sigma
    integer :: x,y
    real(8) :: diff_x, diff_y

    N_0 = nint(.15*N)
    sigma = 4
    psi(:) = 0 

    if (flag == 1) then !barrier(1) or hole(2) then wave packet

       do x = 1, nint(0.3*N)
          diff_x = x-N_0
          psi(x) = exp((-diff_x**2)/(2*sigma**2)+x*dcmplx(0,1))
       end do

!!$    else !single slit(3) or double slit(4) or ball(5) then plane wave
!!$       do x=1,nint(0.2*N)
!!$          do y=1,nint(0.2*N)
!!$              diff_x = x-N_0
!!$              diff_y = y-N_0
!!$              psi(x,y) = exp((-diff_x**2)/(2*sigma**2)+x*dcmplx(0,1)+(-diff_y**2)/(2*sigma**2)+y*dcmplx(0,1))
!!$          end do
!!$       end do
    end if

  end subroutine init_psi

  subroutine init_sys(N,pot,flag)
    integer, intent(in) :: N, flag
    real(8), intent(inout) :: pot(N)
    integer :: x
    integer :: bar_x_min, bar_x_max
    
    bar_x_min = nint(.4*N)
    bar_x_max = nint(.6*N)
    pot(:) = 0

!!$    if (flag == 1) then !barrier
!!$       do x = bar_x_min, bar_x_max
!!$          pot(x) = 2d0
!!$       end do
!!$    else if (flag == 2) then !hole
!!$       do x = bar_x_min, bar_x_max
!!$          pot(x) = -2d0
!!$       end do
!!$    else if (flag == 3) then !single slit
!!$    else if (flag == 4) then !double slit
!!$    end if

  end subroutine init_sys

end module initialization
