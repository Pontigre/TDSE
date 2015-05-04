module initialization

  implicit none

  public init_psi
  public init_sys

contains

  subroutine init_psi(N,psi,psi2,flag)
    integer, intent(in) :: N, flag
    complex(8), intent(inout) :: psi(N), psi2(N,N)
    integer :: N_0, sigma
    integer :: x, y
    real(8) :: diff_x, diff_y

    !N_0 = nint(.15*N)
    N_0 = 50
    sigma = 5 
    psi(:) = 0 
    psi2(:,:) = 0 
    if (flag == 1) then !barrier(1) or hole(2) then wave packet
       !do x = 2, !nint(0.3*N)
        do x = 39,61
	  diff_x = x-N_0
          psi(x) = exp(-(diff_x**2)/(2*sigma**2)+x*dcmplx(0,1))
       end do
    else !single slit(3) or double slit(4) or ball(5) then plane wave
       do x=1,nint(0.2*N)
          do y=1,nint(0.2*N)
              diff_x = x-N_0
              diff_y = y-N_0
              psi2(x,y) = exp(-(diff_x**2)/(2*sigma**2)+x*dcmplx(0,1))
          end do
       end do
    end if

  end subroutine init_psi

  subroutine init_sys(N,pot,pot2,flag)
    integer, intent(in) :: N, flag
    real(8), intent(inout) :: pot(N), pot2(N,N)
    integer :: x,y
    integer :: bar_x_min, bar_x_max
    
    !bar_x_min = nint(.45*N)
    !bar_x_max = nint(.55*N)
    bar_x_min = 70
    bar_x_max = 80

    pot(:) = 0

    if (flag == 1) then !barrier
       do x = bar_x_min, bar_x_max
          pot(x) = 1d0
       end do
    else if (flag == 2) then !hole
       do x = bar_x_min, bar_x_max
          pot(x) = -1d0
       end do
    else if (flag == 3) then !single slit
       do x = bar_x_min, bar_x_max
          do y = 1, nint(.49*N)
             pot2(x,y) = 10d0
          end do
          do y = nint(.51*N), N
             pot2(x,y) = 10d0
          end do
       end do
    else if (flag == 4) then !double slit
       do x = bar_x_min, bar_x_max
          do y = 1, nint(.47*N)
             pot2(x,y) = 10d0
          end do
          do y =  nint(.49*N), nint(.51*N)
             pot2(x,y) = 10d0
          end do
          do y = nint(.53*N), N
             pot2(x,y) = 10d0
          end do
       end do
    end if

  end subroutine init_sys

end module initialization
