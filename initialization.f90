module initialization

  implicit none

  public init_psi
  public init_sys

contains

  subroutine init_psi(N,psi,psi2,flag)
    integer, intent(in) :: N, flag
    complex(8), intent(inout) :: psi(N), psi2(N,N)
    integer :: N_0, sigma
    integer :: x
    real(8) :: diff_x, p

    N_0 = 37
    sigma = 5 
    p = 1
    psi(:) = 0 
    psi2(:,:) = 0 
    if (flag == 1 .or. flag == 2) then !barrier(1) or hole(2) then wave packet
       do x = N_0-15, N_0+15
           diff_x = x-N_0
           psi(x) = exp(-(diff_x**2)/(2*sigma**2)+x*p*dcmplx(0,1))
       end do
    else !single slit(3) or double slit(4) or ball(5) then plane wave
       do x= N_0-15, N_0+15
          diff_x = x-N_0
          psi2(x,:) = exp(-(diff_x**2)/(2*sigma**2)+x*p*dcmplx(0,1))
       end do
    end if

  end subroutine init_psi

  subroutine init_sys(N,pot,pot2,flag)
    integer, intent(in) :: N, flag
    real(8), intent(inout) :: pot(N), pot2(N,N)
    integer :: x, y 
    integer :: bar_x_min, bar_x_max
   
    bar_x_min = 47
    bar_x_max = 53

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
          do y = 1, 47
             pot2(x,y) = 10d0
          end do
          do y = 53, N
             pot2(x,y) = 10d0
          end do
       end do
    else if (flag == 4) then !double slit
       do x = bar_x_min, bar_x_max
          do y = 1, 45
             pot2(x,y) = 10d0
          end do
          do y = 47, 53
             pot2(x,y) = 10d0
          end do
          do y = 55, N
             pot2(x,y) = 10d0
          end do
       end do
    end if

  end subroutine init_sys

end module initialization
