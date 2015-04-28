module initialization

  implicit none

  public init_psi
  public init_sys

contains

  subroutine init_psi(N,psi,flag)
    integer, intent(in) :: N, flag
    complex(8), intent(inout) :: psi(N)
    integer :: N_0, sigma
    integer :: i,j
    real(8) :: diff_x, diff_y

    N_0 = nint(.15*N)
    sigma = 2
    psi(:) = 0 
    if (flag == 1) then !barrier(1) or hole(2) then wave packet
       do i = 1, nint(0.3*N)
          diff_x = i-N_0
          psi(i) = exp((-diff_x**2+dcmplx(0,1)*i)/(2*sigma**2))
       end do
!!$    else !single slit(3) or double slit(4) or ball(5) then plane wave
!!$       do i=1,nint(0.2*N)
!!$          do j=1,nint(0.2*N)
!!$              diff_x = i-N_0
!!$              diff_y = j-N_0
!!$psi(i,j) =  exp((-diff_x**2)/(2*sigma))* exp((-diff_y**2)/(2*sigma))
!!$          end do
!!$       end do
    end if

  end subroutine init_psi

  subroutine init_sys(N,pot,flag)
    integer, intent(in) :: N, flag
    real(8), intent(inout) :: pot(N)
    integer :: i
    integer :: bar_x_min, bar_x_max
    
    bar_x_min = nint(.4*N)
    bar_x_max = nint(.6*N)
    pot(:) = 0

    if (flag == 1) then !barrier
       do i = bar_x_min, bar_x_max
          pot(i) = 2d0
       end do
    else if (flag == 2) then !hole
       do i = bar_x_min, bar_x_max
          pot(i) = -2d0
       end do
!    else if (flag == 3) then !single slit

!    else if (flag == 4) then !double slit

!    else if (flag == 5) then !ball

    end if

  end subroutine init_sys

end module initialization
