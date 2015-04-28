module time_evolution

  implicit none

  public time_evo
!  private runge_kutta_4
  private dx2

contains

subroutine time_evo(N,meth,psi,pot,t,timestep)
integer, intent(in) :: meth, N
complex(8), intent(inout) :: psi(N)
real(8), intent(in) :: pot(N)
real(8), intent(in) :: t, timestep
complex(8) :: psi_p(N)
 if (meth == 1) then
!!$    call runge_kutta_4(N,psi,pot,t,timestep)
!!$ else 
    call dx2(N,psi,psi_p)
    psi(:) = timestep*(dcmplx(0.0,1.0)*psi_p(:)+pot(:)) + psi(:)
 end if
end subroutine time_evo

!!$subroutine runge_kutta_4(N,psi,pot,t,timestep)
!!$integer, intent(in) :: N
!!$real(8), intent(inout) :: psi(N)
!!$real(8), intent(in) :: t, timestep, pot(N)
!!$real(8) :: k1(N), k2(N), k3(N), k4(N)
!!$
!!$call dx2(N,psi,k1)
!!$write(*,*) 'k1 =' k1
!!$call dx2(N,psi+k1*timestep/2,k2)
!!$write(*,*) 'k2 =' k2
!!$call dx2(N,psi+k2*timestep/2,k3)!!$write(*,*) 'k3 =' k3
!!$call dx2(N,psi+k3*timestep,k4)
!!$write(*,*) 'k4 =' k4
!!$
!!$end subroutine runge_kutta_4

subroutine dx2(N,psi,psi_p)
integer, intent(in) :: N
complex(8), intent(in) :: psi(N)
complex(8), intent(out) :: psi_p(N)
integer :: i

psi_p(1) = psi(2) - 2*psi(1) + psi(N)
psi_p(N) = psi(1) - 2*psi(1) + psi(N-1)
do i = 2, N-1
   psi_p(i) = psi(i+1) - 2*psi(i) + psi(i-1)
end do

end subroutine dx2

end module time_evolution
