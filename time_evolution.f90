module time_evolution

  implicit none
!!$
!!$  public time_evo
!!$  private runge_kutta_4
!!$  private euler
!!$  private schrodinger
!!$
!!$contains
!!$
!!$subroutine time_evo(N,meth,psi,t,timestep)
!!$integer, intent(in) :: meth, N
!!$real(8), intent(inout) :: psi(N)
!!$real(8), intent(in) :: t, timestep
!!$ if (meth == 1) then
!!$    call runge_kutta_4(N,psi,pot,t,timestep)
!!$ else 
!!$    call euler(N,psi,pot,t,timestep)
!!$end subroutine time_evo
!!$
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
!!$call dx2(N,psi+k2*timestep/2,k3)
!!$write(*,*) 'k3 =' k3
!!$call dx2(N,psi+k3*timestep,k4)
!!$write(*,*) 'k4 =' k4
!!$
!!$end subroutine runge_kutta_4
!!$
!!$subroutine euler(N,psi,pot,t,timestep)
!!$integer, intent(in) :: N
!!$real(8), intent(inout) :: psi(N)
!!$real(8), intent(in) :: t, timestep, pot(N)
!!$real(8) :: psi_1(N)
!!$
!!$call dx2(N,psi,psi_1)
!!$psi(i) = timestep*(-psi_1(i)+pot(i)) + psi(i)
!!$
!!$
!!$end subroutine euler
!!$
!!$subroutine dx2(N,psi,psi_1)
!!$integer, intent(in) :: N
!!$real(8), intent(in) :: psi(N)
!!$real(8) :: psi_p(N)
!!$real(8), intent(out) :: psi_1(N)
!!$
!!$psi_p(1) = psi(2) - 2*psi(1) + psi(N)
!!$psi_p(N) = psi(1) - 2*psi(1) + psi(N-1)
!!$do i = 2, N-1
!!$   psi_p(i) = psi(i+1) - 2*psi(i) + psi(i-1)
!!$end do
!!$
!!$psi_1(1) = psi_p(2) - 2*psi_p(1) + psi_p(N)
!!$psi_1(N) = psi(1) - 2*psi_p(1) + psi_p(N-1)
!!$do i = 2, N-1
!!$   psi_1(i) = psi_p(i+1) - 2*psi_p(i) + psi_p(i-1)
!!$end do
!!$
!!$end subroutine dx2

end module time_evolution
