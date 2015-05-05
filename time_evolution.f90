module time_evolution

  implicit none

  public time_evo_1D
  public time_evo_2D
  private dx2
  private dx2_2D
  private dy2

contains

subroutine time_evo_1D(N,meth,psi,pot,dt)
integer, intent(in) :: meth, N
complex(8), intent(inout) :: psi(N)
real(8), intent(in) :: pot(N)
real(8), intent(in) :: dt
complex(8) :: psi_p(N), k1(N), k2(N), k3(N), k4(N)


 if (meth == 1) then
    call dx2(N,psi,psi_p)
    psi(:) = dt*(-psi_p(:)+pot(:)*psi(:))/dcmplx(0,1) + psi(:)
 else 
    call dx2(N,psi,k1)
    call dx2(N,psi+k1*dt/2,k2)
    call dx2(N,psi+k2*dt/2,k3)
    call dx2(N,psi+k3*dt,k4)
    psi(:) = dt*(-(k1+2*k2+2*k3+k4)/6+pot(:)*psi(:))/dcmplx(0,1) + psi(:)
 end if
end subroutine time_evo_1D

subroutine time_evo_2D(N,meth,psi,pot,dt)
integer, intent(in) :: meth, N
complex(8), intent(inout) :: psi(N,N)
real(8), intent(in) :: pot(N,N)
real(8), intent(in) :: dt
complex(8) :: psi_px(N,N), psi_py(N,N)
complex(8) :: kx1(N,N), kx2(N,N), kx3(N,N), kx4(N,N)
complex(8) :: ky1(N,N), ky2(N,N), ky3(N,N), ky4(N,N)
 if (meth == 1) then
    call dx2_2D(N,psi,psi_px)
    call dy2(N,psi,psi_py)
    psi(:,:) = dt*(-(psi_px(:,:)+psi_py(:,:))+pot(:,:)*psi(:,:))/dcmplx(0,1) + psi(:,:)
 else 
    call dx2_2D(N,psi,kx1)
    call dx2_2D(N,psi+kx1*dt/2,kx2)
    call dx2_2D(N,psi+kx2*dt/2,kx3)
    call dx2_2D(N,psi+kx3*dt,kx4)
    call dy2(N,psi,ky1)
    call dy2(N,psi+ky1*dt/2,ky2)
    call dy2(N,psi+ky2*dt/2,ky3)
    call dy2(N,psi+ky3*dt,ky4) 
    psi(:,:) = dt*(-(((kx1+2*kx2+2*kx3+kx4)/6)+((ky1+2*ky2+2*ky3+ky4)/6))+pot(:,:)*psi(:,:))/dcmplx(0,1) + psi(:,:)
 end if
end subroutine time_evo_2D

subroutine dx2(N,psi,psi_p)
  integer, intent(in) :: N
  complex(8), intent(in) :: psi(N)
  complex(8), intent(out) :: psi_p(N)
  integer :: i

  psi_p(1) = -(psi(2) - 2*psi(1) + psi(N))
  psi_p(N) = 0 ! psi(1) - 2*psi(1) + psi(N-1)
  do i = 2, N-1
     psi_p(i) = psi(i+1) - 2*psi(i) + psi(i-1)
  end do

end subroutine dx2

subroutine dx2_2D(N,psi,psi_p)
  integer, intent(in) :: N
  complex(8), intent(in) :: psi(N,N)
  complex(8), intent(out) :: psi_p(N,N)
  integer :: i

  psi_p(1,:) = -(psi(2,:) - 2*psi(1,:) + psi(N,:))
  psi_p(N,:) = 0 ! psi(1) - 2*psi(1) + psi(N-1)
  do i = 2, N-1
     psi_p(i,:) = psi(i+1,:) - 2*psi(i,:) + psi(i-1,:)
  end do

end subroutine dx2_2D

subroutine dy2(N,psi,psi_p)
  integer, intent(in) :: N
  complex(8), intent(in) :: psi(N,N)
  complex(8), intent(out) :: psi_p(N,N)
  integer :: i,x

  do x = 1, N
     psi_p(x,1) = -(psi(x,2) - 2*psi(x,1) + psi(x,N))
     psi_p(x,N) = 0 ! psi(1) - 2*psi(1) + psi(N-1)
     do i = 2, N-1
        psi_p(x,i) = psi(x,i+1) - 2*psi(x,i) + psi(x,i-1)
     end do
  end do

end subroutine dy2

end module time_evolution
